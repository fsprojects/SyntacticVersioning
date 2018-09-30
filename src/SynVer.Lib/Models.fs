namespace SynVer
open Operators

type Version = Major | Minor | Patch
with
    override m.ToString ()= match m with | Major -> "Major" | Minor -> "Minor" | Patch -> "Patch"

type NetType =
  | Abstract=0 | Class=1 | Enum=2 | Interface=3 | Static=4 | Struct=5
  (* | Primitive | ValueType *)
  | Module =6| RecordType=7 | SumType=8 | UnionConstructor=9 | UnionTags=10

type InstanceOrStatic = | Static=0 | Instance=1

type Name = string

type Typ = { FullName:string }
with
    override m.ToString ()= m.FullName

type Parameter = { Type:Typ; Name:Name }
with
  static member ToString x=
    if x.Name<>null then
      sprintf "%s:%O" x.Name (x.Type)
    else
      sprintf "%O" x.Type
  static member Create (typ:Typ,name:Name) : Parameter= { Type=typ ; Name=name }
  override x.ToString() = Parameter.ToString x

type ConstructorLike= {Type:Typ; Parameters:Parameter list}
type MethodLike = { Type:Typ; Instance:InstanceOrStatic; Name: Name; Parameters: Parameter list; Result: Typ}

type FieldLike = {Type:Typ; Instance:InstanceOrStatic; Name: Name; Result: Typ}


type Member= 
    |RecordConstructor of ConstructorLike
    |Constructor of ConstructorLike
    |Event of MethodLike
    |Field of FieldLike
    |Method of MethodLike
    |Property of FieldLike
    |UnionConstructor of Typ*ConstructorLike
    |UnionCase of Typ* Name * Parameter list
    |EnumValue of Typ* Name * string
with
  static member private WhenStatic (flag:InstanceOrStatic) (typ:Typ) : string =
    match flag with
      | InstanceOrStatic.Static -> typ.FullName
      | InstanceOrStatic.Instance -> sprintf "(Instance of %s)" typ.FullName
      | _ -> failwith "!"

  static member private Parameters (prms:Parameter list) : string =
    match prms with
      | [] -> "System.Void"
      | _  ->
        let ps =
          prms
          |> List.map(Parameter.ToString)
        let ps' =
              match List.isEmpty ps with
                | true  -> [| |]
                | false -> [| ps |> String.concat " * " |]
        
        ps'
        |> String.concat " -> "
  static member internal isUnionCase = function | UnionCase(_)-> true | _ -> false
  static member internal isEnumValue = function | EnumValue(_)-> true | _ -> false

  static member internal UnionCaseToString m
      =
      match m with
      |  UnionCase (_, name, fields) ->
          let ps =
              match fields with
                  | [] -> ""
                  | fields ->
                  fields
                  |> List.map Parameter.ToString
                  |> String.concat ""
          match System.String.IsNullOrEmpty ps with
              | true -> sprintf "%s" name
              | false -> sprintf "%s of %s" name ps
      | _ -> failwith "Expected union case!"

  static member internal EnumValueToString m
      =
      match m with
      | EnumValue (_,name,value) ->
          sprintf "%s:%s" name value
      | _ -> failwith "Expected enum value!"

  override m.ToString ()=
    match m with
    | RecordConstructor {Type=typ;Parameters=prms}->
      sprintf "{ %s.%s } -> %s" 
        typ.FullName
        (Member.Parameters prms)
        typ.FullName
    | Constructor {Type=typ;Parameters=prms}-> 
        sprintf "new %s : %s -> %s" 
          typ.FullName
          (Member.Parameters prms)
          typ.FullName
    | UnionConstructor (typ,{Type=typ';Parameters=prms})->
        sprintf "%s : %s -> %s"
          typ.FullName
          (Member.Parameters prms)
          typ'.FullName
    | Event {Type=typ; Instance= isStatic; Name=name; Parameters=prms; Result=rtyp}->
      sprintf "%s.%s : %s -> %s"
        (Member.WhenStatic isStatic typ)
        (sprintf "%s.Add" name)
        (Member.Parameters prms)
        (rtyp.FullName)           
    | Field {Type=typ; Instance= isStatic; Name=name; Result=rtyp} -> 
        sprintf "%s.%s : %s"
          (Member.WhenStatic isStatic typ)
          name
          (rtyp.FullName)
    | Method {Type=typ; Instance= isStatic; Name=name; Parameters=prms; Result=rtyp} -> 
        sprintf "%s.%s : %s -> %s"
          (Member.WhenStatic isStatic typ)
          name
          (Member.Parameters prms)
          (rtyp.FullName)
    | Property {Type=typ; Instance= isStatic; Name=name; Result=ptyp} ->
        sprintf "%s.%s : %s"
          (Member.WhenStatic isStatic typ)
          name
          ptyp.FullName
    | UnionCase (typ, _, _) ->
       sprintf "%s.%s" typ.FullName (Member.UnionCaseToString m)
    | EnumValue (typ,_,_) ->
       sprintf "%s.%s" typ.FullName (Member.EnumValueToString m)

type UnionCases =
     {
         Type:Typ
         Cases:Member list
     }
with
  override t.ToString()=
     t.Cases
     |> List.map Member.UnionCaseToString
     |> List.sort
     |> String.concat " | "
     |> fun s ->
       let t' = t.Type.FullName
       sprintf "%s values: [ %s ]" t' s

type SurfaceOfType =
    { 
        Type:Typ
        NetType: NetType
        Members: Member list
        SumType: bool
        BaseType: Typ option
    }
with
  /// Create an instance of Surface of type with the required members set
  static member internal Create t netType members sumType parent=
    {
      Type=t; NetType=netType;Members=members; SumType=sumType; BaseType =parent
    }
  member public this.Enum
          = if this.NetType = NetType.Enum then
              let enumV =
                  this.Members
                  |>List.filter Member.isEnumValue
                  |>List.map (function 
                              | EnumValue (_,name,value)->(name,value) 
                              | _ -> failwith "!")
              Some enumV 
            else
              None
  member public this.UnionCases 
          = if this.NetType = NetType.SumType then
              let cases =
                  this.Members
                  |>List.filter Member.isUnionCase
              Some { Type=this.Type; Cases= cases}
            else
              None
  override x.ToString()=sprintf "%A" x

  static member IsSumType (x:SurfaceOfType) = x.SumType
  static member GetNetType (x:SurfaceOfType) = x.NetType
  [<CompiledName("Typ")>]static member typ (x:SurfaceOfType) = x.Type

type Namespace=
    {
      Namespace:string
      Types: SurfaceOfType list
    }
with
  override x.ToString()=sprintf "%A" x
  [<CompiledName("Name")>]static member name (n:Namespace) = n.Namespace
type Package=
    {
      Namespaces: Namespace list
    }
with
  override x.ToString()=sprintf "%A" x

type 'a AddedAndRemoved when 'a:comparison=
    {
        Added: Set<'a>
        Removed: Set<'a>
    }
with
  override x.ToString()=sprintf "%A" x
  member this.IsEmpty=this.Added.IsEmpty && this.Removed.IsEmpty
module AddedAndRemoved=
  let isEmpty (c:AddedAndRemoved<_>)=c.IsEmpty
type Changes<'t,'m when 't:comparison>=
    {
        Diff: AddedAndRemoved<'t>
        Changes: Map<'t,'m>
    }
with
  override x.ToString()=sprintf "%A" x
  member this.IsEmpty=this.Diff.IsEmpty && this.Changes.IsEmpty
module Changes=
  let isEmpty (c:Changes<_,_>)=c.IsEmpty
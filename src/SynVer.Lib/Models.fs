namespace SynVer
open Chiron
open Operators
open Patterns
type Version = Major | Minor | Patch
with
    override m.ToString ()= match m with | Major -> "Major" | Minor -> "Minor" | Patch -> "Patch"

type NetType =
  | Abstract=0 | Class=1 | Enum=2 | Interface=3 | Static=4 | Struct=5
  (* | Primitive | ValueType *)
  | Module =6| RecordType=7 | SumType=8 | UnionConstructor=9 | UnionTags=10

type InstanceOrStatic = | Static=0 | Instance=1

type Name = string

module private Json =
  let fromEnum (e : 'e when 'e :> System.Enum and 'e : (new:unit->'e) and 'e : struct) =
      e.ToString "G"
      |> String
  let toEnum<'e when 'e :> System.Enum and 'e : (new:unit->'e) and 'e : struct> json =
      match json with
      | String s ->
          match System.Enum.TryParse<'e> s with
          | true, x -> Value x
          | _ -> Error (sprintf "Unable to parse %s as a %s" s typeof<'e>.Name)
      | _ -> Error (sprintf "Unable to parse %A as a %s" json typeof<'e>.Name)

type Typ = { FullName:string }
with
    override m.ToString ()= m.FullName
    static member ToJson (x:Typ) =
      Json.write "typ" x.FullName
    static member FromJson (_:Typ) =
        fun n -> { FullName = n}
      <!> Json.read "typ"

type Parameter = { Type:Typ; Name:Name }
with
  static member ToString x=
    if x.Name<>null then
      sprintf "%s:%O" x.Name (x.Type)
    else
      sprintf "%O" x.Type
  static member Create (typ:Typ,name:Name) : Parameter= { Type=typ ; Name=name }
  override x.ToString() = Parameter.ToString x
  static member FromJson (_:Parameter) =
          fun t n -> { Type = t; Name = n }
      <!> Json.read "typ"
      <*> Json.read "name"
  static member ToJson (x:Parameter) =
          Json.write "typ" x.Type
       *> Json.write "name" x.Name
type ConstructorLike= {Type:Typ; Parameters:Parameter list}
with
  static member FromJson (_:ConstructorLike) =
          fun t p -> { Type = t; Parameters = p }
      <!> Json.read "typ"
      <*> Json.read "params"

  static member ToJson (x:ConstructorLike) =
          Json.write "typ" x.Type
       *> Json.write "params" x.Parameters
type MethodLike = { Type:Typ; Instance:InstanceOrStatic; Name: Name; Parameters: Parameter list; Result: Typ}
with
  static member FromJson (_:MethodLike) =
          fun t p i n r -> { Type = t; Parameters = p; Instance=i; Name=n; Result=r }
      <!> Json.read "typ"
      <*> Json.read "params"
      <*> Json.readWith Json.toEnum<InstanceOrStatic> "instance"
      <*> Json.read "name"
      <*> Json.read "result"

  static member ToJson (x:MethodLike) =
          Json.write "typ" x.Type
       *> Json.write "params" x.Parameters
       *> Json.writeWith Json.fromEnum "instance" x.Instance
       *> Json.write "name" x.Name
       *> Json.write "result" x.Result


type FieldLike = {Type:Typ; Instance:InstanceOrStatic; Name: Name; Result: Typ}
with
  static member FromJson (_:FieldLike) =
          fun t i n r -> { Type = t; Instance=i; Name=n; Result=r }
      <!> Json.read "typ"
      <*> Json.readWith Json.toEnum<InstanceOrStatic> "instance"
      <*> Json.read "name"
      <*> Json.read "result"

  static member ToJson (x:FieldLike) =
          Json.write "typ" x.Type
       *> Json.writeWith Json.fromEnum "instance" x.Instance
       *> Json.write "name" x.Name
       *> Json.write "result" x.Result

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
      | InstanceOrStatic.Instance -> sprintf "(Instance/Inheritance of %s)" typ.FullName
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
  static member FromJson (_ : Member) =
    let inline (|Prop|_|) key = Patterns.(|Property|_|) key

    function 
    | Prop "RecordConstructor" str as json -> Json.init (RecordConstructor str) json
    | Prop "Constructor" str as json -> Json.init (Constructor str) json
    | Prop "UnionConstructor" str as json -> Json.init (UnionConstructor str) json
    | Prop "Event" str as json -> Json.init (Event str) json
    | Prop "Field" str as json -> Json.init (Field str) json
    | Prop "Method" str as json -> Json.init (Method str) json
    | Prop "Property" str as json -> Json.init (Property str) json
    | Prop "UnionCase" str as json -> Json.init (UnionCase str) json
    | Prop "EnumValue" str as json -> Json.init (EnumValue str) json
    | json -> Json.error (sprintf "couldn't deserialise %A to Member" json) json

  static member ToJson (x: Member) =
      match x with
      | RecordConstructor c -> Json.write "RecordConstructor" c
      | Constructor c -> Json.write "Constructor" c
      | UnionConstructor (t,c) -> Json.write "UnionConstructor" (t,c)
      | Event e ->Json.write "Event" e
      | Field e ->Json.write "Field" e
      | Method e ->Json.write "Method" e
      | Property e ->Json.write "Property" e
      | UnionCase (typ,name,param) ->Json.write "UnionCase" (typ,name,param)
      | EnumValue (typ,name,value) ->Json.write "EnumValue" (typ,name,value)

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

  static member FromJson (_:SurfaceOfType) =
          fun t n m s p-> { Type = t; NetType=n; Members=m; SumType=s; BaseType=p }
      <!> Json.read "typ"
      <*> Json.readWith Json.toEnum<NetType> "netType"
      <*> Json.read "members"
      <*> Json.read "sumtype"
      <*> Json.read "baseTyp"

  static member ToJson (x:SurfaceOfType) =
          Json.write "typ" x.Type
       *> Json.writeWith Json.fromEnum "netType" x.NetType
       *> Json.write "members" x.Members
       *> Json.write "sumtype" x.SumType
       *> Json.write "baseTyp" x.BaseType

type Namespace=
    {
      Namespace:string
      Types: SurfaceOfType list
    }
with
  override x.ToString()=sprintf "%A" x
  static member FromJson (_:Namespace) =
          fun n ts -> { Namespace = n; Types=ts }
      <!> Json.read "namespace"
      <*> Json.read "types"

  static member ToJson (x:Namespace) =
          Json.write "namespace" x.Namespace
       *> Json.write "types" x.Types

type Package=
    {
      Namespaces: Namespace list
    }
with
  override x.ToString()=sprintf "%A" x
  static member FromJson (_:Package) =
          fun n -> { Namespaces = n }
      <!> Json.read "namespaces"

  static member ToJson (x:Package) =
          Json.write "namespaces" x.Namespaces

type 'a AddedAndRemoved when 'a:comparison=
    {
        Added: Set<'a>
        Removed: Set<'a>
    }
with
  override x.ToString()=sprintf "%A" x
  member this.IsEmpty=this.Added.IsEmpty && this.Removed.IsEmpty

type Changes<'t,'m when 't:comparison>=
    {
        Diff: AddedAndRemoved<'t>
        Changes: Map<'t,'m>
    }
with
  override x.ToString()=sprintf "%A" x
  member this.IsEmpty=this.Diff.IsEmpty && this.Changes.IsEmpty


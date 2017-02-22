namespace SyntacticVersioning
type Version = Major | Minor | Patch
    with
        override m.ToString ()=sprintf "%A" m
type NetType =
  | Abstract | Class | Enum | Interface | Static | Struct
  (* | Primitive | ValueType *)
  | Module | RecordType | SumType | UnionConstructor | UnionTags
    with
        override m.ToString ()=sprintf "%A" m

type InstanceOrStatic = | Instance | Static
    with
        override m.ToString ()=sprintf "%A" m
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
      override x.ToString() = Parameter.ToString x

type Constructor=Typ * Parameter list
type Member= 
    |RecordConstructor of Constructor
    |Constructor of Constructor
    |Event of Typ * InstanceOrStatic * Name * Parameter list * Typ
    |Field of Typ *InstanceOrStatic * Name * Typ
    |Method of Typ *InstanceOrStatic * Name * Parameter list * Typ
    |Property of Typ *InstanceOrStatic * Name * Typ
    |UnionConstructor of Typ*Constructor
    |UnionCase of Typ* Name * Parameter list
    |EnumValue of Typ* Name * string
    with
       static member private WhenStatic (flag:InstanceOrStatic) (typ:Typ) : string =
          match flag with
            | Static -> typ.FullName
            | Instance -> sprintf "(Instance/Inheritance of %s)" typ.FullName

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
          | RecordConstructor (typ,prms)->
            sprintf "{ %s.%s } -> %s" 
              typ.FullName
              (Member.Parameters prms)
              typ.FullName
          | Constructor (typ,prms)-> 
              sprintf "new %s : %s -> %s" 
                typ.FullName
                (Member.Parameters prms)
                typ.FullName
          | UnionConstructor (typ,(typ',prms))->
              sprintf "%s : %s -> %s"
                typ.FullName
                (Member.Parameters prms)
                typ'.FullName
          | Event (typ,isStatic,name,prms,rtyp)->
            sprintf "%s.%s : %s -> %s"
              (Member.WhenStatic isStatic typ)
              (sprintf "%s.Add" name)
              (Member.Parameters prms)
              (rtyp.FullName)           
          | Field (typ,isStatic,name,rtyp) -> 
              sprintf "%s.%s : %s"
                (Member.WhenStatic isStatic typ)
                name
                (rtyp.FullName)
          | Method (typ,isStatic,name,prms,rtyp) -> 
              sprintf "%s.%s : %s -> %s"
                (Member.WhenStatic isStatic typ)
                name
                (Member.Parameters prms)
                (rtyp.FullName)
          | Property (typ,isStatic,name,ptyp) ->
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
    }
    with
    /// Create an instance of Surface of type with the required members set
    static member Create t netType members=
      {
        Type=t; NetType=netType;Members=members
      }
    member this.Enum
            = if this.NetType = Enum then
                let enumV =
                    this.Members
                    |>List.filter Member.isEnumValue
                    |>List.map (function 
                                | EnumValue (_,name,value)->(name,value) 
                                | _ -> failwith "!")
                Some { Type=this.Type; Values= enumV }
              else
                None
    member this.UnionCases 
            = if this.NetType = SumType then
                let cases =
                    this.Members
                    |>List.filter Member.isUnionCase
                Some { Type=this.Type; Cases= cases}
              else
                None
    override x.ToString()=sprintf "%A" x

type Namespace=
    {
        Name:string
        Types: SurfaceOfType list
    }
    with
      override x.ToString()=sprintf "%A" x

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

type NamespaceChanges=
    {
        Types: AddedAndRemoved<Typ>
        TypeChanges: Map<Typ,AddedAndRemoved<Member>>
    }
    with
      override x.ToString()=sprintf "%A" x

type PackageChanges=
    { 
        Namespaces : AddedAndRemoved<string>
        NamespacesChanged: Map<string,NamespaceChanges>
    }
    with
      override x.ToString()=sprintf "%A" x


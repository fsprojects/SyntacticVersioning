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


type EnumTyp =
    {
        FullName: string
        Values: (string*string) list
    }
    with
      override x.ToString()=sprintf "%A" x



type UnionCase =
    {
        Name: string
        Fields: Parameter list
    }
    with
      static member ToString x=
        let ps =
          match x.Fields with
            | [] -> ""
            | fields ->
              fields
              |> List.map(Parameter.ToString)
              |> String.concat ""
        match System.String.IsNullOrEmpty ps with
          | true -> x.Name
          | false -> sprintf "%s of %s" x.Name ps

      override x.ToString()=UnionCase.ToString x

type UnionCases =
    {
        Type:Typ
        Cases:UnionCase list
    }
    with
      override t.ToString()=
          t.Cases
          |> List.map(UnionCase.ToString)
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
        UnionCases: UnionCases option
        Enum: EnumTyp option
    }
    with
    /// Create an instance of Surface of type with the required members set
    static member Create t netType members=
      {
        Type=t; NetType=netType;Members=members
        UnionCases=None;Enum=None
      }
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


module internal SynVer.Serialization
open FSharp.SExpression
type E = Expression

module private Enums =
  let serialize (e : 'e when 'e :> System.Enum and 'e : (new:unit->'e) and 'e : struct) =
      e.ToString "G"
      |> String
  let deSerialize<'e when 'e :> System.Enum and 'e : (new:unit->'e) and 'e : struct> v =
      match v with
      | String s ->
          match System.Enum.TryParse<'e> s with
          | true, x -> Some x
          | _ -> None
      | _ -> None
module private Bool =
  let serialize (b:bool) =
      b.ToString ()
      |> String
  let deSerialize v =
      match v with
      | String s ->
          match System.Boolean.TryParse s with
          | true, x -> Some x
          | _ -> None
      | _ -> None
module Type=
  let serialize  (x:Typ) =
    E.List [ E.Symbol "typ" ; E.String x.FullName ]
  let deSerialize v : Typ option=
      match v with 
      | E.List [ 
                E.Symbol "typ"; E.String n
               ] -> Some { FullName = n}
      | _ -> None


module Parameter=
  let deSerialize v : Parameter option=
      match v with 
      | E.List [ E.Symbol "typ"; typ; E.Symbol "name"; E.String n ] -> 
        Type.deSerialize typ |> Option.map ( fun t -> { Type = t; Name = n } )
      | _ -> None
  let serialize  (x:Parameter) =
    E.List [ E.Symbol "typ" ; Type.serialize x.Type; E.Symbol "name"; E.String x.Name]

module ConstructorLike=
       let deSerialize v : ConstructorLike option=
          match v with 
          | E.List (E.Symbol "typ" :: typ:: E.Symbol "params" :: [E.List parameters]) -> 
            let p= parameters |> List.map Parameter.deSerialize 
            if List.exists Option.isNone p then
              None
            else
              Type.deSerialize typ |> Option.map ( fun t -> { Type = t; Parameters = p |> List.map Option.get } )
          | _ -> None

       let serialize  (x:ConstructorLike) =
          E.List [ E.Symbol "typ" ; Type.serialize x.Type; E.Symbol "params"; E.List (List.map Parameter.serialize x.Parameters)]

module MethodLike=
  let deSerialize v : MethodLike option=
      match v with 
          | E.List [
                    E.Symbol "typ" ; typ; 
                    E.Symbol "instance" ; instance
                    E.Symbol "name" ; E.String name
                    E.Symbol "result"; result
                    E.Symbol "params" ; E.List parameters
                   ] -> 
            let p= parameters |> List.map Parameter.deSerialize  
            if List.exists Option.isNone p then
              None
            else
              match (Enums.deSerialize instance, Type.deSerialize typ, Type.deSerialize result) with 
              | (Some i, Some t, Some r) ->
                Some { Type = t; Instance=i; Name=name; Result=r ; Parameters = p |> List.map Option.get } 
              | _ ->
                None
          | _ -> None
 
  let serialize  (x:MethodLike) =
      E.List (
        [
          E.Symbol "typ" ; Type.serialize x.Type
          E.Symbol "instance" ; Enums.serialize x.Instance
          E.Symbol "name" ; String x.Name
          E.Symbol "result" ; Type.serialize x.Result
          E.Symbol "params" ; E.List (List.map Parameter.serialize x.Parameters)
        ] )

module FieldLike=
  let deSerialize v : FieldLike option=
      match v with 
      | E.List [
                E.Symbol "typ" ; typ; 
                E.Symbol "instance" ; instance
                E.Symbol "name" ; E.String name
                E.Symbol "result"; result
               ] -> 
          match (Enums.deSerialize instance, Type.deSerialize typ, Type.deSerialize result) with 
          | (Some i, Some t, Some r) ->
            Some { Type = t; Instance=i; Name=name; Result=r }
          | _ ->
            None
      | _ -> None

  let serialize (x:FieldLike) =
      E.List (
        [
          E.Symbol "typ" ; Type.serialize x.Type
          E.Symbol "instance" ; Enums.serialize x.Instance
          E.Symbol "name" ; String x.Name
          E.Symbol "result" ; Type.serialize x.Result
        ] )


module Member=
  let deSerialize v : Member option =
    match v with
    |  E.List [ E.Symbol "RecordConstructor"; c] -> 
        ConstructorLike.deSerialize c |> Option.map RecordConstructor
    | E.List [ E.Symbol "Constructor";c ]-> 
        ConstructorLike.deSerialize c |> Option.map Constructor
    | E.List [ E.Symbol "UnionConstructor"; typ; ctor ]-> 
        match ( ConstructorLike.deSerialize ctor, Type.deSerialize typ) with
        | (Some c, Some t) -> UnionConstructor (t,c) |> Some
        | _ -> None
    | E.List [ E.Symbol "Event";m ]-> MethodLike.deSerialize m |> Option.map Event
    | E.List [ E.Symbol "Field";f ]-> FieldLike.deSerialize f |> Option.map Field
    | E.List [ E.Symbol "Method";m ]-> MethodLike.deSerialize m |> Option.map Method
    | E.List [ E.Symbol "Property";m ]-> FieldLike.deSerialize m |> Option.map Property
    | E.List [ 
              E.Symbol "UnionCase"; 
              E.Symbol "type"; typ; 
              E.Symbol "name"; E.String name;
              E.Symbol "params"; E.List parameters
             ]-> 
        let p= parameters |> List.map Parameter.deSerialize 
        if List.exists Option.isNone p then
          None
        else
          Type.deSerialize typ |> Option.map ( fun t -> UnionCase (t,name, p |> List.map Option.get )) 
    | E.List [ 
              E.Symbol "EnumValue"; 
              E.Symbol "type"; typ; 
              E.Symbol "name"; E.String name;
              E.Symbol "value"; E.String value
             ]-> 
        Type.deSerialize typ |> Option.map ( fun t -> EnumValue (t, name, value))
    | _ -> None
  let serialize  (x: Member) =
      match x with
      | RecordConstructor c -> E.List [ E.Symbol "RecordConstructor"; ConstructorLike.serialize c ]
      | Constructor c -> E.List [ E.Symbol "Constructor"; ConstructorLike.serialize c ]
      | UnionConstructor (t,c) -> E.List [ E.Symbol "UnionConstructor"; Type.serialize t; ConstructorLike.serialize c ]
      | Event e ->E.List [ E.Symbol "Event"; MethodLike.serialize e]
      | Field e ->E.List [ E.Symbol "Field"; FieldLike.serialize e]
      | Method e ->E.List [ E.Symbol "Method"; MethodLike.serialize e]
      | Property e ->E.List [ E.Symbol "Property"; FieldLike.serialize e]
      | UnionCase (typ,name,param) ->
        E.List [
          E.Symbol "UnionCase"
          E.Symbol "type"; Type.serialize typ
          E.Symbol "name"; E.String name
          E.Symbol "params"; E.List (List.map Parameter.serialize param)
        ]
      | EnumValue (typ,name,value) ->
          E.List [
            E.Symbol "EnumValue"
            E.Symbol "type"; Type.serialize typ
            E.Symbol "name"; E.String name
            E.Symbol "value"; E.String value
          ]

module SurfaceOfType=
  let deSerialize v : SurfaceOfType option=
    match v with
    | E.List [ 
              E.Symbol "EnumValue"
              E.Symbol "typ"; typ 
              E.Symbol "netType"; netType
              E.Symbol "members"; E.List members
              E.Symbol "sumtype"; sumtype
              E.Symbol "baseTyp"; baseTyp
             ]-> 
      match (Type.deSerialize typ, Enums.deSerialize netType, Bool.deSerialize sumtype ) with
      | (Some t, Some n, Some s) ->
        let m= members |> List.map Member.deSerialize 
        if List.exists Option.isNone m then
          None
        else
          Some { Type = t; NetType=n; Members=m |> List.map Option.get; SumType=s; BaseType=Type.deSerialize baseTyp }
      | _ -> None
    | _ -> None

  let serialize (x:SurfaceOfType) =
    E.List [ 
              E.Symbol "EnumValue"
              E.Symbol "typ"; Type.serialize x.Type
              E.Symbol "netType"; Enums.serialize x.NetType
              E.Symbol "members"; E.List <| List.map Member.serialize x.Members
              E.Symbol "sumtype"; Bool.serialize x.SumType
              E.Symbol "baseTyp"; (match x.BaseType with | Some b-> Type.serialize b | None -> E.List [])
             ]

module Namespace=
  let deSerialize v : Namespace option=
    match v with 
    | E.List [
              E.Symbol "namespace" ; E.String namespace'
              E.Symbol "types" ; E.List types
             ] -> 
         let ts= types |> List.map SurfaceOfType.deSerialize 
         if List.exists Option.isNone ts then
           None
         else
           Some { Namespace=namespace';Types=ts|>List.map Option.get }
    | _ -> None
  let serialize  (x:Namespace) =
    E.List [
             E.Symbol "namespace" ; E.String x.Namespace
             E.Symbol "types" ; E.List <| List.map SurfaceOfType.serialize x.Types
           ]

module Package=
  let deSerialize v : Package option=
    match v with 
    | E.List [
              E.Symbol "namespaces" ; E.List namespaces
             ] -> 
         let ns= namespaces |> List.map Namespace.deSerialize 
         if List.exists Option.isNone ns then
           None
         else
           Some { Namespaces = ns|>List.map Option.get }
    | _ -> None
  let serialize  (x:Package) =
    E.List [
            E.Symbol "namespaces" ; E.List <| List.map Namespace.serialize x.Namespaces
           ]

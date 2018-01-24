module internal SynVer.Serialization
open FSharp.SExpression
type E = Expression

module private Enums =
  let serialize (e : 'e when 'e :> System.Enum and 'e : (new:unit->'e) and 'e : struct) =
      e.ToString "G"
      |> String
  let deSerialize<'e when 'e :> System.Enum and 'e : (new:unit->'e) and 'e : struct> json =
      match json with
      | String s ->
          match System.Enum.TryParse<'e> s with
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
            let p= parameters |> List.map Parameter.deSerialize  // eat the rest of paramet
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

  let serialize  (x:FieldLike) =
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
        failwith "!"
        //Json.init (RecordConstructor str) json
    | E.List [ E.Symbol "Constructor";c ]-> 
        failwith "!"
        //Json.init (Constructor str) json
    | E.List [ E.Symbol "UnionConstructor"; c ]-> 
        failwith "!"
        //Json.init (UnionConstructor str) json
(*
    | Prop "Event" str as json -> Json.init (Event str) json
    | Prop "Field" str as json -> Json.init (Field str) json
    | Prop "Method" str as json -> Json.init (Method str) json
    | Prop "Property" str as json -> Json.init (Property str) json
    | Prop "UnionCase" str as json -> Json.init (UnionCase str) json
    | Prop "EnumValue" str as json -> Json.init (EnumValue str) json
    | json -> Json.error (sprintf "couldn't deserialise %A to Member" json) json
*)
  let serialize  (x: Member) =
      match x with
      | RecordConstructor c -> E.List [ E.Symbol "RecordConstructor"; ConstructorLike.serialize c ]
      | Constructor c -> E.List [ E.Symbol "Constructor"; ConstructorLike.serialize c ]
      | UnionConstructor (t,c) -> E.List [ E.Symbol "UnionConstructor"; Type.serialize t; ConstructorLike.serialize c ]
      | Event e ->E.List [ E.Symbol "Event"; MethodLike.serialize e]
      | Field e ->E.List [ E.Symbol "Field"; FieldLike.serialize e]
      | Method e ->E.List [ E.Symbol "Method"; MethodLike.serialize e]
      | Property e ->E.List [ E.Symbol "Property"; FieldLike.serialize e]
      //| UnionCase (typ,name,param) ->Json.write "UnionCase" (typ,name,param)
      //| EnumValue (typ,name,value) ->Json.write "EnumValue" (typ,name,value)

module SurfaceOfType=
  let deSerialize v : SurfaceOfType =
      failwith "!"
    (*
          fun t n m s p-> { Type = t; NetType=n; Members=m; SumType=s; BaseType=p }
      <!> Json.read "typ"
      <*> Json.readWith Json.toEnum<NetType> "netType"
      <*> Json.read "members"
      <*> Json.read "sumtype"
      <*> Json.read "baseTyp"
      *)

  let serialize  (x:SurfaceOfType) =
      failwith "!"
    (*
          Json.write "typ" x.Type
       *> Json.writeWith Json.fromEnum "netType" x.NetType
       *> Json.write "members" x.Members
       *> Json.write "sumtype" x.SumType
       *> Json.write "baseTyp" x.BaseType
    *)

module Namespace=
  let deSerialize v : Namespace option=
      failwith "!"
    (*
          fun n ts -> { Namespace = n; Types=ts }
      <!> Json.read "namespace"
      <*> Json.read "types"
    *)
  let serialize  (x:Namespace) =
      failwith "!"
      (*
          Json.write "namespace" x.Namespace
       *> Json.write "types" x.Types
      *)
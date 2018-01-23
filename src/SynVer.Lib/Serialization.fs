namespace SynVer
open FSharp.SExpression

module private Json =
  let fromEnum (e : 'e when 'e :> System.Enum and 'e : (new:unit->'e) and 'e : struct) =
      e.ToString "G"
      |> String
  let toEnum<'e when 'e :> System.Enum and 'e : (new:unit->'e) and 'e : struct> json =
      match json with
      | String s ->
          match System.Enum.TryParse<'e> s with
          | true, x -> Ok x
          | _ -> Error (sprintf "Unable to parse %s as a %s" s typeof<'e>.Name)
      | _ -> Error (sprintf "Unable to parse %A as a %s" json typeof<'e>.Name)

module Type=
  static member ToJson (x:Typ) =
    Json.write "typ" x.FullName
  static member FromJson (_:Typ) =
      fun n -> { FullName = n}
    <!> Json.read "typ"

module Parameter=
  static member FromJson (_:Parameter) =
          fun t n -> { Type = t; Name = n }
      <!> Json.read "typ"
      <*> Json.read "name"
  static member ToJson (x:Parameter) =
          Json.write "typ" x.Type
       *> Json.write "name" x.Name

module ConstructorLike=
       static member FromJson (_:ConstructorLike) =
               fun t p -> { Type = t; Parameters = p }
           <!> Json.read "typ"
           <*> Json.read "params"

       static member ToJson (x:ConstructorLike) =
               Json.write "typ" x.Type
            *> Json.write "params" x.Parameters
module MethodLike=
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

module FieldLike=
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


module Member=
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

module SurfaceOfType=
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

module Namespace=
  static member FromJson (_:Namespace) =
          fun n ts -> { Namespace = n; Types=ts }
      <!> Json.read "namespace"
      <*> Json.read "types"

  static member ToJson (x:Namespace) =
          Json.write "namespace" x.Namespace
       *> Json.write "types" x.Types

module SynVer.Lson
open SynVer.Serialization
open FSharp.SExpression
let serialize (p:Package) = Package.serialize p |> print
let deserialize v :Package =
  let res = Package.deSerialize <| parse v
  if res.IsNone then failwith "expected serialized package !"
  res.Value
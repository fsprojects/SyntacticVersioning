module SynVer.Lson
open SynVer.Serialization
open FSharp.SExpression
let serialize (p:Package) = Package.serialize p |> print
let tryDeserialize v :Package option=
  Package.deSerialize <| parse v

let deserialize v :Package =
  match tryDeserialize v with
  | None -> failwith "expected serialized package !"
  | Some p -> p

module SynVer.Lson
open SynVer.Serialization
open FSharp.SExpression
let serialize (p:Package) = Package.serialize p |> print
let tryDeserialize v :Package option=
  match parse v with
  | [e] -> Package.deSerialize e
  | _ -> None

let deserialize v :Package =
  match tryDeserialize v with
  | None -> failwithf "expected serialized package ! %s" v
  | Some p -> p

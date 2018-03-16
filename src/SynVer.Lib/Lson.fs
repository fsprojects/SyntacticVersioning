module SynVer.Lson
open SynVer.Serialization
open LSON
let serialize (p:Package) = Package.serialize p |> stringify
let tryDeserialize v :Package option=
  Package.deSerialize (LSON.parse v)

let deserialize (v:string) :Package =
  match tryDeserialize v with
  | None -> failwithf "expected serialized package ! %s" v
  | Some p -> p

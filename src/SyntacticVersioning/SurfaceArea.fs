namespace SyntacticVersioning
open System
open Reflect
module SurfaceArea =

  /// Get the surface of a type
  [<CompiledName("SurfaceOfType")>]
  let surfaceOfType (t:Type) : SurfaceOfType=
    let typ = typeToTyp t
    let netType = tagNetType t
    let members = getTypeMembers t
    let s = SurfaceOfType.Create typ netType members
    match netType with
    | Enum -> {s with Enum=toEnumTyp t |> Some }
    | SumType -> { s with UnionCases = Reflect.toUnionCases t |> Some}
    | _ -> s

  /// Get the surface of an assembly
  [<CompiledName("ExportedTypes")>]
  let exportedTypes assembly =
    let types = exportedTypes assembly
    let toNs (ns,ts) ={
                        Name = (if ns <> null then ns else "")
                        Types= ts
                      }
    let actual =
        List.map ((fun (ns,ns_ts)-> (ns,ns_ts |> List.map snd)) >> toNs) (types 
          |> List.map (fun t-> (t.Namespace, surfaceOfType t ))
          |> List.groupBy (fun (ns,_)->ns))
    { Namespaces = actual}
   
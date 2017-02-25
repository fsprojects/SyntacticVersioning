namespace SyntacticVersioning
open System
open Reflect
open System.IO
open System.Security.Cryptography
module SurfaceArea =

  /// Get the surface of a type
  [<CompiledName("SurfaceOfType")>]
  let surfaceOfType (t:Type) : SurfaceOfType=
    let typ = typeToTyp t
    let netType = tagNetType t
    let members = getTypeMembers t
    SurfaceOfType.Create typ netType members (isSumType t)

  /// Get the surface of an assembly
  [<CompiledName("ExportedTypes")>]
  let exportedTypes assembly : Package=
    let types = exportedTypes assembly
    let toNs (ns,ts) ={
                        Namespace = (if ns <> null then ns else "")
                        Types= ts
                      }
    let actual =
        List.map ((fun (ns,ns_ts)-> (ns,ns_ts |> List.map snd)) >> toNs) (types 
          |> List.map (fun t-> (t.Namespace, surfaceOfType t ))
          |> List.groupBy (fun (ns,_)->ns))
    use sha256 = SHA256Managed.Create()
    use f = new FileStream(assembly.Location, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
    use ms =  new MemoryStream(sha256.ComputeHash f)
    use r = new StreamReader(ms)
    let hash = r.ReadToEnd()
    { Namespaces = actual; Sha256 = hash }
   
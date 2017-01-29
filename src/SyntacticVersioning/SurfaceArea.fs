namespace SyntacticVersioning

module SurfaceArea =
    open Reflect
    let exportedTypes assembly =
            let types = exportedTypes assembly
            let actual =
                types 
                |> Array.map (fun t-> (t.Namespace, (t.Name,{ tagNetType t; Members=set (getTypeMembers t)})))
                |> Array.groupBy (fun (ns,_)->ns)
                |> Array.map (fun (ns,ns_ts)-> (ns,ns_ts |> Array.map snd)) 
                |> Array.map (fun (ns,ts)-> ((if ns <> null then ns else ""), 
                                            {
                                                Types= ts|> Map.ofSeq 
                                            } ))
                |> Map.ofSeq
            { Namespaces = actual}
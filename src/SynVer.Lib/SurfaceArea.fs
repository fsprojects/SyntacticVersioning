module SynVer.SurfaceArea
open System
open Mono.Cecil

/// Get the surface of a type
[<CompiledName("OfType")>]
let ofType (t:Type) : SurfaceOfType=
  let typ = Reflect.typeToTyp t
  let netType = Reflect.tagNetType t
  let members = Reflect.getTypeMembers t
  let baseT = t.BaseType |> Option.ofObj |> Option.map Reflect.typeToTyp
  SurfaceOfType.Create typ netType members (Reflect.isSumType t) baseT

/// Get the surface of an assembly
[<CompiledName("OfAssembly")>]
let ofAssembly assembly : Package=
  let types = Reflect.exportedTypes assembly
  let toNs (ns,ts) ={
                      Namespace = (if ns <> null then ns else "")
                      Types= ts
                    }
  let actual =
      List.map ((fun (ns,ns_ts)-> (ns,ns_ts |> List.map snd)) >> toNs) (types 
        |> List.map (fun t-> (t.Namespace, ofType t ))
        |> List.groupBy (fun (ns,_)->ns))
  { Namespaces = actual }
[<CompiledName("OfTypeDefinition")>]
let ofTypeDefinition (t:TypeDefinition) : SurfaceOfType=
  let typ = Decompile.typeToTyp t
  let netType = Decompile.tagNetType t
  let members = Decompile.getTypeMembers t
  let baseT = t.BaseType |> Option.ofObj |> Option.map Decompile.typeToTyp
  SurfaceOfType.Create typ netType members (Decompile.isSumType t) baseT

/// Get the surface of an assembly definition (loaded by Mono Cecil)
[<CompiledName("OfAssemblyDefinition")>]
let ofAssemblyDefinition (assembly:AssemblyDefinition) : Package=
  let types = Decompile.exportedTypes assembly
  let toNs (ns,ts) ={
                      Namespace = (if ns <> null then ns else "")
                      Types= ts
                    }
  let actual =
      List.map ((fun (ns,ns_ts)-> (ns,ns_ts |> List.map snd)) >> toNs) (types 
        |> List.map (fun t-> (t.Namespace, ofTypeDefinition t ))
        |> List.groupBy (fun (ns,_)->ns))
  { Namespaces = actual }

[<AutoOpen>]
module private Print=
  let typeInfo : SurfaceOfType -> string =
    fun x ->
      let bt : Typ option -> string =
        function | None -> "none" | Some t -> t.FullName
       
      sprintf "%s (.NET type: %A and base: %s)"
        (x.Type.FullName) (x.NetType) (bt x.BaseType)

let internal raw (p:Package) : (string * string) Set =
  let ts = p.Namespaces
           |> List.collect (fun x -> x.Types)
  
  let namespaces =
    p.Namespaces
    |> List.map(fun x -> x.Namespace)
    |> List.distinct
    |> List.map (fun x -> x,(sprintf "%s (Namespace)" x))

  let types =
    ts
    |> List.filter(SurfaceOfType.IsSumType >> not)
    |> List.map (fun x -> (x.Type.FullName,typeInfo x))

  let nonSumTypes =ts|> List.filter (not << SurfaceOfType.IsSumType)
  let sumTypes = ts|> List.filter (SurfaceOfType.GetNetType >> (=) NetType.SumType)
  let members =
    (List.collect (fun surfaceOfType->
       surfaceOfType.Members 
       |> List.filter (not << Member.isUnionCase)
       |> List.map (fun m-> (surfaceOfType.Type.FullName, m.ToString()))) nonSumTypes)
  let unionValues =
    (List.map (fun (s:SurfaceOfType)->
         match s.UnionCases with
         | Some v ->
             (s.Type.FullName, v.ToString() )
         | None -> failwithf "Expected %s to have union cases" s.Type.FullName    
    ) sumTypes)

  members
  |> List.append namespaces
  |> List.append types
  |> List.append unionValues
  |> List.distinct
  |> List.filter (fun (_,x) -> System.String.IsNullOrEmpty x |> not)
  |> List.fold(fun a xy -> a |> Set.add xy) Set.empty

[<CompiledName("Bump")>]
let bump (verNr:string) (released: Package) (modified: Package) : (string* SynVer.Version) =
  let pub = raw released
  let dev = raw modified
 
  let deleted = pub - dev
  let created = dev - pub
 
  let ver =
    match Set.isEmpty deleted, Set.isEmpty created with
     | true  , true  -> Patch
     | false , _____ -> Major
     | _____ , false -> Minor
 
  let hlp n xs =
   xs
   |> Array.mapi(
     fun i x ->
       match i with
         | _ when i = n -> x + 1
         | _ when i > n -> 0
         | _ -> x)
 
  let nr =
   verNr.Split('.')
   |> Array.map (
     fun x -> x |> System.Int32.TryParse |> function
       | true, value -> value
       | _ -> 0)
   
  let nr' =
   match ver with
     | Version.Patch -> nr |> hlp 2
     | Version.Minor -> nr |> hlp 1
     | Version.Major -> nr |> hlp 0
   |> Array.map string
   |> Array.reduce (fun x y -> sprintf "%s.%s" x y)
  (nr',ver)


let private prettyPrint (indentation: int) (str: string): string =
 let tab = String.replicate indentation " "
 
 match str.Contains(" : ") with
   | false -> sprintf "\n%s%s" tab str
   | true ->
     str.Split([| " : " |],StringSplitOptions.None)
     |> Array.map(fun x -> tab + x)
     |> Array.reduce(
       fun x y ->
         sprintf "%s : \n%s%s"
           x tab y)
     |> function | s -> "\n" + s
     |> function | s -> s.Split([| " -> " |],StringSplitOptions.None)
     |> Array.reduce(
       fun x y ->
         sprintf "%s\n%s%s-> %s"
           x tab tab y)
     |> fun x -> x.Contains("Removed: ") |> function
       | true  -> x.Replace("Removed: ", "- ")
       | false -> x
     |> fun x -> x.Contains("Added: ") |> function
       | true  -> x.Replace("Added: ", "+ ")
       | false -> x

let private sort' (x: string): string =
   x.Replace("Removed: ", System.String.Empty)
    .Replace("Added: ", System.String.Empty)
    .Replace("new ", System.String.Empty)
    .Replace("(Instance/Inheritance of ", System.String.Empty)
    .Replace("( ", System.String.Empty)
    .Replace("{ ", System.String.Empty)

/// Return differences as a string array, use String.Join(Environment.NewLine, diff) to present the information
[<CompiledName("Diff")>]
let diff : Package -> Package -> string array =
 fun source target ->
   let pub = raw source
   let dev = raw target
   
   let deleted =
     pub - dev
     |> Set.map(fun (x,y) -> (x,"Removed: " + y))
   let created =
     dev - pub
     |> Set.map(fun (x,y) -> (x,"Added: " + y))
   
   deleted + created
   |> Set.toArray
   |> Array.groupBy(fun (g,_) -> g)
   |> Array.sortBy(fun (g,_) -> g)
   |> Array.map(
     fun (g,xs) ->
       let (_,ys) = xs |> Array.unzip
       Array.append
         [| sprintf "\n* %s" g |]
         (ys |> Array.sortBy(sort')) // ?
     )
   |> Array.concat
   |> Array.map (fun x -> x |> prettyPrint 8)
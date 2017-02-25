namespace SyntacticVersioning
open System.Reflection
open System
open Reflect

module Compare =
  [<AutoOpen>]
  module private Print=

    let typeInfo : Type -> string =
      fun x ->
        let bt : Type -> string =
          function | null -> "none" | _ -> x.BaseType |> typeFullName
        
        sprintf "%s (.NET type: %A and base: %s)"
          (typeFullName x) (tagNetType x) (bt x.BaseType)

  let internal raw (ts:Package) : (string * string) Set =

   let namespaces =
     ts.Namespaces
     |> List.map(fun x -> x.Namespace)
     |> List.distinct
     |> List.map (fun x -> x,(sprintf "%s (Namespace)" x))

   let types =
     ts.Namespaces
     |> List.collect (fun x -> x.Types)
     |> List.filter(isSumType >> not)
     |> List.map (fun x -> (typeFullName x,typeInfo x))

   let nonSumTypes =ts|> List.filter (not << isSumType)
   let sumTypes = ts|> List.filter (Reflect.tagNetType >> (=) NetType.SumType)
   let members =
     (List.collect (fun t->
        let surface = SurfaceArea.surfaceOfType t
        surface.Members 
        |> List.filter (not << Member.isUnionCase)
        |> List.map (fun m-> (surface.Type.FullName, m.ToString()))) nonSumTypes)
   let unionValues =
     (List.map (fun t->
          let surface = SurfaceArea.surfaceOfType t
          match surface.UnionCases with
          | Some v ->
              (surface.Type.FullName, v.ToString() )
          | None -> failwithf "Expected %s to have union cases" surface.Type.FullName    
     ) sumTypes)

   members
   |> List.append namespaces
   |> List.append types
   |> List.append unionValues
   |> List.distinct
   |> List.filter (fun (_,x) -> System.String.IsNullOrEmpty x |> not)
   |> List.fold(fun a xy -> a |> Set.add xy) Set.empty

  [<CompiledName("Bump")>]
  let bump (verNr:string) (released: Assembly) (modified: Assembly) : (string* SyntacticVersioning.Version) =
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
  [<CompiledName("Describe")>]
  let describe : Assembly -> Assembly -> string array =
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

  open SurfaceArea  
  type private Ns = Namespace
  type private T = SurfaceOfType
  type private M = Member
  let private diffSet source target =
    { 
      Added=Set.difference target source
      Removed=Set.difference source target
    }

  [<CompiledName("Members")>]
  let members (source:M list) (target: M list) =
    let sourceSet = source |> set
    let targetSet = target |> set
    diffSet sourceSet targetSet

  [<CompiledName("Types")>]
  let types (source:T list) (target: T list) =
    let typ (t:T) = t.Type 
    let setOfNames = List.map typ >> set
    let sourceSet = source |> setOfNames
    let targetSet = target |> setOfNames
    let maybeChanged = Set.intersect targetSet sourceSet
    let compareMembersForTyp t=
      let withTyp = List.find (typ >> (=) t)
      let sourceT = source |> withTyp
      let targetT = target |> withTyp
      members sourceT.Members targetT.Members
    let changed = maybeChanged 
                |> Seq.map (fun t->t, compareMembersForTyp t ) 
                |> Seq.filter (fun (_,c)-> not c.IsEmpty)
                |> Map.ofSeq
    {
        Diff=diffSet sourceSet targetSet
        Changes= changed
    }
  [<CompiledName("Namespaces")>]
  let namespaces (source:Ns list) (target:Ns list) =
    let nameOf (ns:Ns) = ns.Namespace 
    let setOfNames = List.map nameOf >> set
    let sourceSet = source |> setOfNames
    let targetSet = target |> setOfNames

    let maybeChanged = Set.intersect targetSet sourceSet
    let compareTypesForNsWithName t=
      let withName = List.find (nameOf >> (=) t)
      let sourceT = source |> withName
      let targetT = target |> withName
      types sourceT.Types targetT.Types

    let changed = maybeChanged 
                |> Seq.map (fun t->t, compareTypesForNsWithName t ) 
                |> Seq.filter (fun (_,c)-> not c.IsEmpty)
                |> Map.ofSeq
    {
        Diff=diffSet sourceSet targetSet
        Changes=changed
    }

  /// Return differences as Package Canges
  [<CompiledName("Assemblies")>]
  let assemblies (source: Assembly) (target:Assembly)=
    let pub = exportedTypes source
    let dev = exportedTypes target

    namespaces pub.Namespaces dev.Namespaces

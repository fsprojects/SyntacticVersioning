namespace SyntacticVersioning
open System
open System.Reflection
module SurfaceArea =
  open Reflect
  let private typeInfo : Type -> string =
    fun x ->
      let bt : Type -> string =
        function | null -> "none" | _ -> x.BaseType |> typeFullName
      
      sprintf "%s (.NET type: %A and base: %s)"
        (typeFullName x) (tagNetType x) (bt x.BaseType)

  /// Get the surface of a type
  [<CompiledName("SurfaceOfType")>]
  let surfaceOfType (t:Type) : SurfaceOfType=
    let typ = typeToTyp t
    let netType = tagNetType t
    let members = getTypeMembers t
    let s = SurfaceOfType.Create typ netType members
    match netType with
    | Enum -> {s with Enum=toEnumTyp t |> Some }
    | SumType -> { s with UnionCases = Reflect.getUnionCases t |> Some}
    | _ -> s

  /// Get the surface of an assembly
  [<CompiledName("ExportedTypes")>]
  let exportedTypes assembly =
    let types = exportedTypes assembly
    let actual =
        types 
        |> List.map (fun t-> (t.Namespace, surfaceOfType t ))
        |> List.groupBy (fun (ns,_)->ns)
        |> List.map (fun (ns,ns_ts)-> (ns,ns_ts |> List.map snd)) 
        |> List.map (fun (ns,ts)-> ( 
                                    {
                                        Name = (if ns <> null then ns else "")
                                        Types= ts
                                    }))
    { Namespaces = actual}
   
  let private raw (asm:Assembly) : (string * string) Set =
    let ts = Reflect.exportedTypes asm

    let namespaces =
      ts
      |> List.map(fun x -> x.Namespace)
      |> List.distinct
      |> List.map (fun x -> x,(sprintf "%s (Namespace)" x))

    let types =
      ts
      |> List.filter(isSumType >> not)
      |> List.map (fun x -> (typeFullName x,typeInfo x))

    let enums' = ts
                |> List.filter (Reflect.tagNetType >> function | Enum -> true | _ -> false)
                |> List.map toEnumTyp
    let enums =
      enums'
      |> List.map (fun x -> Print.enums x)
      |> List.concat
    let enumValues =
      enums'
      |> List.map (fun x -> Print.enumValues x)
      |> List.concat
    let unionValues =
      ts
      |> List.filter (Reflect.tagNetType >> function | SumType -> true | _ -> false)
      |> List.map Reflect.getUnionCases
      |> List.map (fun x -> Print.unionValues x)
      |> List.concat
    
    ts
    |> List.filter(isSumType >> not)
    |> List.map (fun t->
          let fullName = typeFullName t
          let members = getTypeMembers t
          members |> List.map (fun m-> (fullName,Print.memberToString m))
    )
    |> List.concat
    |> List.append namespaces
    |> List.append types
    |> List.append enums
    |> List.append enumValues
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
  [<CompiledName("DiffAsStrings")>]
  let diffAsStrings : Assembly -> Assembly -> string array =
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


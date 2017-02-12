namespace SyntacticVersioning
open System.Reflection
open System
open Reflect

module Compare =
  [<AutoOpen>]
  module private Print=

    let enumValues (t: EnumTyp) : (string * string) list =
          t.Values
          |> List.map(fun (name,value) -> sprintf "%s:%s" name value)
          |> List.sort
          |> List.reduce(fun x y -> sprintf "%s; %s" x y)
          |> fun s ->
            let t' = t.FullName
            [ t',(sprintf "%s values: [ %s ]" t' s) ]
      
    let enums (t: EnumTyp) : (string * string) list =
          t.Values
          |> List.map(
            fun (name,_) ->
              let t' = t.FullName
              t',(sprintf "%s.%s : %s" t' name t')
            )

    let unionValues (t:UnionCases)=
        let t' = t.Type.FullName
        [ t', t.ToString() ] 

    let typeInfo : Type -> string =
      fun x ->
        let bt : Type -> string =
          function | null -> "none" | _ -> x.BaseType |> typeFullName
        
        sprintf "%s (.NET type: %A and base: %s)"
          (typeFullName x) (tagNetType x) (bt x.BaseType)

  let internal raw (asm:Assembly) : (string * string) Set =
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
               |> List.filter (Reflect.tagNetType >> ((=) NetType.Enum))
               |> List.map toEnumTyp
   let enums =
     List.collect (Print.enums) enums'
   let enumValues =
     List.collect (Print.enumValues) enums'
   let unionValues =
     List.collect Print.unionValues (ts
     |> List.filter (Reflect.tagNetType >> ((=) SumType))
     |> List.map Reflect.toUnionCases)

   let nonSumTypes =(ts|> List.filter(isSumType >> not))

   let members =
     (List.collect (fun t->
           let fullName = typeFullName t
           let members = getTypeMembers t
           members |> List.map (fun m-> (fullName, m.ToString()))) nonSumTypes)

   members
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


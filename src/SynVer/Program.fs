module SynVer
open System
open Argu
open SynVer
open System.Reflection
open System.IO

type CLIArguments =
    | Surface_of of path:string
    | Output of path:string
    | Diff of source:string * target:string
    | Bump of version:string * source:string * target:string
    | Magnitude of source:string * target:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Surface_of _ -> "Get the public api surface of the .net binary as json"
            | Magnitude _-> "Get the magnitude of the difference between two .net binaries"
            | Output _-> "Send output to file"
            | Diff _ -> "Get the difference between two .net binaries"
            | Bump _ -> "Get the next version based on the difference between two .net binaries"

let (|AssemblyFile|JsonFile|Other|) (maybeFile:string) =
  match maybeFile, File.Exists(maybeFile) with
  | f, true when f.EndsWith(".json") -> JsonFile
  | f, true when f.EndsWith(".dll") -> AssemblyFile
  | f, _  -> Other

let loadAssembly f =
  try
    Assembly.LoadFile (Path.GetFullPath f)
    |> Choice1Of2
  with ex ->
    (sprintf "Failed to load assembly %s due to %s\n%s" f ex.Message ex.StackTrace) 
    |> Choice2Of2
    
let mapFirst f= function
                | Choice1Of2 a -> Choice1Of2 (f a)
                | Choice2Of2 err -> Choice2Of2 err

let getSurfaceAreaOf (f:string): Choice<Package,string>=
  match f with
  | JsonFile ->
              File.ReadAllText f
              |> Lson.tryDeserialize

  | AssemblyFile -> 
        loadAssembly f
            |> mapFirst SurfaceArea.ofAssembly
  | Other -> Choice2Of2 "No dll or json specified"


let getDiff released modified : Choice<string,string>=
    let maybeReleased,maybeModified= getSurfaceAreaOf released, getSurfaceAreaOf modified
    match maybeReleased,maybeModified with
    | Choice1Of2 released, Choice1Of2 modified ->
        let changes =  SurfaceArea.diff released modified
        String.Join(Environment.NewLine, changes)
        |> Choice1Of2
    | _, _ ->
        let errors = 
            [maybeReleased;maybeModified] 
            |> List.choose (function Choice2Of2 e-> Some e | _ -> None)
            |> List.toArray
        
        Choice2Of2 (String.Join(Environment.NewLine, errors) )

let getBump version released modified : Choice<string*Version,string>=
    let maybeReleased,maybeModified= getSurfaceAreaOf released, getSurfaceAreaOf modified
    match maybeReleased,maybeModified with
    | Choice1Of2 released, Choice1Of2 modified ->
        SurfaceArea.bump version released modified
        |> Choice1Of2
    | _, _ ->
        let errors = 
            [maybeReleased;maybeModified] 
            |> List.choose (function Choice2Of2 err-> Some err | _ -> None)
            |> List.toArray
        
        Choice2Of2 (String.Join(Environment.NewLine, errors) )

[<EntryPoint>]
let main argv = 
    let parser = ArgumentParser.Create<CLIArguments>(programName = "synver.exe")

    let results = parser.Parse argv
    let writeResult (res:Choice<string,string>)=
        match res with
        | Choice1Of2 msg-> Console.WriteLine msg ; 0
        | Choice2Of2 msg->Console.Error.WriteLine msg ; 1

    let all = results.GetAllResults()
    if List.isEmpty all then
        Choice2Of2(parser.PrintUsage())
    elif results.IsUsageRequested then
        Choice1Of2(parser.PrintUsage())
    else
        let maybeFile = results.TryGetResult(<@ Surface_of @>)
        let maybeDiff = results.TryGetResult(<@ Diff @>)
        let maybeBump = results.TryGetResult(<@ Bump @>)
        let maybeMagnitude = results.TryGetResult(<@ Magnitude @>)
        let maybeOutput = results.TryGetResult(<@ Output @>)

        match maybeFile, maybeDiff, maybeMagnitude, maybeBump with
        | Some file, None, None, None ->
            loadAssembly file
            |> mapFirst (
                SurfaceArea.ofAssembly 
                >> Lson.serialize
            )
        | None, Some (released, modified), None, None ->
            getDiff released modified
        | None, None, Some (released,modified), None ->
            getBump "0.0.0" released modified
            |> function 
                | Choice1Of2 (_,version)->version.ToString() |>Choice1Of2
                | Choice2Of2 a->Choice2Of2 a
        | None, None, None, Some (version,released,modified) ->
            getBump version released modified
            |> function 
                | Choice1Of2 (version,_)->version |>Choice1Of2
                | Choice2Of2 a->Choice2Of2 a
        | _,_,_,_ ->
            Choice2Of2(parser.PrintUsage())
        |> (fun res-> 
            match res, maybeOutput with
            | Choice1Of2 content, Some output-> 
                File.WriteAllText(output, content)
                res
            | _ -> res
            )
    |> writeResult
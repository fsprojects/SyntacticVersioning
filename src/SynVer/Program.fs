module SynVer
open System
open Argu
open SynVer
open System.Reflection
open System.IO
open Mono.Cecil

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
            | Surface_of _ -> "Get the public api surface of the .net binary as lson"
            | Magnitude _-> "Get the magnitude of the difference between two .net binaries"
            | Output _-> "Send output to file"
            | Diff _ -> "Get the difference between two .net binaries"
            | Bump _ -> "Get the next version based on the difference between two .net binaries"

let (|AssemblyFile|LsonFile|Other|) (maybeFile:string) =
  match maybeFile, File.Exists(maybeFile) with
  | f, true when f.EndsWith(".lson") -> LsonFile
  | f, true when f.EndsWith(".dll") -> AssemblyFile
  | f, _  -> Other

let loadAssembly f =
  try
    Decompile.readAssembly (Path.GetFullPath f)
    |> Ok
  with ex ->
    (sprintf "Failed to load assembly %s due to %s\n%s" f ex.Message ex.StackTrace) 
    |> Error


let getSurfaceAreaOf (f:string): Result<Package,string>=
  match f with
  | LsonFile ->
    let res = File.ReadAllText f
              |> Lson.tryDeserialize
    match res with
    | Some p -> Ok p
    | None -> Error "Couldn't deserialize"
  | AssemblyFile -> 
        loadAssembly f
        |> Result.map SurfaceArea.ofAssemblyDefinition
  | Other -> Error "No dll or lson specified"


let getDiff released modified : Result<string,string>=
    let maybeReleased,maybeModified= getSurfaceAreaOf released, getSurfaceAreaOf modified
    match maybeReleased,maybeModified with
    | Ok released, Ok modified ->
        let changes =  SurfaceArea.diff released modified
        String.Join(Environment.NewLine, changes)
        |> Ok
    | _, _ ->
        let errors = 
            [maybeReleased;maybeModified] 
            |> List.choose (function Error e-> Some e | _ -> None)
            |> List.toArray
        
        Error (String.Join(Environment.NewLine, errors) )

let getBump version released modified : Result<string*Version,string>=
    let maybeReleased,maybeModified= getSurfaceAreaOf released, getSurfaceAreaOf modified
    match maybeReleased,maybeModified with
    | Ok released, Ok modified ->
        SurfaceArea.bump version released modified
        |> Ok
    | _, _ ->
        let errors = 
            [maybeReleased;maybeModified] 
            |> List.choose (function Error err-> Some err | _ -> None)
            |> List.toArray
        
        Error (String.Join(Environment.NewLine, errors) )

[<EntryPoint>]
let main argv = 
    let parser = ArgumentParser.Create<CLIArguments>(programName = "synver.exe")

    let results = parser.Parse argv
    let writeResult (res:Result<string,string>)=
        match res with
        | Ok msg-> Console.WriteLine msg ; 0
        | Error msg->Console.Error.WriteLine msg ; 1

    let all = results.GetAllResults()
    if List.isEmpty all then
        Error(parser.PrintUsage())
    elif results.IsUsageRequested then
        Ok(parser.PrintUsage())
    else
        let maybeFile = results.TryGetResult(<@ Surface_of @>)
        let maybeDiff = results.TryGetResult(<@ Diff @>)
        let maybeBump = results.TryGetResult(<@ Bump @>)
        let maybeMagnitude = results.TryGetResult(<@ Magnitude @>)
        let maybeOutput = results.TryGetResult(<@ Output @>)

        match maybeFile, maybeDiff, maybeMagnitude, maybeBump with
        | Some file, None, None, None ->
            loadAssembly file
            |> Result.map (
                SurfaceArea.ofAssemblyDefinition 
                >> Lson.serialize
            )
        | None, Some (released, modified), None, None ->
            getDiff released modified
        | None, None, Some (released,modified), None ->
            getBump "0.0.0" released modified
            |> function 
                | Ok (_,version)->version.ToString() |>Ok
                | Error a->Error a
        | None, None, None, Some (version,released,modified) ->
            getBump version released modified
            |> function 
                | Ok (version,_)->version |>Ok
                | Error a->Error a
        | _,_,_,_ ->
            Error(parser.PrintUsage())
        |> (fun res-> 
            match res, maybeOutput with
            | Ok content, Some output-> 
                File.WriteAllText(output, content)
                res
            | _ -> res
            )
    |> writeResult

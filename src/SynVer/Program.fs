module SynVer.Tool
open System
open Argu
open SynVer
open System.Reflection
open System.IO
open Chiron

type CLIArguments =
    | Surface_of of path:string
    | Output of path:string
    | Diff of source:string * target:string
    | Magnitude of source:string * target:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Surface_of _ -> "Get the public api surface of the .net binary as json"
            | Magnitude _-> "Get the magnitude of the difference between two .net binaries"
            | Output _-> "Send output to file"
            | Diff _ -> "Get the difference between two .net binaries"

let (|AssemblyFile|JsonFile|Other|) (maybeFile:string) =
  match maybeFile, File.Exists(maybeFile) with
  | f, true when f.EndsWith(".json") -> JsonFile
  | f, true when f.EndsWith(".dll") -> AssemblyFile
  | f, _  -> Other

let getSurfaceAreaOf (f:string): Choice<Package,string>=
  match f with
  | JsonFile ->
              File.ReadAllText f
              |> Json.parse
              |> Json.tryDeserialize

  | AssemblyFile -> 
              Assembly.LoadFile f
              |> SurfaceArea.ofAssembly 
              |> Choice1Of2
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
let getMagnitude released modified : Choice<string,string>=
    let maybeReleased,maybeModified= getSurfaceAreaOf released, getSurfaceAreaOf modified
    match maybeReleased,maybeModified with
    | Choice1Of2 released, Choice1Of2 modified ->
        let (_,magnitude) =  SurfaceArea.bump "0.0.0" released modified
        magnitude.ToString() |> Choice1Of2
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
        let maybeMagnitude = results.TryGetResult(<@ Magnitude @>)
        let maybeOutput = results.TryGetResult(<@ Output @>)

        match maybeFile, maybeDiff, maybeMagnitude with
        | Some file, None, None ->
            let assembly = Assembly.LoadFrom(file)
            (SurfaceArea.ofAssembly assembly)
            |> Json.serialize
            |> Json.formatWith JsonFormattingOptions.Pretty
            |> Choice1Of2
        | None, Some (released, modified), None ->
            getDiff released modified
        | None, None, Some (released,modified) ->
            getMagnitude released modified
        | _,_,_ ->
            Choice2Of2(parser.PrintUsage())
        |> (fun res-> 
            match res, maybeOutput with
            | Choice1Of2 content, Some output-> 
                File.WriteAllText(output, content)
                res
            | _ -> res
            )
    |> writeResult
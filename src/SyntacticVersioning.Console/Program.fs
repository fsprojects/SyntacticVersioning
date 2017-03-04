module SyntacticVersioning.Tool
open System
open Argu
open SyntacticVersioning
open System.Reflection
open System.IO
open Chiron

type CLIArguments =
    | Surface_of of path:string
    | Output of path:string
    | Diff of source:string * target:string
    | Magnitude of source:string * target:string
    | Source of uri:string
    | DotNetVersion of NuGet.dotNet
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Surface_of _ -> "Get the public api surface of the .net binary as json"
            | Magnitude _-> "Get the magnitude of the difference between two .net binaries"
            | Output _-> "Send output to file"
            | Diff _ -> "Get the difference between two .net binaries"
            | Source _ -> "The packages source to use for this command"
            | DotNetVersion _ -> "The dotnet version to to use comparing with NuGet"

let (|AssemblyFile|NuGet|JsonFile|Empty|) (maybeFile:string) =
  match maybeFile, File.Exists(maybeFile) with
  | f, true when f.EndsWith(".json") -> JsonFile
  | f, true when f.EndsWith(".dll") -> AssemblyFile
  | f, _    when String.IsNullOrEmpty f -> Empty
  | _, _ -> NuGet

let choiceToResult=function 
    | Choice1Of2 a-> Ok a 
    | Choice2Of2 a-> Result.Error a

let getSurfaceAreaOf nuGetGet (f:string): Result<Package,string>=

  match f with
  | JsonFile ->
              File.ReadAllText f
              |> Json.parse
              |> Json.tryDeserialize
              |> choiceToResult
  | AssemblyFile -> 
              Assembly.LoadFile f
              |> SurfaceArea.ofAssembly 
              |> Ok
  | NuGet -> nuGetGet f
  | Empty -> Result.Error "No package id, dll or json specified"


let getDiff nuGetGet released modified : Result<string,string>=
    let maybeReleased,maybeModified= getSurfaceAreaOf nuGetGet released, getSurfaceAreaOf nuGetGet modified
    match maybeReleased,maybeModified with
    | Ok released, Ok modified ->
        let changes =  SurfaceArea.diff released modified
        String.Join(Environment.NewLine, changes)
        |> Ok
    | _, _ ->
        let errors = 
            [maybeReleased;maybeModified] 
            |> List.choose (function Result.Error e-> Some e | _ -> None)
            |> List.toArray
        
        Result.Error (String.Join(Environment.NewLine, errors) )
let getMagnitude nuGetGet released modified : Result<string,string>=
    let maybeReleased,maybeModified= getSurfaceAreaOf nuGetGet released, getSurfaceAreaOf nuGetGet modified
    match maybeReleased,maybeModified with
    | Ok released, Ok modified ->
        let (_,magnitude) =  SurfaceArea.bump "0.0.0" released modified
        magnitude.ToString() |> Ok
    | _, _ ->
        let errors = 
            [maybeReleased;maybeModified] 
            |> List.choose (function Result.Error err-> Some err | _ -> None)
            |> List.toArray
        
        Result.Error (String.Join(Environment.NewLine, errors) )


[<EntryPoint>]
let main argv = 
    let parser = ArgumentParser.Create<CLIArguments>(programName = "SyntacticVersioning.Tool.exe")

    let results = parser.Parse argv
    let writeResult (res:Result<string,string>)=
        match res with
        | Ok msg-> Console.WriteLine msg ; 0
        | Result.Error msg->Console.Error.WriteLine msg ; 1

    let all = results.GetAllResults()
    if List.isEmpty all then
        Result.Error(parser.PrintUsage())
    elif results.IsUsageRequested then
        Ok(parser.PrintUsage())
    else
        let maybeFile = results.TryGetResult(<@ Surface_of @>)
        let maybeDiff = results.TryGetResult(<@ Diff @>)
        let maybeMagnitude = results.TryGetResult(<@ Magnitude @>)
        let maybeOutput = results.TryGetResult(<@ Output @>)
        let source = results.TryGetResult(<@ Source @>)
        let maybeDotNet = results.TryGetResult(<@ DotNetVersion @>)
        let nuGetGet (package:string) =
          let possiblyPackageAndVersion=
            match package.Split(':')|>List.ofArray with
            | [p;v] -> Ok(p,Some v)
            | [p] -> Ok(p,None)
            | _ -> Result.Error (sprintf "Could not interpret package id %s" package)
          match maybeDotNet, possiblyPackageAndVersion with
          | Some dotNet, Ok (packageId,version)->
              NuGet.get source packageId version dotNet
              |> Result.map (fun (_,a) -> SurfaceArea.ofAssembly a)
          | None, _ -> Result.Error "You need to specify the .net version"
          | _ , Result.Error msg -> Result.Error msg
          
        match maybeFile, maybeDiff, maybeMagnitude with
        | Some file, None, None ->
            let assembly = Assembly.LoadFrom(file)
            (SurfaceArea.ofAssembly assembly)
            |> Json.serialize
            |> Json.formatWith JsonFormattingOptions.Pretty
            |> Result.Ok
        | None, Some (released, modified), None ->
            getDiff nuGetGet released modified
        | None, None, Some (released,modified) ->
            getMagnitude nuGetGet released modified
        | _,_,_ ->
            Result.Error(parser.PrintUsage())
        |> (fun res-> 
            match res, maybeOutput with
            | Result.Ok content, Some output-> 
                File.WriteAllText(output, content)
                res
            | _ -> res
            )
    |> writeResult
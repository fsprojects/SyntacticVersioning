module SynVer
open System
open Argu
open SynVer
open System.Reflection
open System.IO
open Mono.Cecil

type CLIArguments =
    | Surface_of of path:string
    | Decompile
    | Output of path:string
    | Nuget_source of package:string * version:string * framework:string
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
            | Nuget_source _ -> "Load the source assembly from a NuGet package"
            | Diff _ -> "Get the difference between two .net binaries"
            | Bump _ -> "Get the next version based on the difference between two .net binaries"
            | Decompile -> "Decompile instead of using reflection"

let (|AssemblyFile|LsonFile|Other|) (maybeFile:string) =
  match maybeFile, File.Exists(maybeFile) with
  | f, true when f.EndsWith(".lson") -> LsonFile
  | f, true when f.EndsWith(".dll") -> AssemblyFile
  | f, _  -> Other
type 'a IAssemblyInterpreter=interface
  abstract member ReadAssembly: string->'a
  abstract member OfAssembly: 'a->Package
end

let inline loadAssembly f readAssembly=
  try
    readAssembly (Path.GetFullPath f)
    |> Ok
  with ex ->
    (sprintf "Failed to load assembly %s due to %s\n%s" f ex.Message ex.StackTrace) 
    |> Error

type AssemblySource =
  | LocalFile of path:string
  | NuGetPackage of package:string * version:string * framework:string * assembly:string

let getSurfaceAreaOf source (i:IAssemblyInterpreter<_>): Result<Package,string>=
  match source with
  | NuGetPackage (package, version, framework, assembly) ->
    NuGetPackage.downloadAssembly package version framework assembly
    |> Result.bind (fun path ->
      loadAssembly path i.ReadAssembly
      |> Result.map i.OfAssembly)
  | LocalFile f ->
    match f with
    | LsonFile ->
      let res = File.ReadAllText f
                |> Lson.tryDeserialize
      match res with
      | Some p -> Ok p
      | None -> Error "Couldn't deserialize"
    | AssemblyFile ->
          loadAssembly f i.ReadAssembly
          |> Result.map i.OfAssembly
    | Other -> Error "No dll or lson specified"

let getDiff released modified (i:IAssemblyInterpreter<_>): Result<string,string>=
    let maybeReleased,maybeModified= getSurfaceAreaOf released i,
                                     getSurfaceAreaOf modified i
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

let getBump version released modified (i:IAssemblyInterpreter<_>): Result<string*Version,string>=
    let maybeReleased, maybeModified= getSurfaceAreaOf released i, getSurfaceAreaOf modified i

    match maybeReleased, maybeModified with
    | Ok released, Ok modified ->
        SurfaceArea.bump version released modified
        |> Ok
    | _, _ ->
        let errors = 
            [maybeReleased;maybeModified] 
            |> List.choose (function Error err-> Some err | _ -> None)
            |> List.toArray
        
        Error (String.Join(Environment.NewLine, errors) )
let decompiler = { new IAssemblyInterpreter<AssemblyDefinition> with
                   member __.ReadAssembly f = Decompile.readAssembly f
                   member __.OfAssembly a = SurfaceArea.ofAssemblyDefinition a
                 }
let reflector = { new IAssemblyInterpreter<Assembly> with
                  member __.ReadAssembly f = Assembly.LoadFile f
                  member __.OfAssembly a = SurfaceArea.ofAssembly a
                }
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
        let maybeNugetSource = results.TryGetResult(<@ Nuget_source @>)
        let decompile = results.TryGetResult(<@ Decompile @>) |> Option.map (fun _->true) |> Option.defaultValue false
        let source file =
          match maybeNugetSource with
          | Some (package, version, framework) -> NuGetPackage (package, version, framework, file)
          | None -> LocalFile file

        match maybeFile, maybeDiff, maybeMagnitude, maybeBump with
        | Some file, None, None, None ->
            match maybeNugetSource with
            | Some _ ->
              let result =
                if decompile then getSurfaceAreaOf (source file) decompiler
                else getSurfaceAreaOf (source file) reflector
              result |> Result.map Lson.serialize
            | None ->
              if decompile then
                loadAssembly file Decompile.readAssembly
                |> Result.map (SurfaceArea.ofAssemblyDefinition >> Lson.serialize)
              else
                loadAssembly file Assembly.LoadFile
                |> Result.map (SurfaceArea.ofAssembly >> Lson.serialize)
        | None, Some (released, modified), None, None ->
          if decompile then getDiff (source released) (LocalFile modified) decompiler
          else getDiff (source released) (LocalFile modified) reflector
        | None, None, Some (released,modified), None ->
          let res =
            if decompile then getBump "0.0.0" (source released) (LocalFile modified) decompiler
            else getBump "0.0.0" (source released) (LocalFile modified) reflector
          res |> Result.map (snd>>string)
        | None, None, None, Some (version,released,modified) ->
          let res =
            if decompile then getBump version (source released) (LocalFile modified) decompiler
            else getBump version (source released) (LocalFile modified) reflector
          res |> Result.map fst
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

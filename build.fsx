#r "paket: groupref Build //"

open Fake
open Fake.Tools
open Fake.DotNet
open System
open System.IO
open Fake.Core
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake
open Fake.IO
open Fake.Tools
open Fake.Api

let visualStudioInstallDir = Environment.environVarOrDefault "VSInstallDir" @"C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\"

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "SynVer"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Syntactic (Semantic) Versioning for .NET libraries heavily inspired in elm-package (bump and diff)"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "Syntactic (Semantic) Versioning for .NET libraries heavily inspired in elm-package (bump and diff)"

// List of author names (for NuGet package)
let authors = [ "Oskar Gewalli; SPISE MISU ApS (Ramón Soto Mathiesen)" ]

// Tags for your project (for NuGet package)
let tags = "syntactic semantic versioning dotnet assembly nuget csharp fsharp bump diff"

// File system information
let solutionFile  = "SynVer.sln"

// Default target configuration
let configuration = "Release"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "fsprojects"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

// The name of the project on GitHub
let gitName = "SyntacticVersioning"

// The url for the raw files hosted
let gitRaw = Environment.environVarOrDefault "gitRaw" "https://raw.githubusercontent.com/fsprojects"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

let (|FsFile|CsFile|) (codeFileName:string) =
    match codeFileName with
    | f when f.EndsWith(".fsx") -> FsFile
    | f when f.EndsWith(".fs")  -> FsFile
    | f when f.EndsWith(".cs")  -> CsFile
    | _                         -> failwith (sprintf "Code file %s not supported. Unknown code type." codeFileName)


let exampleProjects = "tests/ExampleProjects/"

module DotNet = 
  let binary =
    CreateProcess.fromRawCommand "which" ["dotnet"]
      |> CreateProcess.redirectOutput
      |> CreateProcess.ensureExitCode
      |> Proc.run
      |> fun r -> r.Result.Output.Trim()

module SDKs =
  type Sdk = { version: string; path: string}

  let parseSdk (line: string) =
    match line.Split(' ') with
    | [| s |] when String.IsNullOrWhiteSpace s -> None
    | [| version; location |] -> Some { version = version; path = location.TrimEnd(']').TrimStart('[') }
    | _ ->  failwithf "unknown version string %s" line

  let findVersions () =
    let result =
      CreateProcess.fromRawCommand DotNet.binary ["--list-sdks"]
      |> CreateProcess.redirectOutput
      |> CreateProcess.ensureExitCode
      |> Proc.run
    result.Result.Output
    |> String.split '\n'
    |> List.choose parseSdk

  let latestVersion () = findVersions () |> List.last

module create =
    let fsiExe = 
        let source = (__SOURCE_DIRECTORY__ @@ "packages" @@ "build" @@ "FSharp.Compiler.Tools" @@ "tools" @@ "fsc.exe")
        if Environment.isWindows then source else (__SOURCE_DIRECTORY__ @@ "mono.fsc.sh")
    let failWhenNon0 name res = if res<>0 then failwithf "Non 0 exit code %d of %s" res name

    let execCsc cscPath outputDll refs target sources =
      let refs = 
        refs |> List.map (fun r -> sprintf "-r:%s" r)
      CreateProcess.fromRawCommand DotNet.binary ([ cscPath
                                                    sprintf "-out:%s" outputDll
                                                    sprintf "-target:%s" target ] @ refs @ sources)
      |> CreateProcess.ensureExitCode
      |> Proc.run
      |> ignore

    let sdk = SDKs.latestVersion()

    let fsharpProjectFromFile fileName name =
        let dllName = sprintf "%s.dll" name
        let dll =  exampleProjects </> "lib"</> dllName
        let assemblyInfo =  exampleProjects </> "src"</> "AssemblyInfo.fs"
        [fileName; assemblyInfo]
        |> Fsc.compileExternal fsiExe [ Fsc.Out dll; Fsc.Target Fsc.Library ]
        //fsharpc --target:library --out:"./lib/"${name%.*}".dll" $f "./src/AssemblyInfo.fs"

    let csharpProjectFromFile fileName name =
        let cscLocation (sdk: SDKs.Sdk) =
          Path.Combine(sdk.path, sdk.version, "Roslyn", "bincore", "csc.dll")

        let netstandardDll (sdk: SDKs.Sdk) =
          Path.Combine("packages", "build", "NETStandard.Library", "build", "netstandard2.0", "ref", "netstandard.dll") 

        let dllName = sprintf "%s.dll" name
        let dll =  exampleProjects </> "lib"</> dllName
        let assemblyInfo =  exampleProjects </> "src"</> "AssemblyInfo.cs"

        execCsc (cscLocation sdk) dll [netstandardDll sdk] "library" [fileName; assemblyInfo]
        //csc -target:library -out:"./lib/"${name%.*}".dll" $f "./src/AssemblyInfo.cs"

Target.create "ExampleProjects" (fun _ ->
    !! "tests/ExampleProjects/src/**/*.?s"
    |> Seq.map (fun f -> (f, Path.GetFileNameWithoutExtension(f)) )
    |> Seq.filter (fun (f, n) -> n<>"AssemblyInfo")
    |> Seq.iter (fun (fileName, name) ->
        match fileName with
        | FsFile -> create.fsharpProjectFromFile fileName name
        | CsFile -> create.csharpProjectFromFile fileName name
    )
)

// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the
// src folder to support multiple project outputs
Target.create "CopyBinaries" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) </> "bin" </> configuration, "bin" </> (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
)

// --------------------------------------------------------------------------------------
// Clean build results

let vsProjProps =
    [ ("Configuration", configuration) ]

Target.create "Clean" (fun _ ->
  DotNet.exec id "clean" "" |> ignore
  Shell.cleanDirs ["bin"; "temp"; "docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Restore" (fun _ ->
  DotNet.restore id solutionFile
)

Target.create "Build" (fun _ ->
    DotNet.build (fun o -> { o with Configuration = DotNet.BuildConfiguration.Release }) solutionFile
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target.create "RunNetCoreTests" (fun _ ->
  ["netcoreapp2.2";"netcoreapp3.0"]
  |> List.iter (fun tfm -> 
    DotNet.exec id "run" (sprintf "--project tests/SynVer.Tests/SynVer.Tests.fsproj --summary -f %s" tfm)
    |> ignore
  )
)

Target.create "RunTests" ignore

// --------------------------------------------------------------------------------------
// Generate the documentation


// let fakePath = "packages" </> "build" </> "FAKE" </> "tools" </> "FAKE.exe"
// let fakeStartInfo script workingDirectory args fsiargs environmentVars =
//     (fun (info: System.Diagnostics.ProcessStartInfo) ->
//         #if MONO
//         info.FileName <- "mono"
//         let fullFakePath = System.IO.Path.GetFullPath fakePath
//         info.Arguments <- sprintf "%s %s --fsiargs -d:FAKE %s \"%s\"" fullFakePath args fsiargs script
//         #else
//         info.FileName <- System.IO.Path.GetFullPath fakePath
//         info.Arguments <- sprintf "%s --fsiargs -d:FAKE %s \"%s\"" args fsiargs script
//         #endif
//         info.WorkingDirectory <- workingDirectory
//         let setVar k v =
//             info.EnvironmentVariables.[k] <- v
//         for (k, v) in environmentVars do
//             setVar k v
//         setVar "MSBuild" msBuildExe
//         setVar "GIT" Git.CommandHelper.gitPath
//         setVar "FSI" fsiPath)

// /// Run the given buildscript with FAKE.exe
// let executeFAKEWithOutput workingDirectory script fsiargs envArgs =
//     let exitCode =
//         ExecProcessWithLambdas
//             (fakeStartInfo script workingDirectory "" fsiargs envArgs)
//             TimeSpan.MaxValue false ignore ignore
//     System.Threading.Thread.Sleep 1000
//     exitCode

// // Documentation
// let buildDocumentationTarget fsiargs target =
//     Trace.tracefn "Building documentation (%s), this could take some time, please wait..." target
//     let exit = executeFAKEWithOutput "docs/tools" "generate.fsx" fsiargs ["target", target]
//     if exit <> 0 then
//         failwith "generating reference documentation failed"
//     ()

Target.create "GenerateReferenceDocs" (fun _ -> ()
    //buildDocumentationTarget "-d:RELEASE -d:REFERENCE" "Default"
)

let generateHelp' fail debug =
    let args =
        if debug then "--define:HELP"
        else "--define:RELEASE --define:HELP"
    try
        //buildDocumentationTarget args "Default"
        Trace.traceImportant "Help generated"
    with
    | e when not fail ->
        Trace.traceImportant "generating help documentation failed"

let generateHelp fail =
    generateHelp' fail false

Target.create "GenerateHelp" (fun _ ->
    Shell.rm "docs/content/release-notes.md"
    Shell.cp "RELEASE_NOTES.md" "docs/content/"
    Shell.mv "docs/content/RELEASE_NOTES.md" "docs/content/release-notes.md"

    Shell.rm "docs/content/license.md"
    Shell.cp "LICENSE.txt" "docs/content/"
    Shell.mv "docs/content/LICENSE.txt" "docs/content/license.md"

    generateHelp true
)

Target.create "GenerateHelpDebug" (fun _ ->
    Shell.rm "docs/content/release-notes.md"
    Shell.cp "RELEASE_NOTES.md" "docs/content/"
    Shell.mv "docs/content/RELEASE_NOTES.md" "docs/content/release-notes.md" 

    Shell.rm "docs/content/license.md"
    Shell.cp "LICENSE.txt""docs/content/"
    Shell.mv "docs/content/LICENSE.txt" "docs/content/license.md" 

    generateHelp' true true
)

Target.create "KeepRunning" (fun _ ->
    failwith "! docs"
    (*
    use watcher = !! "docs/content/**/*.*" |> WatchChanges (fun _ ->
         generateHelp' true true
    )

    traceImportant "Waiting for help edits. Press any key to stop."

    System.Console.ReadKey() |> ignore

    watcher.Dispose()
    *)
)

Target.create "GenerateDocs" ignore

let createIndexFsx lang =
    let content = """(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../../bin"

(**
F# Project Scaffold ({0})
=========================
*)
"""
    let targetDir = "docs/content" </> lang
    let targetFile = targetDir </> "index.fsx"
    Shell.mkdir targetDir
    System.IO.File.WriteAllText(targetFile, System.String.Format(content, lang))

Target.create "AddLangDocs" (fun _ ->
    let args = System.Environment.GetCommandLineArgs()
    if args.Length < 4 then
        failwith "Language not specified."

    args.[3..]
    |> Seq.iter (fun lang ->
        if lang.Length <> 2 && lang.Length <> 3 then
            failwithf "Language must be 2 or 3 characters (ex. 'de', 'fr', 'ja', 'gsw', etc.): %s" lang

        let templateFileName = "template.cshtml"
        let templateDir = "docs/tools/templates"
        let langTemplateDir = templateDir </> lang
        let langTemplateFileName = langTemplateDir </> templateFileName

        if System.IO.File.Exists(langTemplateFileName) then
            failwithf "Documents for specified language '%s' have already been added." lang

        Shell.mkdir langTemplateDir
        Shell.cp_r langTemplateDir (templateDir </> templateFileName)

        createIndexFsx lang)
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target.create "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    Shell.cleanDir tempDocsDir
    Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    Shell.copyRecursive "docs/output" tempDocsDir true |> Trace.tracefn "%A"
    Git.Staging.stageAll tempDocsDir
    Git.Commit.execExtended tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion) (String.concat "\n" release.Notes)
    Git.Branches.push tempDocsDir
)

let getUserInput prompt =
  printfn "%s" prompt
  Console.ReadLine()

Target.create "Release" (fun _ ->
    let user =
        match Environment.environVarOrNone "github-user" with
        | Some s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "Username: "
    let pw =
        match Environment.environVarOrNone "github-pw" with
        | Some s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Git.Staging.stageAll ""
    Git.Commit.execExtended "" (sprintf "Bump version to %s" release.NugetVersion) (String.concat "\n" release.Notes)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    // release on github
    GitHub.createClient user pw
    |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    // TODO: |> uploadFile "PATH_TO_FILE"
    |> GitHub.publishDraft
    |> Async.RunSynchronously
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "All" ignore

open Fake.Core.TargetOperators

"Restore"
  ==> "Build"
  ==> "CopyBinaries"
  ==> "ExampleProjects"
  ==> "RunTests"
  ==> "GenerateReferenceDocs"
  ==> "GenerateDocs"
  ==> "All"

"GenerateHelp"
  ==> "GenerateReferenceDocs"
  ==> "GenerateDocs"

"GenerateHelpDebug"
  ==> "KeepRunning"

"Clean"
  ==> "Release"

"ExampleProjects" ==> "RunNetCoreTests" ==> "RunTests"

"ReleaseDocs"
  ==> "Release"

Target.runOrDefaultWithArguments "All"

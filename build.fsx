// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
open Fake.Core
open Fake.Core.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.DotNet
open Fake.IO
open Fake.Tools

let visualStudioInstallDir = getBuildParamOrDefault "VSInstallDir" @"C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\"

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
let gitRaw = environVarOrDefault "gitRaw" "https://raw.githubusercontent.com/fsprojects"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = LoadReleaseNotes "RELEASE_NOTES.md"

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
    | f when f.EndsWith(".fs") -> FsFile
    | f when f.EndsWith(".cs") -> CsFile
    | _                           -> failwith (sprintf "Code file %s not supported. Unknown code type." codeFileName)


let exampleProjects = "tests/ExampleProjects/"
#if MONO
let extraCscParams = id
#else
let extraCscParams (parameters:CscHelper.CscParams) = 
  { parameters with ToolPath = visualStudioInstallDir </> @"MSBuild\15.0\Bin\Roslyn\csc.exe" }
#endif

module create=
    open FscHelper
    let fsharpProjectFromFile fileName name=
        let dllName = sprintf "%s.dll" name
        let dll =  exampleProjects </> "lib"</> dllName
        let assemblyInfo =  exampleProjects </> "src"</> "AssemblyInfo.fs"
        [fileName; assemblyInfo] |>
            compile 
                [ Out dll
                  Target Library ]
        |> ignore
        //fsharpc --target:library --out:"./lib/"${name%.*}".dll" $f "./src/AssemblyInfo.fs"
    open CscHelper
    let csharpProjectFromFile fileName name=
        let dllName = sprintf "%s.dll" name
        let dll =  exampleProjects </> "lib"</> dllName
        let assemblyInfo =  exampleProjects </> "src"</> "AssemblyInfo.cs"
        [fileName; assemblyInfo] |>
            csc (fun parameters ->
                { parameters with Output = dll; Target = Library } |> extraCscParams)
        |> ignore
        //mcs -target:library -out:"./lib/"${name%.*}".dll" $f "./src/AssemblyInfo.cs"

Target.Create "ExampleProjects" (fun _ ->
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
Target.Create "CopyBinaries" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) </> "bin" </> configuration, "bin" </> (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> CopyDir toDir fromDir (fun _ -> true))
)

// --------------------------------------------------------------------------------------
// Clean build results

let vsProjProps = 
#if MONO
    [ ("DefineConstants","MONO"); ("Configuration", configuration) ]
#else
    [ ("Configuration", configuration); ("Platform", "Any CPU") ]
#endif

Target.Create "Clean" (fun _ ->
    !! solutionFile |> MSBuildReleaseExt "" vsProjProps "Clean" |> ignore
    CleanDirs ["bin"; "temp"; "docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.Create "Restore" (fun _ ->
    !! solutionFile
    |> MSBuildReleaseExt "" vsProjProps "Restore"
    |> ignore
)

Target.Create "Build" (fun _ ->
    !! solutionFile
    |> MSBuildReleaseExt "" vsProjProps "Rebuild"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target.Create "RunNet4Tests" (fun _ ->
    Shell.Exec ("tests/SynVer.Tests/bin/"+configuration+"/net461/SynVer.Tests.exe","--summary")
    |> fun r -> if r<>0 then failwith "SynVer.Tests.exe failed"
)

Target.Create "RunNetCoreTests" (fun _ ->
  DotNetCli.RunCommand id (" run --project tests/SynVer.Tests/SynVer.Tests.fsproj --framework netcoreapp2.0 --summary")
)

Target.Create "RunTests" Target.DoNothing

// --------------------------------------------------------------------------------------
// Generate the documentation


let fakePath = "packages" </> "build" </> "FAKE" </> "tools" </> "FAKE.exe"
let fakeStartInfo script workingDirectory args fsiargs environmentVars =
    (fun (info: System.Diagnostics.ProcessStartInfo) ->
        #if MONO
        info.FileName <- "mono"
        let fullFakePath = System.IO.Path.GetFullPath fakePath
        info.Arguments <- sprintf "%s %s --fsiargs -d:FAKE %s \"%s\"" fullFakePath args fsiargs script
        #else
        info.FileName <- System.IO.Path.GetFullPath fakePath
        info.Arguments <- sprintf "%s --fsiargs -d:FAKE %s \"%s\"" args fsiargs script
        #endif
        info.WorkingDirectory <- workingDirectory
        let setVar k v =
            info.EnvironmentVariables.[k] <- v
        for (k, v) in environmentVars do
            setVar k v
        setVar "MSBuild" msBuildExe
        setVar "GIT" Git.CommandHelper.gitPath
        setVar "FSI" fsiPath)

/// Run the given buildscript with FAKE.exe
let executeFAKEWithOutput workingDirectory script fsiargs envArgs =
    let exitCode =
        ExecProcessWithLambdas
            (fakeStartInfo script workingDirectory "" fsiargs envArgs)
            TimeSpan.MaxValue false ignore ignore
    System.Threading.Thread.Sleep 1000
    exitCode

// Documentation
let buildDocumentationTarget fsiargs target =
    trace (sprintf "Building documentation (%s), this could take some time, please wait..." target)
    let exit = executeFAKEWithOutput "docs/tools" "generate.fsx" fsiargs ["target", target]
    if exit <> 0 then
        failwith "generating reference documentation failed"
    ()

Target.Create "GenerateReferenceDocs" (fun _ ->
    buildDocumentationTarget "-d:RELEASE -d:REFERENCE" "Default"
)

let generateHelp' fail debug =
    let args =
        if debug then "--define:HELP"
        else "--define:RELEASE --define:HELP"
    try
        buildDocumentationTarget args "Default"
        traceImportant "Help generated"
    with
    | e when not fail ->
        traceImportant "generating help documentation failed"

let generateHelp fail =
    generateHelp' fail false

Target.Create "GenerateHelp" (fun _ ->
    DeleteFile "docs/content/release-notes.md"
    CopyFile "docs/content/" "RELEASE_NOTES.md"
    Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    DeleteFile "docs/content/license.md"
    CopyFile "docs/content/" "LICENSE.txt"
    Rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp true
)

Target.Create "GenerateHelpDebug" (fun _ ->
    DeleteFile "docs/content/release-notes.md"
    CopyFile "docs/content/" "RELEASE_NOTES.md"
    Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    DeleteFile "docs/content/license.md"
    CopyFile "docs/content/" "LICENSE.txt"
    Rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp' true true
)

Target.Create "KeepRunning" (fun _ ->
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

Target.Create "GenerateDocs" Target.DoNothing

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
    ensureDirectory targetDir
    System.IO.File.WriteAllText(targetFile, System.String.Format(content, lang))

Target.Create "AddLangDocs" (fun _ ->
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

        ensureDirectory langTemplateDir
        Copy langTemplateDir [ templateDir </> templateFileName ]

        createIndexFsx lang)
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target.Create "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Git.Commit.Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)
#if !MONO
#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target.Create "Release" (fun _ ->
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    // release on github
    createClient user pw
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    // TODO: |> uploadFile "PATH_TO_FILE"
    |> releaseDraft
    |> Async.RunSynchronously
)
#else
Target.Create "Release" Target.DoNothing
#endif

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.Create "All" Target.DoNothing
open Fake.Core.TargetOperators
"Restore"
  ==> "Build"
  ==> "CopyBinaries"
  ==> "ExampleProjects"
  ==> "RunTests"
  ==> "GenerateReferenceDocs"
  ==> "GenerateDocs"
  ==> "All"
  =?> ("ReleaseDocs",isLocalBuild)

"GenerateHelp"
  ==> "GenerateReferenceDocs"
  ==> "GenerateDocs"

"GenerateHelpDebug"
  ==> "KeepRunning"

"Clean"
  ==> "Release"

"RunNet4Tests" ==> "RunTests"
"RunNetCoreTests" ==> "RunTests"

"ReleaseDocs"
  ==> "Release"

Target.RunOrDefault "All"

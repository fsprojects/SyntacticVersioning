module SynVer.NuGetPackageTests

open System
open System.IO
open System.IO.Compression
open System.Text
open Expecto
open SynVer

let private createPackage (entries:(string * string) list) =
  let stream = new MemoryStream()
  do
    use archive = new ZipArchive(stream, ZipArchiveMode.Create, true)
    entries
    |> List.iter (fun (path, content) ->
      let entry = archive.CreateEntry path
      use writer = new StreamWriter(entry.Open(), Encoding.UTF8)
      writer.Write content)
  stream.Position <- 0L
  stream

let private withOutput test =
  let output = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N") + ".dll")
  try test output
  finally File.Delete output

[<Tests>]
let tests =
  testList "NuGet package" [
    test "extracts an assembly from the exact target framework" {
      use package =
        createPackage [
          "lib/netstandard2.0/Example.dll", "selected"
          "lib/netstandard2.1/Example.dll", "other"
        ]
      withOutput (fun output ->
        let result = NuGetPackage.extractAssembly package "netstandard2.0" "Example" output
        Expect.equal result (Ok output) "assembly should be extracted"
        Expect.equal (File.ReadAllText output) "selected" "the exact target framework should be used")
    }

    test "prefers a library assembly over a reference assembly" {
      use package =
        createPackage [
          "ref/netstandard2.0/Example.dll", "reference"
          "lib/netstandard2.0/Example.dll", "library"
        ]
      withOutput (fun output ->
        let result = NuGetPackage.extractAssembly package "netstandard2.0" "Example.dll" output
        Expect.equal result (Ok output) "assembly should be extracted"
        Expect.equal (File.ReadAllText output) "library" "the loadable library should be preferred")
    }

    test "reports a missing target framework" {
      use package = createPackage [ "lib/netstandard2.1/Example.dll", "other" ]
      withOutput (fun output ->
        let result = NuGetPackage.extractAssembly package "netstandard2.0" "Example.dll" output
        Expect.equal
          result
          (Error "Package does not contain assembly 'Example.dll' for target framework 'netstandard2.0'.")
          "a different target framework should not be selected")
    }
  ]

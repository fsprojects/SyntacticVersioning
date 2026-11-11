namespace SynVer

open System
open System.IO
open System.IO.Compression
open System.Net.Http

module NuGetPackage =
  let private assemblyEntry (archive:ZipArchive) (framework:string) (assembly:string) =
    let assembly =
      if assembly.EndsWith(".dll", StringComparison.OrdinalIgnoreCase) then assembly
      else assembly + ".dll"
      |> Path.GetFileName

    [ "lib"; "ref" ]
    |> List.map (fun folder -> sprintf "%s/%s/%s" folder framework assembly)
    |> List.tryPick (fun path ->
      archive.Entries
      |> Seq.tryFind (fun entry -> String.Equals(entry.FullName, path, StringComparison.OrdinalIgnoreCase)))

  let extractAssembly (packageStream:Stream) (framework:string) (assembly:string) (outputPath:string) =
    try
      use archive = new ZipArchive(packageStream, ZipArchiveMode.Read, true)
      match assemblyEntry archive framework assembly with
      | Some entry ->
        let outputDirectory = Path.GetDirectoryName outputPath
        if not (String.IsNullOrEmpty outputDirectory) then
          Directory.CreateDirectory outputDirectory |> ignore
        use input = entry.Open()
        use output = File.Create outputPath
        input.CopyTo output
        Ok outputPath
      | None ->
        Error (sprintf "Package does not contain assembly '%s' for target framework '%s'." assembly framework)
    with ex ->
      Error (sprintf "Failed to read NuGet package: %s" ex.Message)

  let downloadAssembly (package:string) (version:string) (framework:string) (assembly:string) =
    try
      let url =
        sprintf
          "https://www.nuget.org/api/v2/package/%s/%s"
          (Uri.EscapeDataString package)
          (Uri.EscapeDataString version)
      use client = new HttpClient()
      use response = client.GetAsync(url).GetAwaiter().GetResult()
      if not response.IsSuccessStatusCode then
        Error (sprintf "Failed to download NuGet package '%s' version '%s': %O." package version response.StatusCode)
      else
        use packageStream = response.Content.ReadAsStreamAsync().GetAwaiter().GetResult()
        let outputPath =
          Path.Combine(Path.GetTempPath(), "synver", Guid.NewGuid().ToString("N") + ".dll")
        match extractAssembly packageStream framework assembly outputPath with
        | Ok path ->
          AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
            try File.Delete path
            with _ -> ())
          Ok path
        | Error error -> Error error
    with ex ->
      Error (sprintf "Failed to download NuGet package '%s' version '%s': %s" package version ex.Message)

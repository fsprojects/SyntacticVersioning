module SyntacticVersioning.NuGet

// Example for latest nuget package:
// https://www.nuget.org/api/v2/package/Newtonsoft.Json

// Example for specific nuget package:
// https://www.nuget.org/api/v2/package/Newtonsoft.Json/9.0.1

open System
open System.IO
open System.IO.Compression
open System.Net
open System.Reflection
open System.Threading
open System.Xml.Linq

type dotNet =
| Net20 | Net35 | Net40 | Net45
| Netcore451
| NetStandard1Dot0
| PortableDashNet40 | PortableDashNet45

let [<Literal>] private nuget = @"https://www.nuget.org"
let [<Literal>] private api = nuget + @"/api/v2/package"

let private ignoreCase = StringComparison.OrdinalIgnoreCase

let package (url: string) : Async<Choice<ZipArchive,exn>> =
  async {
    let req = HttpWebRequest.Create(requestUriString = url) :?> HttpWebRequest
    
    req.Method <- "GET"
    req.Timeout <- Timeout.Infinite
    req.ReadWriteTimeout <- Timeout.Infinite
    
    use! rsp = req.AsyncGetResponse()
    use stream = rsp.GetResponseStream()
    
    return new ZipArchive(stream) } |> Async.Catch

let get (apiUrl:string option) (packageID:string) (versionNumber:string option) (dotnet:dotNet)
  :Choice<string * Assembly,exn>
  =
  let versionNumber' = defaultArg versionNumber (String.Empty)
  
  let apiUrl' = match apiUrl with | Some v -> v | None -> api
  
  let url = sprintf ("%s/%s/%s") apiUrl' packageID versionNumber'
  let dotnet' =
    (sprintf "lib/%A" dotnet).Replace("Dot",".").Replace("Dash","-")
  let maybeZip = package url |> Async.RunSynchronously
  
  maybeZip
  |> function
    | Choice1Of2 zip ->
      try
        let version =
          let nuspec =
            zip.Entries
            |> Seq.filter(fun f -> f.FullName.EndsWith(".nuspec",ignoreCase))
          
          match Seq.isEmpty nuspec with
            | true -> failwith "No .nuspec file present in NuGet package."
            | false ->
              nuspec
              |> Seq.head
              |> fun x ->
                let xml = XDocument.Load(stream = x.Open())
                xml.Descendants()
                |> Seq.where (fun x -> x.Name.LocalName = "version")
                |> Seq.head
                |> fun x -> x.Value
        
        let assembly =
          let dll =
            zip.Entries
            |> Seq.filter(
              fun f ->
                f.FullName.EndsWith(".dll",ignoreCase) &&
                f.FullName.StartsWith(dotnet',ignoreCase))
            
          match Seq.isEmpty dll with
            | true ->
              failwith
                (sprintf "No (%s) file present in NuGet package." dotnet')
            | false ->
              dll
              |> Seq.head
              |> fun x ->
                use stream = x.Open()
                use memstream = new MemoryStream()
                stream.CopyTo(memstream)
                Assembly.Load(rawAssembly = memstream.ToArray())
        
        (version, assembly) |> Choice1Of2
        
      with ex -> failwith ex.Message
      
    | Choice2Of2 ex -> failwith ex.Message
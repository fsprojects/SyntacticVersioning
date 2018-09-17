module SynVer.TestHelper
open SynVer
open System.Reflection

open System.IO
/// path combine
let (</>) = (fun a b -> Path.Combine(a, b))
let nlJoin (lines:string seq)=System.String.Join("\n",lines)
let wTrim (str:string) = str.Trim([| '\n';'\t';' ';'\r' |])
let asm = Assembly.GetExecutingAssembly()
let testPath= Path.GetDirectoryName asm.Location
let exampleProjectsPath = Path.GetFullPath( testPath </> ".."</>".."</>".."</> ".."</> "ExampleProjects")
let packagesPath = Path.GetFullPath( testPath </> ".."</>".."</>".."</>".."</> ".."</> "packages")
let exampleProjectsLibPath = exampleProjectsPath </> "lib"
let enum2txt = exampleProjectsPath </> "src" </> "Enum2.txt" |> File.ReadAllLines |>nlJoin |> wTrim
let enum3txt = exampleProjectsPath </> "src" </> "Enum3.txt" |> File.ReadAllLines |>nlJoin |> wTrim
let fsharp2txt = exampleProjectsPath </> "src" </> "Fsharp2.txt" |> File.ReadAllLines |>nlJoin |> wTrim
module AssemblyDefinition=
  open Mono.Cecil
  open System

  let readAssembly (path:string) = Decompile.readAssembly path
  let getNestedTypes (t:TypeDefinition) =t.NestedTypes
  let rec tryFindNestedType  (t:System.Type) (td:TypeDefinition) =
    if td.FullName = t.FullName.Replace('+','/') then
        Some td
    else 
        Seq.tryPick <| tryFindNestedType t <| getNestedTypes td
  /// the assumption is that this does not fail, i.e. that the tests pick it up in that case
  let getType (t:System.Type) (a:AssemblyDefinition) :TypeDefinition=
    match Seq.tryPick <| tryFindNestedType t <| a.MainModule.Types with
    | Some t' -> t'
    | None -> failwithf "Could not find '%s', %s:%s" (t.FullName.Replace('+','/')) t.Namespace t.Name
  
module TestAssemblies=
    // since all the dll-s have unique names, they can be loaded at the same time
    let csharp = exampleProjectsLibPath </> "Csharp.dll" |> Assembly.LoadFile
    let csharp2 = exampleProjectsLibPath </> "Csharp2.dll" |> Assembly.LoadFile
    let csharpWithAttribute = exampleProjectsLibPath </> "CsharpWithAttribute.dll" |> Assembly.LoadFile
    let enum = exampleProjectsLibPath </> "Enum.dll" |> Assembly.LoadFile
    let enum2 = exampleProjectsLibPath </> "Enum2.dll" |> Assembly.LoadFile
    let enum3 = exampleProjectsLibPath </> "Enum3.dll" |> Assembly.LoadFile
    let fsharp = exampleProjectsLibPath </> "Fsharp.dll" |> Assembly.LoadFile
    let fsharp2 = exampleProjectsLibPath </> "Fsharp2.dll" |> Assembly.LoadFile
    let arguAssembly= packagesPath </>"Argu"</>"lib"</> "net45" </>"Argu.dll" |> Assembly.LoadFile
    let chironAssembly= packagesPath </>"Chiron"</>"lib"</> "net40" </>"Chiron.dll" |> Assembly.LoadFile
module CecilTestAssemblies=
    open Mono.Cecil
    // since all the dll-s have unique names, they can be loaded at the same time
    let csharp = exampleProjectsLibPath </> "Csharp.dll" |> AssemblyDefinition.readAssembly
    let csharp2 = exampleProjectsLibPath </> "Csharp2.dll" |> AssemblyDefinition.readAssembly
    let csharpWithAttribute = exampleProjectsLibPath </> "CsharpWithAttribute.dll" |> AssemblyDefinition.readAssembly
    let enum = exampleProjectsLibPath </> "Enum.dll" |> AssemblyDefinition.readAssembly
    let enum2 = exampleProjectsLibPath </> "Enum2.dll" |> AssemblyDefinition.readAssembly
    let enum3 = exampleProjectsLibPath </> "Enum3.dll" |> AssemblyDefinition.readAssembly
    let fsharp = exampleProjectsLibPath </> "Fsharp.dll" |> AssemblyDefinition.readAssembly
    let fsharp2 = exampleProjectsLibPath </> "Fsharp2.dll" |> AssemblyDefinition.readAssembly
    let arguAssembly= packagesPath </>"Argu"</>"lib"</> "net45" </>"Argu.dll" |> AssemblyDefinition.readAssembly
    let chironAssembly= packagesPath </>"Chiron"</>"lib"</> "net40" </>"Chiron.dll" |> AssemblyDefinition.readAssembly

module Types=
  type UnionCaseWithName = Foo of num: int * diff:float | Bar of diff:float
  type Union = FooBar | Foo | Bar 
  type UnionWithParam = Foo of (int * float) | Bar of float
  [<System.FlagsAttribute>]
  type EnumType = FooBar=0 | Foo=1 | Bar =2
  type RecordType = { Foo:int}
  type UnionWithParamNames = Foo of num: int * diff:float | Bar of diff:float
  let CSharpStructType = TestAssemblies.csharp.ExportedTypes |> Seq.find (fun t-> t.Name= "Struct")

  type FsharpStruct =
   struct
      val x: float
   end
  
  let ModuleT= TestAssemblies.fsharp.ExportedTypes |> Seq.find (fun t-> t.Name="Module")

module CecilTypes=
  open Mono.Cecil
  let assembly =
    AssemblyDefinition.readAssembly (typeof< Types.UnionCaseWithName>).Assembly.Location
  let unionCaseWithName = AssemblyDefinition.getType typeof< Types.UnionCaseWithName> assembly 
  let union = AssemblyDefinition.getType typeof< Types.Union> assembly
  let unionWithParam = AssemblyDefinition.getType typeof< Types.UnionWithParam> assembly
  let enumType = AssemblyDefinition.getType typeof< Types.EnumType> assembly
  let recordType = AssemblyDefinition.getType typeof< Types.RecordType> assembly
  let unionWithParamNames = AssemblyDefinition.getType typeof< Types.UnionWithParamNames> assembly
  let fsharpStruct = AssemblyDefinition.getType typeof< Types.FsharpStruct> assembly

module Assemblies=
  [<CompiledName("Bump")>]
  let bump verNr released modified =
    SurfaceArea.bump verNr 
      (SurfaceArea.ofAssembly released) 
      (SurfaceArea.ofAssembly modified)   

  [<CompiledName("Diff")>]
  let diff released modified =
    SurfaceArea.diff  
      (SurfaceArea.ofAssembly released) 
      (SurfaceArea.ofAssembly modified)

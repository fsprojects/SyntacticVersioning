﻿module SyntacticVersioning.TestHelper
open SyntacticVersioning
open System.Reflection

open System.IO
/// path combine
let (</>) = (fun a b -> Path.Combine(a, b))
let nlJoin (lines:string seq)=System.String.Join("\n",lines)
let wTrim (str:string) = str.Trim([| '\n';'\t';' ';'\r' |])
let asm = Assembly.GetExecutingAssembly()
let testPath= Path.GetDirectoryName asm.Location
let exampleProjectsPath = Path.GetFullPath( testPath </> ".."</>".."</>".."</>"ExampleProjects" )
let exampleProjectsLibPath = exampleProjectsPath </> "lib"
// since all the dll-s have unique names, they can be loaded at the same time
let csharp = exampleProjectsLibPath </> "Csharp.dll" |> Assembly.LoadFile
let csharp2 = exampleProjectsLibPath </> "Csharp2.dll" |> Assembly.LoadFile
let enum = exampleProjectsLibPath </> "Enum.dll" |> Assembly.LoadFile
let enum2 = exampleProjectsLibPath </> "Enum2.dll" |> Assembly.LoadFile
let enum2_txt = exampleProjectsPath </> "src" </> "Enum2.txt" |> File.ReadAllLines |>nlJoin |> wTrim
let enum3 = exampleProjectsLibPath </> "Enum3.dll" |> Assembly.LoadFile
let enum3_txt = exampleProjectsPath </> "src" </> "Enum3.txt" |> File.ReadAllLines |>nlJoin |> wTrim
let fsharp = exampleProjectsLibPath </> "Fsharp.dll" |> Assembly.LoadFile
let fsharp2 = exampleProjectsLibPath </> "Fsharp2.dll" |> Assembly.LoadFile
let fsharp2_txt = exampleProjectsPath </> "src" </> "Fsharp2.txt" |> File.ReadAllLines |>nlJoin |> wTrim


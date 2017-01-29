module SyntacticVersioning.Tests

open SyntacticVersioning
open NUnit.Framework
open System.Reflection
module TestHelper=
  open System.IO
  /// path combine
  let (</>) = (fun a b -> Path.Combine(a, b))
  let asm = Assembly.GetExecutingAssembly()
  let testPath= Path.GetDirectoryName asm.Location
  let exampleProjectsLibPath = testPath </> "../../../ExampleProjects/lib"
  // since all the dll-s have unique names, they can be loaded at the same time
  let csharp = exampleProjectsLibPath </> "Csharp.dll"
  let csharp2 = exampleProjectsLibPath </> "Csharp2.dll"
  let enum = exampleProjectsLibPath </> "Enum.dll"
  let enum2 = exampleProjectsLibPath </> "Enum2.dll"
  let fsharp = exampleProjectsLibPath </> "Fsharp.dll"
  let fsharp2 = exampleProjectsLibPath </> "Fsharp2.dll"

open TestHelper
[<Test>]
let ``csharp vs csharp2`` () =
  let assembly = Assembly.LoadFile csharp
  let updated = Assembly.LoadFile csharp2
  Assert.Fail "Not implemented yet"


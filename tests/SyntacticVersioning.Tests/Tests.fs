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
  let csharp = exampleProjectsLibPath </> "Csharp.dll" |> Assembly.LoadFile
  let csharp2 = exampleProjectsLibPath </> "Csharp2.dll" |> Assembly.LoadFile
  let enum = exampleProjectsLibPath </> "Enum.dll" |> Assembly.LoadFile
  let enum2 = exampleProjectsLibPath </> "Enum2.dll" |> Assembly.LoadFile
  let fsharp = exampleProjectsLibPath </> "Fsharp.dll" |> Assembly.LoadFile
  let fsharp2 = exampleProjectsLibPath </> "Fsharp2.dll" |> Assembly.LoadFile



open TestHelper
[<Test>]
let ``csharp vs csharp2`` () =
  let diff = SurfaceArea.diff csharp csharp2
            |> (fun a-> System.String.Join("\n",a))
  Assert.AreEqual ("<>", diff)

[<Test>]
let ``enum vs enum2`` () =
  let diff = SurfaceArea.diff enum enum2
            |> (fun a-> System.String.Join("\n",a))
  Assert.AreEqual ("<>", diff)

[<Test>]
let ``fsharp vs fsharp2`` () =
  let diff = SurfaceArea.diff fsharp fsharp2
            |> (fun a-> System.String.Join("\n",a))
  Assert.AreEqual ("<>", diff)

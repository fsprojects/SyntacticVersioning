module SyntacticVersioning.Tests

open SyntacticVersioning
open NUnit.Framework
open System.Reflection

open TestHelper
[<Test>]
let ``csharp vs csharp2`` () =
  let diff = SurfaceArea.diffAsStrings csharp csharp2
            |> nlJoin
            |> wTrim
  Assert.AreEqual ("", diff)

[<Test>]
let ``enum vs enum2`` () =
  let diff = SurfaceArea.diffAsStrings enum enum2
            |> nlJoin
            |> wTrim
  Assert.AreEqual (enum2_txt, diff)

[<Test>]
let ``enum vs enum3`` () =
  let diff = SurfaceArea.diffAsStrings enum enum3
            |> nlJoin
            |> wTrim
  //System.Console.WriteLine( diff)
  Assert.AreEqual (enum3_txt, diff)

[<Test>]
let ``fsharp vs fsharp2`` () =
  let diff = SurfaceArea.diffAsStrings fsharp fsharp2
            |> nlJoin
            |> wTrim
  Assert.AreEqual (fsharp2_txt, diff)

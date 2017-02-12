module SyntacticVersioning.Tests

open SyntacticVersioning
open NUnit.Framework
open System.Reflection

open TestHelper
[<Test>]
let ``csharp vs csharp2`` () =
  let diff = Compare.diffAsStrings csharp csharp2
            |> nlJoin
            |> wTrim
  Assert.AreEqual ("", diff)

[<Test>]
let ``enum vs enum2`` () =
  let diff = Compare.diffAsStrings enum enum2
            |> nlJoin
            |> wTrim
  Assert.AreEqual (enum2txt, diff)

[<Test>]
let ``enum vs enum3`` () =
  let diff = Compare.diffAsStrings enum enum3
            |> nlJoin
            |> wTrim
  //System.Console.WriteLine( diff)
  Assert.AreEqual (enum3txt, diff)

[<Test>]
let ``fsharp vs fsharp2`` () =
  let diff = Compare.diffAsStrings fsharp fsharp2
            |> nlJoin
            |> wTrim
  Assert.AreEqual (fsharp2txt, diff)

(* Should attributes be considered part of the signature?
[<Test>]
let ``csharp with attributes`` () =
  let res = SurfaceArea.raw csharpWithAttribute
            |> Set.toArray
            |> Array.iter(fun (prefix, body) -> printfn "%s - %s" prefix body)

  Assert.AreEqual ("", res)
*)
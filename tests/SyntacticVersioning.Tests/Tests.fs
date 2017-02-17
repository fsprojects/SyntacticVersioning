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
  let bump = Compare.bump "1.0.0" csharp csharp2
  Assert.AreEqual ( ("1.0.1", Patch), bump)

[<Test>]
let ``enum vs enum2`` () =
  let diff = Compare.diffAsStrings enum enum2
            |> nlJoin
            |> wTrim
  Assert.AreEqual (enum2txt, diff)
  let bump = Compare.bump "1.0.0" enum enum2
  Assert.AreEqual ( ("2.0.0", Major), bump)

[<Test>]
let ``enum vs enum3`` () =
  let diff = Compare.diffAsStrings enum enum3
            |> nlJoin
            |> wTrim
  Assert.AreEqual (enum3txt, diff)
  let bump = Compare.bump "1.0.0" enum enum3
  Assert.AreEqual ( ("2.0.0", Major), bump)

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
module SynVer.Tests
open SynVer
open NUnit.Framework

open TestHelper
[<Test>]
let ``csharp vs csharp2`` () =
  let diff = Assemblies.diff csharp csharp2
            |> nlJoin
            |> wTrim
  Assert.AreEqual ("", diff)
  let bump = Assemblies.bump "1.0.0" csharp csharp2
  Assert.AreEqual ( ("1.0.1", Patch), bump)

[<Test>]
let ``enum vs enum2`` () =
  let diff = Assemblies.diff enum enum2
            |> nlJoin
            |> wTrim
  Assert.AreEqual (enum2txt, diff)
  let bump = Assemblies.bump "1.0.0" enum enum2
  Assert.AreEqual ( ("2.0.0", Major), bump)

[<Test>]
let ``enum vs enum3`` () =
  let diff = Assemblies.diff enum enum3
            |> nlJoin
            |> wTrim
  Assert.AreEqual (enum3txt, diff)
  let bump = Assemblies.bump "1.0.0" enum enum3
  Assert.AreEqual ( ("1.1.0", Minor), bump)

[<Test>]
let ``fsharp vs fsharp2`` () =
  let diff = Assemblies.diff fsharp fsharp2
            |> nlJoin
            |> wTrim
  Assert.AreEqual (fsharp2txt, diff)


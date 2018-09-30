module SynVer.DecompileTests
open SynVer
open Expecto

open TestHelper
open CecilTestAssemblies
[<Tests>]
let tests =
  testList "Decompile tests" [
    test "csharp vs csharp2" {
      let diff = AssemblyDefinition.diff csharp csharp2
                |> nlJoin
                |> wTrim
      Expect.equal "" diff "Empty diff"
      let bump = AssemblyDefinition.bump "1.0.0" csharp csharp2
      Expect.equal  ("1.0.1", Patch) bump "Patch change"
    }
    test "csharp2 vs csharp3" {
      let diff = AssemblyDefinition.diff csharp2 csharp3
                |> nlJoin
                |> wTrim
      Expect.equal csharp3txt diff "csharp 3 diff"
      let bump = AssemblyDefinition.bump "1.0.0" csharp2 csharp3
      Expect.equal  ("1.1.0", Minor) bump "Minor change"
    }
    test "enum vs enum2" {
      let diff = AssemblyDefinition.diff enum enum2
                |> nlJoin
                |> wTrim
      Expect.equal enum2txt diff "enum 2 diff"
      let bump = AssemblyDefinition.bump "1.0.0" enum enum2
      Expect.equal bump ("2.0.0", Major) "Major change"
    }

    test "enum vs enum3" {
      let diff = AssemblyDefinition.diff enum enum3
                |> nlJoin
                |> wTrim
      Expect.equal enum3txt diff "enum 3 diff"
      let bump = AssemblyDefinition.bump "1.0.0" enum enum3
      Expect.equal bump ("1.1.0", Minor) "Minor change"
    }
    ptest "fsharp vs fsharp2" {
      let diff = AssemblyDefinition.diff fsharp fsharp2
                |> nlJoin
                |> wTrim
      Expect.equal diff fsharp2txt "fsharp 2 diff"
    }

  ]


module SyntacticVersioning.SerializationTests

open SyntacticVersioning
open NUnit.Framework
open TestHelper
open Chiron
[<Test>]
let ``Should be able to serialize `` () =
  let surface = SurfaceArea.exportedTypes csharp
  let json = surface
              |> Json.serialize
              |> Json.formatWith JsonFormattingOptions.Pretty
  Assert.AreEqual( "<>", json)
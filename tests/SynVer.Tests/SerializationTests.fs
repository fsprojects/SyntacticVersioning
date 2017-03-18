module SynVer.SerializationTests
open SynVer
open NUnit.Framework
open TestHelper
open Chiron

let assertCanSerializeAndDeserialize assembly
  =
    let surface = SurfaceArea.ofAssembly assembly
    let deserialized 
        = surface
          |> Json.serialize
          |> Json.deserialize
    let same = deserialized = surface
    Assert.IsTrue(same)
[<Test>]
let ``csharp`` () =
  assertCanSerializeAndDeserialize csharp
[<Test>]
let ``csharp2`` () =
  assertCanSerializeAndDeserialize csharp2
[<Test>]
let ``enum`` () =
  assertCanSerializeAndDeserialize enum
[<Test>]
let ``enum2`` () =
  assertCanSerializeAndDeserialize enum2
[<Test>]
let ``enum3`` () =
  assertCanSerializeAndDeserialize enum3
[<Test>]
let ``fsharp`` () =
  assertCanSerializeAndDeserialize fsharp
[<Test>]
let ``fsharp2`` () =
  assertCanSerializeAndDeserialize fsharp2
[<Test>]
let ``argu`` () =
  assertCanSerializeAndDeserialize arguAssembly
[<Test>]
let ``chiron`` () =
  assertCanSerializeAndDeserialize chironAssembly


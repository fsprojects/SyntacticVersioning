module SynVer.SerializationTests
open SynVer
open Expecto
open TestHelper

let assertCanSerializeAndDeserialize assembly
  =
    let surface = SurfaceArea.ofAssembly assembly
    let deserialized 
        = surface
          |> Lson.serialize
          |> Lson.deserialize
    let same = deserialized = surface
    Expect.isTrue same (assembly.ToString())
[<Tests>]
let tests =
  testList "Serialization tests" [    
    test "csharp" {
      assertCanSerializeAndDeserialize csharp
    }
    test "csharp2" {
      assertCanSerializeAndDeserialize csharp2
    }
    test "enum" {
      assertCanSerializeAndDeserialize enum
    }
    test "enum2" {
      assertCanSerializeAndDeserialize enum2
    }
    test "enum3" {
      assertCanSerializeAndDeserialize enum3
    }
    test "fsharp" {
      assertCanSerializeAndDeserialize fsharp
    }
    test "fsharp2" {
      assertCanSerializeAndDeserialize fsharp2
    }
    test "argu" {
      assertCanSerializeAndDeserialize arguAssembly
    }
    test "chiron"{
      assertCanSerializeAndDeserialize chironAssembly
    }
  ]


module SynVer.SerializationTests
open SynVer
open Expecto
open TestHelper
open TestAssemblies

let assertCanSerializeAndDeserialize assembly
  =
    let surface = SurfaceArea.ofAssembly assembly
    let deserialized 
        = surface
          |> Lson.serialize
          |> Lson.deserialize
    Expect.equal surface deserialized (assembly.ToString())
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
    ptest "fsharp" {
      assertCanSerializeAndDeserialize fsharp
    }
    ptest "fsharp2" {
      assertCanSerializeAndDeserialize fsharp2
    }
    ptest "argu" {
      assertCanSerializeAndDeserialize arguAssembly
    }
    ptest "chiron"{
      assertCanSerializeAndDeserialize chironAssembly
    }
  ]


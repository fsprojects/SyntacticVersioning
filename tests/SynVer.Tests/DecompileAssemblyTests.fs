module SynVer.DecompileAssemblyTests
open SynVer
open Expecto

open TestHelper
module CT= CecilTestAssemblies
module T= TestAssemblies

let assertCanSerializeAndDeserialize assembly
  =
    let surface = SurfaceArea.ofAssemblyDefinition assembly
    let deserialized 
        = surface
          |> Lson.serialize
          |> Lson.deserialize
    Expect.equal surface deserialized (assembly.ToString())
let assertAssemblyDefinitionMatchesAssembly assemblyDef assembly 
  =
    let surfaceDef = SurfaceArea.ofAssemblyDefinition assemblyDef
    let surface = SurfaceArea.ofAssembly assembly
    let comparison = Compare.packages surface surfaceDef
    Expect.equal surfaceDef surface (sprintf "\n\n\n%A\n\n\n" comparison)
[<Tests>]
let tests =
  testList "Read assembly definition tests" [    
    test "csharp can be serialized and deserialized" {
      assertCanSerializeAndDeserialize CT.csharp
    }
    test "csharp2 can be serialized and deserialized" {
      assertCanSerializeAndDeserialize CT.csharp2
    }
    test "enum can be serialized and deserialized" {
      assertCanSerializeAndDeserialize CT.enum
    }
    test "enum2 can be serialized and deserialized" {
      assertCanSerializeAndDeserialize CT.enum2
    }
    test "enum3 can be serialized and deserialized" {
      assertCanSerializeAndDeserialize CT.enum3
    }
    
    
    ptest "csharp can be read by both cecil and reflection" {
      assertAssemblyDefinitionMatchesAssembly CT.csharp T.csharp
    }
    ptest "csharp2 can be read by both cecil and reflection" {
      assertAssemblyDefinitionMatchesAssembly CT.csharp2 T.csharp2
    }
    ptest "enum can be read by both cecil and reflection" {
      assertAssemblyDefinitionMatchesAssembly CT.enum T.enum
    }
    ptest "enum2 can be read by both cecil and reflection" {
      assertAssemblyDefinitionMatchesAssembly CT.enum2 T.enum2
    }
    ptest "enum3 can be read by both cecil and reflection" {
      assertAssemblyDefinitionMatchesAssembly CT.enum3 T.enum3
    }

    ptest "fsharp can be read by both cecil and reflection" {
      assertAssemblyDefinitionMatchesAssembly CT.fsharp T.fsharp
    }
    ptest "fsharp2 can be read by both cecil and reflection" {
      assertAssemblyDefinitionMatchesAssembly CT.fsharp2 T.fsharp2
    }
    ptest "fsharp can be serialized and deserialized" {
      assertCanSerializeAndDeserialize CT.fsharp
    }
    ptest "fsharp2 can be serialized and deserialized" {
      assertCanSerializeAndDeserialize CT.fsharp2
    }
    ptest "argu can be serialized and deserialized" {
      assertCanSerializeAndDeserialize CT.arguAssembly
    }
    ptest "chiron can be serialized and deserialized"{
      assertCanSerializeAndDeserialize CT.chironAssembly
    }
  ]


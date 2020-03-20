module SynVer.DecompileNetTypeTests
open SynVer
open Expecto
open TestHelper
module CT = CecilTypes

[<Tests>]
let tests =
  testList "Net Type of Type definition" [

    test "Tag net type union" {
      let t = CT.union
      Expect.equal (Decompile.tagNetType t) NetType.SumType "sumtype union"
    }
    test "Tag net type union prime" {
      let t = CT.unionWithParam
      Expect.equal (Decompile.tagNetType t) NetType.SumType "sumtype union prime"
    }
    test "Tag net type union with names" {
      let t = CT.unionWithParamNames
      Expect.equal (Decompile.tagNetType t) NetType.SumType "sumtype with tag"
    }
    test "Tag net type enum" {
      let t = CT.enumType
      Expect.equal (Decompile.tagNetType t) NetType.Enum "enum type"
    }
    test "Tag net type record" {
      let t = CT.recordType
      Expect.equal (Decompile.tagNetType t) NetType.RecordType "record type"
    }
    test "Tag net type fsharp struct"{
      let t = CT.fsharpStruct
      Expect.equal (Decompile.tagNetType t) NetType.Struct "struct type"
    }
    test "net type fullname"{
      Expect.equal (Decompile.typeFullName CT.enumType) "SynVer.TestHelper.Types.EnumType" "enum type"
      Expect.equal (Decompile.typeFullName CT.fsharpStruct) "SynVer.TestHelper.Types.FsharpStruct" "fsharp struct type"
      Expect.equal (Decompile.typeFullName CT.recordType) "SynVer.TestHelper.Types.RecordType" "fsharp record type"
      Expect.equal (Decompile.typeFullName CT.openGeneric) "SynVer.TestHelper.Types.GenericType<a>" "open generic"
    }
  ]
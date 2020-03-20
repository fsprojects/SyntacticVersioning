module SynVer.NetTypeTests
open SynVer
open Expecto
open SynVer.TestHelper.Types

open TestHelper.Types
[<Tests>]
let tests =
  testList "Net Type" [

    test "Tag net type NetType" {
      let t = typeof<Version>
      Expect.equal (Reflect.tagNetType t) NetType.SumType "sum type t"
      let n = Version.Major
      let nt = n.GetType()
      Expect.equal (Reflect.tagNetType nt) NetType.SumType "sum type nt"
    }
    test "Tag net type union" {
      let t = typeof<Union>
      Expect.equal (Reflect.tagNetType t) NetType.SumType "sumtype union"
    }
    test "Tag net type union prime" {
      let t = typeof<UnionWithParam>
      Expect.equal (Reflect.tagNetType t) NetType.SumType "sumtype union prime"
      let foo=UnionWithParam.Foo (1,0.1)
      let fooT =foo.GetType()
      Expect.isTrue (Reflect.isSumType fooT) "fooT"
    }
    test "Tag net type union with names" {
      let t = typeof<UnionWithParamNames>
      Expect.equal (Reflect.tagNetType t) NetType.SumType "sumtype with tag"
      let foo = UnionWithParamNames.Foo(num=1,diff=0.1)
      let fooT = foo.GetType()
      Expect.isTrue (Reflect.isSumType fooT) "is sumtype"
    }
    test "Tag net type enum" {
      let t = typeof<EnumType>
      Expect.equal (Reflect.tagNetType t) NetType.Enum "enum type"
    }
    test "Tag net type record" {
      let t = typeof<RecordType>
      Expect.equal (Reflect.tagNetType t) NetType.RecordType "record type"
    }
    test "Tag net type csharp struct"{
      Expect.equal (Reflect.tagNetType CSharpStructType) NetType.Struct "struct type"
    }
    test "Tag net type fsharp struct"{
      let t = typeof<FsharpStruct>
      Expect.equal (Reflect.tagNetType t) NetType.Struct "struct type"
    }
    test "Tag net type module"{
      Expect.equal (Reflect.tagNetType ModuleT) NetType.Module "module"
    }
    test "net type fullname"{
      let enumType = typeof<EnumType>
      let fsharpStruct = typeof<FsharpStruct>
      let recordType = typeof<RecordType>
      let openGeneric = typedefof<GenericType<_>>
      let closedGeneric = typeof<GenericType<string>>
      let closedGeneric2 = typeof<GenericType<RecordType>>
      Expect.equal (Reflect.typeFullName enumType) "SynVer.TestHelper.Types.EnumType" "enum type"
      Expect.equal (Reflect.typeFullName fsharpStruct) "SynVer.TestHelper.Types.FsharpStruct" "fsharp struct type"
      Expect.equal (Reflect.typeFullName recordType) "SynVer.TestHelper.Types.RecordType" "fsharp record type"
      Expect.equal (Reflect.typeFullName openGeneric) "SynVer.TestHelper.Types.GenericType<'A>" "Open generic"
      Expect.equal (Reflect.typeFullName closedGeneric) "SynVer.TestHelper.Types.GenericType<System.String>" "Closed generic"
      Expect.equal (Reflect.typeFullName closedGeneric2) "SynVer.TestHelper.Types.GenericType<SynVer.TestHelper.Types.RecordType>" "Closed generic 2"
    }
  ]
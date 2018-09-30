module SynVer.NetTypeTests
open SynVer
open Expecto
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
  ]
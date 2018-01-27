module SynVer.NetTypeTests
open SynVer
open Expecto
open TestHelper.Types
[<Tests>]
let tests =
  testList "Net Type" [

    test "Tag net type NetType" {
      let t = typeof<Version>
      Expect.equal NetType.SumType (Reflect.tagNetType t) "sum type t"
      let n = Version.Major
      let nt = n.GetType()
      Expect.equal NetType.SumType (Reflect.tagNetType nt) "sum type nt"
    }
    test "Tag net type union" {
      let t = typeof<Union>
      Expect.equal NetType.SumType (Reflect.tagNetType t) "sumtype union"
    }
    test "Tag net type union prime" {
      let t = typeof<UnionWithParam>
      Expect.equal NetType.SumType (Reflect.tagNetType t) "sumtype union prime"
      let foo=UnionWithParam.Foo (1,0.1)
      let fooT =foo.GetType()
      Expect.isTrue (Reflect.isSumType fooT) "fooT"
    }
    test "Tag net type union with names" {
      let t = typeof<UnionWithParamNames>
      Expect.equal NetType.SumType (Reflect.tagNetType t) "sumtype with tag"
      let foo = UnionWithParamNames.Foo(num=1,diff=0.1)
      let fooT = foo.GetType()
      Expect.isTrue (Reflect.isSumType fooT) "is sumtype"
    }
    test "Tag net type enum" {
      let t = typeof<EnumType>
      Expect.equal NetType.Enum (Reflect.tagNetType t) "enum type"
    }
    test "Tag net type record" {
      let t = typeof<RecordType>
      Expect.equal NetType.RecordType (Reflect.tagNetType t) "record type"
    }
    test "Tag net type csharp struct"{
      Expect.equal NetType.Struct (Reflect.tagNetType CSharpStructType) "struct type"
    }
    test "Tag net type fsharp struct"{
      let t = typeof<FsharpStruct>
      Expect.equal NetType.Struct (Reflect.tagNetType t) "struct type"
    }
    test "Tag net type module"{
      Expect.equal NetType.Module (Reflect.tagNetType ModuleT) "module"
    }
  ]
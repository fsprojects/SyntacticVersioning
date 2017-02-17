module SyntacticVersioning.NetTypeTests

open SyntacticVersioning
open NUnit.Framework
open TestHelper.Types
[<Test>]
let ``Tag net type NetType`` () =
  let t = typeof<NetType>
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType t)
  let n = NetType.UnionTags
  let nt = n.GetType()
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType nt)

[<Test>]
let ``Tag net type union`` () =
  let t = typeof<Union>
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType t)

[<Test>]
let ``Tag net type union prime`` () =
  let t = typeof<UnionWithParam>
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType t)
  let foo=UnionWithParam.Foo (1,0.1)
  let fooT =foo.GetType()
  Assert.IsTrue(Reflect.isSumType fooT)

[<Test>]
let ``Tag net type union with names`` () =
  let t = typeof<UnionWithParamNames>
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType t)
  let foo = UnionWithParamNames.Foo(num=1,diff=0.1)
  let fooT = foo.GetType()
  Assert.IsTrue(Reflect.isSumType fooT)

[<Test>]
let ``Tag net type enum`` () =
  let t = typeof<EnumType>
  Assert.AreEqual(NetType.Enum, Reflect.tagNetType t)

[<Test>]
let ``Tag net type record`` () =
  let t = typeof<RecordType>
  Assert.AreEqual(NetType.RecordType, Reflect.tagNetType t)

[<Test>]
let ``Tag net type csharp struct`` ()=
  Assert.AreEqual(NetType.Struct, Reflect.tagNetType CSharpStructType)

[<Test>]
let ``Tag net type fsharp struct`` ()=
  let t = typeof<FsharpStruct>
  Assert.AreEqual(NetType.Struct, Reflect.tagNetType t)

[<Test>]
let ``Tag net type module`` ()=
  Assert.AreEqual(NetType.Module, Reflect.tagNetType ModuleT)

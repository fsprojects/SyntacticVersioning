module SyntacticVersioning.SurfaceAreaTests

open SyntacticVersioning
open NUnit.Framework
open System.Reflection
[<Test>]
let ``Tag net type NetType`` () =
  let t = typeof<NetType>
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType t)
  let n = NetType.UnionTags
  let nt = n.GetType()
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType nt)

type Union = FooBar | Foo | Bar 
[<Test>]
let ``Tag net type union`` () =
  let t = typeof<Union>
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType t)
(*
  let foo = Union.Foo
  let fooT = foo.GetType()
  Assert.IsTrue(Reflect.isSumType fooT)
*)

[<Test>]
let ``Union surface area`` () =
  let t = typeof<Union>
  let area = SurfaceArea.surfaceOfType t
  let expected =[ { Name= "FooBar";Fields= []}
                  { Name= "Foo";Fields= []}
                  { Name= "Bar";Fields= []}
                ]
                  
  Assert.AreEqual(expected, area.UnionCases.Value.Cases)
  Assert.IsTrue(area.Enum.IsNone)

type UnionPrime = Foo of (int * float) | Bar of float

[<Test>]
let ``Tag net type union prime`` () =
  let t = typeof<UnionPrime>
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType t)
  let foo=UnionPrime.Foo (1,0.1)
  let fooT =foo.GetType()
  Assert.IsTrue(Reflect.isSumType fooT)

[<Test>]
let ``Union with params surface area`` () =
  let t = typeof<UnionPrime>
  let area = SurfaceArea.surfaceOfType t
  let expected = [
                  { Name= "Foo"
                    Fields= [{Type= {FullName="System.Tuple<System.Int32,System.Double>"}; Name=null }
                            ]}
                  { Name= "Bar"
                    Fields= [{Type={FullName="System.Double"}; Name=null}]}
                  ]

  Assert.AreEqual(expected, area.UnionCases.Value.Cases)
  Assert.IsTrue(area.Enum.IsNone)

type UnionWithNames = Foo of num: int * diff:float | Bar of diff:float

[<Test>]
let ``Tag net type union with names`` () =
  let t = typeof<UnionWithNames>
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType t)
  let foo = UnionWithNames.Foo(num=1,diff=0.1)
  let fooT = foo.GetType()
  Assert.IsTrue(Reflect.isSumType fooT)

[<Test>]
let ``Union with names surface area`` () =
  let t = typeof<UnionWithNames>
  let area = SurfaceArea.surfaceOfType t
  let expected = [
                  { Name= "Foo"
                    Fields= [{Type={FullName="System.Int32"}; Name="num"}
                             {Type={FullName="System.Double"};Name="diff"}
                            ]}
                  { Name= "Bar"
                    Fields= [{Type={FullName="System.Double"};Name="diff"}]}
                  ]
  //printf "%A" area.UnionCases.Value.Cases
  Assert.AreEqual(expected, area.UnionCases.Value.Cases)
  Assert.IsTrue(area.Enum.IsNone)

[<System.FlagsAttribute>]
type EnumT = FooBar=0 | Foo=1 | Bar =2

[<Test>]
let ``Tag net type enum`` () =
  let t = typeof<EnumT>
  Assert.AreEqual(NetType.Enum, Reflect.tagNetType t)

[<Test>]
let ``Enum surface area`` () =
  let t = typeof<EnumT>
  let area = SurfaceArea.surfaceOfType t
  let expected = [("FooBar","0"); ("Foo","1"); ("Bar","2")]

  Assert.AreEqual(expected, area.Enum.Value.Values)
  Assert.IsTrue(area.UnionCases.IsNone)


type RecordT = { Foo:int}

[<Test>]
let ``Tag net type record`` () =
  let t = typeof<RecordT>
  Assert.AreEqual(NetType.RecordType, Reflect.tagNetType t)

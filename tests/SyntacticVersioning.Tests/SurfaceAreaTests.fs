module SyntacticVersioning.SurfaceAreaTests

open SyntacticVersioning
open NUnit.Framework
open System.Reflection

type Union = FooBar | Foo | Bar 
[<Test>]
let ``Tag net type union`` () =
  let t = typeof<Union>
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType t)

[<Test>]
let ``Union surface area`` () =
  let t = typeof<Union>
  let area = SurfaceArea.surfaceOfType t
  let expected =[ { Name= "FooBar";Fields= []}
                  { Name= "Foo";Fields= []}
                  { Name= "Bar";Fields= []}
                ]
                  
  Assert.AreEqual(expected, area.UnionCases.Value.Cases)

type UnionPrime = Foo of (int * float) | Bar of float with
  member x.Baz = 42
  static member Qux () = 42

[<Test>]
let ``Tag net type union prime`` () =
  let t = typeof<Union>
  Assert.AreEqual(NetType.SumType, Reflect.tagNetType t)

[<Test>]
let ``Union with params surface area`` () =
  let t = typeof<UnionPrime>
  let area = SurfaceArea.surfaceOfType t
  let ts = sprintf "%A" area.UnionCases
  let expected = [
                  { Name= "Foo"
                    Fields= [{FullName="System.Tuple<System.Int32,System.Double>"}
                            ]}
                  { Name= "Bar"
                    Fields= [{FullName="System.Double"}]}
                  ]

  Assert.AreEqual(expected, area.UnionCases.Value.Cases)
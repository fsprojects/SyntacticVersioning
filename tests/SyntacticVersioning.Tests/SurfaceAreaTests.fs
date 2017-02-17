module SyntacticVersioning.SurfaceAreaTests

open SyntacticVersioning
open NUnit.Framework
open TestHelper.Types

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

[<Test>]
let ``Union with params surface area`` () =
  let t = typeof<UnionWithParam>
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

[<Test>]
let ``Union with names surface area`` () =
  let t = typeof<UnionCaseWithName>
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


[<Test>]
let ``Enum surface area`` () =
  let t = typeof<EnumType>
  let area = SurfaceArea.surfaceOfType t
  let expected = [("FooBar","0"); ("Foo","1"); ("Bar","2")]

  Assert.AreEqual(expected, area.Enum.Value.Values)
  Assert.IsTrue(area.UnionCases.IsNone)

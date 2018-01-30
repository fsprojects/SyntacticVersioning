module SynVer.SurfaceAreaTests
open SynVer
open Expecto
open TestHelper.Types

[<Tests>]
let tests =
  testList "Surface area" [
    test "Union surface area" {
      let t = typeof<Union>
      let area = SurfaceArea.ofType t
      let typ= area.Type
      let expected =[ UnionCase (typ,"FooBar", [])
                      UnionCase (typ,"Foo",[])
                      UnionCase (typ,"Bar",[])
                    ]
      Expect.equal expected area.UnionCases.Value.Cases "union cases"
      Expect.isTrue (area.Enum.IsNone) "is none"
    }
    test "Union with params surface area" {
      let t = typeof<UnionWithParam>
      let area = SurfaceArea.ofType t
      let typ= area.Type
      let expected = [
                      UnionCase (typ, "Foo",
                        [{Type= {FullName="System.Tuple<System.Int32,System.Double>"}; Name=null }])
                      UnionCase (typ, "Bar",
                        [{Type={FullName="System.Double"}; Name=null}])
                      ]

      Expect.equal expected area.UnionCases.Value.Cases "union cases"
      Expect.isTrue (area.Enum.IsNone) "is none"
    }
    test "Union with names surface area" {
      let t = typeof<UnionCaseWithName>
      let area = SurfaceArea.ofType t
      let typ= area.Type
      let expected = [
                      UnionCase (typ, "Foo",
                          [{Type={FullName="System.Int32"}; Name="num"}
                           {Type={FullName="System.Double"};Name="diff"}])
                      UnionCase (typ, "Bar",
                          [{Type={FullName="System.Double"};Name="diff"}])
                      ]
      //printf "%A" area.UnionCases.Value.Cases
      Expect.equal expected area.UnionCases.Value.Cases "union cases"
      Expect.isTrue (area.Enum.IsNone) "is none"
    }

    test "Enum surface area" {
      let t = typeof<EnumType>
      let area = SurfaceArea.ofType t
      let expected = [("FooBar","0"); ("Foo","1"); ("Bar","2")]

      Expect.equal expected area.Enum.Value "enum value"
      Expect.isTrue (area.UnionCases.IsNone) "is none"
    }
  ]
module SynVer.DecompileSurfaceAreaTests
open SynVer
open Expecto

open TestHelper
module CT = CecilTypes

[<Tests>]
let tests =
  testList "Surface area of Type Definition" [
    test "Union surface area" {
      let t = CT.union
      let area = SurfaceArea.ofTypeDefinition t
      let typ= area.Type
      let expected =[ UnionCase (typ,"FooBar", [])
                      UnionCase (typ,"Foo",[])
                      UnionCase (typ,"Bar",[])
                    ]
      Expect.equal expected area.UnionCases.Value.Cases "union cases"
      Expect.isTrue (area.Enum.IsNone) "is none"
    }
    test "Union with params surface area" {
      let t = CT.unionWithParam
      let area = SurfaceArea.ofTypeDefinition t
      let typ= area.Type
      let expected = [
                      UnionCase (typ, "Foo",
                        [{Type= {FullName="System.Tuple<System.Int32,System.Double>"}; Name=null }])
                      UnionCase (typ, "Bar",
                        [{Type={FullName="System.Double"}; Name=null}])
                      ]

      Expect.equal area.UnionCases.Value.Cases expected "union cases"
      Expect.isTrue (area.Enum.IsNone) "is none"
    }
    test "Union with names surface area" {
      let t = CT.unionCaseWithName
      let area = SurfaceArea.ofTypeDefinition t
      let typ= area.Type
      let expected = [
                      UnionCase (typ, "Foo",
                          [{Type={FullName="System.Int32"}; Name="num"}
                           {Type={FullName="System.Double"};Name="diff"}])
                      UnionCase (typ, "Bar",
                          [{Type={FullName="System.Double"};Name="diff"}])
                      ]
      //printf "%A" area.UnionCases.Value.Cases
      Expect.equal area.UnionCases.Value.Cases expected "union cases"
      Expect.isTrue (area.Enum.IsNone) "is none"
    }

    test "Enum surface area" {
      let t = CT.enumType
      let area = SurfaceArea.ofTypeDefinition t
      let expected = [("FooBar","0"); ("Foo","1"); ("Bar","2")]

      Expect.equal area.Enum.Value expected "enum value"
      Expect.isTrue (area.UnionCases.IsNone) "is none"
    }
    test "Class with event surface area" {
      let t = CT.myClassWithCLIEvent
      let area = SurfaceArea.ofTypeDefinition t
      let expected = [Method {Type = {FullName = "SynVer.TestHelper.Types.MyClassWithCLIEvent";};
                              Instance = InstanceOrStatic.Instance;
                              Name = "TestEvent";
                              Parameters = [{Type = {FullName = "System.Object";};
                                             Name = "arg";}];
                              Result = {FullName = "System.Void";};};
                      Constructor
                        {Type = {FullName = "SynVer.TestHelper.Types.MyClassWithCLIEvent";};
                         Parameters = [];};
                      Event
                        {Type =
                          {FullName =
                            "SynVer.TestHelper.Types.MyClassWithCLIEvent";};
                         Instance = InstanceOrStatic.Instance;
                         Name = "Event1";
                         Parameters =
                          [{Type = {FullName = "System.Object";};
                            Name = "sender";};
                           {Type =
                             {FullName =
                               "System.Tuple<SynVer.TestHelper.Types.MyClassWithCLIEvent,System.Object>";};
                            Name = "args";}];
                         Result = {FullName = "System.Void";};}]

      Expect.equal area.Members expected (sprintf "\n\n\nExpected empty diff \n\n\n%A\n\n\n" (Compare.members area.Members expected))
      Expect.isTrue (area.UnionCases.IsNone) "is none"
    }
  ]
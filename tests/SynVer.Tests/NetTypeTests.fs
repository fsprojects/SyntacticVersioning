module SynVer.NetTypeTests
open SynVer
open Expecto
open SynVer.TestHelper.Types

open TestHelper.Types
open TestHelper.TestAssemblies
open TestHelper
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

    test "net type simple fullname"{
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
    test "net type fullname of open generic"{
      let namedStaticClass t = Type.name t = "StaticClass"
      let scl = csharpWithGenerics.ExportedTypes |> Seq.find namedStaticClass
      let m = scl.GetMethod("Method")
      Expect.isTrue m.ReturnType.IsGenericType "generic type"
      Expect.equal m.ReturnType.Name "List`1" "short name"
      Expect.isNull m.ReturnType.FullName "full name"
      Expect.equal (Reflect.typeFullName m.ReturnType) "List<'TR>" "return type"
      let prm= m.GetParameters() |> Array.head
      Expect.equal (Reflect.typeFullName prm.ParameterType) "List<'T>" "parameter typ"
    }
    test "net type fullname of closed generic"{
      let namedClass t = Type.name t = "Class"
      let scl = csharpWithGenerics.ExportedTypes |> Seq.find namedClass
      let m = scl.GetMethod("Method")
      Expect.isNotNull m "method"
      Expect.isTrue m.ReturnType.IsGenericType "generic type"
      Expect.equal m.ReturnType.Name "List`1" "short name"
      //Expect.equal m.ReturnType.FullName "System.Collections.Generic.List`1[[System.Int32, System.Private.CoreLib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=7cec85d7bea7798e]]" "full name"
      Expect.equal (Reflect.typeFullName m.ReturnType) "System.Collections.Generic.List<System.Int32>" "return type"
      let prm= m.GetParameters() |> Array.head
      Expect.equal (Reflect.typeFullName prm.ParameterType) "System.Collections.Generic.List<SpiseMisu.MyEventArgs>" "parameter typ"
    }

    test "net type fullname of closed generic tuple"{
      let t= typeof<(RecordType*int)>
      Expect.equal t.Name "Tuple`2" "name"
      Expect.isTrue t.IsGenericType "is not a generic type"
      Expect.isTrue t.IsConstructedGenericType "IsConstructedGenericType"
      Expect.equal (Reflect.typeFullName t) "System.Tuple<SynVer.TestHelper.Types.RecordType,System.Int32>" "tuple type"
    }
    test "net type fullname of closed generic tuple in array"{
      let t= typeof<(RecordType*int)[]>
      Expect.equal t.Name "Tuple`2[]" "name"
      Expect.isFalse t.IsGenericType "is not a generic type"
      Expect.isFalse t.IsConstructedGenericType "IsConstructedGenericType"
      Expect.isTrue t.IsArray "is array"
      Expect.isTrue t.HasElementType "has element type"
      Expect.equal (Reflect.typeFullName t) "System.Tuple<SynVer.TestHelper.Types.RecordType,System.Int32>[]" "tuple type in array"
    }
    test "net type fullname of closed generic array"{
      let t= typeof<RecordType[]>
      Expect.equal t.Name "RecordType[]" "name"
      Expect.isFalse t.IsGenericType "is not a generic type" 
      Expect.isFalse t.IsConstructedGenericType "IsConstructedGenericType"
      Expect.isTrue t.IsArray "IsArray"
      Expect.equal (Reflect.typeFullName t) "SynVer.TestHelper.Types.RecordType[]" "array"
    }
  ]
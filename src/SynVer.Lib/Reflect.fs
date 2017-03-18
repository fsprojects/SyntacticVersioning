namespace SynVer

open Microsoft.FSharp.Reflection
open System
open System.Reflection
open Microsoft.FSharp.Reflection
 
module Reflect =
  type private CAtDat = CustomAttributeData
  open Microsoft.FSharp.Core

  let private _tagNetType (t: Type) : NetType =
      let attrType (a:CAtDat)= a.AttributeType
      let attrName a= (attrType a).Name
      let debuggerAttr a=(attrName a).Contains("Debugger")
      let serializableAttr a = attrType a = typeof<System.SerializableAttribute>
      let compilationMappingAttr a = attrType a = typeof<CompilationMappingAttribute>
      let structAttr a = attrType a = typeof<StructAttribute>
      let attrs = t.CustomAttributes
                    |> Seq.filter( debuggerAttr >> not )
                    |> Seq.filter( serializableAttr >> not )
      
      let compilationAttr = attrs |> Seq.tryFind compilationMappingAttr
      let structAttr = attrs |> Seq.tryFind structAttr

      let fromTypeFlags ()=
          match t with
            | _ when t.IsInterface ->
              NetType.Interface
            | _ when t.IsEnum ->
              NetType.Enum
            | _ when t.IsAbstract && t.IsClass && t.IsSealed ->
              NetType.Static
            | _ when t.IsValueType && not t.IsEnum && not t.IsPrimitive ->
              NetType.Struct
            | _ when t.IsAbstract ->
              NetType.Abstract
            | _ -> NetType.Class

      match compilationAttr, structAttr with
      | Some attr, _-> 
          let h = 
            attr.ConstructorArguments
              |> Seq.map(fun x -> x.Value) 
              |> Seq.cast<SourceConstructFlags>
              |> Seq.head 
          match h with
          | SourceConstructFlags.Module     -> NetType.Module
          | SourceConstructFlags.RecordType -> NetType.RecordType
          | SourceConstructFlags.SumType    -> NetType.SumType
          | _ -> fromTypeFlags()
      | _ , Some _ ->NetType.Struct
      | _ , _ -> fromTypeFlags()
  let private memoize f =    
    let cache = new System.Collections.Generic.Dictionary<_, _>()
    (fun x ->
        match cache.TryGetValue(x) with
        | true, v -> v
        | false, _ ->
          let v = f(x) 
          cache.Add(x, v)
          v)

  [<CompiledName("TagNetType")>]
  let tagNetType = memoize _tagNetType

  [<CompiledName("ExportedTypes")>]
  let exportedTypes (asm: Assembly): Type list =
      let ts =
        try
          asm.GetExportedTypes()
        with
          | :? ReflectionTypeLoadException as ex ->
            ex.Types
            |> Array.filter(function | null -> false | _ -> true)
          | ex -> failwith ex.Message
      ts |> Array.toList
  let rec internal typeFullName (t:Type) =
      let fsharpCoreAssembly = [].GetType().Assembly
      let fullname =
        match t.IsGenericType with
          | false ->
            match String.IsNullOrEmpty t.FullName with
              | true -> sprintf "'%s" (t.Name.ToUpperInvariant())
              | false -> t.FullName 
          | true ->
            let args =
              t.GetGenericArguments()
              |> Array.map typeFullName
              |> Array.reduce(sprintf "%s,%s")
            let removeAfterTick (a:string)=
                let i = a.IndexOf('`')
                if i>1 then a.Substring(0, i)
                else a
            let name = 
                if t.Assembly <> fsharpCoreAssembly && (not << String.IsNullOrEmpty) ( t.FullName ) then
                    removeAfterTick t.FullName 
                else 
                    removeAfterTick t.Name
            sprintf "%s<%s>" name args

      let guid = Guid.NewGuid().ToString()
      fullname.Replace("+Tags",guid).Replace('+','.').Replace(guid,"+Tags")

  [<CompiledName("TypeToTyp")>]
  let typeToTyp (t:Type):Typ={ FullName=typeFullName t }
  [<AutoOpen>]
  module private ToMember=
    type 'a Roc = System.Collections.ObjectModel.ReadOnlyCollection<'a>
    type CatArg = CustomAttributeTypedArgument


    let parameterToParameter (p:ParameterInfo) :Parameter= { Type=typeToTyp p.ParameterType; Name=p.Name }
    let parametersToParameter prms= prms |>Seq.map parameterToParameter |> List.ofSeq
    let deconstructConstructor (ctr:ConstructorInfo) :ConstructorLike =
      let params' = ctr.GetParameters() |> parametersToParameter
      {Type=typeToTyp ctr.ReflectedType;Parameters=params'}
    let recordConstructor ctr : Member=
      RecordConstructor (deconstructConstructor ctr)
    let constructor' ctr : Member=
      Constructor (deconstructConstructor ctr)
    let isStatic b = match b with | true -> InstanceOrStatic.Static | false -> InstanceOrStatic.Instance

    let event' (ei:EventInfo) : Member=
      let mi = ei.EventHandlerType.GetMethod("Invoke")
      let params' = mi.GetParameters() |> parametersToParameter
      Event {Type=typeToTyp mi.ReflectedType
             Instance= isStatic mi.IsStatic 
             Name= ei.Name
             Parameters= params'
             Result= typeToTyp mi.ReturnType}
    let field (fi:FieldInfo) : Member=
      Field {Type=typeToTyp fi.ReflectedType
             Instance=isStatic fi.IsStatic
             Name= fi.Name
             Result= typeToTyp fi.FieldType}
    let method' (mi:MethodInfo) : Member= 
      let params' = mi.GetParameters() |> parametersToParameter
      Method {Type=typeToTyp mi.ReflectedType
              Instance= isStatic mi.IsStatic 
              Name= mi.Name
              Parameters= params'
              Result= typeToTyp mi.ReturnType}
    let property (pi:PropertyInfo) : Member= 
      Property {Type=typeToTyp pi.ReflectedType
                Instance=isStatic (pi.GetGetMethod().IsStatic)
                Name= pi.Name
                Result= typeToTyp pi.PropertyType}

    let constructors (bfs: BindingFlags list) (t: Type): (ConstructorLike) array=
        t.GetConstructors(bfs |> List.fold(( ||| )) BindingFlags.Instance)
        |> Array.sortBy(fun x -> x.Name, x.GetParameters() |> Array.length)
        |> Array.map(deconstructConstructor)
    let unionConstructors' t ctors :Member list=
      ctors|> List.map (fun ctor-> UnionConstructor (t,ctor))

    let toUnionCases (t:Type) : Member list=
      let tp = typeToTyp t
      let mapNullOrItemToNull n = 
              if String.IsNullOrEmpty n || n = "Item" then 
                null
              else
                n
      FSharpType.GetUnionCases(t)
      |> Array.map(
        fun x ->
          let ps = x.GetFields()
                |> Array.map(fun pi -> 
                      Parameter.Create(typeToTyp pi.PropertyType, mapNullOrItemToNull pi.Name))
                |> Array.toList
          UnionCase (tp, x.Name, ps) //{ Name=x.Name; Fields=ps }
      )
      |> List.ofArray

    let getTypeMember (t:Typ) (m:MemberInfo) : Member list=
      let tag = tagNetType m.ReflectedType

      // Handle cases like the Fsharp.Core does (and a bit more):
      //
      // https://github.com/Microsoft/visualfsharp/blob/master/src/fsharp/
      //   FSharp.Core.Unittests/LibraryTestFx.fs#L103-L110
      //
      match m.MemberType with
        | MemberTypes.Constructor ->
          let nameInfo = (m :?> ConstructorInfo)
          match tag with
            | NetType.RecordType ->[ (recordConstructor nameInfo)]
            | __________ ->[ (constructor' nameInfo) ]
        | MemberTypes.Event ->
          [ (event' (m :?> EventInfo))]
        | MemberTypes.Field -> 
          [ (field (m :?> FieldInfo))]
        | MemberTypes.Method ->
          [ (method' (m :?> MethodInfo))]
        | MemberTypes.NestedType ->
          let nt = (m :?> Type)
          match tag with
            | NetType.SumType ->
              [| nt |]
              |> Array.collect (constructors [BindingFlags.NonPublic])
              |> Array.toList
              |> unionConstructors' t
            | _ ->
              // Already handled in `let types = ...`
              []
        | MemberTypes.Property ->
          [ (property (m :?> PropertyInfo))]
        | _ ->
          let fullname = m.ReflectedType.FullName
          failwith
            (sprintf "not handled: (%A,%s,%A)" tag fullname m.MemberType)
   
  let enumValues (t:Type) =
      let typ = typeToTyp t
      t.IsEnum |> function
        | false -> []
        | true ->
          t.GetFields()
          |> Array.filter(fun x -> x.FieldType.IsEnum)
          |> Array.map(fun x -> EnumValue(typ, x.Name, sprintf "%A" (x.GetRawConstantValue())))
          |> List.ofArray

  [<CompiledName("GetTypeMembers")>]
  let getTypeMembers (t:Type) : Member list=
    let t' = typeToTyp t
    let netT = tagNetType t
    let l= List.collect (getTypeMember t') (t.GetMembers()|> Array.toList)
    match netT with
    | NetType.SumType -> l @ toUnionCases t
    | NetType.Enum -> l @ (enumValues t)
    | _ -> l

  /// Concrete type of a SumType, i.e. Foo and Bar when 
  /// type SumType=Foo|Bar
  [<CompiledName("IsSumType")>]
  let isSumType (t: Type): bool =
        t.IsNested &&
        (t.BaseType |> function
           | null  -> false
           | type' -> (tagNetType type') = NetType.SumType)



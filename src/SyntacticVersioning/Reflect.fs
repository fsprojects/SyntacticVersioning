namespace SyntacticVersioning

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
            let name = 
                if t.Assembly <> fsharpCoreAssembly then
                    (t.FullName.Substring(0,t.FullName.IndexOf('`'))) 
                else 
                    (t.Name.Substring(0,t.Name.IndexOf('`'))) 
            sprintf "%s<%s>" name args

      let guid = Guid.NewGuid().ToString()
      fullname.Replace("+Tags",guid).Replace('+','.').Replace(guid,"+Tags")

  [<CompiledName("TypeToTyp")>]
  let typeToTyp (t:Type):Typ={ FullName=typeFullName t }
  [<AutoOpen>]
  module private ToMember=
    type 'a Roc = System.Collections.ObjectModel.ReadOnlyCollection<'a>
    type CatArg = CustomAttributeTypedArgument


    let parameterToParameter (p:ParameterInfo) = { Type=typeToTyp p.ParameterType; Name=p.Name }
    let parametersToParameter prms= prms |>Seq.map parameterToParameter |> List.ofSeq
    let deconstructConstructor (ctr:ConstructorInfo) =
      let params' = ctr.GetParameters() |> parametersToParameter
      (typeToTyp ctr.ReflectedType, params')
    let recordConstructor ctr : Member=
      RecordConstructor (deconstructConstructor ctr)
    let constructor' ctr : Member=
      Constructor (deconstructConstructor ctr)
    let isStatic b = match b with | true -> Static | false -> Instance

    let event' (ei:EventInfo) : Member=
      let mi = ei.EventHandlerType.GetMethod("Invoke")
      let params' = mi.GetParameters() |> parametersToParameter
      Event (typeToTyp mi.ReflectedType,isStatic mi.IsStatic, ei.Name, params',typeToTyp mi.ReturnType)
    let field (fi:FieldInfo) : Member=
      Field (typeToTyp fi.ReflectedType,isStatic fi.IsStatic, fi.Name, typeToTyp fi.FieldType)
    let method' (mi:MethodInfo) : Member= 
      let params' = mi.GetParameters() |> parametersToParameter
      Method (typeToTyp mi.ReflectedType,isStatic mi.IsStatic, mi.Name, params', typeToTyp mi.ReturnType)
    let property (pi:PropertyInfo) : Member= 
      Property (typeToTyp pi.ReflectedType, isStatic (pi.GetGetMethod().IsStatic), pi.Name, typeToTyp pi.PropertyType)
    let constructors (bfs: BindingFlags list) (t: Type): (Constructor) array=
        t.GetConstructors(bfs |> List.fold(( ||| )) BindingFlags.Instance)
        |> Array.sortBy(fun x -> x.Name, x.GetParameters() |> Array.length)
        |> Array.map(deconstructConstructor)
    let unionConstructors' t ctors :Member list=
      ctors|> List.map (fun ctor-> UnionConstructor (t,ctor))

    let toUnionCases (t:Type) : Member list=
      let tp = typeToTyp t
      FSharpType.GetUnionCases(t)
      |> Array.map(
        fun x ->
          let ps = x.GetFields()
                |> Array.map(fun pi ->
                      { 
                        Type= typeToTyp pi.PropertyType
                        Name= if String.IsNullOrEmpty pi.Name || pi.Name = "Item" then 
                                null
                              else
                                pi.Name
                      })
                |> Array.toList
          UnionCase(tp, x.Name, ps) //{ Name=x.Name; Fields=ps }
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
            | RecordType ->[ (recordConstructor nameInfo)]
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
            | SumType ->
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

  [<CompiledName("GetTypeMembers")>]
  let getTypeMembers (t:Type) : Member list=
    let t' = typeToTyp t
    let netT = tagNetType t
    let l= List.collect (getTypeMember t') (t.GetMembers()|> Array.toList)
    match netT with
    | SumType -> l @ toUnionCases t
    | _ -> l

  /// Concrete type of a SumType, i.e. Foo and Bar when 
  /// type SumType=Foo|Bar
  [<CompiledName("IsSumType")>]
  let isSumType (t: Type): bool =
        t.IsNested &&
        (t.BaseType |> function
           | null  -> false
           | type' -> (tagNetType type') = SumType)
   
  [<CompiledName("ToEnumTyp")>]
  let toEnumTyp (t:Type) : EnumTyp=
    let enumHlp : Type -> (string * string) list =
      fun t -> t.IsEnum |> function
        | false -> []
        | true ->
          t.GetFields()
          |> Array.filter(fun x -> x.FieldType.IsEnum)
          |> Array.map(fun x -> x.Name, sprintf "%A" (x.GetRawConstantValue()))
          |> List.ofArray
    {
       FullName= typeFullName t
       Values= enumHlp t
    }


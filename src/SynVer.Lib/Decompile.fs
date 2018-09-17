namespace SynVer

open System
open Mono.Cecil

module Decompile =
  open Microsoft.FSharp.Core

  let private _tagNetType (t: TypeDefinition) : NetType =
      let debuggerAttr a=(CustomAttribute.typeName a).Contains("Debugger")
      let serializableAttr a = CustomAttribute.typeFullName a = "System.SerializableAttribute"
      let compilationMappingAttr a = CustomAttribute.typeName a = "CompilationMappingAttribute"
      let structAttr a = CustomAttribute.typeName a = "StructAttribute"
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
    let cache = new System.Collections.Concurrent.ConcurrentDictionary<'x, 'y>()
    (fun (x:'x) ->
        cache.GetOrAdd(x, System.Func<'x,'y>(f)))

  [<CompiledName("TagNetType")>]
  let tagNetType = memoize _tagNetType

  [<CompiledName("ExportedTypes")>]
  let exportedTypes (a:AssemblyDefinition) : TypeDefinition list =
    //let r = ReaderParameters() 
    //let a = AssemblyDefinition.ReadAssembly (filename, r)
    a.Modules 
      |> Seq.map (fun m-> m.Types)
      |> Seq.concat
      |> Seq.toList
  let rec internal typeFullName (t:TypeReference) =
      let fullname =
        match t.HasGenericParameters with
          | false ->
            match String.IsNullOrEmpty t.FullName with
              | true -> sprintf "'%s" (t.Name.ToUpperInvariant())
              | false -> t.FullName 
          | true ->
            let args =
              t.GenericParameters
              |> Seq.map typeFullName
              |> Seq.reduce(sprintf "%s,%s")
            let removeAfterTick (a:string)=
                let i = a.IndexOf('`')
                if i>1 then a.Substring(0, i)
                else a
            let name = 
                if (not << String.IsNullOrEmpty) ( t.FullName ) then
                    removeAfterTick t.FullName 
                else 
                    removeAfterTick t.Name
            sprintf "%s<%s>" name args

      let guid = Guid.NewGuid().ToString()
      fullname.Replace("+Tags",guid).Replace('+','.').Replace(guid,"+Tags")

  [<CompiledName("TypeToTyp")>]
  let typeToTyp (t:TypeReference):Typ={ FullName=typeFullName t }
  [<AutoOpen>]
  module private ToMember=
    type 'a Roc = System.Collections.ObjectModel.ReadOnlyCollection<'a>
    type CatArg = CustomAttributeTypedArgument

    let notNull value = 
      if obj.ReferenceEquals(value, null) then None 
      else Some value

    let parameterToParameter (p:ParameterReference) :Parameter= { Type=typeToTyp p.ParameterType; Name=p.Name }
    let parametersToParameter prms= prms |>Seq.map parameterToParameter |> List.ofSeq
    let deconstructConstructor t (ctr:MethodDefinition) :ConstructorLike =
      let params' = ctr.Parameters |> parametersToParameter
      {Type=typeToTyp t; Parameters=params'}
    let recordConstructor t ctr : Member=
      RecordConstructor (deconstructConstructor t ctr)
    let constructor' t ctr : Member=
      Constructor (deconstructConstructor t ctr)
    let isStatic b = match b with | true -> InstanceOrStatic.Static | false -> InstanceOrStatic.Instance

    let event' t (ei:EventDefinition) : Member=
      let mi = ei.InvokeMethod //ei.EventType.GetMethod("Invoke")
      let params' = mi.Parameters |> parametersToParameter
      Event {Type=typeToTyp t
             Instance= isStatic mi.IsStatic 
             Name= ei.Name
             Parameters= params'
             Result= typeToTyp mi.ReturnType}
    let field t (fi:FieldDefinition) : Member=
      Field {Type=typeToTyp t
             Instance=isStatic fi.IsStatic
             Name= fi.Name
             Result= typeToTyp fi.FieldType}
    let method' t (mi:MethodDefinition) : Member= 
      let params' = mi.Parameters |> parametersToParameter
      Method {Type=typeToTyp t
              Instance= isStatic mi.IsStatic 
              Name= mi.Name
              Parameters= params'
              Result= typeToTyp mi.ReturnType}
    let property t (pi:PropertyDefinition) : Member= 
      let static' =
          match notNull (pi.GetMethod), notNull (pi.SetMethod) with
          |  Some getMethod, _ -> getMethod.IsStatic
          | _, Some setMethod -> setMethod.IsStatic
          | _, _ -> true
          

      Property {Type=typeToTyp t
                Instance=isStatic static'
                Name= pi.Name
                Result= typeToTyp pi.PropertyType}

    let constructors (t: TypeDefinition): (ConstructorLike) seq=
        let deconstructConstructor =deconstructConstructor t
        t.Methods |> Seq.filter (fun m -> m.IsNewSlot)
        //t.GetConstructors(bfs |> List.fold(( ||| )) BindingFlags.Instance)
        |> Seq.sortBy(fun x -> x.Name, x.Parameters.Count)
        |> Seq.map deconstructConstructor
    let unionConstructors' t ctors :Member list=
      ctors|> List.map (fun ctor-> UnionConstructor (t,ctor))

    let toUnionCases (t:TypeReference) : Member list=
      let tp = typeToTyp t
      let mapNullOrItemToNull n = 
              if String.IsNullOrEmpty n || n = "Item" then 
                null
              else
                n
      FSharpType.GetUnionCases (t.Resolve())
      |> Array.map(
        fun x ->
          let ps = x.GetFields()
                |> Array.map(fun pi -> 
                      Parameter.Create(typeToTyp pi.PropertyType, mapNullOrItemToNull pi.Name))
                |> Array.toList
          UnionCase (tp, x.Name, ps) //{ Name=x.Name; Fields=ps }
      )
      |> List.ofArray

    let getTypeMember (t:TypeDefinition) (m:MemberReference) : Member list=
      let tag = tagNetType t
      let m' = m.Resolve()
      // Handle cases like the Fsharp.Core does (and a bit more):
      //
      // https://github.com/Microsoft/visualfsharp/blob/master/src/fsharp/
      //   FSharp.Core.Unittests/LibraryTestFx.fs#L103-L110
      //
      match m with
        (*| :? ConstructorReference as nameInfo->
          match tag with
            | NetType.RecordType ->[ (recordConstructor nameInfo)]
            | __________ ->[ (constructor' nameInfo) ] *)
        | :? EventReference as m ->
          [ (event' t (m.Resolve()))]
        | :? FieldReference as m ->
          [ (field t (m.Resolve()))]
        | :? MethodReference as m ->
          [ (method' t (m.Resolve()))]
        (*| MemberTypes.NestedType ->
          let nt = (m :?> Type)
          match tag with
            | NetType.SumType ->
              [| nt |]
              |> Array.collect (constructors [BindingFlags.NonPublic])
              |> Array.toList
              |> unionConstructors' t
            | _ ->
              // Already handled in `let types = ...`
              [] *)
        | :? PropertyReference as p ->
          [ (property t (p.Resolve()))]
        | _ ->
          let fullname = t.FullName
          failwith
            (sprintf "not handled: (%A,%s,%A)" tag fullname (m.GetType().Name))
   
  let enumValues (t:TypeDefinition) =
      let typ = typeToTyp t
      t.IsEnum |> function
        | false -> []
        | true ->
          t.Fields
          |> Seq.filter(fun x -> not x.IsSpecialName && not x.IsRuntimeSpecialName)
          |> Seq.map(fun x -> EnumValue(typ, x.Name, sprintf "%A" (x.Constant)))
          |> List.ofSeq

  [<CompiledName("GetTypeMembers")>]
  let getTypeMembers (t:TypeDefinition) : Member list=
    //let t' = typeToTyp t
    let netT = tagNetType t
    //t.Methods
    let l= List.collect (getTypeMember t) (t.Properties |> Seq.toList)
    let l2= List.collect (getTypeMember t) (t.Fields |> Seq.toList)
    match netT with
    | NetType.SumType -> l @ l2 @ toUnionCases t
    | NetType.Enum -> l @ l2 @ (enumValues t)
    | _ -> l

  /// Concrete type of a SumType, i.e. Foo and Bar when 
  /// type SumType=Foo|Bar
  [<CompiledName("IsSumType")>]
  let isSumType (t: TypeDefinition): bool =
        t.IsNested &&
        (t.BaseType |> function
           | null  -> false
           | type' -> (tagNetType (type'.Resolve())) = NetType.SumType)

  [<CompiledName("ReadAssembly")>]
  let readAssembly (path:string)=
    let r = ReaderParameters()
    r.AssemblyResolver <- {
      new DefaultAssemblyResolver()
      with
        override this.Resolve(name:AssemblyNameReference)=
          try
            base.Resolve(name)
          with _->
            // hack to avoid assembly load failure on Windows
            if name.Name.Equals("FSharp.Core", StringComparison.OrdinalIgnoreCase) then 
              AssemblyDefinition.ReadAssembly (typeof< FSharp.Core.FSharpTypeFunc>).Assembly.Location
            else
              null
    }
    AssemblyDefinition.ReadAssembly(path, r)
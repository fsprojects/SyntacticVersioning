namespace SyntacticVersioning

open Microsoft.FSharp.Reflection
open System
open System.Reflection
open Microsoft.FSharp.Reflection
 
module Reflect =
  type private CAtDat = CustomAttributeData

  let private tagNetHlp (t: Type) : (string option * string option) =
      let debuggerAttr (a:CAtDat) = a.AttributeType.Name.Contains("Debugger")
      match t with
        | _ when t.FullName.EndsWith("+Tags") -> (Some "UnionTags", None)
        | _ -> 
          t.CustomAttributes
          |> Seq.filter( debuggerAttr >> not )
          |> Seq.map(
            fun x ->
              x.AttributeType.Name,
              x.ConstructorArguments |> Seq.map(fun x -> x.Value.ToString()))
          |> Seq.map(
            fun (x,ys) ->
              (if x.Equals("CompilationMappingAttribute") then None else Some x),
              (if Seq.isEmpty ys then None else Seq.head ys |> Some))
          |> fun xs ->
            if Seq.isEmpty xs then (None,None) else Seq.head xs
  
  [<CompiledName("TagNetType")>]
  let tagNetType (t: Type) : NetType =
      // TODO: Active/Partial Patterns, MeasureOfUnits? Any other?
      match tagNetHlp t with
        | Some "UnionTags"       , _________________ -> NetType.UnionTags
        | Some "StructAttribute" , _________________ -> NetType.Struct
        | ______________________ , Some "Module"     -> NetType.Module
        | ______________________ , Some "RecordType" -> NetType.RecordType
        | ______________________ , Some "SumType"    -> NetType.SumType
        | ______________________ ,__________________ ->
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
  [<CompiledName("ExportedTypes")>]
  let exportedTypes (asm: Assembly): Type list =
      let ts =
        try
          asm.GetExportedTypes()
        with
          | :? ReflectionTypeLoadException as ex ->
            ex.Types
            |> Array.filter(function | null -> false | _ -> true)
          | _ as ex -> failwith ex.Message
      ts |> Array.toList
  let rec internal typeFullName (t:Type) =
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
            sprintf "%s<%s>" (t.FullName.Substring(0,t.FullName.IndexOf('`'))) args
      let guid = Guid.NewGuid().ToString()
      fullname.Replace("+Tags",guid).Replace('+','.').Replace(guid,"+Tags")
  [<CompiledName("TypeToTyp")>]
  let typeToTyp (t:Type):Typ={ FullName=typeFullName t }
  [<AutoOpen>]
  module private ToMember=
    type 'a roc = System.Collections.ObjectModel.ReadOnlyCollection<'a>
    type catarg = CustomAttributeTypedArgument

    let attributeToAttribute (a:CAtDat) : SyntacticVersioning.Attribute= 
      let args =a.ConstructorArguments 
                |> Seq.map (fun ca-> {Value=ca.Value; Index=1 }) 
                |> List.ofSeq
      { FullName=a.AttributeType.FullName
        ConstructorArguments=args }
    let parameterToParameter (p:ParameterInfo) = { Type=typeToTyp p.ParameterType; Name=p.Name }
    let attributesToString atrs= atrs |>Seq.map attributeToAttribute |> List.ofSeq
    let parametersToParameter prms= prms |>Seq.map parameterToParameter |> List.ofSeq
    let deconstructConstructor (ctr:ConstructorInfo) =
      let attr = ctr.CustomAttributes |> attributesToString
      let params' = ctr.GetParameters() |> parametersToParameter
      (typeToTyp ctr.ReflectedType, attr, params')
    let recordConstructor ctr : Member=
      RecordConstructor (deconstructConstructor ctr)
    let constructor' ctr : Member=
      Constructor (deconstructConstructor ctr)
    let isStatic b = match b with | true -> Static | false -> Instance

    let event' (ei:EventInfo) : Member=
      let mi = ei.EventHandlerType.GetMethod("Invoke")
      let attr = ei.CustomAttributes |> attributesToString
      let params' = mi.GetParameters() |> parametersToParameter
      Event (typeToTyp mi.ReflectedType,isStatic mi.IsStatic, ei.Name,attr,params',typeToTyp mi.ReturnType)
    let field (fi:FieldInfo) : Member=
      Field (typeToTyp fi.ReflectedType,isStatic fi.IsStatic, fi.Name, typeToTyp fi.FieldType)
    let method' (mi:MethodInfo) : Member= 
      let attr = mi.CustomAttributes |> attributesToString
      let params' = mi.GetParameters() |> parametersToParameter
      Method (typeToTyp mi.ReflectedType,isStatic mi.IsStatic, mi.Name,attr,params', typeToTyp mi.ReturnType)
    let property (pi:PropertyInfo) : Member= 
      Property (typeToTyp pi.ReflectedType, isStatic (pi.GetGetMethod().IsStatic), pi.Name, typeToTyp pi.PropertyType)
    let constructors (bfs: BindingFlags list) (t: Type): (Constructor) array=
        t.GetConstructors(bfs |> List.fold(fun a x -> a ||| x) BindingFlags.Instance)
        |> Array.sortBy(fun x -> x.Name, x.GetParameters() |> Array.length)
        |> Array.map(fun x -> deconstructConstructor x)
    let unionConstructors' t ctors :Member list=
      ctors|> List.map (fun ctor-> UnionConstructor (t,ctor))

  let private getTypeMember (t:Typ) (m:MemberInfo) : Member list=
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
    t.GetMembers()
    |> Array.toList
    |> List.map (getTypeMember t')
    |> List.concat

  [<CompiledName("IsSumType")>]         
  let isSumType (t: Type): bool =
        t.IsNested &&
        (t.BaseType |> function
           | null  -> false
           | type' -> (tagNetType type') = SumType)
   
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
    
  let getUnionCases (t:Type) : UnionCases=
    let cases = 
      FSharpType.GetUnionCases(t)
      |> Array.map(
        fun x ->
          let ps = x.GetFields()
                |> Array.map(fun pi -> typeToTyp pi.PropertyType)
                |> Array.toList
          { Name=x.Name; Fields=ps }
      )
      |> List.ofArray
    { Type=typeToTyp t; Cases =cases }

namespace SyntacticVersioning

open Microsoft.FSharp.Reflection
open System
open System.Reflection
  
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
  let exportedTypes (asm: Assembly): Type array =
      let ts =
        try
          asm.GetExportedTypes()
        with
          | :? ReflectionTypeLoadException as ex ->
            ex.Types
            |> Array.filter(function | null -> false | _ -> true)
          | _ as ex -> failwith ex.Message
      ts

  module private ToMember=
    let rec typeFullName (t:Type) =
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
    let typeToType (t:System.Type):SyntacticVersioning.Type={ FullName=typeFullName t }
    let attributeToAttribute (a:CAtDat) : SyntacticVersioning.Attribute= 
      let args =a.ConstructorArguments |> Seq.map (fun ca-> {Value=ca.Value}) |> List.ofSeq
      { FullName=a.AttributeType.FullName
        ConstructorArguments=args }
    let parameterToParameter (p:ParameterInfo) = { Type=typeToType p.ParameterType; Name=p.Name }
    let attributesToString atrs= atrs |>Seq.map attributeToAttribute |> List.ofSeq
    let parametersToParameter prms= prms |>Seq.map parameterToParameter |> List.ofSeq
    let deconstructConstructor (ctr:ConstructorInfo) =
      let attr = ctr.CustomAttributes |> attributesToString
      let params' = ctr.GetParameters() |> parametersToParameter
      (typeToType ctr.ReflectedType, attr, params')
    let recordConstructor ctr : Member=
      RecordConstructor (deconstructConstructor ctr)
    let constructor' ctr : Member=
      Constructor (deconstructConstructor ctr)
    let isStatic b = match b with | true -> Static | false -> Instance

    let event' (ei:EventInfo) : Member=
      let mi = ei.EventHandlerType.GetMethod("Invoke")
      let attr = ei.CustomAttributes |> attributesToString
      let params' = mi.GetParameters() |> parametersToParameter
      Event (typeToType mi.ReflectedType,isStatic mi.IsStatic, ei.Name,attr,params',typeToType mi.ReturnType)
    let field (fi:FieldInfo) : Member=
      Field (typeToType fi.ReflectedType,isStatic fi.IsStatic, fi.Name, typeToType fi.FieldType)
    let method' (mi:MethodInfo) : Member= 
      let attr = mi.CustomAttributes |> attributesToString
      let params' = mi.GetParameters() |> parametersToParameter
      Method (typeToType mi.ReflectedType,isStatic mi.IsStatic, mi.Name,attr,params', typeToType mi.ReturnType)
    let property (pi:PropertyInfo) : Member= 
      Property (typeToType pi.ReflectedType, isStatic (pi.GetGetMethod().IsStatic), pi.Name, typeToType pi.PropertyType)
    let constructors : BindingFlags list -> Type -> (SyntacticVersioning.Type*SyntacticVersioning.Member) array=
      fun bfs t ->
        t.GetConstructors(bfs |> List.fold(fun a x -> a ||| x) BindingFlags.Instance)
        |> Array.sortBy(fun x -> x.Name, x.GetParameters() |> Array.length)
        |> Array.map(fun x -> typeToType t, constructor' x)
  open ToMember
  [<CompiledName("GetTypeMembers")>]
  let getTypeMembers (t:Type)=
        let tag = tagNetType t
        t.GetMembers() 
        |> Array.choose ( fun (m) ->
            // Handle cases like the Fsharp.Core does (and a bit more):
            //
            // https://github.com/Microsoft/visualfsharp/blob/master/src/fsharp/
            //   FSharp.Core.Unittests/LibraryTestFx.fs#L103-L110
            //
            match m.MemberType with
              | MemberTypes.Constructor ->
                let nameInfo = (m :?> ConstructorInfo)
                match tag with
                  | RecordType ->Some (recordConstructor nameInfo)
                  | __________ ->Some (constructor' nameInfo)
              | MemberTypes.Event ->
                Some (event' (m :?> EventInfo))
              | MemberTypes.Field -> 
                Some (field (m :?> FieldInfo))
              | MemberTypes.Method ->
                Some (method' (m :?> MethodInfo))
              | MemberTypes.NestedType ->
                let nt = (m :?> Type)
                match tag with
                  | SumType ->
                    None // TODO: Translate
                    (*
                    [| nt |]
                    |> Array.collect (constructors [BindingFlags.NonPublic])
                    |> Array.map (fun (x,y) -> unionConstructor' (x,fullname,y))
                    |> Array.fold(fun a x -> x + a) String.Empty
                    |> (fun f->f) 
                    *)
                  | _ ->
                    // Already handled in `let types = ...`
                     None
              | MemberTypes.Property ->
                Some (property (m :?> PropertyInfo))
              | _ ->
                let fullname = t.FullName
                failwith
                  (sprintf "not handled: (%A,%s,%A)" tag fullname m.MemberType))
         
            
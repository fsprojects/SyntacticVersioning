namespace SyntacticVersioning

open Microsoft.FSharp.Reflection
open System
open System.Reflection
  
module Reflect =
  let private tagNetHlp (t: Type) : (string option * string option) =
      let debuggerAttr (a:CustomAttributeData) = a.AttributeType.Name.Contains("Debugger")
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
    let attributeToString (a:CustomAttributeData)= a.ToString()
    let parameterToParameter (p:ParameterInfo) = { Type=p.ParameterType.FullName; Name=p.Name }
    let attributesToString atrs= atrs |>Seq.map attributeToString |> List.ofSeq
    let parametersToParameter prms= prms |>Seq.map parameterToParameter |> List.ofSeq
    let deconstructConstructor (ctr:ConstructorInfo) =
      let attr = ctr.CustomAttributes |> attributesToString
      let params' = ctr.GetParameters() |> parametersToParameter
      (attr,params')
    let recordConstructor ctr : Member=
      RecordConstructor (deconstructConstructor ctr)
    let constructor' ctr : Member=
      Constructor (deconstructConstructor ctr)
    let isStatic b = match b with | true -> Static | false -> Instance

    let event' (ei:EventInfo) : Member=
      let mi = ei.EventHandlerType.GetMethod("Invoke")
      let attr = ei.CustomAttributes |> attributesToString
      let params' = mi.GetParameters() |> parametersToParameter
      Event (isStatic mi.IsStatic, ei.Name,attr,params', mi.ReturnType.FullName)
    let field (fi:FieldInfo) : Member=
      Field (isStatic fi.IsStatic, fi.Name, fi.FieldType.FullName)
    let method' (mi:MethodInfo) : Member= 
      let attr = mi.CustomAttributes |> attributesToString
      let params' = mi.GetParameters() |> parametersToParameter
      Method (isStatic mi.IsStatic, mi.Name,attr,params', mi.ReturnType.FullName)
    let property (pi:PropertyInfo) : Member= 
      Property (isStatic (pi.GetGetMethod().IsStatic), pi.Name, pi.PropertyType.FullName)

  open ToMember
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
                    |> Array.collect (Reflect.constructers [BindingFlags.NonPublic])
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
         
            
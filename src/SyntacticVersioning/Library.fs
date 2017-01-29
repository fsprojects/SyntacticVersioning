namespace SyntacticVersioning

type version = Major | Minor | Patch
type netType =
  | Abstract | Class | Enum | Interface | Static | Struct
  (* | Primitive | ValueType *)
  | Module | RecordType | SumType | UnionConstructor | UnionTags

open Microsoft.FSharp.Reflection
open System
open System.Reflection
  
module Reflect =
  let private tagNetHlp : Type -> (string option * string option) =
    fun t ->
      match t with
        | _ when t.FullName.EndsWith("+Tags") -> (Some "UnionTags", None)
        | _ -> 
          t.CustomAttributes
          |> Seq.filter(
            fun x ->
              x.AttributeType.Name.Contains("Debugger") |> not)
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
  
  let tagNetType : Type -> netType =
    fun t ->
      // TODO: Active/Partial Patterns, MeasureOfUnits? Any other?
      match tagNetHlp t with
        | Some "UnionTags"       , _________________ -> netType.UnionTags
        | Some "StructAttribute" , _________________ -> netType.Struct
        | ______________________ , Some "Module"     -> netType.Module
        | ______________________ , Some "RecordType" -> netType.RecordType
        | ______________________ , Some "SumType"    -> netType.SumType
        | ______________________ ,__________________ ->
          match t with
            | _ when t.IsInterface ->
              netType.Interface
            | _ when t.IsEnum ->
              netType.Enum
            | _ when t.IsAbstract && t.IsClass && t.IsSealed ->
              netType.Static
            | _ when t.IsValueType && not t.IsEnum && not t.IsPrimitive ->
              netType.Struct
            | _ when t.IsAbstract ->
              netType.Abstract
            | _ -> netType.Class
  
  let exportedTypes : Assembly -> Type array =
    fun asm ->
      let ts =
        try
          asm.GetExportedTypes()
        with
          | :? ReflectionTypeLoadException as ex ->
            ex.Types
            |> Array.filter(function | null -> false | _ -> true)
          | _ as ex -> failwith ex.Message
      ts
       
namespace SynVer

open System
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.TypeSystem
//open Mono.Cecil
// port of selected parts of
// https://github.com/fsharp/fsharp/blob/7ac8931b4595f0f30bc9b3bc8a094ecc69ccc443/src/fsharp/FSharp.Core/reflect.fs
// to Mono.Cecil

type BindingFlags=System.Reflection.BindingFlags
//NOTE: hacky way to filter some of the things and removing special special things
module internal BindingFlags=
  let matches (t:ITypeDefinition) (b:BindingFlags) =
    let mutable m=true
    let isPublic = t.Accessibility.HasFlag(Accessibility.Public)
    if (b&&& BindingFlags.Public) = BindingFlags.Public && isPublic then m<-true
    if (b&&& BindingFlags.NonPublic) = BindingFlags.NonPublic && not isPublic then m<-true
    m && not <| t.IsCompilerGenerated()
module internal MethodDefinition=
  let matches (t:IMethod) (b:BindingFlags) =
    let mutable m=true
    let isPublic = t.Accessibility.HasFlag(Accessibility.Public)
    if (b&&& BindingFlags.Public) = BindingFlags.Public && isPublic then m<-true
    if (b&&& BindingFlags.NonPublic) = BindingFlags.NonPublic && not isPublic then m<-true
    m && not <| t.IsCompilerGenerated()
module internal PropertyDefinition=
  let matches (t:IProperty) (b:BindingFlags) =
    let mutable m=true
    let isPublic = t.Accessibility.HasFlag(Accessibility.Public)
    if (b&&& BindingFlags.Public) = BindingFlags.Public && isPublic then m<-true
    if (b&&& BindingFlags.NonPublic) = BindingFlags.NonPublic && not isPublic then m<-true
    m && not <| t.IsCompilerGenerated()
module internal FieldDefinition=
  let matches (t:IField) (b:BindingFlags) =
    let mutable m=true
    let isPublic = t.Accessibility.HasFlag(Accessibility.Public)
    if (b&&& BindingFlags.Public) = BindingFlags.Public && isPublic then m<-true
    if (b&&& BindingFlags.NonPublic) = BindingFlags.NonPublic && not isPublic then m<-true
    m && not <| t.IsCompilerGenerated()
module internal CustomAttribute=
  let typ (a:CustomAttribute)= a.AttributeType
  let typeName a = (typ a).Name
  let typeFullName a = (typ a).FullName
module internal TypeDefinition =
  let getNestedType (n:string) (b:BindingFlags) (t:ITypeDefinition)=
    t.NestedTypes |> Seq.tryFind (fun t' -> t'.Name = n && BindingFlags.matches t' b)
module internal AssemblyDefinition=
  /// the assumption is that this does not fail, i.e. that the tests pick it up in that case
  let getType (t:Type) (a:AssemblyDefinition) :ITypeDefinition= 
    match a.MainModule.Types |> Seq.tryFind (fun t'->t'.FullName = t.FullName) with
    | Some t' -> t'
    | None -> failwithf "Could not find '%s'" t.FullName 
    
module Impl=
    let isNamedType(typ:ITypeReference) = not (typ.IsArray || typ.IsByReference || typ.IsPointer)


    let isCompilationMappingAttr a = CustomAttribute.typeName a = "CompilationMappingAttribute"
    let isCompilationRepresentationAttr a = CustomAttribute.typeName a = "CompilationRepresentationAttribute"
    let getGenericTypeDefinition (ty:TypeReference) = 
        failwithf "! %s" ty.FullName
    
    let equivHeadTypes (ty1:TypeReference) (ty2:TypeReference) = 
        isNamedType(ty1) &&
        if ty1.IsGenericInstance then 
          ty2.IsGenericInstance && (getGenericTypeDefinition ty1).Equals(getGenericTypeDefinition ty2)
        else 
          ty1.Equals(ty2)

    //let func = typedefof<(obj -> obj)>
    
    let fsharpAssembly =
        AssemblyDefinition.ReadAssembly (typeof< FSharp.Core.FSharpTypeFunc>).Assembly.Location
    let isOptionType typ = equivHeadTypes typ <| AssemblyDefinition.getType (typedefof<_ option>) fsharpAssembly 
    let isFunctionType typ = equivHeadTypes typ <| AssemblyDefinition.getType (typedefof<(_-> _)>) fsharpAssembly
    let isListType typ = equivHeadTypes typ <| AssemblyDefinition.getType (typedefof<_ list>) fsharpAssembly
    let getInstancePropertyInfo (typ: TypeDefinition,propName,bindingFlags) = 
        typ.Properties |> Seq.tryFind (fun p->p.Name = propName && p.HasThis && PropertyDefinition.matches p bindingFlags) 
    let getInstancePropertyInfos (typ,names,bindingFlags) = 
        names |> Array.choose (fun nm -> getInstancePropertyInfo (typ,nm,bindingFlags)) 
    
    let tryFindCompilationMappingAttribute (attrs:CustomAttribute[]) =
      match attrs with
      | null | [| |] -> None
      | [| res |] ->
        // https://github.com/fsharp/fsharp/blob/3ef054469387449dc7bd6f151960825235fe491c/src/fsharp/FSharp.Core/prim-types.fs#L220-L223
        match res.ConstructorArguments |> Seq.toList with
        | [sourceConstructFlags] ->
          Some (sourceConstructFlags.Value :?>SourceConstructFlags, 0, 0)
        | [sourceConstructFlags; sequenceNumber] when sourceConstructFlags.Type.Name = "SourceConstructFlags" ->
          Some (sourceConstructFlags.Value :?>SourceConstructFlags, sequenceNumber.Value :?>int, 0)
        | [_; _] ->
          Some (SourceConstructFlags.None, 0, 0)
        | [sourceConstructFlags;variantNumber; sequenceNumber] ->
          Some (sourceConstructFlags.Value :?>SourceConstructFlags, sequenceNumber.Value :?>int, variantNumber.Value:?>int)
        | _ -> raise <| System.InvalidOperationException "constructor"
      | _ -> raise <| System.InvalidOperationException "multipleCompilationMappings"
    let tryFindCompilationRepresentationAttribute (attrs:CustomAttribute[]) =
      match attrs with
      | null | [| |] -> None
      | [| res |] ->
        let flags = res.ConstructorArguments |> Seq.find (fun p->p.Type.Name = "CompilationRepresentationFlags") 
        Some (flags.Value :?>CompilationRepresentationFlags)
      | _ -> raise <| System.InvalidOperationException "multipleCompilationRepresentations"
    let findCompilationMappingAttribute (attrs:CustomAttribute[]) =
      match tryFindCompilationMappingAttribute attrs with
      | None -> failwith "no compilation mapping attribute"
      | Some a -> a
    let tryFindCompilationMappingAttributeFromType (typ:ITypeDefinition) = 
        tryFindCompilationMappingAttribute ( typ.CustomAttributes |> Seq.filter isCompilationMappingAttr |> Seq.toArray)

    let tryFindSourceConstructFlagsOfType (typ:TypeDefinition) = 
      match tryFindCompilationMappingAttributeFromType typ with 
      | None -> None
      | Some (flags,_n,_vn) -> Some flags

    let tryFindCompilationMappingAttributeFromMemberInfo (info:IMember) = 
        tryFindCompilationMappingAttribute ( info.CustomAttributes |> Seq.filter isCompilationMappingAttr |> Seq.toArray)
        
    let findCompilationMappingAttributeFromMemberInfo (info: IMember) = findCompilationMappingAttribute ( info.CustomAttributes |> Seq.filter isCompilationMappingAttr |> Seq.toArray)
    let sequenceNumberOfMember          (x: IMember) = let (_,n,_) = findCompilationMappingAttributeFromMemberInfo x in n
    let variantNumberOfMember           (x: IMember) = let (_,_,vn) = findCompilationMappingAttributeFromMemberInfo x in vn

    let isExceptionRepr (typ:ITypeDefinition,bindingFlags) = 
        match tryFindSourceConstructFlagsOfType(typ) with 
        | None -> false 
        | Some(flags) -> 
          ((flags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.Exception) &&
          // We see private representations only if BindingFlags.NonPublic is set
          (if (flags &&& SourceConstructFlags.NonPublicRepresentation) <> enum(0) then 
              (bindingFlags &&& BindingFlags.NonPublic) <> enum(0)
           else 
              true)
              
    let isFieldProperty (prop : IProperty) =
        match tryFindCompilationMappingAttributeFromMemberInfo(prop) with
        | None -> false
        | Some (flags,_n,_vn) -> (flags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.Field

    let isUnionType (typ:ITypeDefinition,bindingFlags:BindingFlags) = 
        isOptionType typ || 
        isListType typ || 
        match tryFindSourceConstructFlagsOfType(typ) with 
        | None -> false
        | Some(flags) ->
          (flags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.SumType &&
          // We see private representations only if BindingFlags.NonPublic is set
          (if (flags &&& SourceConstructFlags.NonPublicRepresentation) <> enum(0) then 
              (bindingFlags &&& BindingFlags.NonPublic) <> enum(0)
           else 
              true)
    let unionTypeOfUnionCaseType (typ: ITypeDefinition,bindingFlags) = 
        let rec get (typ:ITypeDefinition) = if isUnionType (typ,bindingFlags) then typ else match typ.BaseType with null -> typ | b -> get <| b.Resolve()
        get typ
    // Check the base type - if it is also an F# type then
    // for the moment we know it is a Discriminated Union
    let isConstructorRepr (typ:TypeDefinition,bindingFlags:BindingFlags) = 
        let rec get (typ:TypeDefinition) = isUnionType (typ,bindingFlags) || match typ.BaseType with null -> false | b -> get (b.Resolve())
        get typ 
    let rec isClosureRepr (typ:TypeReference) = 
        isFunctionType typ || 
        (match typ.Resolve().BaseType with null -> false | bty -> isClosureRepr bty)
    let getTypeOfReprType (typ: ITypeDefinition,bindingFlags)= 
        if isExceptionRepr(typ,bindingFlags) then typ.BaseType.Resolve()
        elif isConstructorRepr(typ,bindingFlags) then unionTypeOfUnionCaseType(typ,bindingFlags)
        elif isClosureRepr(typ) then 
          let rec get (typ:TypeDefinition) = if isFunctionType typ then typ else match typ.BaseType with null -> typ | b -> get <| b.Resolve()
          get typ 
        else typ
        
    let getUnionTypeTagNameMap (typ: ITypeDefinition,bindingFlags) = 
        let maybeEnumTyp =TypeDefinition.getNestedType "Tags" bindingFlags typ
        // Unions with a singleton case do not get a Tags type (since there is only one tag), hence enumTyp may be null in this case
        match maybeEnumTyp with
        | None -> 
            typ.Methods |> Seq.filter (fun m->m.IsStatic && MethodDefinition.matches m bindingFlags) 
            |> Seq.choose (fun minfo -> 
                match tryFindCompilationMappingAttributeFromMemberInfo(minfo) with
                | None -> None
                | Some (flags,n,_vn) -> 
                    if (flags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.UnionCase then 
                        let nm = minfo.Name 
                        // chop "get_" or  "New" off the front 
                        let nm = 
                            if not (isListType typ) && not (isOptionType typ) then 
                                if   nm.Length > 4 && nm.[0..3] = "get_" then nm.[4..] 
                                elif nm.Length > 3 && nm.[0..2] = "New" then nm.[3..]
                                else nm
                            else nm
                        Some (n, nm)
                    else
                        None) 
        | Some enumTyp -> 
            enumTyp.Fields |> Seq.filter (fun f->f.IsStatic && FieldDefinition.matches f bindingFlags) 
            |> Seq.filter (fun f -> f.IsStatic && f.IsLiteral) 
            |> Seq.sortWith (fun f1 f2 -> compare (f1.Constant :?> int) (f2.Constant :?> int))
            |> Seq.map (fun tagfield -> (tagfield.Constant :?> int),tagfield.Name)
    let getUnionCaseTyp (typ: ITypeDefinition, tag: int, bindingFlags) = 
        let tagFields = getUnionTypeTagNameMap(typ,bindingFlags) |> Seq.toArray
        let tagField = tagFields |> Array.pick (fun (i,f) -> if i = tag then Some f else None)
        if tagFields.Length = 1 then 
            typ
        else
            // special case: two-cased DU annotated with CompilationRepresentation(UseNullAsTrueValue)
            // in this case it will be compiled as one class: return self type for non-nullary case and null for nullary
            let isTwoCasedDU =
                if tagFields.Length = 2 then
                    match tryFindCompilationRepresentationAttribute ( typ.CustomAttributes |> Seq.filter isCompilationRepresentationAttr |> Seq.toArray) with
                    | Some flags ->
                      (flags &&& CompilationRepresentationFlags.UseNullAsTrueValue) = CompilationRepresentationFlags.UseNullAsTrueValue
                    | _ -> false
                else
                    false
            if isTwoCasedDU then
                typ
            else
            let casesTyp = typ //getUnionCasesTyp (typ, bindingFlags)
            //let caseTyp =  //bindingFlags) // if this is null then the union is nullary
            match casesTyp.NestedTypes |> Seq.tryFind (fun ct->ct.Name=tagField) with 
            | None -> null
            | Some caseTyp when caseTyp.IsGenericInstance ->
                failwithf "! %s" caseTyp.Name 
                //caseTyp.MakeGenericType(casesTyp.GetGenericArguments())
            | Some caseTyp -> caseTyp
    let fieldsPropsOfUnionCase(typ: ITypeDefinition, tag:int, bindingFlags) =
        if isOptionType typ then 
            match tag with 
            | 0 (* None *) -> getInstancePropertyInfos (typ,[| |],bindingFlags) 
            | 1 (* Some *) -> getInstancePropertyInfos (typ,[| "Value" |] ,bindingFlags) 
            | _ -> failwith "fieldsPropsOfUnionCase"
        elif isListType typ then 
            match tag with 
            | 0 (* Nil *)  -> getInstancePropertyInfos (typ,[| |],bindingFlags) 
            | 1 (* Cons *) -> getInstancePropertyInfos (typ,[| "Head"; "Tail" |],bindingFlags) 
            | _ -> failwith "fieldsPropsOfUnionCase"
        else
            // Lookup the type holding the fields for the union case
            let caseTyp = getUnionCaseTyp (typ, tag, bindingFlags)
            let caseTyp = match caseTyp with null ->  typ | _ -> caseTyp
            caseTyp.Properties |> Seq.filter (fun p->p.HasThis && PropertyDefinition.matches p bindingFlags) 
            |> Seq.filter isFieldProperty
            |> Seq.filter (fun prop -> variantNumberOfMember prop = tag)
            |> Seq.sortWith (fun p1 p2 -> compare (sequenceNumberOfMember p1) (sequenceNumberOfMember p2))
            |> Seq.toArray
    let getUnionTagConverter (typ:ITypeDefinition,bindingFlags) = 
        if isOptionType typ then (fun tag -> match tag with 0 -> "None" | 1 -> "Some" | _ -> invalidArg "tag" "outOfRange")
        elif isListType typ then (fun tag -> match tag with  0 -> "Empty" | 1 -> "Cons" | _ -> invalidArg "tag" "outOfRange")
        else 
          let tagfieldmap = getUnionTypeTagNameMap (typ,bindingFlags) |> Map.ofSeq
          (fun tag -> tagfieldmap.[tag])                
[<Sealed>]
type UnionCaseInfo(typ: ITypeDefinition, tag:int) =
    // Cache the tag -> name map
    let mutable names = None
    //let getMethInfo() = Impl.getUnionCaseConstructorMethod (typ, tag, BindingFlags.Public ||| BindingFlags.NonPublic) 
    member __.Name = 
        match names with 
        | None -> (let conv = Impl.getUnionTagConverter (typ,BindingFlags.Public ||| BindingFlags.NonPublic) in names <- Some conv; conv tag)
        | Some conv -> conv tag
        
    member __.DeclaringType = typ

    member __.GetFields() = 
        let props = Impl.fieldsPropsOfUnionCase(typ,tag,BindingFlags.Public ||| BindingFlags.NonPublic) 
        props
    

    (* member __.GetCustomAttributes() = getMethInfo().GetCustomAttributes(false) 
    *)
    
    (* member __.GetCustomAttributes(attributeType) = getMethInfo().GetCustomAttributes(attributeType,false) 
    *)

    (* member __.GetCustomAttributesData() = getMethInfo().CustomAttributes |> Seq.toArray :> System.Collections.Generic.IList<_>
    *)
    member __.Tag = tag
    //override x.ToString() = typ.Name + "." + x.Name
    override x.GetHashCode() = typ.GetHashCode() + tag
    override __.Equals(obj:obj) = 
        match obj with 
        | :? UnionCaseInfo as uci -> uci.DeclaringType = typ && uci.Tag = tag
        | _ -> false
        
[<AbstractClass; Sealed>]
type FSharpType = 

    static member GetUnionCases (unionType:TypeDefinition,?bindingFlags) = 
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public
        //Impl.checkNonNull "unionType" unionType
        let unionType = Impl.getTypeOfReprType (unionType ,bindingFlags)
        //Impl.checkUnionType(unionType,bindingFlags);
        Impl.getUnionTypeTagNameMap(unionType,bindingFlags) |> Seq.mapi (fun i _ -> UnionCaseInfo(unionType,i)) |> Seq.toArray
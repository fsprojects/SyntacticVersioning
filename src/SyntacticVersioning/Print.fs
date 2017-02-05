namespace SyntacticVersioning

module Print=
  [<AutoOpen>]
  module internal Helpers =
    let whenStatic (flag:InstanceOrStatic) (typ:Typ) : string =
        match flag with
          | Static -> typ.FullName
          | Instance -> sprintf "(Instance/Inheritance of %s)" typ.FullName
    let parameters (attrs:Attribute list) (prms:Parameter list) : string =
        match prms with
        | [] -> "System.Void"
        | _  -> ""
                (*prms 
                |> 
                |> List.reduce(sprintf "%s -> %s")
                 *)
  [<CompiledName("MemberToString")>]
  let memberToString m : string=
    match m with
    | RecordConstructor (typ,attrs,prms)->
        sprintf "{ %s.%s } -> %s" 
          typ.FullName
          (parameters attrs prms)
          typ.FullName
    | Constructor (typ,attrs,prms)-> 
        sprintf "new %s : %s -> %s" 
          typ.FullName
          (parameters attrs prms)
          typ.FullName
    | Event (typ,isStatic,name,attrs,prms,rtyp) -> 
        sprintf "%s.%s : %s -> %s"
          (whenStatic isStatic typ)
          (sprintf "%s.Add" name)
          (parameters attrs prms)
          (rtyp.FullName)
    | Field (typ,isStatic,name,rtyp) -> 
        sprintf "%s.%s : %s"
          (whenStatic isStatic typ)
          name
          (rtyp.FullName)
    | Method (typ,isStatic,name,attrs,prms,rtyp) -> 
        sprintf "%s.%s : %s -> %s"
          (whenStatic isStatic typ)
          name
          (parameters attrs prms)
          (rtyp.FullName)
    | Property (typ,isStatic,name,ptyp) ->
        sprintf "%s.%s : %s"
          (whenStatic isStatic typ)
          name
          ptyp.FullName
    | UnionConstructors (typ,ctors)->""
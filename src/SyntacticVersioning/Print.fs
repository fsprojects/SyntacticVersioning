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
        | _  ->
          let ps =
            prms
            |> List.map(
              fun x -> sprintf "%s:%s" x.Name (x.Type.FullName))
          let ps' =
            match Seq.isEmpty attrs with
              | _ ->
                match List.isEmpty ps with
                  | true  -> [| |]
                  | false -> [| ps |> List.reduce(sprintf "%s * %s") |]
          
          ps'
          |> Array.reduce(sprintf "%s -> %s")

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
    | UnionConstructor (typ,(typ',attrs,prms))->
        sprintf "%s : %s -> %s"
          typ.FullName (parameters attrs prms) typ'.FullName

  let enumValues (t: EnumTyp) : (string * string) list =
        t.Values
        |> List.map(fun (name,value) -> sprintf "%s:%s" name value)
        |> List.sort
        |> List.reduce(fun x y -> sprintf "%s; %s" x y)
        |> fun s ->
          let t' = t.FullName
          [ t',(sprintf "%s values: [ %s ]" t' s) ]
    
  let enums (t: EnumTyp) : (string * string) list =
        t.Values
        |> List.map(
          fun (name,_) ->
            let t' = t.FullName
            t',(sprintf "%s.%s : %s" t' name t')
          )

  let unionValues (t:UnionCases)=
      t.Cases
      |> List.map(
        fun x ->
          let ps =
            match x.Fields with
              | [] -> ""
              | fields ->
                fields
                |> List.map(fun pi -> pi.FullName)
                |> List.fold(fun a x -> a + x) ""
          match System.String.IsNullOrEmpty ps with
            | true -> x.Name
            | false -> sprintf "%s of %s" x.Name ps
        )
      |> List.sort
      |> List.reduce(fun x y -> sprintf "%s | %s" x y)
      |> fun s ->
        let t' = t.Type.FullName
        [ t',(sprintf "%s values: [ %s ]" t' s) ] 
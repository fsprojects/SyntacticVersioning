namespace SyntacticVersioning
module Print=

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
                |> String.concat ""
          match System.String.IsNullOrEmpty ps with
            | true -> x.Name
            | false -> sprintf "%s of %s" x.Name ps
        )
      |> List.sort
      |> List.reduce(fun x y -> sprintf "%s | %s" x y)
      |> fun s ->
        let t' = t.Type.FullName
        [ t',(sprintf "%s values: [ %s ]" t' s) ] 
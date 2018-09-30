module SynVer.Compare
type private Ns = Namespace
type private T = SurfaceOfType
type private M = Member
let private diffSet source target =
  { 
    Added=Set.difference target source
    Removed=Set.difference source target
  }

[<CompiledName("Members")>]
let members (source:M list) (target: M list) =
  let sourceSet = source |> set
  let targetSet = target |> set
  diffSet sourceSet targetSet

[<CompiledName("Types")>]
let types (source:T list) (target: T list) =
  let setOfNames = List.map T.typ >> set
  let sourceSet = source |> setOfNames
  let targetSet = target |> setOfNames
  let maybeChanged = Set.intersect targetSet sourceSet
  let compareMembersForTyp t=
    let withTyp = List.find (T.typ >> (=) t)
    let sourceT = source |> withTyp
    let targetT = target |> withTyp
    members sourceT.Members targetT.Members
  let changed = maybeChanged 
              |> Seq.map (fun t->t, compareMembersForTyp t ) 
              |> Seq.filter (snd >> AddedAndRemoved.isEmpty >> not)
              |> Map.ofSeq
  {
      Diff=diffSet sourceSet targetSet
      Changes= changed
  }

/// Return differences as Package Canges
[<CompiledName("Packages")>]
let packages (source: Package) (target:Package)=
  let namespaces (source:Ns list) (target:Ns list) =
    let setOfNames = List.map Ns.name >> set
    let sourceSet = source |> setOfNames
    let targetSet = target |> setOfNames

    let maybeChanged = Set.intersect targetSet sourceSet
    let compareTypesForNsWithName t=
      let withName = List.find (Ns.name >> (=) t)
      let sourceT = source |> withName
      let targetT = target |> withName
      types sourceT.Types targetT.Types

    let changed = maybeChanged 
                |> Seq.map (fun t->t, compareTypesForNsWithName t ) 
                |> Seq.filter (snd >> Changes.isEmpty >> not)
                |> Map.ofSeq
    {
        Diff=diffSet sourceSet targetSet
        Changes=changed
    }

  namespaces source.Namespaces target.Namespaces

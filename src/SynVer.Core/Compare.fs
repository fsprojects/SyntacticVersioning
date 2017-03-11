module SynVer.Compare
open System.Reflection
open System
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
  let typ (t:T) = t.Type 
  let setOfNames = List.map typ >> set
  let sourceSet = source |> setOfNames
  let targetSet = target |> setOfNames
  let maybeChanged = Set.intersect targetSet sourceSet
  let compareMembersForTyp t=
    let withTyp = List.find (typ >> (=) t)
    let sourceT = source |> withTyp
    let targetT = target |> withTyp
    members sourceT.Members targetT.Members
  let changed = maybeChanged 
              |> Seq.map (fun t->t, compareMembersForTyp t ) 
              |> Seq.filter (fun (_,c)-> not c.IsEmpty)
              |> Map.ofSeq
  {
      Diff=diffSet sourceSet targetSet
      Changes= changed
  }

/// Return differences as Package Canges
[<CompiledName("Packages")>]
let packages (source: Package) (target:Package)=
  let namespaces (source:Ns list) (target:Ns list) =
    let nameOf (ns:Ns) = ns.Namespace 
    let setOfNames = List.map nameOf >> set
    let sourceSet = source |> setOfNames
    let targetSet = target |> setOfNames

    let maybeChanged = Set.intersect targetSet sourceSet
    let compareTypesForNsWithName t=
      let withName = List.find (nameOf >> (=) t)
      let sourceT = source |> withName
      let targetT = target |> withName
      types sourceT.Types targetT.Types

    let changed = maybeChanged 
                |> Seq.map (fun t->t, compareTypesForNsWithName t ) 
                |> Seq.filter (fun (_,c)-> not c.IsEmpty)
                |> Map.ofSeq
    {
        Diff=diffSet sourceSet targetSet
        Changes=changed
    }

  namespaces source.Namespaces target.Namespaces

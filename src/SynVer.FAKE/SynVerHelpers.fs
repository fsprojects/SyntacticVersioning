namespace SynVer.FAKE
open Fake
open Fake.DotNet.NuGet
open Fake.Core
open Fake.DotNet.NuGet.Version
open SynVer
[<AutoOpen>]
module SynVerHelpers=
  let bump (version:SemVerInfo) released modified =
      let (_,magnitude) = SurfaceArea.bump "0.0.0" released modified
      match magnitude with
      | Major -> IncMajor version
      | Minor -> IncMinor version
      | Patch -> IncPatch version

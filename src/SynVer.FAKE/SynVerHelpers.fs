namespace SynVer.FAKE
open Fake
open Fake.NuGetVersion
open Fake.SemVerHelper
open SynVer.Core
[<AutoOpen>]
module SynVerHelpers=
  let bump (version:SemVerInfo) released modified =
      let (_,magnitude) = SurfaceArea.bump "0.0.0" released modified
      match magnitude with
      | Major -> IncMajor version
      | Minor -> IncMinor version
      | Patch -> IncPatch version

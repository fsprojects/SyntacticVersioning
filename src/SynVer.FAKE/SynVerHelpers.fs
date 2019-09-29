namespace Fake.Tools

open Fake.Core
open SynVer

module SynVer =
  let bump (version: SemVerInfo) released modified =
      let (version, magnitude) = SurfaceArea.bump (string version) released modified
      SemVer.parse version

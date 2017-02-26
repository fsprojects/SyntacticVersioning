module SyntacticVersioning.Assemblies
open System.Reflection
open System
[<CompiledName("Bump")>]
let bump verNr released modified =
  SurfaceArea.bump verNr 
    (SurfaceArea.ofAssembly released) 
    (SurfaceArea.ofAssembly modified)   

[<CompiledName("Diff")>]
let diff released modified =
  SurfaceArea.diff  
    (SurfaceArea.ofAssembly released) 
    (SurfaceArea.ofAssembly modified)
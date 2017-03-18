[![Issue Stats](http://issuestats.com/github/fsprojects/SyntacticVersioning/badge/issue)](http://issuestats.com/github/fsprojects/SyntacticVersioning)
[![Issue Stats](http://issuestats.com/github/fsprojects/SyntacticVersioning/badge/pr)](http://issuestats.com/github/fsprojects/SyntacticVersioning)
[![NuGet Status](https://img.shields.io/nuget/v/SynVer.svg?style=flat)](https://www.nuget.org/packages/SynVer/)

# SyntacticVersioning

## Goal

Be able to get an idea about the semantic version changes based on the surface area changes of a .net assembly.

Developers should be able to use the tool to give a magnitude of any API changes. This is helpful when pushing NuGet-packages from a continuous integration server. Internally used NuGet packages of a .net developer organisation often focus on delivering on business goals; the versioning is easily forgotten. I.e. we want to be able to use this from [TeamCity](https://www.jetbrains.com/teamcity/) and others.

When upgrading a package you want to know the magnitude of the changes. In many cases the easiest way right now is to upgrade and look at the compilation errors. Having more information directly in NuGet helps.

## Integration

- You can use the SynVer.Lib found on NuGet [![NuGet Status](https://img.shields.io/nuget/v/SynVer.Lib.svg?style=flat)](https://www.nuget.org/packages/SynVer.Lib/)
- In order to integrate with FAKE you can use the NuGet [![NuGet Status](https://img.shields.io/nuget/v/SynVer.FAKE.svg?style=flat)](https://www.nuget.org/packages/SynVer.FAKE/)

## Requirements

SyntacticVersioning requires a local git installation. You can download git from [Git Downloads](https://git-scm.com/downloads).

## Build Status

Mono | .NET
---- | ----
[![Mono CI Build Status](https://img.shields.io/travis/fsprojects/SyntacticVersioning/master.svg)](https://travis-ci.org/fsprojects/SyntacticVersioning) | [![Build status](https://ci.appveyor.com/api/projects/status/tm885fiupthd22dp/branch/master?svg=true)](https://ci.appveyor.com/project/wallymathieu/syntacticversioning/branch/master)

## Maintainer(s)

- [@gentauro](https://github.com/gentauro)
- [@wallymathieu](https://github.com/wallymathieu)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)

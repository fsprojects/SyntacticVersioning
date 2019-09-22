#!/usr/bin/env bash
set -eu

mono ./packages/build/FSharp.Compiler.Tools/tools/fsc.exe $@

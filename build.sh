#!/usr/bin/env bash
set -eu

dotnet tool install fake-cli --tool-path .fake
dotnet tool install paket --tool-path .paket

PAKET_EXE=.paket/paket
FAKE_EXE=.fake/fake

$PAKET_EXE restore

[ ! -e build.fsx ] && run $PAKET_EXE update
$FAKE_EXE run build.fsx $@


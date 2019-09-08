#!/usr/bin/env bash
dotnet tool install fake-cli --tool-path .fake
dotnet tool install paket --tool-path .paket

set -eu

PAKET_EXE=.paket/paket
FAKE_EXE=.fake/fake

$PAKET_EXE restore


[ ! -e build.fsx ] && run $PAKET_EXE update

$FAKE_EXE run build.fsx $@


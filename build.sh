#!/bin/bash

set -e

if [ "$OS" = "Windows_NT" ]; then
    fake() { packages/build/FAKE/tools/FAKE.exe "$@" --fsiargs build.fsx; }
    paket() { .paket/paket.exe "$@"; }
else
    fake() { mono packages/build/FAKE/tools/FAKE.exe "$@" --fsiargs -d:MONO build.fsx; }
    paket() { mono .paket/paket.exe "$@"; }
fi

paket restore -g build

if [ "$BuildBranch" != "" ]; then
    fake ws-checkout
    export BuildFromRef=$(<.fake/buildFromRef)
fi

exec tools/build.sh "$@"

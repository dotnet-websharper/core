#!/bin/bash

set -e

if [ "$OS" = "Windows_NT" ]; then
    NO_MONO=true
fi

if [ "$WsUpdate" == "" ]; then
    if [ "$NO_MONO" != "" ]; then
        .paket/paket.exe restore
    else
        mono .paket/paket.exe restore
    fi
else
    if [ "$NO_MONO" != "" ]; then
        .paket/paket.exe update -g wsbuild
    else
        mono .paket/paket.exe update -g wsbuild
    fi
fi

export DOTNETSOLUTION="WebSharper.Compiler.sln;WebSharper.sln"
exec paket-files/wsbuild/github.com/dotnet-websharper/build-script/WebSharper.Fake.sh "$@"


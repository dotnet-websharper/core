#!/bin/bash

set -e

paket() {
    if [ "$OS" = "Windows_NT" ]; then
        .paket/paket.exe "$@"
    else
        mono .paket/paket.exe "$@"
    fi
}

if [ "$WsUpdate" != "" ]; then
    paket update -g wsbuild --no-install
fi

paket restore

export DOTNETSOLUTION="WebSharper.Compiler.sln;WebSharper.sln"
exec paket-files/wsbuild/github.com/dotnet-websharper/build-script/WebSharper.Fake.sh "$@"


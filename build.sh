#!/bin/bash

dotnet tool restore

dotnet paket update -g wsbuild --no-install

export DOTNETSOLUTION="WebSharper.Compiler.sln;WebSharper.sln"
. paket-files/wsbuild/github.com/dotnet-websharper/build-script/build.sh "$@"

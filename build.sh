#!/bin/bash

dotnet tool restore
dotnet paket update -g wsbuild --no-install

. paket-files/wsbuild/github.com/dotnet-websharper/build-script/update.sh

dotnet paket restore
dotnet restore WebSharper.Compiler.sln
dotnet restore WebSharper.sln

export DOTNETSOLUTION="WebSharper.Compiler.sln;WebSharper.sln"
. paket-files/wsbuild/github.com/dotnet-websharper/build-script/build.sh "$@"

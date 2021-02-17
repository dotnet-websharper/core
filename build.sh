#!/bin/bash

dotnet tool restore
dotnet paket restore
dotnet restore WebSharper.Compiler.sln
dotnet restore WebSharper.sln

. paket-files/wsbuild/github.com/dotnet-websharper/build-script/update.sh
. paket-files/wsbuild/github.com/dotnet-websharper/build-script/build.sh "$@"

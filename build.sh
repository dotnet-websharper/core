#!/bin/bash

dotnet restore

. paket-files/wsbuild/github.com/dotnet-websharper/build-script/update.sh
. paket-files/wsbuild/github.com/dotnet-websharper/build-script/build.sh "$@"

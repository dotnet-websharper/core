#!/bin/bash

dotnet tool restore

dotnet paket update -g wsbuild --no-install

export DOTNETSOLUTION="WebSharper.Compiler.sln;WebSharper.sln"

# Allow running `build SomeTask` instead of `build -t SomeTask`
if [ "$1" != "" -a "$1" != -t -a "$1" != --target ]; then EXTRA_ARG=-t; else EXTRA_ARG=; fi

dotnet fsi build.fsx $EXTRA_ARG "$@"

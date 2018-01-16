#!/bin/bash

set -e

if [ "$OS" = "Windows_NT" ]; then
    .paket/paket.bootstrapper.exe
    .paket/paket.exe restore -g build
else
    mono .paket/paket.bootstrapper.exe
    mono .paket/paket.exe restore -g build
fi

exec tools/WebSharper.Fake.sh "$@"

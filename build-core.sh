#!/bin/sh

set -e

case "$1" in
    "net4")
        frameworks=("net461")
        arg="-f net461"
        ;;
    "netcore")
        frameworks=("netcoreapp2.0")
        arg="-f netcoreapp2.0"
        ;;
    "")
        frameworks=("net461" "netcoreapp2.0")
        arg=""
        ;;
esac

pids=()

for i in "${frameworks[@]}"; do
    dotnet publish src/compiler/WebSharper.FSharp/WebSharper.FSharp.fsproj -f $i -v n &
    pids+=($!)
done

for pid in ${pids[*]}; do
    wait $pid
done

echo PUBLISH COMPILER DONE.

exec ./build-core-libs.sh

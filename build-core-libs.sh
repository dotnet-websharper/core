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

dotnet build src/stdlib/WebSharper.Main.Proxies/WebSharper.Main.Proxies.fsproj $arg -v n
dotnet build src/stdlib/WebSharper.Main/WebSharper.Main.fsproj $arg -v n

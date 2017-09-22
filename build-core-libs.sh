#!/bin/sh

set -e

dotnet build src/stdlib/WebSharper.Main.Proxies/WebSharper.Main.Proxies.fsproj -v n
dotnet build src/sitelets/WebSharper.Web/WebSharper.Sitelets.fsproj -v n

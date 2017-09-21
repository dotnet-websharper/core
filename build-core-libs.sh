#!/bin/sh

set -e

dotnet build src/stdlib/WebSharper.Main.Proxies/WebSharper.Main.Proxies.fsproj -v n
dotnet build src/sitelets/WebSharper.Web/WebSharper.Web.fsproj -v n -f net461
dotnet build src/stdlib/WebSharper.Testing/WebSharper.Testing.fsproj -v n

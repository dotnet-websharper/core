@echo off
setlocal

dotnet tool restore

call paket-files\wsbuild\github.com\dotnet-websharper\build-script\update.cmd

dotnet paket restore
dotnet restore WebSharper.Compiler.sln
dotnet restore WebSharper.sln

set DOTNETSOLUTION="WebSharper.Compiler.sln;WebSharper.sln"
call paket-files\wsbuild\github.com\dotnet-websharper\build-script\build.cmd %*

@echo off
setlocal

dotnet tool restore

dotnet paket update -g wsbuild --no-install
if errorlevel 1 exit /b %errorlevel%

set DOTNETSOLUTION="WebSharper.Compiler.sln;WebSharper.sln"
dotnet fsi ./build.fsx -t %*
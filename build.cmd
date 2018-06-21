@echo off
setlocal

if "%WsNoUpdate%"=="" (
  .paket\paket.exe update -g wsbuild
  if errorlevel 1 exit /b %errorlevel%
) else (
  .paket\paket.exe restore -g wsbuild
  if errorlevel 1 exit /b %errorlevel%
)

set DOTNETSOLUTION="WebSharper.Compiler.sln;WebSharper.sln"
call paket-files\wsbuild\github.com\dotnet-websharper\build-script\WebSharper.Fake.cmd %*

@echo off
setlocal

if not "%WsUpdate%"=="" (
  .paket\paket.exe update -g wsbuild --no-install
  if errorlevel 1 exit /b %errorlevel%
)

.paket\paket.exe restore
if errorlevel 1 exit /b %errorlevel%

set DOTNETSOLUTION="WebSharper.Compiler.sln;WebSharper.sln"
call paket-files\wsbuild\github.com\dotnet-websharper\build-script\WebSharper.Fake.cmd %*

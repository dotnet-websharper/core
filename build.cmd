@echo off
setlocal

dotnet restore
if errorlevel 1 exit /b %errorlevel%

call paket-files\wsbuild\github.com\dotnet-websharper\build-script\update.cmd
call paket-files\wsbuild\github.com\dotnet-websharper\build-script\build.cmd %*

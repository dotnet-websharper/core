@echo off
setlocal

.paket\paket.bootstrapper.exe
if errorlevel 1 exit /b %errorlevel%

.paket\paket.exe restore -g build
if errorlevel 1 exit /b %errorlevel%

call tools\WebSharper.Fake.cmd %*

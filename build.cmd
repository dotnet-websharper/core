@echo off
setlocal
set PATH=%GitToolPath%;%PATH%

cls

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore --touch-affected-refs
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*

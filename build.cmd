@echo off
setlocal
set PATH=%GitToolPath%;%PATH%

cls

.paket\paket.exe restore --touch-affected-refs
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\FAKE\tools\FAKE.exe build.fsx %*

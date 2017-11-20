@echo off
setlocal
set PATH=%GitToolPath%;%PATH%

cls

if not "%BuildBranch%"=="" (
  packages\build\FAKE\tools\FAKE.exe build.fsx ws-checkout
  if errorlevel 1 (
    exit /b %errorlevel%
  )

  set /p BuildFromRef=<.fake\buildFromRef
)

.paket\paket.exe restore --touch-affected-refs
if errorlevel 1 (
  exit /b %errorlevel%
)

if "%VisualStudioVersion%"=="" (
  set VisualStudioVersion=15.0
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*

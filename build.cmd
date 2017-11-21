@echo off
setlocal
set PATH=%GitToolPath%;%PATH%

cls

.paket\paket.exe restore -g build
if errorlevel 1 exit /b %errorlevel%

if not "%BuildBranch%"=="" (
  packages\build\FAKE\tools\FAKE.exe build.fsx ws-checkout
  if errorlevel 1 exit /b %errorlevel%

  set /p BuildFromRef=<.fake\buildFromRef
  tools\build.cmd %*
) else (
  tools\build.cmd %*
)


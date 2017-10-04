@echo off
setlocal
set PATH=%GitToolPath%;%PATH%

cls

dotnet restore WebSharper.sln
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*

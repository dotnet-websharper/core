@echo off
setlocal

dotnet restore WebSharper.sln
if errorlevel 1 exit /b %errorlevel%

if "%VisualStudioVersion%"=="" (
  set VisualStudioVersion=15.0
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*

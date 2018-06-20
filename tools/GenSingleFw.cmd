@echo off

cd %~dp0\..
if not exist packages (
  dotnet restore WebSharper.sln
)
.\packages\build\FAKE\tools\FAKE.exe tools\GenSingleFw.fsx

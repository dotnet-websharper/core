@echo off
setlocal

dotnet restore WebSharper.sln
if errorlevel 1 exit /b %errorlevel%

call tools\WebSharper.Fake.cmd %*

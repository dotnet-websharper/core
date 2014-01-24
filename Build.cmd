@ECHO OFF
REM NOTE: This file was auto-generated with `IB.exe prepare` from `IntelliFactory.Build`.

setlocal
set PATH=%PATH%;%ProgramFiles(x86)%\Microsoft SDKs\F#\3.1\Framework\v4.0
set PATH=%PATH%;%ProgramFiles(x86)%\Microsoft SDKs\F#\3.0\Framework\v4.0
set PATH=%PATH%;%ProgramFiles%\Microsoft SDKs\F#\3.1\Framework\v4.0
set PATH=%PATH%;%ProgramFiles%\Microsoft SDKs\F#\3.0\Framework\v4.0
set PATH=%PATH%;tools\NuGet
nuget install IntelliFactory.Build -pre -ExcludeVersion -o tools\packages
fsi.exe --exec build.fsx %*
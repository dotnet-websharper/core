@ECHO OFF
REM NOTE: This file was auto-generated with `IB.exe prepare` from `IntelliFactory.Build`.
IF NOT "%NuGetHome%"=="" GOTO :nuget
SET NuGetHome=tools\NuGet
:nuget
"%NuGetHome%\NuGet.exe" install IntelliFactory.Build -pre -ExcludeVersion -o tools\packages
IF NOT "%FSharpHome%"=="" GOTO :fs
SET PF=%ProgramFiles(x86)%
IF NOT "%PF%"=="" GOTO w64
SET PF=%ProgramFiles%
:w64
SET FSharpHome=%PF%\Microsoft SDKs\F#\3.0\Framework\v4.0
:fs
"%FSharpHome%\fsi.exe" --exec build.fsx %*
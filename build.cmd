@ECHO OFF
setlocal
set PATH=%PATH%;%ProgramFiles(x86)%\Microsoft SDKs\F#\3.1\Framework\v4.0
set PATH=%PATH%;%ProgramFiles%\Microsoft SDKs\F#\3.1\Framework\v4.0
set PATH=%PATH%;%WINDIR%\Microsoft.NET\Framework\v4.0.30319
fsi.exe --exec tools/configure.fsx
fsi.exe --exec tools/minify.fsx
MSBuild.exe msbuild/WebSharper.proj


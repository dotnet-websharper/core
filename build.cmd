@ECHO OFF
setlocal
set PATH=%PATH%;%ProgramFiles(x86)%\MSBuild\14.0\Bin;%ProgramFiles(x86)%\MSBuild\12.0\Bin;%WINDIR%\Microsoft.NET\Framework\v4.0.30319
MSBuild.exe msbuild\WebSharper.proj /verbosity:minimal /p:Arguments="%*"

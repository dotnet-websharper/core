@ECHO OFF
setlocal
set PATH=%PATH%;%WINDIR%\Microsoft.NET\Framework\v4.0.30319
MSBuild.exe msbuild\WebSharper.proj /verbosity:minimal /p:Arguments="%*"

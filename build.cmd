@ECHO OFF
setlocal
set PATH=%ProgramFiles(x86)%\MSBuild\14.0\Bin;%WINDIR%\Microsoft.NET\Framework\v4.0.30319;%PATH%
rd /s /q build
MSBuild.exe msbuild\WebSharper.proj /verbosity:minimal /p:Arguments="%*" /fileLogger /flp:PerformanceSummary /m

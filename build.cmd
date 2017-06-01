@ECHO OFF
setlocal

if "%VisualStudioVersion%"=="" (
  if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\" (
    set VisualStudioVersion=15.0
  ) else (
    set VisualStudioVersion=14.0
  )
)

set PATH=%ProgramFiles(x86)%\MSBuild\14.0\Bin;%WINDIR%\Microsoft.NET\Framework\v4.0.30319;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\MSBuild\15.0\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\MSBuild\15.0\Bin;%PATH%

echo Building with Visual Studio %VisualStudioVersion% tools
MSBuild.exe msbuild\WebSharper.proj /verbosity:minimal /p:VisualStudioVersion=%VisualStudioVersion% /p:Arguments="%*" /fileLogger /flp:PerformanceSummary /m

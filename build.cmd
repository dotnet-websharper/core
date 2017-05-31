@ECHO OFF
setlocal

if "%VisualStudioVersion%"=="" (
  if exist "%ProgramFiles(x86)%\Microsoft\VisualStudio\v15.0\FSharp\Microsoft.FSharp.Targets" (
    set VisualStudioVersion=15.0
  ) else (
    set VisualStudioVersion=14.0
  )
)

set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\MSBuild\%VisualStudioVersion%\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\MSBuild\%VisualStudioVersion%\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\MSBuild\%VisualStudioVersion%\Bin;%PATH%
MSBuild.exe msbuild\WebSharper.proj /verbosity:minimal /p:VisualStudioVersion=%VisualStudioVersion% /p:Arguments="%*" /fileLogger /flp:PerformanceSummary /m

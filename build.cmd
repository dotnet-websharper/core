@ECHO OFF
setlocal
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\MSBuild\15.0\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\MSBuild\15.0\Bin;%PATH%
MSBuild.exe msbuild\WebSharper.proj /verbosity:minimal /p:VisualStudioVersion=15.0 /p:Arguments="%*" /fileLogger /flp:PerformanceSummary /m

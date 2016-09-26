#! /bin/sh

rm -rf build
xbuild msbuild/WebSharper.proj "/verbosity:minimal" "/p:MonoPrefix=mono" "/p:Arguments=$*" "/flp:PerformanceSummary"

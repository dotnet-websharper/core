#! /bin/sh

if which xbuild > /dev/null; then
    xbuild msbuild/WebSharper.proj
else
    echo "Failed to find xbuild in the system path."
    exit 1
fi

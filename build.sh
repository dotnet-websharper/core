#!/bin/bash

dotnet restore WebSharper.sln
exit_code=$?
if [ $exit_code -ne 0 ]; then
    exit $exit_code
fi

if test "$OS" = "Windows_NT"; then
  # use .Net
  packages/build/FAKE/tools/FAKE.exe $@ --fsiargs build.fsx
else
  # use mono
  mono packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx
fi

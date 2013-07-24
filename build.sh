#!/bin/bash
export EnableNuGetPackageRestore=true
: ${MonoHome=/usr/lib/mono}
: ${FSharpHome=$MonoHome/4.0}
: ${NuGetHome=tools/NuGet}
export FSharpHome
export MonoHome
export NuGetHome
mono $NuGetHome/NuGet.exe install IntelliFactory.Build -pre -ExcludeVersion -o tools/packages
mono $FSharpHome/fsi.exe --exec build.fsx %*
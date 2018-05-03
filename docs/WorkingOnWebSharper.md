# Working on this repository

This repository uses the [Paket](https://fsprojects.github.io/paket) package manager and [FAKE](https://fake.build) build system. To build the project, simply run the build script:

```
# On Windows:
.\build.cmd

# On Unix:
./build.sh
```

## Single-target projects

This project uses multi-targeting projects to compile libraries for both .NET Framework 4.6.1 and .NET Standard 2.0, and executables for both .NET Framework 4.6.1 and .NET Core 2.0. However, tooling can still be flimsy around multi-targeting projects, in particular Visual Studio. To circumvent this and make it easier to work on WebSharper, there is a script that duplicates the projects into single-target versions.

```
# On Windows:
tools\GenSingleFw.cmd

# On Unix:
tools/GenSingleFw.sh
```

This creates two folders, `netcore` and `netfx`, which each contains duplicates of the solutions and projects in this repository. These projects point back to the original F# / C# source files, so you can just work on them in Visual Studio. You should first compile the compiler itself:

```
dotnet build netcore/WebSharper.Compiler.sln
# Or:
dotnet build netfx/WebSharper.Compiler.sln
```

And you can then open `netcore/WebSharper.sln` or `netfx/WebSharper.sln` in Visual Studio.

If you need to make a change to a project file itself, you should edit the original project file and re-run `GenSingleFw` to apply the change to the `net*` subfolders.

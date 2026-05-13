# WebSharper Core Agent Instructions

First, always check if serena MCP server is running for any coding requests or questions/investigation about code. If not, STOP and ask the user to start it. The other three MCP servers are also recommended, ask the user to run them when they would be helpful: microsoft-learn for .NET documentation, nuget for available packages, github for accessing tickets and repo history.

## Repository Map

- `core` contains the WebSharper compiler, standard library (.NET and FSharp.Core proxies - stand-in types to use in JavaScript translation), web app libraries, unit test suite, test web projects, Interface Generator - an DSL for defining JavaScript bindings to use libraries from .NET, a metadata explorer tool. There are not working/WIP functionalities too: a C# analyzer for showing WebSharper translation errors/warnings in Visual Studio, a .d.ts to Interface Generator bridge. Also the TypeScript and .d.ts outputs are unfinished/experimental.

- `ui` is a reactive UI library, the main way to construct HTML on both server and client side in WebSharper.

- `templates` contains all the `dotnet new` templates.

- `docs` has latest documentation on the `fumadocs` branch.

- `build-script` has GitHub actions to build the whole WebSharper stack, test templates, and release to nuget.org on `master` branch. It also has common build script helpers on the `websharper80` branch.

## Dependencies

WebSharper uses F#-based community tools: Paket for Nuget dependency management, and Fake for build script. It also supports the Giraffe web library.

## Working Guidelines

- If any relevant repository is not part of current workspace, stop and ask the user to add it.
- Prefer existing build scripts and repository conventions over ad hoc commands.
- Keep changes scoped to the requested behavior.
- When changing compiler behavior or proxies, add or update focused tests near the affected area.
- Do not edit generated files or sections of files directly. These are `WebSharper.Compiler.CSharp/RoslynHelpers.g.fs` (fully generated) and `WebSharper.Core/AST.fs` (partially generated). For the latter, edit `getAST.fsx` and run it to make modifications.
- When changing AST or other parts of WebSharper metadata, bump the minor version in `build.fsx` and metadata flag in `WebSharper.Core/Metadata.fs` to match it.
- For an overview of the whole compilation pipeline read  `content\metaprogramming\compilation-pipeline.md` in the `docs` repo.
- Keep track of unreleased changes in Release Note files in `docs/release-notes` folder in the `core` repo.

## Skills

Each skill description should eventually include prerequisites, exact commands, expected outputs, and common failure modes.

### Build And Run The Full Test Suite

Run `./build CI-Release *> build-output.txt` in PowerShell to build and test everything, up to outputting Nuget packages to the default output folder. Look at `build-output.txt` for the sections under `Starting target 'RunMainTestsRelease'`, `Starting target 'RunCompilerTestsRelease'`, and the final build time report. Some tests print values to console, this is expected.

Expected passing output includes:

- `All tests passed: N/N` during `RunMainTestsRelease`, where both numbers match.
- `Passed!  - Failed:     0` for `WebSharper.Core.JavaScript.Tests.dll`.
- `Passed!  - Failed:     0` for `WebSharper.CSharp.Analyzer.Tests.dll`.
- `Finished (Success) 'RunMainTestsRelease'`, `Finished (Success) 'RunCompilerTestsRelease'`, `Finished (Success) 'WS-Package'`, and `Finished (Success) 'CI-Release'`.
- The final build time report ends with `Status:                   Ok`.

Warnings are currently present in a passing run, including NuGet pruning/duplicate package warnings, .NET target framework support warnings, F# deprecation warnings, and expected WebSharper test warnings.

### Build NuGet Packages For Local Repo Consumption

- Set env var `WSPackageFolder=../localnuget` for the session.
- Set env var `BUILD_NUMBER=9999`
- Run `./build CI-Release` on `core`
- For all packages in `localnuget`, purge global nuget cache `rm -rf ~/.nuget/packages/PACKAGENAME`
- Run `dotnet paket update --force` on `ui`
- Run `./build CI-Release` on `ui`
- Same, purge again, update `templates` or the next repo, build that, and so on

### Spot-test compiler changes with snippets

The `core/tests/Scripts/FSharpTester.fsx` script contains helpers to run the F# compiler on small snippets, and `FSharpTester.fsx` does the same for C#. 

Put runnable F# snippets in `core/tests/Scripts/fsharp`. Each snippet script should `#load "../FSharpTester.fsx"` and call `FSharpTester.toJSFiles source` or `FSharpTester.toJSBundle source`. Run `dotnet fsi tests/WebSharper.Tests/Scripts/fsharp/add1.fsx` from `core` for the basic smoke test.

The C# tester follows the same patterns, run `dotnet fsi tests/WebSharper.Tests/Scripts/csharp/add1.fsx` from `core` for the C# smoke test.

These tester scripts will log type names defined, and the AST in various stages. This can be used for debugging exactly where does a transformation happens, see the scripts for the steps they call from the compiler and what they log.

If a fix is found for a bug, move it as proper code to `core/tests/WebSharper.Tests/Regression.fs` and make a unit test out of it for F#, or `core/tests/WebSharper.CSharp.Tests/Regression.cs` for C#.
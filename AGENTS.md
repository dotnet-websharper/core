# WebSharper Core Agent Instructions

## MCP Setup

- Serena is required for semantic search and code diagnostics during code work. This repo is initialized with `.serena/project.yml` using `fsharp`, `csharp`, and `typescript`.
- If Serena is not installed, prompt the user to run `uv tool install -p 3.13 serena-agent@latest --prerelease=allow`.
- For VS Code and GitHub Copilot, `.vscode/mcp.json` should contain a `serena` stdio server with command `serena` and args `["start-mcp-server"]`, alongside the `microsoft-learn`, `nuget`, and `github` servers.
- For Codex, run `serena setup codex` or ensure `%USERPROFILE%\.codex\config.toml` contains:

```toml
[mcp_servers.serena]
command = "serena"
args = ["start-mcp-server"]
enabled = true
```

- MCP clients usually load server config only at session startup. If `serena.exe start-mcp-server` is running but the client exposes no Serena tools/resources, restart the Codex/VS Code session after fixing MCP config.
- If `.serena/project.yml` is missing, recreate it from the repo root with `serena project create . --name core --language fsharp --language csharp --language typescript --index`.
- Use `serena project health-check .` when semantic tools behave oddly, and `serena project index .` when the symbol cache is stale. On Windows consoles using cp1252, Serena can finish successfully and then fail printing Unicode status symbols; rerun as `$env:PYTHONIOENCODING='utf-8'; serena project health-check .` if that happens. Serena diagnostics do not replace proper `dotnet build`: WebSharper translation errors, including non-JavaScript-compatible method calls, are build-time errors.
- Other three MCP servers are also recommended, ask the user to run them when they would be helpful: microsoft-learn for .NET documentation, nuget for available packages, github for accessing tickets and repo history.

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
- A proxy is a re-implementation of a .NET type, optimized for WebSharper translation. First look it up in Microsoft Learn documentation or dotnet/runtime or dotnet/fsharp code from GitHub. Keep as much of the original behavior as possible when implementing proxies, ask questions if unclear. However, WebSharper comes with limitations because it is targeting the JavaScript runtime: e.g. there is no synchronous waiting or background threads, no System.Reflection support, and many commonly used types translate to common JavaScript equivalents with some potential tweaks like added custom fields.
- The `core/src/compiler/WebSharper.Core.JavaScript/Runtime.js` file is the main set of helpers used by WebSharper-generated code. It contains modern JS syntax not yet supported by the full WebSharper pipeline. The `Lazy` helper is especially important as it can delay executing a class definition, while keeping the shape of the object fully intact via using a JS `Proxy` (note: this is different to what WebSharper uses the term "proxy" for).
- The `WebSharper.StdLib.Proxies` project does not actually produce a dll. Its job is to store all the files that will go to the WebSharper-translation of `WebSharper.StdLib`, so that all the .NET types do not bloat the dll. To build it, build `WebSharper.StdLib.Proxies` first, then `WebSharper.StdLib`, the latter will expose all the warnings/errors for the proxies.
- Do not edit generated files or sections of files directly. These are `WebSharper.Compiler.CSharp/RoslynHelpers.g.fs` (fully generated) and `WebSharper.Core/AST.fs` (partially generated). For the latter, edit `getAST.fsx` and run it to make modifications.
- When changing AST or other parts of WebSharper metadata, bump the minor version in `build.fsx` and metadata flag in `WebSharper.Core/Metadata.fs` to match it.
- For an overview of the whole compilation pipeline read  `content/metaprogramming/compilation-pipeline.md` in the `docs` repo.
- Keep track of unreleased changes in Release Note files in `docs/release-notes` folder in the `core` repo. If latest version is already released publiclz, create a new RN with bumping revision when adding new features or bug fixes, and bumping minor when making metadata changes. See latest public release at https://github.com/dotnet-websharper/core/releases.

## Skills

Each skill description should eventually include prerequisites, exact commands, expected outputs, and common failure modes.

### Build for debugging

`build.cmd`/`build.sh` run `dotnet fsi build.fsx` and add `-t` automatically when the first argument is a target name. `build.fsx` defaults to Release unless `--debug` is passed after `--`.

Useful targets:

- `.\build.cmd Build` builds `WebSharper.Compiler.sln` in Release.
- `.\build.cmd Build -- --debug` builds `WebSharper.Compiler.sln` in Debug.
- `.\build.cmd WS-BuildRelease` runs the main Release build action: compiler solution, published compiler tools, `WebSharper.sln`, then stops the compiler service.
- `.\build.cmd WS-BuildDebug` runs the same build action in Debug.
- `.\build.cmd CI-Release` restores, builds Release, runs compiler tests, runs browser/QUnit tests, and packages.

When changing compiler behavior, rebuild the compiler before snippet tests or downstream test projects. The snippet testers currently use Release builds, so prefer `.\build.cmd Build` for a fast compiler rebuild, then use a narrower project build or snippet run. Use `.\build.cmd WS-BuildRelease` when the web test projects or libraries need regenerated Release outputs from the changed compiler.

When changing `WebSharper.StdLib.Proxies`, first build `WebSharper.StdLib.Proxies`, then build `WebSharper.StdLib`; the latter reports the proxy translation warnings/errors. `WS-BuildRelease` does this as part of the full solution build, but direct project builds are faster for a tight proxy loop.

If a debug or test build fails during clean with `Access to the path 'WebSharper.MSBuild.FSharp.dll' is denied`, check for language-server or build-server processes holding the compiler task DLL. First run `dotnet build-server shutdown`. If the lock persists, identify the holder with:

```powershell
$target='WebSharper.MSBuild.FSharp.dll'; Get-Process | ForEach-Object { $p=$_; try { foreach($m in $p.Modules){ if($m.ModuleName -eq $target){ [pscustomobject]@{Id=$p.Id; ProcessName=$p.ProcessName; Path=$p.Path; Module=$m.FileName} } } } catch {} }
```

Serena can leave its own F# language server running after MCP windows are closed; in one observed case this was `C:\Users\andra\.serena\language_servers\static\FSharpLanguageServer\fsharp-lsp\fsautocomplete.exe`. Stop only the process reported by the module scan, for example `Stop-Process -Id <pid>`, then rerun the build. Restart Serena only when semantic code analysis is needed again.

### Build And Run The Full Test Suite

Run `./build CI-Release *> build-output.txt` in PowerShell to build and test everything, up to outputting Nuget packages to the default output folder. Full recompilations are expected to last around 10 minutes, do not set a short timeout. Look at `build-output.txt` for the sections under `Starting target 'RunMainTestsRelease'`, `Starting target 'RunCompilerTestsRelease'`, and the final build time report. Some tests print values to console, this is expected.

Expected passing output includes:

- `All tests passed: N/N` during `RunMainTestsRelease`, where both numbers match.
- `Passed!  - Failed:     0` for `WebSharper.Core.JavaScript.Tests.dll`.
- `Passed!  - Failed:     0` for `WebSharper.CSharp.Analyzer.Tests.dll`.
- `Finished (Success) 'RunMainTestsRelease'`, `Finished (Success) 'RunCompilerTestsRelease'`, `Finished (Success) 'WS-Package'`, and `Finished (Success) 'CI-Release'`.
- The final build time report ends with `Status:                   Ok`.

Warnings are currently present in a passing run, including NuGet pruning/duplicate package warnings, .NET target framework support warnings, F# deprecation warnings, and expected WebSharper test warnings.

### Run QUnit Puppeteer Tests Without Rebuilding

`RunMainTestsRelease` depends on `WS-BuildRelease`, so running it through FAKE rebuilds first. Use `RunMainTestsNoBuildRelease` when Release outputs are already current and you only want the browser test pass:

```powershell
.\build.cmd RunMainTestsNoBuildRelease
```

This target starts `build/Release/Tests/net10.0/Web.dll` from `tests/Web`, waits for `Application started.`, runs `npm install`, then executes `node runtests.js`. `runtests.js` drives `node-qunit-puppeteer` against `https://localhost:44336/tests` with a 120 second timeout.

If the no-build target says `Release Web test project not found`, build the web tests first. For compiler or proxy changes, prefer:

```powershell
.\build.cmd WS-BuildRelease
```

For a faster loop after the compiler and libraries are already current, rebuild only the web test app:

```powershell
dotnet build tests/Web/Web.csproj -c Release --no-restore
.\build.cmd RunMainTestsNoBuildRelease
```

Manual equivalent for debugging the browser runner:

```powershell
Push-Location tests/Web
dotnet exec ..\..\build\Release\Tests\net10.0\Web.dll --server.urls https://localhost:44336
Pop-Location
```

Then from repo root in another terminal:

```powershell
npm install
node runtests.js
```

If port `44336` is already in use, stop the old `dotnet exec ... Web.dll` process before retrying.

### Build NuGet Packages For Local Repo Consumption

- Set env var `WSPackageFolder=../localnuget` for the session.
- Set env var `BUILD_NUMBER=9999`
- Run `./build CI-Release` on `core`
- For all packages in `localnuget`, purge global nuget cache `rm -rf ~/.nuget/packages/PACKAGENAME`
- Run `dotnet paket update --force` on `ui`
- Run `./build CI-Release` on `ui`
- Same, purge again, update `templates` or the next repo, build that, and so on

### Spot-test compiler changes with snippets

The `core/tests/Scripts/FSharpTester.fsx` script contains helpers to run the F# compiler on small snippets, and `core/tests/Scripts/CSharpTester.fsx` does the same for C#. These scripts use Release builds of the compiler now.

Put runnable F# snippets in `core/tests/Scripts/fsharp`. Each snippet script should `#load "../FSharpTester.fsx"` and call `FSharpTester.toJSFiles source` or `FSharpTester.toJSBundle source`. Run `dotnet fsi tests/Scripts/fsharp/add1.fsx` from `core` for the basic smoke test.

Current caveat: with .NET SDK `10.0.300`, the F# snippet smoke test can fail before WebSharper translation with `FS0193` because `dotnet fsi` hosts `FSharp.Compiler.Service` `43.11.300` while this repo pins `FSharp.Compiler.Service` `43.12.202`. Treat that as a tester-host mismatch, not a WebSharper translation failure; use a matching SDK/FSI host or fall back to project builds until the F# snippet runner is moved to a compiled host. The C# snippet smoke test is not affected.

The C# tester follows the same patterns, run `dotnet fsi tests/Scripts/csharp/add1.fsx` from `core` for the C# smoke test.

These tester scripts will log type names defined, and the AST in various stages. This can be used for debugging exactly where does a transformation happens, see the scripts for the steps they call from the compiler and what they log.

If a fix is found for a bug, move it as proper code to `core/tests/WebSharper.Tests/Regression.fs` and make a unit test out of it for F#, or `core/tests/WebSharper.CSharp.Tests/Regression.cs` for C#.

### Fast Compiler/Proxy Iteration Loop

1. Use Serena semantic search and diagnostics to find the relevant compiler/proxy code without recompiling.
2. For compiler changes, rebuild only the compiler first with `.\build.cmd Build`; use `.\build.cmd Build -- --debug` only when a Debug compiler is needed.
3. For proxy changes, directly build the proxy/library projects when possible, or use `.\build.cmd WS-BuildRelease` when generated Release outputs must be refreshed.
4. Spot-check translation with `dotnet fsi tests/Scripts/fsharp/<snippet>.fsx` or `dotnet fsi tests/Scripts/csharp/<snippet>.fsx`.
5. Add focused tests near the affected suite. Bug-fix/regression tests go in `tests/WebSharper.Tests/Regression.fs` or `tests/WebSharper.CSharp.Tests/Regression.cs`; other tests should go in dedicated files such as `Task.fs`/`Task.cs`, or in generic feature files such as `Basis.fs`/`Syntax.cs` when that matches the surrounding context. Examine nearby tests before choosing.
6. Rebuild the affected test project or `tests/Web/Web.csproj` in Release, then run `.\build.cmd RunMainTestsNoBuildRelease` for QUnit/Puppeteer without another compiler rebuild.
7. Before finishing, run the appropriate broader verification: `.\build.cmd RunCompilerTestsRelease` for compiler test coverage, `.\build.cmd RunMainTestsRelease` when a rebuild plus browser tests is needed, or `.\build.cmd CI-Release` for the full final pass.

# Suggested commands

Windows/PowerShell is the active environment.

Repo inspection:
- `rg "pattern" path` for fast text search.
- `rg --files path` for fast file listing.
- `Get-Content path` to read a file.
- `Get-ChildItem path` to list directories.
- `git status --short` before and after edits.

Build and test:
- Full CI build/test/package: `./build CI-Release *> build-output.txt`
- Check full build output around `RunMainTestsRelease`, `RunCompilerTestsRelease`, and final build time report.
- Basic F# compiler snippet smoke test: `dotnet fsi tests/WebSharper.Tests/Scripts/fsharp/add1.fsx`
- Basic C# compiler snippet smoke test: `dotnet fsi tests/WebSharper.Tests/Scripts/csharp/add1.fsx`

Proxy workflow:
- `WebSharper.StdLib.Proxies` stores proxy source files and does not produce the final stdlib dll.
- Build `WebSharper.StdLib.Proxies` first, then `WebSharper.StdLib`; the latter exposes proxy warnings/errors.

Local package consumption:
- Set `WSPackageFolder=../localnuget` and `BUILD_NUMBER=9999`, then run `./build CI-Release` in `core`.
- Purge corresponding global NuGet caches before updating/building downstream repos.
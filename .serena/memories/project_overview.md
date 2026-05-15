# WebSharper Core overview

This repository contains WebSharper core: the compiler, standard library and FSharp.Core/.NET proxies used for JavaScript translation, web app libraries, unit tests, test web projects, Interface Generator, metadata explorer tooling, and some WIP analyzer/TypeScript-related pieces.

Primary stack: F#, C#, JavaScript runtime helpers, .NET SDK, Paket for NuGet dependency management, FAKE build scripts. Main solutions include `WebSharper.sln`, `WebSharper.Compiler.sln`, `WebSharper.NoTests.sln`, and `WebSharper.Tools.sln`.

Important directories: `src` for compiler/runtime/stdlib code, `tests` for test suites and snippet testers, `docs/release-notes` for unreleased changes, `tools` for supporting tools, `.paket`/`paket.dependencies`/`paket.lock` for dependency management.

For coding work, use Serena semantic tools first. Keep changes scoped and follow existing conventions. Do not edit generated files directly: `WebSharper.Compiler.CSharp/RoslynHelpers.g.fs` is fully generated and `WebSharper.Core/AST.fs` is partially generated from `getAST.fsx`.
# Style and conventions

Follow the local F# and C# style already used in the edited files. Prefer existing helper APIs and established proxy patterns over new abstractions.

When changing compiler behavior or proxies, add or update focused tests near the affected behavior. For proxies, first consult Microsoft Learn or dotnet/runtime/dotnet/fsharp source and preserve .NET behavior as far as WebSharper's JavaScript runtime model allows. Known limitations include no synchronous waiting/background threads and no System.Reflection support.

Runtime helper code is in `src/compiler/WebSharper.Core.JavaScript/Runtime.js`; it may use modern JavaScript syntax not supported by the full WebSharper pipeline. The runtime `Lazy` helper uses JavaScript `Proxy`, unrelated to WebSharper .NET proxies.

Generated files must not be edited directly. For AST changes, edit `getAST.fsx` and regenerate. Metadata/AST changes require minor version and metadata flag updates.

Keep unreleased user-visible changes recorded in `docs/release-notes` when appropriate.
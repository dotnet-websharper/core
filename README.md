
# WebSharper

WebSharper is a web programming platform including compilers from F# and C# code to JavaScript. WebSharper is open source under the Apache 2.0 license and is developed by IntelliFactory, with contributions from the community.

For building or contributing to this repository see [CONTRIBUTING.md](CONTRIBUTING.md).

## Links

- Legacy website: https://websharper.com/
- Upcoming website: https://websharper.io/
- Current documentation: https://docs.websharper.com/
- Live try examples for WebSharper 4x: https://try.websharper.com
- Contributing guide: [CONTRIBUTING.md](CONTRIBUTING.md)
- License: [LICENSE.md](LICENSE.md)
- Support plans and Contact form: https://websharper.io/support
- Twitter: https://x.com/websharper
- Discord: https://discord.gg/cwwq3sGUrC

## Quick start

To create a new F# web project from the templates and run it locally:

```powershell
dotnet new install WebSharper.Templates
dotnet new websharper-web -lang F# -n MyWebSharperApp
cd MyWebSharperApp
dotnet run
```

The `dotnet new websharper-web` template includes `Startup.fs` / `Startup.cs` showing the recommended hosting setup:

```fsharp
app.UseWebSharper(fun ws -> ws.Sitelet(Site.Main) |> ignore)
```

where `Site.Main` is your main sitelet. See the [documentation on sitelets](https://docs.websharper.com/core/sitelets) on how to define sitelets, which is the main abstraction for routing and serving pages of web applications in WebSharper.

For further project types and how to add WebSharper to an existing project, see the [Getting Started](https://docs.websharper.com/core/getting-started) page.

## Related repositories

- [Docs](https://github.com/dotnet-websharper/docs) - Documentation for WebSharper.
- [WebSharper.UI](https://github.com/dotnet-websharper/ui) - A library for building reactive user interfaces in WebSharper.
- [WebSharper.Templates](https://github.com/dotnet-websharper/templates) - Project templates for WebSharper.
- [MVU](https://github.com/dotnet-websharper/mvu) - A library for building applications in the Model-View-Update architecture style based on `WebSharper.UI.
- [Build scripts](https://github.com/dotnet-websharper/build-script) - Contains common build helpers and GitHub Actions workflows.
- Extensions: See the [dotnet-websharper organization](https://github.com/dotnet-websharper/) for many more repositories with extensions wrapping popular JavaScript libraries and frameworks such as `WebSharper.React`.

# WebSharper

WebSharper is an F#-based web programming platform including compilers from F# and C# code to JavaScript. WebSharper is open source under the Apache 2.0 license and is developed by IntelliFactory, with contributions from the community.

For building or contributing to this repository see [CONTRIBUTING.md](CONTRIBUTING.md).

## Links

- Official website: https://websharper.io/
- Documentation: https://docs.websharper.com/
- Live try examples for WebSharper 4x: https://try.websharper.com
- Contributing guide: [CONTRIBUTING.md](CONTRIBUTING.md)
- License: [LICENSE.md](LICENSE.md)
- Support plans and Contact form: https://websharper.io/support
- Twitter: https://x.com/websharper
- Discord: https://discord.gg/cwwq3sGUrC

## Quick start

Create a new project from the templates and run it locally:

```powershell
dotnet new websharper-web -lang F# -n MyWebSharperApp
cd MyWebSharperApp
dotnet run
```

The `dotnet new websharper-web` template includes `Startup.fs` / `Startup.cs` showing the recommended hosting setup; in your app add:

```fsharp
app.UseWebSharper(fun ws -> ws.Sitelet(Site.Main) |> ignore)
```

where `Site.Main` is your main sitelet. See the [documentation on sitelets](https://docs.websharper.com/core/sitelets) on how to define sitelets, which is the main abstraction for building web applications in WebSharper.

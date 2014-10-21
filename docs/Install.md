# Installation

Developing with WebSharper in Visual Studio currently requires:

* [Visual Studio Express 2012/2013 for Web][vsx] with [F# tools][fsharp] or
  [Visual Studio 2012/2013][vs] - either of these options installs F# 3.0/3.1
  and .NET Framework 4.5.x automatically.

* [NuGet 2.7 or higher][nuget]

**Important**: Note that Visual Studio 2012 may ship with an outdated
version of the NuGet Package Manager. Please go to "Tools > Extensions
and Updates" and make sure that the latest version is installed (at
least NuGet 2.7).

When your environment is ready, download and install the main
WebSharper `.vsix` file from the [WebSharper download page][downloads].
This will install the WebSharper project templates into Visual Studio
(you may have to restart Visual Studio if you have it running while
you install WebSharper), making it easy to get started with new projects.

When you create a new WebSharper project from a Visual Studio template,
it will use the version of WebSharper that came bundled with the 
Visual Studio installer you used.

WebSharper extensions, as well as the core WebSharper binaries, are
distributed via Nuget. This means that you can upgrade WebSharper in
or add WebSharper extensions to your existing Visual Studio projects
by using the NuGet package manager, as you would with any other Nuget
package.

[downloads]: http://websharper.com/downloads
[fsharp]: http://www.microsoft.com/web/gallery/install.aspx?appid=FSharpVWD11
[nuget]: http://nuget.org
[vs]: http://www.microsoft.com/visualstudio/eng/downloads
[vsx]: http://www.microsoft.com/visualstudio/eng/downloads
[ws]: http://bitbucket.org/IntelliFactory/websharper

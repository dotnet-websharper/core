# Installation

Developing with WebSharper currently requires:

* [Visaul Studio Express 2012 for Web][vsx] with [F# tools][fsharp] or
  [Visual Studio 2012][vs] - either of these options installs F# 3.0
  and .NET Framework 4.5 automatically.

* [NuGet 2.7][nuget]

**Important**: Note that Visual Studio 2012 may ship with an outdated
version of the NuGet Package Manager. Please go to "Tools > Extensions
and Updates" and make sure that the latest version is installed (at
least NuGet 2.7).

When your environment is ready, obtain and install the
[WebSharper Visual Studio extension][downloads] by downloading and
clicking on the relevant `.vsix` file.

WebSharper binaries are distributed via the `WebSharper` NuGet package
which is self-contained. The Visual Studio extension simply installs
templates to make it easy to create new projects with starter code.
Once the project is created, you can upgrade WebSharper using NuGet
package manager, as you would with any other package.

[downloads]: http://websharper.com/downloads
[fsharp]: http://www.microsoft.com/web/gallery/install.aspx?appid=FSharpVWD11
[nuget]: http://nuget.org
[vs]: http://www.microsoft.com/visualstudio/eng/downloads
[vsx]: http://www.microsoft.com/visualstudio/eng/downloads#d-2012-express
[ws]: http://bitbucket.org/IntelliFactory/websharper

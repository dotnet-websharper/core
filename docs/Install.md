## Installing WebSharper into Visual Studio

Developing with WebSharper in Visual Studio currently requires:

* [Visual Studio 2012 or later][vs] with Web Developer Tools installed (or Visual Studio Express for Web).
Also Visual F# is needed to use WebSharper for F#.

When your environment is ready, download and install the
WebSharper `.vsix` file for one or both languages from the [WebSharper download page][downloads].
These will install the WebSharper project templates into Visual Studio
(you may have to restart Visual Studio if you have it running while
you install WebSharper), making it easy to get started with new projects.

## Visual Studio templates

Once you installed WebSharper and, if needed, restarted Visual Studio, you should see the main WebSharper templates in the New Project dialog.

![Visual Studio templates](images/VisualStudioTemplates.png)

## Updating WebSharper in existing projects

When you create a new WebSharper project from a Visual Studio template,
it will use the version of WebSharper that came bundled with the 
Visual Studio installer you used.

WebSharper extensions, as well as the core WebSharper binaries, are
distributed via Nuget. This means that you can upgrade WebSharper in
or add WebSharper extensions to your existing Visual Studio projects
by using the NuGet package manager, as you would with any other Nuget
package.

[downloads]: http://websharper.com/downloads
[vs]: http://www.microsoft.com/visualstudio/eng/downloads
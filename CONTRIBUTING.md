# Contributing to WebSharper

WebSharper is a Free Software project, and we welcome your contributions!

[The core repository](https://github.com/dotnet-websharper/core) contains the F# and C#-to-JavaScript compiler and core libraries. WebSharper consists of this repository as well as a constellation of libraries and extensions, located [in the `dotnet-websharper` GitHub organization](https://github.com/dotnet-websharper). Don't hesitate to contribute to these too!

* [What to contribute](#what-to-contribute)
* [How to contribute](#how-to-contribute)
  * [Required software](#requirements)
  * [Building WebSharper from the command line](#build-cli)
  * [Setting up your development environment](#devenv)
  * [Running the tests](#tests)
  * [Building WebSharper to work on a project](#ext-project)
  * [Project structure](#structure)

<a name="what-to-contribute"></a>
## What to contribute?

We welcome all types of contributions, particularly:

* Bug fixes in [the issue tracker](https://github.com/dotnet-websharper/core/issues)
* Standard library improvements (JavaScript APIs, etc.)
* Compiler optimizations and improvements
* Documentation: improvements to [the documentation website](https://developers.websharper.com) can be contributed [in the "docs" repository](https://github.com/dotnet-websharper/docs)
* Feature suggestions are welcome on [the Gitter chat](https://gitter.im/intellifactory/websharper) and [the issue tracker](https://github.com/dotnet-websharper/core/issues); we suggest that you discuss new features with the rest of the team on these channels before getting started on implementation.

<a name="how-to-contribute"></a>
## How to contribute

<a name="requirements"></a>
### Required software

It is possible to work on WebSharper on Windows, Linux and OSX.

To compile WebSharper, you need the following installed:

* The .NET SDK 6.0.10 or newer. You can download it [here](https://www.microsoft.com/net/download).

<a name="build-cli"></a>
### Building WebSharper from the command line

WebSharper can be built using the script `build.cmd` on Windows, or `build.sh` on Linux andr OSX.
In the following shell snippets, a command line starting with `build` means `.\build.cmd` on Windows and `./build.sh` on Linux and OSX.

Simply running `build` compiles the WebSharper compiler, standard libraries and tests in debug mode. The following targets are available:

* `build ws-builddebug`

    Equivalent to simple `build`: compiles the compiler, standard libraries and tests in debug mode.

* `build ws-buildrelease`

    Compiles the compiler, standard libraries and tests in release mode.

* `build ws-package`

    Compiles the compiler, standard libraries and tests in release mode, then creates NuGet packages in the `build` folder.

* `build ws-clean`

    Deletes temporary and output directories.

* `build ci-release`

    Full build as is used for releases. Update non-fixed dependencies, build everything, run unit tests, package.
    
The following options are available:

* `build [TARGET] -ef verbose`

    Makes compilation more verbose. Equivalently, set the `verbose` environment variable to `true`.

<a name="devenv"></a>
### Setting up your development environment

We recommend that you use one of the following development environments:

* On Windows: [Visual Studio 2022](https://visualstudio.microsoft.com/vs/).
* On all platforms: [Visual Studio Code](https://code.visualstudio.com/) with the following extensions:
  * `ionide-fsharp` for F# support
  * `ms-vscode.csharp` for C# support

<a name="tests"></a>
### Running the tests

WebSharper defines and uses its own test framework, WebSharper.Testing. It runs on the client side and is backed by [qUnit](https://qunitjs.com/). So running the WebSharper test suite consists in running a web application which looks like this:

![Unit testing screenshot](https://github.com/dotnet-websharper/core/raw/master/docs/qunit.png)

The recommended way to run these tests is to run the `tests/Web` project. It is an ASP.NET application hosting the test suite, under the "Client-side test suite" on its home page.

* If you are using Visual Studio, you can simply open `WebSharper.sln`, set `tests/Web` as the startup project, and Run. Check your build target. Update the build target according to your build flags. Target Debug only if you used ws-builddebug.

* On Linux or OSX, you can browse into the `tests/Web` folder and simply run `xsp`.

To find where to add tests for your code, check the [project structure](#structure).

<a name="ext-project"></a>
### Building WebSharper to work on a project

Did you encounter a WebSharper bug while working on your project, and want to implement a fix and try it locally? The easiest way to do so is to create NuGet packages for WebSharper and use them locally.

* In websharper, write your code fix and run `build ws-package`.
* In your project, add WebSharper's build folder as a NuGet source repository and update the packages:
  * If you're using Paket (for example if you're working on one of [WebSharper's libraries and extensions](https://github.com/dotnet-websharper)):
    * Add the following line to your `paket.dependencies`:
    
        ```
        source /path/to/websharper-repo/build
        ```
        
    * Run `paket update`.
  * If you're using the standard NuGet package manager:
    * Add the following to `NuGet.config` in your project repository:
    
        ```xml
        <?xml version="1.0" encoding="utf-8"?>
        <configuration>
          <packageSources>
            <add key="local-websharper" value="/path/to/websharper-repo/build" />
          </packageSources>
        </configuration>
        ```
        
    * Update the NuGet packages either from Visual Studio's GUI, or with the command line:
    
        ```sh
        dotnet add WebSharper
        dotnet add WebSharper.FSharp -- if you're using F#
        dotnet add WebSharper.CSharp -- if you're using C#
        ```

<a name="structure"></a>
### Project structure

Here is the detail of the project structure. The repository contains two solutions:

* `WebSharper.Compiler.sln` contains the F# and C#-to-JavaScript compilers, the MSBuild task for the C# compiler, and their dependencies. Under `src/compiler/`:
  * `WebSharper.Core.JavaScript` contains facilities for parsing and writing plain JavaScript.
  * `WebSharper.Core` contains most everything that is common between the compilers, user libraries and runtime: attributes and core type definitions, JSON serialization, macro API, etc.
  * `WebSharper.InterfaceGenerator` contains the type definitions for the Interface Generator.
  * `WebSharper.Compiler` contains the compiler code common between the F# and C# compilers.
  * `WebSharper.Compiler.CSharp` contains the C# compiler as a library.
  * `WebSharper.CSharp` contains the C# compiler executable.
  * `WebSharper.Compiler.FSharp` contains the F# compiler as a library.
  * `WebSharper.FSharp` contains the F# compiler executable.
  * `WebSharper.MSBuild.CSharp` contains the MSBuild task that invokes the C# compiler after csc. The F# compiler entirely replaces fsc, so it does not need a build task.
  * `src/compiler/WebSharper.CSharp.Analyzer` contains the Roslyn analyzer, which provides the code service for C#.
* `WebSharper.sln` contains the standard libraries, tests, and their dependencies.
  * Under `src/stdlib/`:
    * `WebSharper.JavaScript` contains the type definitions for the JavaScript standard libraries: EcmaScript types, DOM, HTML5 APIs, etc.
    * `WebSharper.JQuery` contains the type definitions for jQuery.
    * `WebSharper.Main` contains the main WebSharper client-side libraries, such as the `JS` module, `Optional` and `Union` types, remoting client-side types, etc.
    * `WebSharper.Main.Proxies` contains the standard library proxies, ie the client-side implementations for the a good part of the .NET standard library and FSharp.Core.  
      This project is peculiar because it is not compiled directly; instead it is combined with `WebSharper.Main` to create the `WebSharper.Main` assembly. This assembly contains the .NET code of the `WebSharper.Main` project, but the embedded WebSharper files contain everything from `WebSharper.Main.Proxies`.
    * `WebSharper.MathJS` contains the type definitions for MathJS.
    * `WebSharper.MathJS.Extensions` contains the optional MathJS-based proxies for `decimal`, `bigint` and `Complex`.
    * `WebSharper.Collections` contains proxies for some collection types such as F#'s `Set` and `Map`, as well as the LINQ methods.
    * `WebSharper.Control` contains proxies for .NET events, observables and F# `MailboxProcessor`.
    * `WebSharper.Testing` contains the WebSharper client-side unit testing framework.
  * Under `src/sitelets/`:
    * `WebSharper.Web` contains the server-side remoting runtime as well as some client-side HTML types.
    * `WebSharper.Sitelets` contains the Sitelets API and runtime.
    * `WebSharper.Sitelets.Offline` contains the machinery for static Html project compilation.
  * Under `tests/`:
    * `WebSharper.Core.JavaScript.Tests` contains tests for JavaScript parsing and writing.
    * `WebSharper.Tests` contains tests for `stdlib/WebSharper.Main`, `Main.Proxies` and `Control`.
    * `WebSharper.Html5.Tests` contains tests for HTML5 bindings in `stdlib/WebSharper.JavaScript`.
    * `WebSharper.Collections.Tests` contains tests for `stdlib/WebSharper.Collections`.
    * `WebSharper.InterfaceGenerator.Tests` contains a test interface generator. This interface is then validated in `WebSharper.Tests`.
    * `WebSharper.Web.Tests` contains remoting, client-side routing and JSON tests.
    * `WebSharper.CSharp.Tests` contains C#-specific tests.
    * `WebSharper.Sitelets.Tests` contains F# server-side tests.
    * `WebSharper.CSharp.Sitelets.Tests` contains C# server-side tests.
    * `WebSharper.SPA.Tests` is a single-page application serving a harness for all the above client-side tests.
    * `WebSharper.StaticHtml.Tests` is a static HTML application serving a harness for all the above client-side tests.
    * `Website` defines a sitelet that includes all the above server-side, client-side and cross-tier tests.
    * `Web` is a client-server application serving the sitelet defined in `Website`. Therefore it is a harness for the whole test suite.

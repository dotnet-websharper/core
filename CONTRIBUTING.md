# Contributing to WebSharper

WebSharper is a Free Software project, and we welcome your contributions!

[The core repository](https://github.com/dotnet-websharper/core) contains the F# and C#-to-JavaScript compiler and core libraries. WebSharper consists of this repository as well as a constellation of libraries and extensions, located [in the `dotnet-websharper` GitHub organization](https://github.com/dotnet-websharper). Don't hesitate to contribute to these too!

## What to contribute?

We welcome all types of contributions, particularly:

* Bug fixes in [the issue tracker](https://github.com/dotnet-websharper/core/issues)
* Standard library improvements (JavaScript APIs, etc.)
* Compiler optimizations and improvements
* Documentation: improvements to [the documentation website](https://docs.websharper.com) can be contributed [in the "docs" repository](https://github.com/dotnet-websharper/docs)
* Feature suggestions are welcome on [Discord](https://discord.gg/VU99asn4) and [the issue tracker](https://github.com/dotnet-websharper/core/issues); we suggest that you discuss new features with the rest of the team on these channels before getting started on implementation.

## How to contribute

### Required software

It is possible to work on WebSharper on Windows, Linux, and macOS.

To compile WebSharper, you need the following installed:

* **.NET SDK 9.0 (or newer in the 9.x band)**. You can download it [here](https://www.microsoft.com/net/download).

### Building WebSharper from the command line

WebSharper can be built using the script `build.cmd` on Windows, or `build.sh` on Linux and macOS.  
In the following shell snippets, a command line starting with `build` means `.\build.cmd` on Windows and `./build.sh` on Linux/macOS.

Simply running `build` compiles the WebSharper compiler, standard libraries and tests in **Debug** mode. The following targets are available:

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

### Setting up your development environment

We recommend that you use one of the following development environments:

* On Windows: [Visual Studio 2022](https://visualstudio.microsoft.com/vs/).
* On all platforms: [Visual Studio Code](https://code.visualstudio.com/) with:
  * `ms-dotnettools.csharp` (C# for VS Code)
  * `ms-dotnettools.csdevkit` (C# Dev Kit)
  * `ionide-fsharp` (F# support)

### Running the tests

WebSharper defines and uses its own test framework, WebSharper.Testing. It runs on the client side and is backed by [QUnit](https://qunitjs.com/). So running the WebSharper test suite consists in running a web application which looks like this:

![Unit testing screenshot](https://github.com/dotnet-websharper/core/raw/master/docs/qunit.png)

The recommended way to run these tests is to run the `tests/Web` project. It is an **ASP.NET Core** application hosting the test suite, under the "Client-side test suite" on its home page.

* If you are using Visual Studio, you can simply open `WebSharper.sln`, set `tests/Web` as the startup project, and Run. Check your build target. Update the build target according to your build flags. Target Debug only if you used ws-builddebug.
* From the command line (Linux/macOS/Windows):  
  ```bash
  cd tests/Web
  dotnet run
  ```

Then open the app in the browser and choose **Client-side test suite**.

To find where to add tests for your code, check the project structure below.

### Linux/WSL quick start

If you’re on Linux (e.g., Ubuntu 24.04) or Windows using **WSL2**, here’s a minimal setup:

```bash
# 1) Install .NET 9 SDK (Ubuntu 24.04)
sudo add-apt-repository ppa:dotnet/backports -y
sudo apt-get update && sudo apt-get install -y dotnet-sdk-9.0

# 2) Install Node via nvm (LTS recommended)
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/master/install.sh | bash
# restart your shell
nvm install --lts

# 3) Install Google Chrome (for browser tests)
sudo apt update && sudo apt upgrade -y
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install -y ./google-chrome-stable_current_amd64.deb
google-chrome --version

# 4) Verify toolchain
dotnet --version
node --version
npm --version

# 5) Build (debug first)
chmod +x build.sh
./build.sh ws-builddebug

# 6) Run the web test
cd tests/Web
dotnet run
```

### Building WebSharper to work on a project

Did you encounter a WebSharper bug while working on your project, and want to implement a fix and try it locally? The easiest way is to create NuGet packages for WebSharper and use them locally.

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
        dotnet add package WebSharper
        dotnet add package WebSharper.FSharp # if you're using F#
        dotnet add package WebSharper.CSharp # if you're using C#
        ```

### Project structure

Here is the detail of the project structure. The repository contains multiple solutions:

* `WebSharper.Compiler.sln` contains the F# and C#-to-JavaScript compilers, the MSBuild tasks for the F# and C# compilers, and their dependencies. Under `src/compiler/`:

  * `WebSharper.Core.JavaScript` contains facilities for parsing and writing plain JavaScript.
  * `WebSharper.Core` contains most everything that is common between the compilers, user libraries and runtime: attributes and core type definitions, JSON serialization, macro API, etc.
  * `WebSharper.InterfaceGenerator` contains the type definitions for the Interface Generator.
  * `WebSharper.Compiler` contains the compiler code common between the F# and C# compilers.
  * `WebSharper.Compiler.CSharp` contains the C# compiler as a library.
  * `WebSharper.CSharp` contains the C# compiler executable.
  * `WebSharper.Compiler.FSharp` contains the F# compiler as a library.
  * `WebSharper.FSharp` contains the F# compiler executable.
  * `WebSharper.FSharp.Service` contains F# service utilities (parsing/typing hooks, etc.).
  * `WebSharper.MSBuild.FSharp` contains the MSBuild task/targets that integrate the F# compiler.
  * `WebSharper.MSBuild.CSharp` contains the MSBuild task that invokes the C# compiler after csc.
  * `WebSharper.CSharp.Analyzer` contains the Roslyn analyzer, which provides the code service for C#.

* `WebSharper.sln` contains the standard libraries, tests, and their dependencies.

  * Under `src/stdlib/`:

    * `WebSharper.JavaScript` contains the type definitions for the JavaScript standard libraries: ECMAScript types, DOM, HTML5 APIs, etc.
    * `WebSharper.StdLib` contains the main WebSharper client-side libraries, such as the `JS` module, `Optional` and `Union` types, remoting client-side types, etc. **Note:** standard library proxies are included in this assembly.
    * `WebSharper.Testing` contains the WebSharper client-side unit testing framework.

  * Under `src/sitelets/`:

    * `WebSharper.Web` contains the server-side remoting runtime as well as some client-side HTML types.
    * `WebSharper.Sitelets` contains the Sitelets API and runtime.
    * `WebSharper.Sitelets.Offline` contains the machinery for static HTML project compilation.
    * `WebSharper.AspNetCore` contains ASP.NET Core integration.

  * Under `tests/`:
    * `WebSharper.Core.JavaScript.Tests` contains tests for JavaScript parsing and writing.
    * `WebSharper.Tests` contains tests for `stdlib/WebSharper.StdLib` (including proxies).
    * `WebSharper.Html5.Tests` contains tests for HTML5 bindings in `stdlib/WebSharper.JavaScript`.
    * `WebSharper.Collections.Tests` contains tests for collection proxies.
    * `WebSharper.InterfaceGenerator.Tests` contains a test interface generator. This interface is then validated in `WebSharper.Tests`.
    * `WebSharper.Web.Tests` contains remoting, client-side routing and JSON tests.
    * `WebSharper.CSharp.Tests` contains C#-specific tests.
    * `WebSharper.Sitelets.Tests` contains F# server-side tests.
    * `WebSharper.CSharp.Sitelets.Tests` contains C# server-side tests.
    * `WebSharper.SPA.Tests` is a single-page application serving a harness for all the above client-side tests.
    * `WebSharper.StaticHtml.Tests` is a static HTML application serving a harness for all the above client-side tests.
    * `Website` defines a sitelet that includes all the above server-side, client-side and cross-tier tests.
    * `Web` is a client-server application serving the sitelet defined in `Website`. Therefore it is a harness for the whole test suite.
    * `WebSharper.InterfaceGenerator.Tests.LatestFSharp` contains tests for the Interface Generator targeting the latest F# features.
    * `WebSharper.CSharp.Interop.Tests` contains tests for C# interoperability scenarios.
    * `WebSharper.CSharp.StaticHtml.Tests` contains a static HTML test harness specific to C# components.
    * `WebSharper.CSharp.Analyzer.Tests` contains tests for the C# Roslyn analyzer.
    * `WebSharper.Module.Tests` contains tests for module-related behaviors.
    * `WebSharper.Compiler.FSharp.Tests` contains integration tests for the F# compiler component.
    * `WebSharper.Library.Tests` contains library-level infrastructure tests.
    * `Web.FSharp` contains an F#-specific test harness.
    * `Web.TypeScript` contains tests related to TypeScript interop or code-generation functionality.
    * `ProxyProjectTest` and `ProxyProjectTest.Proxy` contain tests for proxy project generation.
    * `Web.Giraffe` contains integration tests for WebSharper with the Giraffe framework.
    * `StressTesting` contains performance and stress test scenarios.
    * `WebSharper.StaticHtml.Tests.NetStandard` contains a .NET Standard variant of the static HTML test harness.

* `WebSharper.Tools.sln` contains developer tools and metadata inspection.
  * Under the `src/compiler/`:
    * `WebSharper.DllBrowser` contains a Windows-only GUI tool to inspect WebSharper metadata in assemblies; the UI is minimal (no menus/Open dialog) and you load assemblies by drag-and-dropping them onto the tree view, similar to ILSpy.
    * `WebSharper.TypeScriptParser` contains a Node.js-style project (`.njsproj`) that provides TypeScript parsing capabilities for WebSharper tools.
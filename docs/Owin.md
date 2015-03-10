# WebSharper.Owin

The WebSharper.Owin project provides more options for hosting WebSharper Sitelets applications using [OWIN](http://owin.org/). For example, you can use it to compile a WebSharper Sitelet into a self-contained executable that runs a self-hosted website using [Katana](https://katanaproject.codeplex.com/). That is what the Self-Hosted Client-Server Web Application [project template](ProjectTemplates.md) does.

WebSharper.Owin is available [from NuGet](http://nuget.org/packages/websharper.owin), and its source code is [on GitHub](http://github.com/intellifactory/websharper.owin).


## WebSharper OWIN Middleware

WebSharper.Owin provides two OWIN middleware classes, one for remoting and one for sitelets. In both cases, there are two ways to use the middleware class:

* Either call the middleware's constructor with the next `AppFunc` as first argument, and use the method `Invoke : AppFunc`,
* Or use the static method `AsMidFunc : (arguments) -> Func<AppFunc, AppFunc>`.

The middleware classes are:

* `WebSharper.Owin.RemotingMiddleware` serves RPC functions. Here are its constructor / MidFunc arguments:

    * `(options: Options)`

    Serves RPC functions based on the given [options](#options).

    * `(webRoot: string, meta: Metadata.Info)`

    Serves the RPC functions defined in the provided WebSharper metadata. Uses the given directory as web application root.

    * `(meta: Metadata.Info)`

    Serves the RPC functions defined in the provided WebSharper metadata. Uses the current working directory as web application root.

    * `(webRoot: string, ?binDirectory: string)`

    Serves the RPC functions defined in assemblies located in the bin directory. Uses the given directory as web application root. If the bin directory is not provided, then the `bin` subdirectory of the web root is used.

* `WebSharper.Owin.SiteletMiddleware` serves the provided Sitelet. Here are its constructor / MidFunc arguments:

    * `(options: Options, sitelet: Sitelet<'T>)`

    Serves the provided sitelet with the given [options](#options).

    * `(webRoot: string, sitelet: Sitelet<'T>, ?binDirectory: string)`

    Serves the provided sitelet using the given directory as web application root. Also serves RPC functions as per `RemotingMiddleware(webRoot, ?binDirectory)`. Also serves RPC functions as per `RemotingMiddleware(webRoot, ?binDirectory)`.

    * `Create(webRoot: string, ?binDirectory: string)`

    Searches for a sitelet to serve in assemblies located in the provided bin directory. Uses the given directory as web application root. If the bin directory is not provided, then the `bin` subdirectory of the web root is used. Also serves RPC functions as per `RemotingMiddleware(webRoot, ?binDirectory)`.

### Notes

* The `RemotingMiddleware` should always appear *before* any self-contained application in the stack. In particular, it should appear before `SiteletMiddleware`.

* `SiteletMiddleware` does not serve any static files, and in particular it does not serve the WebSharper-generated JavaScript files. You need to use a file system middleware such as `Microsoft.Owin.StaticFiles`, as shown in the [Self-Hosted Client-Server Web Application project template](https://github.com/intellifactory/websharper.visualstudio/blob/master/templates/owin-selfhost/Main.fs).

## Katana IAppBuilder extension methods

WebSharper.Owin also provides a set of convenient extension methods on the type `IAppBuilder` from Katana (a.k.a. Microsoft.Owin).

* The `UseRemoting` overloads are equivalent to the `RemotingMiddleware` overloads.

* `UseCustomSitelet` is equivalent to `SiteletMiddleware(options, sitelet)`.

* `UseSitelet` is equivalent to `SiteletMiddleware(webRoot, sitelet, ?binDirectory)`.

* `UseDiscoveredSitelet` is equivalent to `SiteletMiddleware(webRoot, ?binDirectory)`.


<a name="options"></a>
## Options

Several methods in the WebSharper.Owin API receive options via the `Options` type. Options can be created with the `Options.Create` static member:

* `Create()` creates empty options. No RPC function will be served.

* `Create(meta: Metadata.Info)` creates options with the given WebSharper metadata. The metadata is used to determine the RPC functions to serve.

* `Create(webRoot: string, ?binDirectory: string)` creates options with the given web application root and bin directory. Metadata is automatically loaded from assemblies in the bin directory. If the bin directory is not provided, then the `bin` subdirectory of the web root is used.

Options can be altered using the following methods:

* `WithDebug()` enables debugging: WebSharper-generated JavaScript files are requested uncompressed.

* `WithDebug(bool)` indicates whether debugging is enabled. The default is false.

* `WithServerRootDirectory(webRoot: string)` indicates the web application root. The default is the current working directory.

* `WithUrlPrefix(string)` indicates that sitelets are served under the given URL prefix.

* `WithRunRemoting(bool)` indicates whether running the Sitelets middleware also automatically runs the remoting middleware. The default is true.
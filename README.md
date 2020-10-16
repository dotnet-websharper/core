
# WebSharper

[![Join the chat at https://gitter.im/intellifactory/websharper](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/intellifactory/websharper?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

WebSharper is an [F#][fsharp]-based web programming platform including a compiler from F# code to JavaScript.

* [Installing](#installing)
* [Running your applications](#running)
* [Hello world!](#helloworld)
* [Single-Page Applications](#spa)
* [Multi-Page Applications](#mpa)
* [Adding client-side functionality](#clientside)
* [Using JavaScript libraries](#jslibs)
* [Creating REST applications](#rest)
* [Contributing](#contrib)
* [Links](#links)

<a name="installing"></a>
## Installing

The easiest way to get started is from an application template. You can install the various WebSharper [project templates](http://websharper.com/docs/templates) by following the instructions below for:

 * [Visual Studio](http://websharper.com/docs/install)
 * [Xamarin Studio/MonoDevelop](http://websharper.com/docs/install-xs)

Alternatively, you can use the [F# Yeoman Generator](https://github.com/fsprojects/generator-fsharp) to install template files for your favorite editor.  You can then build your project by running `msbuild` in the project root folder.

### Creating WebSharper project by hand

If you are using any one of the available WebSharper project templates, they should compile and run without any modifications.

If you are creating your project files manually, the following is a typical string of steps to get you up and running, although you should really consider starting off of an existing WebSharper template:

 * Start from an ordinary F# library project
 * Install `WebSharper` using [paket](https://fsprojects.github.io/Paket/). This will include the main `WebSharper.targets` and the core references in your project file.
 * Add a special project file property to drive how you want to compile your project. These are:
 
    * `<WebSharperProject>Html</WebSharperProject>` for HTML Applications
    * `<WebSharperProject>Site</WebSharperProject>` for Client-Server Applications
    * `<WebSharperProject>Bundle</WebSharperProject>` for Single-Page Applications
 
 * Include any further bits in your project file you may need. For instance, you will need to reference `Microsoft.WebApplication.targets` if you intend to host your application in IIS or the built-in web server in Visual Studio.

<a name="running"></a>
## Running your applications

With the exception of the self-hosted templates, all WebSharper templates produce applications that can run inside an ASP.NET-compatible container.  (HTML applications can be deployed in any web server by copying the contents of the `bin\html` folder.)

In the examples below, you will see how to create [WebSharper sitelets](http://websharper.com/docs/sitelets). Sitelets are web applications encoded in the F# type system. They have a set of endpoints (accessed via GET, POST, etc.) to which they respond by serving web content asynchronously. You can run these the following ways:

 * **In IIS or any other ASP.NET-compatible container**

   Annotate your main sitelet with the `[<Website>]` attribute:
   
   ```fsharp
   [<Website>]
   let MySite = ...
   ```
   
 * **As a self-hosted executable using WebSharper.Warp**
 
   Warp provides a way to self-host sitelets via OWIN and `Microsoft.Owin`.  To use Warp, you need to add `WebSharper.Warp` to your project (which should be a console project), and invoke the host machinery with your sitelet:
   
   ```fsharp
   [<EntryPoint>]
   do Warp.RunAndWaitForInput(MySite) |> ignore
   ```

  By default, sites are served on `http://localhost:9000`.

 * **As a Suave application**

   Suave is a light-weight web server built in F#.  You can easily use WebSharper in your existing Suave application, or host your WebSharper applications (which should be a console project) on Suave, by adding `WebSharper.Suave` to your project and calling the WebSharper adapter to convert your sitelet to a Suave `WebPart`:

   ```fsharp
   module WebSharperOnSuave
   
   open WebSharper
   open WebSharper.Sitelets
   
   let MySite =
       Application.Text (fun ctx -> "Hello World")
   
   open global.Suave
   open Suave.Web
   open WebSharper.Suave
   
   startWebServer defaultConfig
       (WebSharperAdapter.ToWebPart(MySite, RootDirectory="../.."))
   ```

<a name="helloworld"></a>
## Hello World!

With WebSharper you can develop pure JS/HTML, and single- and multi-page web applications with an optional server side, all in F#.  Unless you are looking for low-level control, we recommend that you start by creating a [sitelet](http://websharper.com/docs/sitelets).

The simplest sitelet serves text on a single endpoint at the root of the application:

```fsharp
module YourApp

open WebSharper
open WebSharper.Sitelets

[<Website>]
let Main = Application.Text (fun ctx -> "Hello World!")
```

[![](http://i.imgur.com/fZgqeKjm.png)](http://i.imgur.com/fZgqeKjl.png)

<a name="spa"></a>
## Single Page Applications

While serving text is fun and often useful, going beyond isn't any complicated. For instance, you can easily construct single-page applications:

```fsharp
module YourApp

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI.Html
open WebSharper.UI.Server

[<Website>]
let Main =
    Application.SinglePage (fun ctx ->
        Content.Page(
            h1 [] [text "Hello World!"]
        )
    )
```

This code creates an empty HTML document and inserts a header node.

[![](http://i.imgur.com/xYITvCqm.png)](http://i.imgur.com/xYITvCql.png)

### HTML responses

Pages are a special type of content responses, and you can easily finetune them by specifying where you want content to be added, by using an optional `Title`, `Head`, `Body`, and `Doctype`.

```fsharp
    ...
    Application.SinglePage (fun ctx ->
        Content.Page(
            Title = "My Hello World app",
            Body = [
                h1 [text "Hello World!"]
            ],
            ...
        )
    )
```

You can construct HTML via the (soon legacy) WebSharper 3.x markup combinators in `WebSharper.Html.Server` and `WebSharper.Html.Client` (for client-side markup, see the section below), or using the next generation reactive HTML language from WebSharper UI (as above and in the examples on this page; formerly called UI.Next). A quick syntax guide to the HTML constructors in WebSharper UI:

(TBA)

### Custom responses

Content responses are asynchronous.  Next to full HTML pages, you can return:

 * **Plain text** with `Content.Text`:
 
    ```fsharp
    Content.Text "Hello World!"
    ```
 * **JSON values** with `Content.Json` (visit [JSON documentation](http://websharper.com/docs/json) or
 [JSON cheatsheet](http://websharper.com/docs/json-ref) for more info):
 
    ```fsharp
    type Person = { First: string; Last: string; Age: int}

    Content.Json { First="John"; Last="Smith"; Age=30 }
    ```
 * **Files** with `Content.File`:

    ```fsharp
    Content.File("Main.fs", ContentType="text/plain")
    ```
 * Various **error codes**:
   * `Content.Unauthorized` (401)
   * `Content.Forbidden` (403)
   * `Content.NotFound` (404)
   * `Content.MethodNotAllowed` (405)
   * `Content.ServerError` (500)
   
   You can also create your own custom error code response:
    ```fsharp
    Content.Custom(Status=Http.Status.Custom 402 (Some "Payment Required"))
    ```
 * Any other **custom content** with `Content.Custom`.
 
<a name="mpa"></a>
## Multi-page applications

Multi-page applications have multiple endpoints: pairs of HTTP verbs and paths, and are represented as an annotated **union type** we typically call `Endpoints` (or `Action` in previous terminology).  The endpoints, as defined by this union type - given the various annotations on each union case - are mapped to content to be served using `Application.MultiPage`.  Links to endpoints in your site can be calculated from the serving context, so you will never have invalid URLs.

```fsharp
module YourApp

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Server

type Endpoints =
    | [<EndPoint "GET /">] Home
    | [<EndPoint "GET /about">] About

[<Website>]
let Main =
    Application.MultiPage (fun ctx endpoint ->
        let (=>) label endpoint = a [attr.href (ctx.Link endpoint)] [text label]
        match endpoint with
        | Endpoints.Home ->
            Content.Page(
                Body = [
                    h1 [] [text "Hello world!"]
                    "About" => Endpoints.About
                ]
            )
        | Endpoints.About ->
            Content.Page(
                Body = [
                    p [] [text "This is a simple app"]
                    "Home" => Endpoints.Home
                ]
            )
    )
```

[![](http://i.imgur.com/WMnmzIPm.png)](http://i.imgur.com/WMnmzIPl.png)

<a name="clientside"></a>
## Adding client-side functionality

WebSharper applications can easily incorporate client-side content, expressed in F#, giving an absolute edge over any web development library. Just mark your client-side functions or modules with `[<JavaScript>]` and embed them into server side markup using `client`. Server-side RPC functions are annotated with `[<Rpc>]`.

The example below is reimplemented from the blog entry [Deploying WebSharper apps to Azure via GitHub](http://websharper.com/blog-entry/4368), also available in the main WebSharper templates, and although it omits the more advanced templating in that approach (which is straightforward to add to this implementation), it should give you an recipe for adding client-side functionality to your sitelets easily.

```fsharp
module YourApp

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client

module Server =
    [<Rpc>]
    let DoWork (s: string) = 
        async {
            return System.String(List.ofSeq s |> List.rev |> Array.ofList)
        }

[<JavaScript>]
module Client =
    open WebSharper.JavaScript

    let Main () =
        let input = input [attr.value ""] []
        let output = h1 [] []
        div [
            input
            button [
                on.click (fun _ _ ->
                    async {
                        let! data = Server.DoWork input.Value
                        output.Text <- data
                    }
                    |> Async.Start
                )
            ] [text "Send"]
            hr [] []
            h4A [attr.``class`` "text-muted"] [text "The server responded:"]
            div [attr.``class`` "jumbotron"] [output]
        ]

open WebSharper.UI.Server

[<Website>]
let MySite =
    Application.SinglePage (fun ctx ->
        Content.Page(
            Body = [
                h1 [] [text "Say Hi to the server"]
                div [] [client <@ Client.Main() @>]
            ]
        )
    )
```

[![](http://i.imgur.com/9sPa4lzm.png)](http://i.imgur.com/9sPa4lzl.png)

<a name="jslibs"></a>
## Using JavaScript libraries
WebSharper extensions bring JavaScript libraries to WebSharper.  You can [download](http://websharper.com/downloads) extensions or develop your own using [WIG](http://websharper.com/docs/wig), among others. Below is an example using [WebSharper.Charting](https://github.com/intellifactory/websharper.charting) and [chart.js](http://www.chartjs.org/) underneath.

Note that these and any other dependencies you may be using will be automatically injected into a `Content.Page` or other sitelet HTML response, and you will never have to deal with them manually.

```fsharp
module YourApp

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI
open WebSharper.UI.Html

[<JavaScript>]
module Client =
    open WebSharper.JavaScript
    open WebSharper.UI.Client
    open WebSharper.Charting

    let RadarChart () =
        let labels =    
            [| "Eating"; "Drinking"; "Sleeping";
               "Designing"; "Coding"; "Cycling"; "Running" |]
        let data1 = [|28.0; 48.0; 40.0; 19.0; 96.0; 27.0; 100.0|]
        let data2 = [|65.0; 59.0; 90.0; 81.0; 56.0; 55.0; 40.0|]

        let ch =
            Chart.Combine [
                Chart.Radar(Seq.zip labels data1)
                    .WithFillColor(Color.Rgba(151, 187, 205, 0.2))
                    .WithStrokeColor(Color.Rgba(151, 187, 205, 1.))
                    .WithPointColor(Color.Rgba(151, 187, 205, 1.))

                Chart.Radar(Seq.zip labels data2)
                    .WithFillColor(Color.Rgba(220, 220, 220, 0.2))
                    .WithStrokeColor(Color.Rgba(220, 220, 220, 1.))
                    .WithPointColor(Color.Rgba(220, 220, 220, 1.))
            ]
        Renderers.ChartJs.Render(ch, Size = Size(400, 400))

open WebSharper.UI.Server

[<Website>]
let MySite =
    Application.SinglePage (fun ctx ->
        Content.Page(
            Body = [
                h1 [] [text "Charts are easy with WebSharper Warp!"]
                div [] [client <@ Client.RadarChart() @>]
            ])
    )
```

[![](http://i.imgur.com/9o7x2b1m.png)](http://i.imgur.com/9o7x2b1l.png)

<a name="rest"></a>
## Creating REST applications

TBA.

<hr />

<a name="contrib"></a>
## Contributing to WebSharper

WebSharper is an open-source project, and contributions are welcome!

First, don't hesitate to [report issues on the tracker][issues].

To contribute code, please check the [contributing guide](CONTRIBUTING.md).

<a name="links"></a>
## Links

* [Documentation][doc]
* [Downloads][downloads]
* [Examples](https://try.websharper.com)
* [Forums](https://forums.websharper.com)
* [License][license]
* Community
  * [Source code on GitHub][gh]
  * [Issue tracker on GitHub][issues]
  * [Chat on Gitter.im](https://gitter.im/intellifactory/websharper)
  * [WebSharper on StackOverflow](http://stackoverflow.com/questions/tagged/websharper)
  * [#websharper on freenode][chat]
  * [Contact IntelliFactory][contact]

[chat]: http://webchat.freenode.net/?channels=#websharper
[contact]: http://websharper.com/contact
[downloads]: http://websharper.com/downloads
[fsharp]: http://fsharp.org
[gh]: http://github.com/dotnet-websharper/core
[issues]: https://github.com/dotnet-websharper/core/issues
[license]: http://github.com/dotnet-websharper/core/blob/master/LICENSE.md
[doc]: https://developers.websharper.com
[nuget]: http://nuget.org

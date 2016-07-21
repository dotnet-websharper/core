# How to run WebSharper F# examples

Most often, and in particular in the [Examples](/samples) section of this site,
you will find WebSharper snippets and examples that define a WebSharper control
such as:

```
namespace Samples
...
[<JavaScript>]
module HelloWorld =

    let Main () = ...

type HelloWorldViewer() =
    inherit Web.Control()
    
    [<JavaScript>]
    override this.Body = HelloWorld.Main () :> _
```

This section explains how you can run and test these controls, using the
[Hello World](/samples/HelloWorld) example as defined above.

## Testing via single-page applications (SPAs)

Most of the examples on this web site do not require a server side, e.g. they
come without an RPC-based backend. One of the easiest ways to run these examples
is within a single-page application (SPA).

To embed the Hello World example in an HTML application, follow these steps:

 1. Create a new SPA application using the "Single-Page Application" project template.

 2. Open `Client.fs` and edit its content as follows:

   a. Keep the namespace declaration

   b. Override the rest of the file with the code from the example. You don't need
      to carry over the code for the `HelloWorldViewer` type. Instead, add the
      following lines at the end of the example module to execute the pagelet
      by appending it to a placeholder node with ID `entrypoint` (choose freely):

		[<SPAEntryPoint>]
		let Run() = (Main ()).AppendTo "entrypoint"

      With these changes, your code should look like this (with the namespace
      matching the one you created):

        namespace Bundle1

        open WebSharper
        open WebSharper.Html

        [<JavaScript>]
        module HelloWorld =

			let Main () =
                let welcome = P [Text "Welcome"]
                Div [
                    welcome
                    Button [Text "Click Me!"]
                    |>! OnClick (fun e args ->
                        welcome.Text <- "Hello, world!")
                ]

			[<SPAEntryPoint>]
			let Run() = (Main ()).AppendTo "entrypoint"

      Alternatively, you add the `SPAEntryPoint` attribute to `Main`, and
      modify the function to append its result to your chosen placeholder:

        namespace Bundle1

        open WebSharper
        open WebSharper.Html

        [<JavaScript; SPAEntryPoint>]
        module HelloWorld =

            let Main =
                let welcome = P [Text "Welcome"]
                Div [
                    welcome
                    Button [Text "Click Me!"]
                    |>! OnClick (fun e args ->
                        welcome.Text <- "Hello, world!")
                ]
                |> fun res -> res.AppendTo "entrypoint"


   c. Add the pagelet placeholder you used, in `index.html` before the generated JavaScript include:

        ...
        <div id="entrypoint" />
        <script type="text/javascript" src="Content/Bundle1.min.js"></script>
        ...

   d. At this point, you should be able to compile your application and run it. Please note that
      this project template by default starts a web deployment and tries to open the root of the
      web application (instead of an actual HTML file), and will fail doing so.  Instead, you
      should open `index.html`.

## Testing via HTML applications

An alternative way to run examples on this site is by embedding them
into a sitelet that generates an HTML application. Follow these steps:

 1. Create a new HTML application using the "HTML Application" project template.

 2. Open `Client.fs` and edit its contents as follows:

   a. Keep the namespace declaration

   b. Override the rest of the file with the code from the example. You don't need
      to carry over the code for the `HelloWorldViewer` type. Your code should look
      like (with the namespace matching the one you created):

        namespace HtmlApp1

        open WebSharper
        open WebSharper.Html

        [<JavaScript>]
        module HelloWorld =

            let Main () =
                let welcome = P [Text "Welcome"]
                Div [
                    welcome
                    Button [Text "Click Me!"]
                    |>! OnClick (fun e args ->
                        welcome.Text <- "Hello, world!")
                ]

 3. Open `Main.fs` and change the reference to the pagelet contained on the Home page:

        ...
        module Controls =

            [<Sealed>]
            type EntryPoint() =
                inherit Web.Control()

                [<JavaScript>]
                override __.Body =
                    HelloWorld.Main() :> _
        ...

 4. You can now compile the application, navigate to `bin\html` and open `index.html`
    to see the Hello World snippet run.

This project template contains more code in `Main.fs` to manage an HTML template and
two pages in the master sitelet. You can safely remove these unused parts and reduce
`Main.fs` to the following code:

    namespace HtmlApp1

    open IntelliFactory.Html
    open WebSharper
    open WebSharper.Sitelets

    type Action =
        | Home

    module Controls =

        [<Sealed>]
        type EntryPoint() =
            inherit Web.Control()

            [<JavaScript>]
            override __.Body =
                HelloWorld.Main() :> _

    module Site =

        let HomePage =
            Content.PageContent <| fun ctx ->
                { Page.Default with 
                    Title = Some "Home page"
                    Body =
                        [
                            Div [new Controls.EntryPoint()]
                        ]
                }

        let Main =
            Sitelet.Content "/" Home HomePage

    [<Sealed>]
    type Website() =
        interface IWebsite<Action> with
            member this.Sitelet = Site.Main
            member this.Actions = [Home]

    [<assembly: Website(typeof<Website>)>]
    do ()
 

## Testing via client-server applications

While most of the examples on this site are "client-only", a handful of them are actually
client-server applications, such as the [Remoting](/samples/Remoting) and
the [Chat](/samples/Chat) examples.

To run these more complex examples, follow these steps:

 1. Create a new client-server application using the "Client-Server Application" project template.

 2. This project template features three separate files: `Client.fs`, `Remoting.fs` and
    `Main.fs` for the client, server, and putting-together sides of things. To keep things
    simple, you can remove `Remoting.fs` from your project, and keep both tiers in `Client.fs`.

 3. Open `Client.fs` and edit its contents as follows:

   a. Keep the namespace declaration

   b. Override the rest of the file with the code from the example. You don't need
      to carry over the code for the xxx`Viewer` type. Your code should look
      like (with the namespace matching the one you created, and assuming you
      adapted the Chat example):

        namespace Website2

        ...

        module Chat =
            ...

            module Auth =
            ...
            module State =
            ...
            module Rpc =
            ...

            [<JavaScript>]
            module Ui =
                let ChatBox () =
                ...

            [<JavaScript>]
            let Main () = Ui.ChatBox()


 4. Simplify `Main.fs` as follows:

        namespace Website2

        open IntelliFactory.Html
        open WebSharper
        open WebSharper.Sitelets

        type Action =
            | Home

        module Controls =

            [<Sealed>]
            type EntryPoint() =
                inherit Web.Control()

                [<JavaScript>]
                override __.Body =
                    Chat.Main() :> _

        module Site =

            let HomePage =
                Content.PageContent <| fun ctx ->
                    { Page.Default with 
                        Title = Some "Home page"
                        Body =
                            [
                                Div [new Controls.EntryPoint()]
                            ]
                    }

            let Main =
                Sitelet.Content "/" Home HomePage

        [<Sealed>]
        type Website() =
            interface IWebsite<Action> with
                member this.Sitelet = Site.Main
                member this.Actions = [Home]

        type Global() =
            inherit System.Web.HttpApplication()
 
            member g.Application_Start(sender: obj, args: System.EventArgs) =
                ()

        [<assembly: Website(typeof<Website>)>]
        do ()

 5. You can now run your application.

## Testing via ASPX markup

To test via ASPX markup, in addition to the steps outlined in the
[ASP.NET integration](/docs/aspnet) page, you need to configure your
WebSharper control to be visible as an ASP.NET server control.

Assuming the `Samples` namespace as in the above code snippet, 
and `WebSharperProject` as the name of your assembly, you
should add an entry in your `Web.Config` to reference this namespace,
along with the desired tag prefix and the assembly that contains it.

It should look something like this:

``` xml
<configuration>
    <system.web>
		<pages>
			<controls>
			...
				<add tagPrefix="samples"
					 namespace="Samples"
					 assembly="WebSharperProject"/>
			</controls>
			...
```

Once configured and having added the WebSharper script manager tag,
you can embed your WebSharper control in your ASPX markup as:

```
<samples:HelloWorldViewer runat="server" />
```

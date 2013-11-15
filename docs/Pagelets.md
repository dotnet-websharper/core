# Developing Pagelets

In most JavaScript user interface frameworks widget construction goes
through the same three-stage workflow:

* Construct DOM nodes.

* Embed DOM nodes on the page, allowing the browser to calculate their
  position and size.

* Enhance the nodes with custom appearance and behavior.

WebSharper defines the `IPagelet` interface to capture this protocol.
In essense, a pagelet is DOM element combined with a rendering
function:

    type IPagelet =
        abstract member Body : Dom.Node
        abstract member Render : unit -> uni

To use a pagelet, its `Body` element is inserted into the document,
and then its `Render` method is called. This is an important part of
the `IPagelet` protocol not captured by the type system: the `Render`
method is always called exactly once, after the `Body` has been
inserted into the document.

`Render` should rarely, if ever, be called explicitly, as WebSharper
does it automatically for pagelets embedded in web pages.

## HTML Combinators

The most commonly used pagelets in WebSharper are constructed using
the HTML combinators from the `IntelliFactory.WebSharper.Html`.

    Div [Width "200px"] -< [
        H1 [Text "HELLO, WORLD!"]
        P [Text "123.."]
    ]

The above code roughly corresponds to:

    <div width="200px">
        <h1>HELLO, WORLD!</div>
        <p>123...</p>
    </div>

The combinators both consume and produce `IPagelet` values, allowing
to compose simple pagelets into more complex pagelets.

Some attributes such as `Attr.Type` and `Attr.Value` have to be
qualified so as not to clash with common identifiers.

## HTML Events

The combinators support a subset of DOM/HTML events. Event handlers
are attached destructively with functions `OnClick`, `OnMouseDown` and
so on that take a pagelet and mutate its body, returning `unit`. To
assist with compositionality, WebSharper exposes a destructive piping
combinator `|>!` defined as:

    let ( |>! ) x f = f x; x

With this combinator, event handlers can be attached locally:

    Div [
        Input [Attr.Type "button"; Attr.Value "Click me!"]
        |>! OnClick (fun element eventArguments ->
            element.Value <- "Clicked")
    ]

For convenience, the event combinators pass the typed reference to the
event target (the `element` argument in the example above).

## Pagelet Rendering Events

In addition to DOM events, WebSharper exposes combinators to hook into
the rendering event, `OnBeforeRender` and `OnAfterRender`.

Example:

    Div [
        Div [Text "Foo"]
        |>! OnAfterRender (fun div -> ...)
    ]

The initialization logic in the handler is only called once before or
after the pagelet is attached to the page.

## Custom Pagelets

Custom pagelets can be created by implementing `IPagelet` directly, or
by using the HTML combinators.

## Developing ASP.NET Controls and Pages

`WebSharper` integrates with ASP.NET by exposing pagelets as ASP.NET
controls, for example:

    let MyPagelet : IPagelet =
        Div [Text "Hello World!"] :> _

    type MyControl() =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body = MyPagelet

With these definitions, in ASP.NET code you now can use:

    <MyControl runat="server" />

The mechanics are as follows:

* An instance of the control is constructed on the server.

* The instance is serialized to JSON that is emitted with the page.

* The instance is deserialized on the client, the pagelet is
  reconstructed and rendered into the placeholder corresponding to the
  location of the control.

In this way the controls can have fields that pass information from
the server-side to the client-side context.

Every page that uses `WebSharper` controls is required to include the
`IntelliFactory.WebSharper.Web.ScriptManager` control in the `<head>`
section:

    <head runat="server">
    <ws:ScriptManager runat="server" />

During rendering, `ScriptManager` emits a correctly ordered transitive
closure of all JavaScript and CSS resources required by the controls
on the page.

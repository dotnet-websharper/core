# Using HTML Combinators

WebSharper introduces a syntax to create HTML markup directly in F#. This syntax is available in two flavors with distinct types and purposes:

* Server-side combinators from the `WebSharper.Html.Server` namespace are used to generate the markup of the served HTML page.

* Client-side combinators from the `WebSharper.Html.Client` namespace are used to generate DOM nodes on the client.

## Combinator Syntax

The basic syntax to create an HTML element is to call the function with its (capitalized) name and pass it a sequence representing its contents. WebSharper knows which tags need an open and a close tag and which need a self-closing tag.

```fsharp
let myDiv = Div []
// HTML: <div></div>

let myRule = HR []
// HTML: <hr/>

let mySection = Section [HR []]
// HTML: <section><hr/></section>
```

Text nodes are represented using the function `Text`. They are automatically escaped.

```fsharp
let myList =
    UL [
        LI [Text "First item"]
        LI [Text "Second item"]
    ]
// HTML: <ul>
//         <li>First item</li>
//         <li>Second item</li>
//       </ul>

let mySpan = Span [Text "some content & some <entities>."]
// HTML: <span>some content &amp; some &lt;entities&gt;.</span>
```

Attributes are represented by functions with their (capitalized) name that take a string as argument.

```fsharp
let myImage = Img [Src "/logo.png"]
// HTML: <img src="/logo.png" />
```

Because text nodes, attributes and elements have different types, putting them in the same list can throw off F#'s type inference. Instead, you can use the operator `-<` to keep adding more attributes or children to a node.

```fsharp
let myP = P [Class "paragraph"] -< [Text "This is a paragraph."]
// HTML: <p class="paragraph">This is a paragraph.</p>
```

Some tags and attributes are not exposed directly when opening `WebSharper.Html.{Client|Server}`, because their name would collide with existing element languages or common WebSharper patterns. In such a case, you can find them under the modules `Tags` and `Attr`, respectively.

```fsharp
let myForm =
    Form [Attr.Action "/post"] -< [
        Select [
            Tags.Option [Text "Option 1."]
            Tags.Option [Text "Option 2."]
        ]
    ]
// HTML: <form action="/post">
//         <select>
//           <option>Option 1.</option>
//           <option>Option 2.</option>
//         </select>
//       </form>
```

## Embedding Client-Side Controls in Server-Side Markup

Integrating client-side content in server-side content is done via `WebSharper.Web.Control`.

```fsharp
module Client =
    open WebSharper.Html.Client

    type MyControl() =
        inherit WebSharper.Web.Control ()

        [<JavaScript>]
        override this.Body =
            I [Text "Client control"] :> _

module Server =
    open WebSharper.Html.Server

    let Page : Content<Action> =
        PageContent <| fun context ->
            {Page.Default with
                Title = Some "Index"
                Body =
                    [
                        Div [ new Client.MyControl ()]
                    ]
            }
```

Here, `MyControl` inherits from `WebSharper.Web.Control` and overrides the `Body` property with some client-side HTML. This control is then placed within a server-side `div` tag, and its contents is created on page load.

Web Controls can also be included in an ASP.NET page, as shown [here](aspnet).

## Client-Side Specificities

### Event Handlers

Client-side HTML can be augmented with event handlers. The syntax is as follows:

```fsharp
let myButton =
    Button [Text "Click me!"]
    |>! OnClick (fun button event ->
        button.Text <- "Clicked!"
    )
```

If you want to bind an event for which there is no provided `On*` function, then you can use `OnEvent` with the name of the event:

```fsharp
let myInput =
    Input []
    |>! OnEvent "paste" (fun input event ->
        JS.Alert "Some content was pasted into the input."
    )
```

### Rendering

Client-side elements, attributes and text nodes all inherit from the type `Pagelet`. This type includes a method `Render()` that is called exactly once when the element is inserted into the DOM. The event handler `OnAfterRender` can be used to run code right after the element has been inserted:

```fsharp
let alertOnStartup =
    Span []
    |>! OnAfterRender (fun span ->
        JS.Alert "The span was just inserted in the DOM."
    )
```

### Element Members

Client-side HTML elements have a number of properties and methods to facilitate their dynamic use. Here are the most useful:

* The `Text` property gets and sets the text content of a node.

* The `Html` property gets and sets the HTML content of a node. Be careful, inserting user-generated content in this way can cause security issues!

* The `Dom` property gets the underlying `Dom.Element`. Similarly, for all Pagelets, the `Body` property gets the underlying `Dom.Node`.

* `SetAttribute`, `RemoveAttribute`, `AddClass`, `RemoveClass`, `SetCss` modify the attributes of the element.

* `Append` adds a child element. If:
    * the current element is in the DOM,
    * the child is a `Pagelet`,
    * and the child's `Render` hasn't been called yet,

    then the child's `Render` is called.

* `AppendTo` inserts this element into the DOM as a child of the element with the given id, and calls `Render` if necessary.

* `Remove` detaches this element from the DOM.

* `Clear` removes all of this element's children from the DOM.
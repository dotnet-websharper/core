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

Integrating client-side content in server-side content can be done in two ways: using the `ClientSide` function, or by explicitly creating a web control class.

### The `ClientSide` function

Since WebSharper 3.0.60, the easiest way to embed client-side code in server-side markup is by calling the `ClientSide` function with a quotation containing your client-side element.

```fsharp
[<JavaScript>]
module Client =
    open WebSharper.Html.Client

    let myContent() = I [Text "Client control"]

module Server =
    open WebSharper.Html.Server

    let Page : Content<Action> =
        PageContent <| fun context ->
            {Page.Default with
                Title = Some "Index"
                Body =
                    [
                        Div [ ClientSide <@ Client.myContent() @> ]
                    ]
            }
```

The generated server-side HTML will contain a placeholder `div` at the location of the `ClientSide` call, which is dynamically replaced by `myContent()` on page load.

There are several constraints when using `ClientSide`:

* The body of the quotation must be a call to a top-level value or function, or a static method or property.
* Any arguments passed inside the quotation must be either literals or references to local variables.
* Arguments must be serializable by WebSharper.

For example, given the following client-side code:

```fsharp
[<JavaScript>]
module Client =
    open WebSharper.Html.Client

    let myContent text1 text2 = I [Text (text1 + "; " + text2)]
```

The following is valid:

```
module Server =
    open WebSharper.Html.Server

    let Body =
        let t = "a local variable"
        Div [ ClientSide <@ Client.myContent t "a literal" @> ]
```

But the following isn't:

```
module Server =
    open WebSharper.Html.Server

    // Invalid: the quotation must be a call to a global
    // value, function, property or method.
    let Body1 =
        let t1 = "a global variable"
        let body = Client.myContent t1 "a literal"
        Div [ClientSide <@ body @>]

    // Invalid: the argument can't be a compound expression.
    let Body2 =
        let t2 = "a local variable"
        Div [ ClientSide <@ Client.myContent t2 ("an" + " expression") @> ]

    // Invalid: the argument can't be a global variable.
    let t3 = "a global variable"
    let Body3 = Div [ ClientSide <@ Client.myContent t3 "a literal" @> ]

    // If you want to pass global variables or compound expressions as argument,
    // you can alias them locally:
    let t4 = "a global variable"
    let Body4 =
        let t4 = t4
        let t5 = "an" + " expression"
        Div [ ClientSide <@ Client.myContent t4 t5 @> ]
```

### Explicit `Web.Control`

The more verbose way to embed client-side code is to create a control class.

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

Here, `MyControl` inherits from `WebSharper.Web.Control` and overrides the `Body` property with some client-side HTML. As with `ClientSide`, the generated server-side HTML will contain a placeholder `div` at the location of the `MyControl` call, which is dynamically replaced by `Body` on page load.

Any WebSharper-serializable data can be passed to `MyControl`'s constructor, without the local-or-literal limitation that `ClientSide` has.

Another advantage of creating your own web control class is that it can also be included in an ASP.NET page, as shown [here](aspnet).

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
# WebSharper Extensions for DOM

DOM types can be found under the `WebSharper.JavaScript.Dom`
namespace.  The extension is based on the DOM specification, and
therefore does not provide any browser-specific methods, such as
`innerHtml`.  Be warned that DOM compliance varies from browser to
browser, and therefore relying on DOM may cause your code to be
browser-specific.

## WebSharper Elements and DOM Nodes

WebSharper HTML elements (from `WebSharper.Html.Client`) create and
instantiate DOM nodes lazily as they are attached to the document and
rendered. A typical way to access these DOM nodes after they are
rendered is using the `OnAfterRender` function:

    Div []
    |>! OnAfterRender (fun element ->
        element.Text <- "Hello World")

## The Document Object

The DOM `Document` object instance accessed as `document` from
JavaScript is available in F# as:

    let doc = JS.Document
    let doctype = doc.Doctype.Name
    let uri = doc.DocumentURI

## Events

You can easily add event listeners to any DOM element as follows:

    let listener = fun () -> div2.TextContent <- "Clicked!"
    div1.AddEventListener("click",listener,false)

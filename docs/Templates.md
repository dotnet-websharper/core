# HTML Templates

Default sitelet projects will contain a `Main.html`
file similar to the following:

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>${title}</title>
    <meta name="generator" content="websharper" data-replace="scripts" />
</head>
<body>
  <div data-replace="body">
  </div>
</body>
</html>
```

This is a template file, specifying a simple HTML structure.
Templates are currently parsed with an XML parser, and may contain
holes allowing dynamic elements to be inserted. Holes are defined
using a special syntax:

1. `${NameOfHole}` declares for a placeholder for textual substitution
   and is allowed in text nodes and attribute value positions.

2. `data-replace="NameOfHole"` and `data-hole="NameOfHole"` declare an
   element to be a placeholder that is either substituted
   (`data-replace`) or filled with (`data-hole`) nodes.

A typical use of the templates can be found in the `Skin` module in
the default sitelet templates. Here, a collection of holes and their
types is declaratively defined using F# combinators, and an instance
of the template is specified using a webroot-relative path such as
`~/Main.html`:

```fsharp
module Skin =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Sitelets
 
    type Page =
        {
            Title : string
            Body : list<Content.HtmlElement>
        }
 
    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)
```

With these definitions in place, you can easily define convenience
functions for building `Content` objects required by sitelets:

```fsharp
let WithTemplate title body : Content<Action> =
    Content.WithTemplate MainTemplate <| fun context ->
        {
            Title = title
            Body = body context
        }
```

A special placeholder named "scripts" is replaced with a series of
JavaScript and CSS resource links to support the client-side part of
your application.

# HTML Templates

## Syntax

Default sitelet projects will contain a `Main.html`
file similar to the following:

```xml
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

## Usage for a full page

A typical use of the templates can be found in the `Skin` module in
the default sitelet templates. Here, a collection of holes and their
types is declaratively defined using F# combinators, and an instance
of the template is specified using a webroot-relative path such as
`~/Main.html`:

```fsharp
module Skin =
    open WebSharper
    open WebSharper.Sitelets
    open WebSharper.Html.Server
 
    type Page =
        {
            Title : string
            Body : list<Element>
        }
 
    let MainTemplate : Content.Template<Page> =
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

## Usage for a snippet

You can also use a template to contain just a part of a page, with the method `Run` which returns a `list<Element>`. For example, with this content in `Article.html`:

```xml
<article>
<h2>${title}</h2>
<section class="main" data-hole="content"></section>
</article>
```

You can insert it in a page with the following:

```fsharp
type Article =
    {
        Title : string
        Content : list<Element>
    }

let ArticleTemplate : Content.Template<Article> =
        Content.Template<Article>("~/Article.html")
            .With("title", fun x -> x.Title)
            .With("content", fun x -> x.Content)

let MainPage : Content<Action> =
    Skin.WithTemplate "My blog" <| fun context ->
		let articles = GetArticles() // eg. from database
		[
			yield H1 [Text "Welcome to my blog!"]
			for article in articles do
				yield! ArticleTemplate.Run(article, context.RootFolder)
		]
```
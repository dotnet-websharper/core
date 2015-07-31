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

With these definitions in place, you can easily create [Sitelet content](Sitelets.md#content) with `Content.WithTemplate`:

```fsharp
    let MainPage (context: Context<EndPoint>) : Async<Content<EndPoint>> =
        Content.WithTemplate MainTemplate
            {
                Title = "My page"
                Body = [H1 [Text "Body of the page]]
            }
```

Three special placeholders are provided to include client-side content in the page:

* `scripts` is replaced with the JavaScript files required by the client-side code included in the page.
* `styles` is replaced with the CSS files required by the client-side code included in the page.
* `meta` is replaced with a `<meta>` tag that contains initialization data for client-side controls.

If neither `styles` nor `meta` is provided, then `scripts` is replaced with the normal content of `meta`, `styles` and `scripts`.

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

let ArticleTemplate =
        Content.Template<Article>("~/Article.html")
            .With("title", fun x -> x.Title)
            .With("content", fun x -> x.Content)

let MainPage (context: Context<EndPoints>) =
    let articles = GetArticles() // eg. from database
    Content.WithTemplate MainTemplate
		{
			Title = "Welcome to my blog!"
			Body = [
				yield H1 [Text "Welcome to my blog!"]
				for article in articles do
					yield! ArticleTemplate.Run(article, context.RootFolder)
			]
		}
```
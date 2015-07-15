# Developing With Sitelets

Sitelets are WebSharper's primary way to create server-side content. They provide facilities to route requests and generate HTML pages or JSON responses.

Sitelets allow you to:

* Dynamically construct pages and serve arbitrary content.

* Have full control of your URLs by specifying [custom routers](#advanced-sitelets) for linking them to content, or let the URLs be [automatically inferred](#sitelet-infer) from an action type.

* Compose contents into sitelets, which may themselves be [composed into larger sitelets](#sitelet-combinators).

* Have [safe links](#linking) for referencing other content contained within your site.

* Use the type-safe HTML [templating facilities](Templates.md) that come with sitelets.

* Automatically [parse JSON requests and generate JSON responses](Json.md) based on your types.

Below is a minimal example of a complete site serving one HTML page:

```fsharp
namespace SampleWebsite

open WebSharper.Sitelets

module SampleSite =
    open WebSharper
    open WebSharper.Html.Server

    type Action =
        | Index

    let IndexContent : Content<Action> =
        PageContent <| fun context ->
            {Page.Default with
                Title = Some "Index"
                Body =
                    let time = System.DateTime.Now.ToString()
                    [H1 [Text <| "Current time: " + time]]}

    type MySampleWebsite() =
        interface IWebsite<Action> with
            member this.Sitelet =
                Sitelet.Content "/index" Action.Index IndexContent
            member this.Actions = []

[<assembly: WebsiteAttribute(typeof<SampleSite.MySampleWebsite>)>]
do ()
```

First, a custom action type is defined. It is used for linking URLs to content within your sitelet. Here, you only need one action, `Action.Index` corresponding to your only page.

The content of the index page is defined as a `PageContent`, where the body consists of a server side HTML element.  Here the current time is computed and displayed within an H1 tag.

The `MySampleWebsite` type specifies the sitelet to be served by implementing the `IWebsite` interface.  In this case, the sitelet is defined using the `Sitelet.Content` operator for constructing a sitelet of the Index content.  In the resulting sitelet, the `Action.Index` value is associated with the path `/index` and the given content.

## Sitelet routing

WebSharper Sitelets abstract away URLs by using an action type that represents the different pages available in a website. For example, a site's URL scheme can be represented by the following action type:

```fsharp
type Action =
    | Index
    | Stats of username: string
    | BlogArticle of id: int * slug: string
```

Based on this, a Sitelet is a value that represents the following mappings:

* Mapping from URLs to actions. A Sitelet is able to parse a URL such as `/blog/1243/some-article-slug` into the action value `BlogArticle (id = 1243, slug = "some-article-slug").`

* Mapping from actions to URLs. This allows you to have internal links that are verified by the type system, instead of writing URLs by hand and being at the mercy of a typo or a change in the URL scheme. You can read more on this [in the "Context" section](#context).

* Mapping from actions to content. Finally, once a URL has been parsed, this determines what content (HTML or other) must be returned to the client.

A number of primitives are available to create and compose Sitelets.

<a name="sitelet-infer"></a>
### Sitelet.Infer

The easiest way to create a Sitelet is to automatically generate URLs from the shape of your action type using `Sitelet.Infer`. This function parses slash-separated path segments into the corresponding Action value, and lets you match this action and return the appropriate content. Here is an example sitelet using `Infer`:

```fsharp
open WebSharper.Sitelets

type Action =
    | Index
    | Stats of username: string
    | BlogArticle of id: int * slug: string

type MyWebsite() =
    interface IWebsite<Action> with
        member this.Sitelet =
            Sitelet.Infer <| function
                | Index ->
                    // Content of the index page
                    Content.PageContent <| fun ctx ->
                        { Page.Default with
                            Title = Some "Welcome!"
                            Body = [H1 [Text "Index page"]] }
                | Stats username ->
                    // Content of the stats page, which depends on the username
                    Content.PageContent <| fun ctx ->
                        { Page.Default with
                            Body = [Text ("Stats for " + username)] }
                | BlogArticle (id, slug) ->
                    // Content of the article page, which depends on id and slug
                    Content.PageContent <| fun ctx ->
                        { Page.Default with
                            Body = [Text (sprintf "Article id %i, slug %s" id slug)] }

        member this.Actions = []

[<assembly: WebsiteAttribute(typeof<MyWebsite>)>]
do ()
```

The above sitelets accepts URLs with the following shape:

```xml
Accepted Request:    GET /Index
Parsed Action:       Index
Returned Content:    <!DOCTYPE html>
                     <html>
                         <head><title>Welcome!</title></head>
                         <body>
                             <h1>Index page</h1>
                         </body>
                     </html>

Accepted Request:    GET /Stats/someUser
Parsed Action:       Stats (username = "someUser")
Returned Content:    <!DOCTYPE html>
                     <html>
                         <head><title></title></head>
                         <body>
                             Stats for someUser
                         </body>
                     </html>

Accepted Request:    GET /BlogArticle/1423/some-article-slug
Parsed Action:       BlogArticle (id = 1423, slug = "some-article-slug")
Returned Content:    <!DOCTYPE html>
                     <html>
                         <head><title></title></head>
                         <body>
                             Article id 1423, slug some-article-slug
                         </body>
                     </html>
```

The following types are accepted by `Sitelet.Infer`:

* Numbers and strings are encoded as a single path segment.

```fsharp
type Action = string

// Accepted Request:    GET /abc
// Parsed Action:       "abc"
// Returned Content:    (determined by Sitelet.Infer)

type Action = int

// Accepted Request:    GET /1423
// Parsed Action:       1423
// Returned Content:    (determined by Sitelet.Infer)
```

* Tuples and records are encoded as consecutive path segments.

```fsharp
type Action = int * string

// Accepted Request:    GET /1/abc
// Parsed Action:       (1, "abc")
// Returned Content:    (determined by Sitelet.Infer)

type Action = { Number : int; Name : string }

// Accepted Request:    GET /1/abc
// Parsed Action:       { Number = 1; Name = "abc" }
// Returned Content:    (determined by Sitelet.Infer)
```

* Union types are encoded as a path segment identifying the case, followed by segments for the arguments (see the example above).

```fsharp
type Action = string option

// Accepted Request:    GET /Some/abc
// Parsed Action:       Some "abc"
// Returned Content:    (determined by Sitelet.Infer)
//
// Accepted Request:    GET /None
// Parsed Action:       None
// Returned Content:    (determined by Sitelet.Infer)
```

* Lists and arrays are encoded as a number representing the length, followed by each element. For example:

```fsharp
type Action = string list

// Accepted Request:    GET /2/abc/def
// Parsed Action:       ["abc"; "def"]
// Returned Content:    (determined by Sitelet.Infer)
```

* Enumerations are encoded as their underlying type.

```fsharp
type Action = System.IO.FileAccess
// Accepted Request:    GET /3
// Parsed Action:       System.IO.FileAccess.ReadWrite
// Returned Content:    (determined by Sitelet.Infer)
```

* `System.DateTime` is serialized with the format `yyyy-MM-dd-HH.mm.ss`. See below to customize this format.

```fsharp
type Action = System.DateTime
// Accepted Request:    GET /2015-03-24-15.05.32
// Parsed Action:       System.DateTime(2015,3,24,15,5,32)
// Returned Content:    (determined by Sitelet.Infer)
```

### Customizing Sitelet.Infer

It is possible to annotate your Action type with attributes to customize the `Sitelet.Infer`'s request inference. Here are the available attributes:

* `[<Method("GET", "POST", ...)>]` on a union case indicates which methods are parsed by this action.

```fsharp
type Action =
    | [<Method "POST">] PostArticle of id: int

// Accepted Request:    POST /PostArticle/12
// Parsed Action:       PostArticle 12
// Returned Content:    (determined by Sitelet.Infer)
```

* `[<EndPoint "/string">]` on a union case indicates the identifying segment.

```fsharp
type Action =
    | [<EndPoint "/blog-article">] BlogArticle of id: int * slug: string

// Accepted Request:    GET /blog-article/1423/some-article-slug
// Parsed Action:       BlogArticle(id = 1423, slug = "some-article-slug")
// Returned Content:    (determined by Sitelet.Infer)
```

* A common trick is to use `[<EndPoint "/">]` on an argument-less union case to indicate the home page.

```fsharp
type Action =
    | [<EndPoint "/">] Home

// Accepted Request:    GET /
// Parsed Action:       Home
// Returned Content:    (determined by Sitelet.Infer)
```

* If several cases have the same `EndPoint`, then parsing tries them in the order in which they are declared until one of them matches:

```fsharp
type Action =
  | [<EndPoint "/blog">] AllArticles
  | [<EndPoint "/blog">] ArticleById of id: int
  | [<EndPoint "/blog">] ArticleBySlug of slug: string

// Accepted Request:    GET /blog
// Parsed Action:       AllArticles
// Returned Content:    (determined by Sitelet.Infer)
//
// Accepted Request:    GET /blog/123
// Parsed Action:       ArticleById 123
// Returned Content:    (determined by Sitelet.Infer)
//
// Accepted Request:    GET /blog/my-article
// Parsed Action:       ArticleBySlug "my-article"
// Returned Content:    (determined by Sitelet.Infer)
```

* `[<Query("arg1", "arg2", ...)>]` on a union case indicates that the fields with the given names must be parsed as GET query parameters instead of path segments. The value of this field must be either a base type (number, string) or an option of a base type (in which case the parameter is optional).

```fsharp
type Action =
    | [<Query("id", "slug")>] BlogArticle of id: int * slug: string option

// Accepted Request:    GET /BlogArticle?id=1423&slug=some-article-slug
// Parsed Action:       BlogArticle(id = 1423, slug = Some "some-article-slug")
// Returned Content:    (determined by Sitelet.Infer)
//
// Accepted Request:    GET /BlogArticle?id=1423
// Parsed Action:       BlogArticle(id = 1423, slug = None)
// Returned Content:    (determined by Sitelet.Infer)
```

* You can of course mix Query and non-Query parameters.

```fsharp
type Action =
    | [<Query("slug")>] BlogArticle of id: int * slug: string option

// Accepted Request:    GET /BlogArticle/1423?slug=some-article-slug
// Parsed Action:       BlogArticle(id = 1423, slug = Some "some-article-slug")
// Returned Content:    (determined by Sitelet.Infer)
```

* Similarly, `[<Query>]` on a record field indicates that this field must be parsed as a GET query parameter.

```fsharp
type Action =
    {
        id : int
        [<Query>] slug : string option
    }

// Accepted Request:    GET /1423?slug=some-article-slug
// Parsed Action:       { id = 1423; slug = Some "some-article-slug" }
// Returned Content:    (determined by Sitelet.Infer)
```

<a name="json-request"></a>

* `[<Json "arg">]` on a union case indicates that the field with the given name must be parsed as JSON from the body of the request. If an action type contains several `[<Json>]` fields, a compile-time error is thrown.

    [Learn more about JSON parsing.](Json.md)

```fsharp
type Action =
    | [<Method "POST"; Json "data">] PostBlog of id: int * data: BlogData
and BlogData =
    {
        slug: string
        title: string
    }

// Accepted Request:    POST /PostBlog/1423
//
//                      {"slug": "some-blog-post", "title": "Some blog post!"}
//
// Parsed Action:       PostBlog(
//                          id = 1423,
//                          data = { slug = "some-blog-post"
//                                   title = "Some blog post!" })
// Returned Content:    (determined by Sitelet.Infer)
```

* Similarly, `[<Json>]` on a record field indicates that this field must be parsed as JSON from the body of the request.

```fsharp
type Action =
    | [<Method "POST">] PostBlog of BlogPostArgs
and BlogPostArgs =
    {
        id: int
        [<Json>] data: BlogData
    }
and BlogData =
    {
        slug: string
        title: string
    }

// Accepted Request:    POST /PostBlog/1423
//
//                      {"slug": "some-blog-post", "title": "Some blog post!"}
//
// Parsed Action:       PostBlog { id = 1423,
//                                 data = { slug = "some-blog-post"
//                                          title = "Some blog post!" } }
// Returned Content:    (determined by Sitelet.Infer)
```

* `[<FormData("arg1", "arg2", ...)>]` on a union case indicates that the fields with the given names must be parsed from the body as form data (`application/x-www-form-urlencoded` or `multipart/form-data`) instead of path segments. The value of this field must be either a base type (number, string) or an option of a base type (in which case the parameter is optional).

```fsharp
type Action =
    | [<FormData("id", "slug")>] BlogArticle of id: int * slug: string option

// Accepted Request:    POST /BlogArticle
//                      Content-Type: application/x-www-form-urlencoded
//
//                      id=1423&slug=some-article-slug
//
// Parsed Action:       BlogArticle(id = 1423, slug = Some "some-article-slug")
// Returned Content:    (determined by Sitelet.Infer)
//
// Accepted Request:    POST /BlogArticle
//                      Content-Type: application/x-www-form-urlencoded
//
//                      id=1423
//
// Parsed Action:       BlogArticle(id = 1423, slug = None)
// Returned Content:    (determined by Sitelet.Infer)
```

* Similarly, `[<FormData>]` on a record field indicates that this field must be parsed from the body as form data.

```fsharp
type Action =
    {
        id : int
        [<FormData>] slug : string option
    }

// Accepted Request:    POST /1423
//                      Content-Type: application/x-www-form-urlencoded
//
//                      slug=some-article-slug
//
// Parsed Action:       { id = 1423; slug = Some "some-article-slug" }
// Returned Content:    (determined by Sitelet.Infer)
```

* `[<DateTimeFormat(string)>]` on a record field or named union case field of type `System.DateTime` indicates the date format to use. Be careful as some characters are not valid in URLs; in particular, the ISO 8601 round-trip format (`"o"` format) cannot be used because it uses the character `:`.

```fsharp
type Action =
    {
        [<DateTimeFormat "yyyy-MM-dd">] dateOnly: System.DateTime
    }

// Accepted Request:    GET /2015-03-24
// Parsed Action:       System.DateTime(2015,3,24)
// Returned Content:    (determined by Sitelet.Infer)

type Action =
    | [<DateTimeFormat("time", "HH.mm.ss")>] A of time: System.DateTime

// Accepted Request:    GET /A/15.05.32
// Parsed Action:       A (System.DateTime(2015,3,24,15,5,32))
// Returned Content:    (determined by Sitelet.Infer)
```

* `[<Wildcard>]` on a union case indicates that the last argument represents the remainder of the url's path. That argument can be a `list<'T>`, an `array<'T>`, or a `string`.

```fsharp
type Action =
    | [<Wildcard>] Articles of pageId: int * tags: list<string>
    | [<Wildcard>] Articles2 of array<int * string>
    | [<Wildcard>] GetFile of path: string

// Accepted Request:    GET /Articles/123/fsharp/websharper
// Parsed Action:       Articles(123, ["fsharp"; "websharper"])
// Returned Content:    (determined by Sitelet.Infer)
//
// Accepted Request:    GET /Articles/123/fsharp/456/websharper
// Parsed Action:       Articles2 [(123, "fsharp"); (456, "websharper")]
// Returned Content:    (determined by Sitelet.Infer)
//
// Accepted Request:    GET /GetFile/css/main.css
// Parsed Action:       GetFile "css/main.css"
// Returned Content:    (determined by Sitelet.Infer)
```

### Catching wrong requests with Sitelet.InferWithErrors

By default, `Sitelet.Infer` ignores requests that it fails to parse, in order to give potential other components (such as [ASP.NET](http://websharper.com/docs/aspnet)) a chance to respond to the request. However, if you want to send a custom response for badly-formatted requests, you can use `Sitelet.InferWithErrors` instead. This function wraps the parsed request in the `ActionEncoding.DecodeResult<'Action>` union. Here are the cases you can match against:

* `ActionEncoding.Success of 'Action`: The request was successfully parsed.

* `ActionEncoding.InvalidMethod of 'Action * method: string`: An action was successfully parsed but with the given wrong HTTP method.

* `ActionEncoding.MissingQueryParameter of 'Action * name: string`: The URL path was successfully parsed but a mandatory query parameter with the given name was missing. The action contains a default value (`Unchecked.defaultof<_>`) where the query parameter value should be.

* `ActionEncoding.InvalidJson of 'Action`: The URL was successfully parsed but the JSON body wasn't. The action contains a default value (`Unchecked.defaultof<_>`) where the JSON-decoded value should be.

* `ActionEncoding.MissingFormData of 'Action * name: string`: The URL was successfully parsed but a form data parameter with the given name was missing or wrongly formatted. The action contains a default value (`Unchecked.defaultof<_>`) where the form body-decoded value should be.

If the URL path isn't matched, then the request falls through as with `Sitelet.Infer`.

```fsharp
type Action =
    | [<Method "GET"; Query "page">] Articles of page: int

let MySitelet = Sitelet.InferWithErrors <| function
    | Success (Articles page) ->
        Content.JsonContent <| fun ctx ->
            "serving page " + string page
    | InvalidMethod (_, m) ->
        Content.JsonContent <| fun ctx ->
            "Invalid method: " + m
        |> Content.SetStatus (Http.Status.MethodNotAllowed)
    | MissingQueryParameter (_, p) ->
        Content.JsonContent <| fun ctx ->
            "Missing parameter: " + p
        |> Content.SetStatus (Http.Status.Custom 400 (Some "Bad Request"))
    | _ ->
        Content.JsonContent <| fun ctx ->
            "We don't have JSON or FormData, so this shouldn't happen"
        |> Content.SetStatus Http.Status.InternalServerError

// Accepted Request:    GET /Articles?page=123
// Parsed Action:       Articles 123
// Returned Content:    200 Ok
//                      "serving page 123"
//
// Accepted Request:    POST /Articles?page=123
// Parsed Action:       InvalidMethod(Articles 123, "POST")
// Returned Content:    405 Method Not Allowed
//                      "Invalid method: POST"
//
// Accepted Request:    GET /Articles
// Parsed Action:       MissingQueryParameter(Articles 0, "page")
// Returned Content:    400 Bad Request
//                      "Missing parameter: page"
//
// Request:             GET /this-path-doesnt-exist
// Parsed Action:       (none)
// Returned Content:    (not found page provided by the host)
```

<a name="sitelet-combinators"></a>
### Other Constructors and Combinators

The following functions are available to build simple sitelets or compose more complex sitelets out of simple ones:

* `Sitelet.Content`, as shown in the first example, builds a sitelet that accepts a single URL and maps it to a given action and content.

```fsharp
Sitelet.Content "/index" Index IndexContent

// Accepted Request:    GET /index
// Parsed Action:       Index
// Returned Content:    (value of IndexContent : Content<Action>)
```

* `Sitelet.Sum` takes a sequence of Sitelets and tries them in order until one of them accepts the URL. It is generally used to combine a list of `Sitelet.Content`s.

  The following sitelet accepts `/index` and `/about`:

```fsharp
Sitelet.Sum [
    Sitelet.Content "/index" Index IndexContent
    Sitelet.Content "/about" About AboutContent
]

// Accepted Request:    GET /index
// Parsed Action:       Index
// Returned Content:    (value of IndexContent : Content<Action>)
//
// Accepted Request:    GET /about
// Parsed Action:       About
// Returned Content:    (value of AboutContent : Content<Action>)
```

* `<|>` takes two Sitelets and tries them in order. `s1 <|> s2` is equivalent to `Sitelet.Sum [s1; s2]`.

```fsharp
Sitelet.Content "/index" Index IndexContent
<|>
Sitelet.Content "/about" About AboutContent

// Accepted Request:    GET /index
// Parsed Action:       Index
// Returned Content:    (value of IndexContent : Content<Action>)
//
// Accepted Request:    GET /about
// Parsed Action:       About
// Returned Content:    (value of AboutContent : Content<Action>)
```

* `Sitelet.Shift` takes a Sitelet and shifts it by a path segment.

```fsharp
Sitelet.Content "index" Index IndexContent
|> Sitelet.Shift "folder"

// Accepted Request:    GET /folder/index
// Parsed Action:       Index
// Returned Content:    (value of IndexContent : Content<Action>)
```

* `Sitelet.Folder` takes a sequence of Sitelets and shifts them by a path segment. It is effectively a combination of `Sum` and `Shift`.

```fsharp
Sitelet.Folder "folder" [
    Sitelet.Content "/index" Index IndexContent
    Sitelet.Content "/about" About AboutContent
]

// Accepted Request:    GET /folder/index
// Parsed Action:       Index
// Returned Content:    (value of IndexContent : Content<Action>)
//
// Accepted Request:    GET /folder/about
// Parsed Action:       About
// Returned Content:    (value of AboutContent : Content<Action>)
```

* `Sitelet.Protect` creates protected content, i.e.  content only available for authenticated users:

```fsharp
module Sitelet =
    type Filter<'Action> =
        {
            VerifyUser : string -> bool;
            LoginRedirect : 'Action -> 'Action
        }

val Protect : Filter<'Action> -> Sitelet<'Action> -> Sitelet<'Action>
```

Given a filter value and a sitelet, `Protect` returns a new sitelet that requires a logged in user that passes the `VerifyUser` predicate, specified by the filter.  If the user is not logged in, or the predicate returns false, the request is redirected to the action specified by the `LoginRedirect` function specified by the filter. [See here how to log users in and out.](WebContext.md)

<a name="content"></a>
## Content

Content is conceptually a function from a context to an HTTP response. There are four main ways to create content, each with a synchronous and an asynchronous variant:

```fsharp
module Content =
    open Http

    val PageContent                       : (Context<'Action> -> Page)            -> Content<'Action>
    val PageContentAsync                  : (Context<'Action> -> Async<Page>)     -> Content<'Action>

    val WithTemplate      : Template<'T> -> (Context<'Action> -> 'T)              -> Content<'Action>
    val WithTemplateAsync : Template<'T> -> (Context<'Action> -> Async<'T>)       -> Content<'Action>

    val JsonContent                       : (Context<'Action> -> 'T)              -> Content<'Action>
    val JsonContentAsync                  : (Context<'Action> -> Async<'T>)       -> Content<'Action>

    val CustomContent                     : (Context<'Action> -> Response)        -> Content<'Action>
    val CustomContentAsync                : (Context<'Action> -> Async<Response>) -> Content<'Action>
```

Each of them receives as argument a callback from `Context<_>` to the type of content to return. The context contains various runtime information, notably on how to resolve links and authenticate users. [See here for more information about using the context.](#context)

### PageContent

The first type of content you can return is HTML content, using `PageContent` or `PageContentAsync`. Here is a simple example:

```fsharp
let IndexPage : Content<Action> =
    PageContent <| fun context ->
        { Page.Default with
            Title = Some "Welcome!"
            Head = [ Link [HRef "/css/style.css"; Rel "stylesheet"] ]
            Body = [ H1 [Text "Welcome to my site."] ]
        }
```

The value you must return from the content callback is of record type `Page`. It is recommended to use record update on the provided `Page.Default`, as shown above.

The fields `Head` and `Body` have type `seq<Html.Server.Element>`. [See here for more information about generating HTML content with `Html.Server`.](HtmlCombinators.md)

### WithTemplate

Very often, most of a page is constant, and only parts of it need to be generated. Templates allow you to use a static HTML file for the main structure, with placeholders for generated content. [See here for more information about templates.](Templates.md)

<a name="json-response"></a>
### JsonContent

If you are creating a web API, then Sitelets can automatically generate JSON content for you based on the type of your data. Simply return your value from the content callback, and WebSharper will serialize it. The format is the same as when parsing requests. [See here for more information about the JSON format.](Json.md)

```fsharp
type BlogArticleResponse =
    {
        id: int
        slug: string
        title: string
    }

let content id =
    Content.JsonContent <| fun context ->
        {
            id = id
            slug = "some-blog-article"
            title = "Some blog article!"
        }

type Action =
    | GetBlogArticle of id: int

let sitelet = Sitelet.Infer <| function
    | GetBlogArticle id -> content id

// Accepted Request:    GET /GetBlogArticle/1423
// Parsed Action:       GetBlogArticle 1423
// Returned Content:    {"id": 1423, "slug": "some-blog-article", "title": "Some blog article!"}
```

### CustomContent

CustomContent can be used to output any type of content. The value you must return from the content callback has the following record type:

```fsharp
module Http =
    type Response =
        {
            Status : Status
            Headers : seq<Header>
            WriteBody : System.IO.Stream -> unit
        }
```

* `Status` is the HTTP status code. It can be created using the function `Http.Status.Custom`, or you can use one of the predefined statuses such as `Http.Status.Forbidden`.

* `Headers` is the HTTP headers. You can create them using the function `Http.Header.Custom`.

* `WriteBody` writes the response body.

```fsharp
let content =
    Content.CustomContent <| fun context ->
        {
            Status = Http.Status.Ok
            Headers = [Http.Header.Custom "Content-Type" "text/plain"]
            WriteBody = fun stream ->
                use w = new System.IO.StreamWriter(stream)
                w.Write("The contents of the text file.")
        }

type Action =
    | GetSomeTextFile

let sitelet = Sitelet.Content "/someTextFile.txt" GetSomeTextFile content

// Accepted Request:    GET /someTextFile.txt
// Parsed Action:       GetSomeTextFile
// Returned Content:    The contents of the text file.
```

### Helpers

In addition to the four standard Content families above, the `Content` module contains a few helper functions.

* Redirection:

```fsharp
module Content =
    /// Permanently redirect to an action. (HTTP status code 301)
    val Redirect : 'Action -> Content<'Action>
    /// Permanently redirect to a URL. (HTTP status code 301)
    val RedirectToUrl : string -> Content<'Action>
    /// Temporarily redirect to an action. (HTTP status code 307)
    val RedirectTemporary : 'Action -> Content<'Action>
    /// Temporarily redirect to a URL. (HTTP status code 307)
    val RedirectTemporaryToUrl : string -> Content<'Action>
```

* Response mapping: if you want to return HTML or JSON content, but further customize the HTTP response, then you can use one of the following:

```fsharp
module Content =
    val SetStatus : Http.Status -> Content<'T> -> Content<'T>
    val WithHeaders : seq<Header> -> Content<'T> -> Content<'T>
    val MapResponse : (Http.Response -> Http.Response)

// Example use
let customForbidden =
    Content.PageContent <| fun context ->
        { Page.Default with
            Title = "No entrance!"
            Body = [Text "Oops! You're not supposed to be here."] }
    // Set the HTTP status code to 403 Forbidden:
    |> Content.MapResponse (fun response ->
        { response with Status = Http.Status.Forbidden })
    // Or equivalently:
    |> Content.SetStatus Http.Status.Forbidden
```

* Converting a `Content<'T>` to a `Http.Response`:

```fsharp
module Content =
    val ToResponseAsync : Content<'T> -> Context<'T> -> Async<Http.Response>
```

Sometimes it is necessary to access the context before even choosing what type of `Content` you want to return. For this purpose, you can use `Content.ToResponseAsync` inside `Content.CustomContentAsync`, like so:

```fsharp
let content =
    Content.CustomContentAsync <| fun context ->
        async {
            let! loggedInUser = context.UserSession.GetLoggedInUser()
            let content =
                match loggedInUser with
                | None -> Content.Redirect Index
                | Some username -> Content.PageContent <| fun ctx ->
                    { Page.Default with
                        Body = [Text ("Welcome, " + username + "!")] }
            return! Content.ToResponseAsync content context
        }
```

Note that using the synchronous `Content.ToResponse` is obsolete and strongly discouraged, as it can lead to deadlocks in some situations.

<a name="context"></a>
## Using the Context

Regardless of which function you use to generate your Content, your callback is supplied with a context of type `Context<'T>`. This context can be used for several purposes; the most important are creating internal links and managing user sessions.

<a name="linking"></a>
### Creating links

Since every accepted URL is uniquely mapped to a strongly typed action value, it is also possible to generate internal links from an action value. For this, you can use the function `context.Link`.

```fsharp
let HomePage =
  PageContent <| fun context ->
    {Page.Default with
      Title = Some "Welcome!"
      Body =
        [
          H1 [Text "Index page"]
          A [HRef (context.Link (BlogArticle(1423, "some-article-slug")))]
            -< [Text "Go to some article"]
          Br []
          A [HRef (context.ResolveUrl "~/Page2.html")] -< [Text "Go to page 2"]
        ]
    }
```

Note how `context.Link` is used in order to resolve the URL to the `BlogArticle` action.  Action URLs are always constructed relative to the application root, whether the application is deployed as a standalone website or in a virtual folder.  `context.ResolveUrl` helps to manually construct application-relative URLs to resources that do not map to actions.

### Managing User Sessions

Since `Context<'T>` implements the interface `WebSharper.Web.IContext`, it can be used to access the currently logged in user. [See here for more information about user sessions.](WebContext.md)

### Other Context members

* `context.ApplicationPath` returns the web root of the application. Most of the time this will be `"/"`, unless you use a feature such as an ASP.NET virtual directory.

* `context.Request` returns the `Http.Request` being responded to. This is useful to access elements such as HTTP headers, posted files or cookies.

* `context.ResolveUrl` resolves links to static pages in your application. A leading `~` character is translated to the `ApplicationPath` described above.

* `context.RootFolder` returns the physical folder on the server machine from which the application is running.


<a name="advanced-sitelets"></a>
## Advanced Sitelets

So far, we have constructed sitelets using built-in constructors such as `Sitelet.Infer`. But if you want finer-grained control over the exact URLs that it parses and generates, you can create sitelets by hand.

A sitelet consists of two parts; a router and a controller.  The job of the router is to map actions to URLs and to map HTTP requests to actions. The controller is responsible for handling actions, by converting them into content that in turn produces the HTTP response. The overall architecture is analogous to ASP.NET/MVC, and other `Model-View-Controller` -based web frameworks.

### Routers

The router component of a sitelet can be constructed in a variety of ways.  The following example shows how you can create a complete customized router of type `Action`.

```fsharp
type Action = | Page1 | Page2

let MyRouter : Router<Action> =
    let route (req: Http.Request) =
        if req.Uri.LocalPath = "/page1" then
            Some Page1
        elif req.Uri.LocalPath = "/page2" then
            Some Page2
        else
            None
    let link action =
        match action with
        | Action.Page1 ->
            System.Uri("/page2", System.UriKind.Relative)
            |> Some
        | Action.Page2 ->
            System.Uri("/page1", System.UriKind.Relative)
            |> Some
    Router.New route link
```

Specifying routers manually gives you full control of how to parse incoming requests and to map actions to corresponding URLs.  It is your responsibility to make sure that the router forms a bijection of URLs and actions, so that linking to an action produces a URL that is in turn routed back to the same action.

Constructing routers manually is only required for very special cases. The above router can for example be generated using `Router.Table`:

```fsharp
let MyRouter : Router<Action> =
    [
        Action.Page1, "/page1"
        Action.Page2, "/page2"
    ]
    |> Router.Table
```

Even simpler, if you want to create the same URL shapes that would be generated by `Sitelet.Infer`, you can simply use `Router.Infer()`:

```fsharp
let MyRouter : Router<Action> =
    Router.Infer ()
```

### URL Parsing Helpers

To simplify the parsing of URLs in the Router, some active patterns are provided in the module `UrlHelpers`.

* Method parsing: the active patterns `GET`, `POST`, etc. match a request with the given method, and return the list of parameters (from the query for `GET`, from the body for the others) and the URI.

* `SPLIT_BY` then splits the path of a URI into fragments.

* Finally, individual fragments can be parsed with `EOL`, `SLASH` `INT`, `FLOAT`, `ALPHA`, `ALPHA_NUM` and `REGEX`.

The three sets of patterns above are generally used together to create a whole parser.

```fsharp
type Action =
    | Index
    | Stats of username: string
    | BlogArticle of id: int * slug: string

let myRouter =
    let route request =
        match request with
        | GET (_, SPLIT_BY '/' []) ->
            Some Index
        | GET (_, SPLIT_BY '/' ["stats"; username]) ->
            Some (Stats username)
        | GET (_, SPLIT_BY '/' ["blog-article"; INT id; slug]) ->
            Some (BlogArticle (id, slug))
        | _ -> None
    let link action =
        match action with
        | Index ->
            System.Uri "/"
        | Stats u ->
            System.Uri ("/stats/" + u)
        | BlogArticle (id, slug) ->
            System.Uri (sprintf "/blog-article/%i/%s" id slug)
        |> Some
    Router.New route link
```

### Controllers

If an incoming request can be mapped to an action by the router, it is passed on to the controller. The job of the controller is to map actions to content. Here is an example of a controller handling actions of the `Action` type defined above.

```fsharp
let MyController : Controller<Action> =
    {
        Handle = fun action ->
            match action with
            | Action.Page1  -> MyContent.Page1
            | Action.Page2  -> MyContent.Page2
    }
```

Finally, the router and the controller components are combined into a sitelet:

```fsharp
let MySitelet : Sitelet<Action> =
    {
        Router = MyRouter
        Controller = MyController
    }
```

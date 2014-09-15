# Developing With Sitelets

WebSharper sitelets provide a way to alleviate the need of serving
content via ASPX pages.  Instead, you can define complete websites
programmatically using F#.

Sitelets go one step further in bridging the gap between the server
and client by allowing server-side HTML to be constructed by
combinators similar to those used in its WebSharper client-side
counterpart.  These combinators also allow you to embed WebSharper
client-side controls.

You will benefit from using sitelets by:

* Being able to dynamically construct pages and serve arbitrary
  content.

* Having full control of your URLs by specifying custom routers for
  linking them to content.

* Composing contents into sitelets, which may themselves be composed
  into larger sitelets.

* Having safe links for referencing other content contained within
  your site.

* Being able to use the type-safe HTML templating facilities that come
  with sitelets.

Below is a minimal example of a complete site serving one HTML page:

```fsharp
namespace SampleWebsite

open IntelliFactory.WebSharper.Sitelets

module SampleSite =
    open IntelliFactory.WebSharper
    open IntelliFactory.Html

    type Action = | Index

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

First, a custom action type is defined. It is used for linking URLs to
content within your sitelet. Here, you only need one action
corresponding to your only page.

The content of the index page is defined as a `PageContent`, where the
body consists of a server side HTML element.  Here the current time is
computed and displayed within an H1 tag.

The `MySampleWebsite` type specifies the sitelet to be served by
implementing the `IWebsite` interface.  In this case, the sitlet is
defined using the `Sitelet.Content` operator for constructing a
sitelet of the Index content.  In the resulting sitelet, the
`Action.Index` value is associated with the path `/index` and the
given content.

## Sitelet routing

WebSharper Sitelets abstract away URLs by using an action type that
represents the different pages available in a website. For example, a
site with an index and a blog article page parameterized by an
(integer) article id and a slug can be represented by the following
action type:

```fsharp
type Action =
    | Index
    | BlogArticle of int * string
```

Based on this, a Sitelet is a value that represents the following
mappings:

* Mapping from URLs to actions. A Sitelet is able to parse a URL such
  as `/blog/1243/some-article-slug` into the action value `BlogArticle
  (1243, "some-article-slug").`

* Mapping from actions to URLs. This allows you to have internal links
  that are verified by the type system, instead of writing URLs by
  hand and being at the mercy of a typo or a change in the URL scheme.
  You can read more on this in the "Content" section below.

* Mapping from actions to content. Finally, once a URL has been
  parsed, this determines what content (HTML or other) must be
  returned to the client.

A number of primitives are available to create and compose Sitelets.

### Sitelet.Infer

The easiest way to create a Sitelet is to automatically generate URLs
from the shape of your action type using `Sitelet.Infer`. This
function parses slash-separated path segments into the corresponding
Action value, and lets you match this action and return the
appropriate content. Here is an example sitelet using `Infer`:

```fsharp
open IntelliFactory.WebSharper.Sitelets

type Action =
    | Index
    | Stats of username: string
    | [<CompiledName "blog">] BlogArticle of int * string

type MyWebsite() =
    interface IWebsite<Action> with
        member this.Sitelet =
            Sitelet.Infer <| function
                | Index ->
                    // Content of the index page
                    Content.PageContent <| fun ctx ->
                        { Page.Default with
                            Body = [Text "Index page"] }
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

```
/Index
    --> action value: Index
/Stats/someUser
    --> action value: Stats "someUser"
/blog/1423/some-article-slug
    --> action value: Article (1423, "some-article-slug")
```

The following types are accepted by `Sitelet.Infer`:

* Numbers and strings are encoded as a single path segment.

* Tuples and records are encoded as consecutive path segments. For
  example:
    * `(1, "a")` is encoded as `/1/a`.
    * `{ Number = 1; Name = "a" }` is encoded as `/1/a`.

* Union types are encoded as a path segment identifying the case,
  followed by segments for the arguments (see the example above). For
  example, `option<'a>` is encoded as either `/None` or
  `/Some/<value>`.

  The attribute `CompiledName` can be used to provide an identifying
  segment which is different from the actual case's name. In
  particular, if you use `CompiledName ""` on an argument-less case,
  then the root url `/` will be mapped to this action.

* Lists and arrays are encoded as a number representing the length,
  followed by each element. For example, `["a"; "b"]` is encoded as
  `/2/a/b`.

### Other Constructors and Combinators

The following functions are available to build simple sitelets or
compose more complex sitelets out of simple ones:

* `Sitelet.Content`, as shown in the first example, builds a sitelet
  that accepts a single URL and maps it to a given action and content.

  The following sitelet accepts `/index`:

```fsharp
Sitelet.Content "/index" Action.Index IndexContent
```

* `Sitelet.Sum` takes a sequence of Sitelets and tries them in order
  until one of them accepts the URL. It is generally used to combine a
  list of `Sitelet.Content`s.

  The following sitelet accepts `/index` and `/about`:

```fsharp
Sitelet.Sum [
    Sitelet.Content "/index" Action.Index IndexContent
    Sitelet.Content "/about" Action.About AboutContent
]
```

* `<|>` takes two Sitelets and tries them in order. `s1 <|> s2` is
  equivalent to `Sitelet.Sum [s1; s2]`.

  The following sitelet accepts `/index` and `/about`:

```fsharp
Sitelet.Content "/index" Action.Index IndexContent
<|>
Sitelet.Content "/about" Action.About AboutContent
```

* `Sitelet.Shift` takes a Sitelet and shifts it by a path segment.

  The following sitelet accepts `/folder/index`:

```fsharp
Sitelet.Content "index" Action.Index IndexContent
|> Sitelet.Shift "folder"
```

* `Sitelet.Folder` takes a sequence of Sitelets and shifts them by a
  path segment. It is effectively a combination of `Sum` and `Shift`.

  The following sitelet accepts `/sub/index` and `/sub/about`:

```fsharp
Sitelet.Folder "sub" [
    Sitelet.Content "/index" Action.Index IndexContent
    Sitelet.Content "/about" Action.About AboutContent
]
```

## Content

Content is conceptually a function from a context to an HTTP response.
For convenience it differentiates between custom content and ones
producing HTML pages:

```fsharp
type Content<'Action> =
    | CustomContent of (Context<'Action> -> Http.Response)
    | PageContent   of (Context<'Action> -> Page)
```

Values of type `Context` contain runtime information of how to resolve
links to actions and resources.

The example below defines a page content with a link to another page

```fsharp
let Page1 : Content<Action> =
  PageContent <| fun context ->
    {Page.Default with
      Title = Some "Title of Page 1"
      Body =
        [
          H1 [Text "Page 1"]
          A [HRef (context.Link Action.Page2)] -< [Text "Page 2"]
          A [HRef (context.ResolveUrl "~/Page3.html")] -< [Text "Page 3"]
        ]
    }
```

Note how `context.Link` is used in order to resolve the URL to the
`Page2` action.  Action URLs are always constructed relative to the
application root, whether the application is deployed as a standalone
website or in a virtual folder.  `context.ResolveUrl` helps to
manually construct application-relative URLs to resources that do not
map to actions.

Contents are not restricted to produce HTML responses.  To change the
content type and encoding, you can customize the meta information that
drives the HTTP headers of the response.  Below is an example of
defining JSON data.

```fsharp
let JsonData : Content<Action> =
    CustomContent <| fun context ->
        {
            Status = Http.Status.Ok
            Headers = [Http.Header.Custom "Content-Type" "application/json"]
            WriteBody = fun stream ->
                use tw = new System.IO.StreamWriter(stream)
                tw.WriteLine "{X: 10, Y: 20}"
        }
```

### Embedding Client-Side Controls

The integration of WebSharper controls(i.e. code that translates to
JavaScript and runs on the client) in sitelet content is
straightforward. They can be directly embedded within server-side
HTML:

```fsharp
module Client =
    open IntelliFactory.WebSharper.Html

    type MyControl() =
        inherit IntelliFactory.WebSharper.Web.Control ()
        [<JavaScript>]
        override this.Body =
            I [Text "Client control"] :> IPagelet

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

Here, `MyControl` inherits from
`IntelliFactory.WebSharper.Web.Control` and overrides the `Body`
property with some client-side HTML.  This control is then placed
within a server-side `div` tag.

## User Sessions and Protecting Content

The `UserSession` module provides three primitive functions for
handling user sessions:

* `LoginUser : string -> unit` : Logs in a user with the given
  identifier.

* `GetLoggedInUser` - Returns the currently logged in user identifier
   as an option value.  If no user is logged in, the value `None` is
   returned.

* `Logout : unit -> unit` : Removes the logged in identifier.

The implementation of these functions relies on cookies and thus
requires that the browser has enabled cookies.

In addition to the functions above a combinator `Protect` is provided
in order create protected content, i.e.  content only available for
authenticated users:

```fsharp
Protect : Filter<'Action> -> Sitelet<'Action> -> Sitelet<'Action>
```

where the type `Filter` is defined as:

```fsharp
type Filter<'Action> =
    {
        VerifyUser : string -> bool;
        LoginRedirect : 'Action -> 'Action
    }
```

Given a filter value and a sitelet, `Protect` returns a new sitelet
that requires a logged in user that passes the `VerifyUser` predicate,
specified by the filter.  If the user is not logged in, or the
predicate returns false, the request is redirected to the action
specified by the `LoginRedirect` function specified by the filter.

Below is an example of creating a protected sitelet containing two
pages:

```fsharp
let protected =
    let filter : Sitelet.Filter<Action> =
        {
            VerifyUser = fun _ -> true
            LoginRedirect = fun _ -> Action.Login
        }
    Sitelet.Protect filter (
        [
            Sitelet.Content "/p1" Action.P1 P1
            Sitelet.Content "/p2" Action.P2 P2
        ]
        |> Sitelet.Folder "protected"
    )
```

## Advanced Sitelets

So far, we have constructed sitelets using built-in constructors such
as `Sitelet.Infer`. But if you want finer-grained control over the
exact URLs that it parses and generates, you can create sitelets by
hand.

A sitelet consists of two parts; a router and a controller.  The job
of the router is to map actions to URLs and to map HTTP requests to
actions. The controller is responsible for handling actions, by
converting them into content that in turn produces the HTTP
response. The overall architecture is analogous to ASP.NET/MVC, and
other `Model-View-Controller` -based web frameworks.

### Routers

The router component of a sitelet can be constructed in a variety of
ways.  The following example shows how you can create a complete
customized router of type `Action`.

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

Specifying routers manually gives you full control of how to parse
incoming requests and to map actions to corresponding URLs.  It is
your responsibility to make sure that the router forms a bijection of
URLs and actions, so that linking to an action produces a URL that is
in turn routed back to the same action.

Constructing routers manually is only required for very special cases.
The above router can for example be generated using `Router.Table`:

```fsharp
let MyRouter : Router<Action> =
    [
        Action.Page1, "/page1"
        Action.Page2, "/page2"
    ]
    |> Router.Table
```

Even simpler, if you want to create the same URL shapes that would be
generated by `Sitelet.Infer`, you can simply use `Router.Infer()`:

```fsharp
let MyRouter : Router<Action> =
    Router.Infer ()
```

### Controllers

If an incoming request can be mapped to an action by the router, it is
passed on to the controller. The job of the controller is to map
actions to content. Here is an example of a controller handling
actions of the `Action` type defined above.

```fsharp
let MyController : Controller<Action> =
    {
        Handle = fun action ->
            match action with
            | Action.Page1  -> MyContent.Page1
            | Action.Page2  -> MyContent.Page2
    }
```

Finally, the router and the controller components are combined into a
sitelet:

```fsharp
let MySitelet : Sitelet<Action> =
    {
        Router = MyRouter
        Controller = MyController
    }
```

### Handling HTTP Parameters

The `Http.Request` represents the incomming request containing the GET
and POST parameters.  By defining your sitelet manually you can decide
how to route requests depending on these parameters.

The `Router` module contains some funcions for simplifying the
construction of routers that map requests containing `POST`
parameters.

Here is an example of creating a sitelet with a router for handling
POST requests. The function `Router.FromPostParameter` and `Router.At`
are utilized for constructing a router mapping POST requests to the
url `/post`:

```fsharp
let MyRouter =
    [
        Router.Table [
            Action.Index , "/index"
        ]
        Router.FromPostParameter ParamName
        |> Router.At "/post"
        |> Router.TryMap
            (Action.Post >> Some)
            (function | Post x -> Some x | _ -> None)
    ]
    |> Router.Sum

let Handle action =
    match action with
    | Action.Index -> Pages.Index
    | Action.Post value -> Pages.Post value

let MySitelet =
    {
        Router = MyRouter
        Controller = { Handle = Handle }
    }
```

The function `Router.TryMap` is used for mapping the string router to
one of type `Action`:

Alternatively, the parameteres can be obtained at the content
generation phase, via the `Context` object:

```fsharp
let Index =
    Content.PageContent <| fun context ->
        let param =
            match context.Request.Post.["post"] with
            | Some n    -> n
            | None      -> "No Param"
        {Page.Default with Body = [Text param}
```

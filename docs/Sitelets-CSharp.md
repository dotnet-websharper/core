# Developing with Sitelets in C# #

Sitelets are WebSharper's primary way to create server-side content. They provide facilities to route requests and generate HTML pages or JSON responses.

Sitelets allow you to:

* Dynamically construct pages and serve arbitrary content.

* Have full control of your URLs by specifying [custom routers](#advanced-sitelets) for linking them to content, or let the URLs be [automatically inferred](#sitelet-infer) from an endpoint type.

* Have [safe links](#linking) for referencing other content contained within your site.

* Use the type-safe HTML [templating facilities](Templates.md) that come with sitelets.

* Automatically [parse JSON requests and generate JSON responses](Json.md) based on your types.

Below is a minimal example of a complete site serving one HTML page:

```csharp
using System;
using System.Threading.Tasks;
using WebSharper.Sitelets;
using static WebSharper.UI.Next.CSharp.Html;

namespace MyWebsite
{
    /// Endpoint for the single URL "/".
    [EndPoint("/")]
    public class Index { }

    public class SampleSite
    {
        /// The content returned when receiving an Index request.
        public static Task<Content> IndexContent(Context ctx)
        {
            var time = DateTime.Now.ToString();
            return Content.Page(
                Title: "Index",
                Body: h1("Current time: ", time)
            );
        }

        /// Defines the website's responses based on the parsed Endpoint.
        [Website]
        public static Sitelet<object> MySampleWebsite =>
            new SiteletBuilder()
                .With<Index>((ctx, endpoint) => IndexContent(ctx))
                .Install();
    }
}
```

First, custom endpoint types are defined. They are used for linking requests to content within your sitelet. Here, you only need one endpoint, `Index`, corresponding to your only page.

The content of the index page is defined as a `Content.Page`, where the body consists of a server side HTML element.  Here the current time is computed and displayed within an `<h1>` tag.

The `MySampleWebsite` value has type `Sitelet<object>`. It defines a complete website: the URL scheme, the `EndPoint` value corresponding to each served URL (only one in this case), and the content to serve for each endpoint. It uses the `SiteletBuilder` class to construct a sitelet for the Index endpoint, associating it with the `/index` URL and serving `IndexContent` as a response.

`MySampleWebsite` is annotated with the attribute `[Website]` to indicate that this is the sitelet that should be served.

<a name="sitelet-routing"></a>
## Sitelet routing

WebSharper Sitelets abstract away URLs and request parsing by using an endpoint type that represents the different HTTP endpoints available in a website. For example, a site's URL scheme can be represented by the following endpoint types:

```csharp
[EndPoint("/")]
public class Index { }

[EndPoint("/stats")]
public class Stats
{
    public string username;
}

[EndPoint("/blog")]
public class BlogArticle
{
    public int id;
    public string slug;
}
```

Based on this, a Sitelet is a value that represents the following mappings:

* Mapping from requests to endpoints. A Sitelet is able to parse a URL such as `/blog/1243/some-article-slug` into the endpoint value `new BlogArticle {id = 1243, slug = "some-article-slug"}`. More advanced definitions can even parse query parameters, JSON bodies or posted forms.

* Mapping from endpoints to URLs. This allows you to have internal links that are verified by the type system, instead of writing URLs by hand and being at the mercy of a typo or a change in the URL scheme. You can read more on this [in the "Context" section](#context).

* Mapping from endpoints to content. Once a request has been parsed, this determines what content (HTML or other) must be returned to the client.

### SiteletBuilder

The key way to create a Sitelet in C# is using the `SiteletBuilder` class. It functions quite similarly to `StringBuilder`:
* First, create an instance of `StringBuilder()`;
* Then, add your mapping using the `.With()` method (counterpart of `StringBuilder`'s `.Append()`);
* Finally, use `.Install()` to return your constructed `Sitelet` (counterpart of `StringBuilder`'s `.ToString()`).

For example, using the endpoint types defined [in the above section](#sitelet-routing), you can create the following Sitelet:

```csharp
[Website]
public static Sitelet<object> MySampleWebsite =>
    new SiteletBuilder()
        .With<Index>((ctx, endpoint) =>
            Content.Page(
                Title: "Welcome!",
                Body: h1("Index page")
            )
        )
        .With<Stats>((ctx, endpoint) =>
            Content.Page(
                Body: doc("Stats for ", endpoint.username)
            )
        )
        .With<BlogArticle>((ctx, endpoint) =>
            Content.Page(
                Body: doc($"Article id {endpoint.id}, slug {endpoint.slug}")
            )
        )
        .Install();
```

The above sitelets accepts URLs with the following shape:

```xml
Accepted Request:    GET /Index
Parsed Endpoint:     new Index()
Returned Content:    <!DOCTYPE html>
                     <html>
                         <head><title>Welcome!</title></head>
                         <body>
                             <h1>Index page</h1>
                         </body>
                     </html>

Accepted Request:    GET /Stats/someUser
Parsed Endpoint:     new Stats { username = "someUser" }
Returned Content:    <!DOCTYPE html>
                     <html>
                         <head></head>
                         <body>
                             Stats for someUser
                         </body>
                     </html>

Accepted Request:    GET /BlogArticle/1423/some-article-slug
Parsed Endpoint:     new BlogArticle { id = 1423, slug = "some-article-slug" }
Returned Content:    <!DOCTYPE html>
                     <html>
                         <head></head>
                         <body>
                             Article id 1423, slug some-article-slug
                         </body>
                     </html>
```

It is also possible to create an endpoint for a specific URL, without associating an endpoint type to it:

```csharp
new SiteletBuilder()
    .With("/static-url", ctx =>
        Content.Text("Replying to /static-url")
    )

// Accepted Request:    GET /static-url
// Returned Content:    Replying to /static-url
```

### Defining EndPoints

The following types can be used as endpoints:

* Numbers and strings are encoded as a single path segment.

```csharp
SiteletBuilder().With<string>(/* ... */)

// Accepted Request:    GET /abc
// Parsed Endpoint:     "abc"
// Returned Content:    (determined by .With())

SiteletBuilder().With<int>(/* ... */)

// Accepted Request:    GET /1423
// Parsed Endpoint:     1423
// Returned Content:    (determined by .With())
```

* Arrays are encoded as a number representing the length, followed by each element.

```csharp

SiteletBuilder().With<string[]>(/* ... */)

// Accepted Request:    GET /2/abc/def
// Parsed Endpoint:     new string[] { "abc", "def" }
// Returned Content:    (determined by .With())
```

* Tuples and objects are encoded with their fields as consecutive path segments.

```csharp
SiteletBuilder().With<Tuple<int, string>>(/* ... */)

// Accepted Request:    GET /1/abc
// Parsed Endpoint:     new Tuple<int, string>(1, "abc")
// Returned Content:    (determined by .With())

class T
{
    int Number;
    string Name;
}

// Accepted Request:    GET /1/abc
// Parsed Endpoint:     new T { Number = 1, Name = "abc" }
// Returned Content:    (determined by .With())
```

* Objects with an `[EndPoint]` attribute are prefixed with the given path segment.

```csharp
[EndPoint("/test")]
class T
{
    int number;
    string name;
}

SiteletBuilder().With<T>(/* ... */)

// Accepted Request:    GET /test/1/abc
// Parsed Endpoint:     new EndPoint { number = 1, name = "abc" }
// Returned Content:    (determined by .With())
```

* Enumerations are encoded as their underlying type.

```csharp
SiteletBuilder().With<System.IO.FileAccess>(/* ... */)

// Accepted Request:    GET /3
// Parsed Endpoint:     System.IO.FileAccess.ReadWrite
// Returned Content:    (determined by .With())
```

* `System.DateTime` is serialized with the format `yyyy-MM-dd-HH.mm.ss` by default. Use `[DateTimeFormat(string)]` on a field to customize it. Be careful as some characters are not valid in URLs; in particular, the ISO 8601 round-trip format (`"o"` format) cannot be used because it uses the character `:`.

```csharp
SiteletBuilder().With<DateTime>(/* ... */)

// Accepted Request:    GET /2015-03-24-15.05.32
// Parsed Endpoint:     System.DateTime(2015,3,24,15,5,32)
// Returned Content:    (determined by .With())

class T
{
    [DateTimeFormat("yyy-MM-dd")]
    DateTime date;
}

SiteletBuilder().With<T>(/* ... */)

// Accepted Request:    GET /2015-03-24
// Parsed Endpoint:     System.DateTime(2015,3,24)
// Returned Content:    (determined by .With())
```

* The attribute `[Method("GET", "POST", ...)]` on a class indicates which methods are accepted by this endpoint. Without this attribute, all methods are accepted.

```csharp
[Method("POST")]
class PostArticle
{
    int id;
}

SiteletBuilder().With<PostArticle>(/* ... */)

// Accepted Request:    POST /article/12
// Parsed Endpoint:     new PostArticle { id = 12 }
// Returned Content:    (determined by .With())
```

* If an endpoint accepts only one method, then a more concise way to specify it is directly in the `[EndPoint]` attribute:

```csharp
[EndPoint("POST /article")]
class PostArticle
{
    int id;
}

SiteletBuilder().With<PostArticle>(/* ... */)

// Accepted Request:    POST /article/12
// Parsed Endpoint:     new PostArticle { id = 12 }
// Returned Content:    (determined by .With())
```

* A common trick is to use `[EndPoint("GET /")]` on a field-less class to indicate the home page.

```csharp
[EndPoint("/")]
class Home { }

SiteletBuilder().With<Home>(/* ... */)

// Accepted Request:    GET /
// Parsed Endpoint:     new Home()
// Returned Content:    (determined by .With())
```

* If several classes have the same `[EndPoint]`, then parsing tries them in the order in which they are passed to `.With()` until one of them matches:

```csharp
[EndPoint("GET /blog")]
class AllArticles { }

[EndPoint("GET /blog")]
class ArticleById
{
    int id;
}

[EndPoint("GET /blog")]
class ArticleBySlug
{
    string slug;
}

SiteletBuilder()
    .With<AllArticles>(/* ... */)
    .With<ArticleById>(/* ... */)
    .With<ArticleBySlug>(/* ... */)

// Accepted Request:    GET /blog
// Parsed Endpoint:     new AllArticles()
// Returned Content:    (determined by .With())
//
// Accepted Request:    GET /blog/123
// Parsed Endpoint:     new ArticleById { id = 123 }
// Returned Content:    (determined by .With())
//
// Accepted Request:    GET /blog/my-article
// Parsed Endpoint:     new ArticleBySlug { slug = "my-article" }
// Returned Content:    (determined by .With())
```

* `[Query]` on a field indicates that this field must be parsed as a GET query parameter instead of a path segment. The value of this field must be either a base type (number, string) or an `Nullable` of a base type (in which case the parameter is optional).

```csharp
[EndPoint]
class Article
{
    [Query]
    int id;
    [Query]
    string slug;
}

SiteletBuilder().With<Article>(/* ... */)

// Accepted Request:    GET /article?id=1423&slug=some-article-slug
// Parsed Endpoint:     new Article { id = 1423, slug = "some-article-slug" }
// Returned Content:    (determined by .With())
//
// Accepted Request:    GET /article?id=1423
// Parsed Endpoint:     new Article { id = 1423, slug = null }
// Returned Content:    (determined by .With())
```

* You can of course mix Query and non-Query parameters.

```csharp
[EndPoint]
class Article
{
    int id;
    [Query]
    string slug;
}

SiteletBuilder().With<Article>(/* ... */)

// Accepted Request:    GET /article/1423?slug=some-article-slug
// Parsed Endpoint:     new Article { id = 1423, slug = Some "some-article-slug" }
// Returned Content:    (determined by .With())
```

<a name="json-request"></a>

* `[Json]` on a field indicates that it must be parsed as JSON from the body of the request. If an endpoint type contains several `[Json]` fields, a compile-time error is thrown.

    [Learn more about JSON parsing.](Json.md)

```csharp
[EndPoint("POST /article")]
class PostArticle
{
    int id;
    [Json]
    PostArticleData data;
}

[Serializable]
class PostArticleData
{
    string slug;
    string title;
}

SiteletBuilder().With<PostArticle>(/* ... */)

// Accepted Request:    POST /article/1423
//
//                      {"slug": "some-blog-post", "title": "Some blog post!"}
//
// Parsed Endpoint:     new PostArticle {
//                          id = 1423,
//                          data = new PostArticleData {
//                              slug = "some-blog-post",
//                              title = "Some blog post!" } }
// Returned Content:    (determined by .With())
```

* `[Wildcard]` on a field indicates that it represents the remainder of the url's path. That field can be a `T[]` or a `string`. If an endpoint type contains several `[Wildcard]` fields, a compile-time error is thrown.

```csharp
[EndPoint("/articles")]
class Articles
{
    int pageId;
    [Wildcard]
    string[] tags;
}

[EndPoint("/articles")]
class Articles2
{
    [Wildcard]
    Tuple<int, string>[] tags;
}

[EndPoint("/file")]
class File
{
    [Wildcard]
    string file;
}

SiteletBuilder()
    .With<Articles>(/* ... */)
    .With<Articles2>(/* ... */)
    .With<File>(/* ... */)

// Accepted Request:    GET /articles/123/fsharp/websharper
// Parsed Endpoint:     new Articles {
//                          pageId = 123,
//                          tags = new[] { "fsharp", "websharper" } }
// Returned Content:    (determined by .With())
//
// Accepted Request:    GET /articles/123/fsharp/456/websharper
// Parsed Endpoint:     new Articles2 { tags = new[] {
//                          { 123, "fsharp"}, { 456, "websharper" } } }
// Returned Content:    (determined by .With())
//
// Accepted Request:    GET /file/css/main.css
// Parsed Endpoint:     new File { file = "css/main.css" }
// Returned Content:    (determined by .With())
```

## Content

Content describes the response to send back to the client: HTTP status, headers and body. Content is always worked with asynchronously: all the constructors and combinators described below take and return values of type `Task<Content>`.

### Creating Content

There are several functions that create different types of content, including ordinary text (`Content.Text`), file (`Content.File`), HTML page (`Content.Page`), JSON (`Content.Json`), any custom content (`Content.Custom`), and HTTP error codes and redirects.

#### Content.Text

The simplest response is plain text content, created by passing a string to `Content.Text`.

```csharp
new SiteletBuilder()
    .With<T>((ctx, endpoint) =>
        Content.Text("This is the response body.")
    )
```

#### Content.File

You can serve files using `Content.File`.  Optionally, you can set the content type returned for the file response and whether file access is allowed outside of the web root:

```csharp
new SiteletBuilder()
    .With<T>((ctx, endpoint) =>
        Content.File("../Main.fs",
            AllowOutsideRootFolder: true,
            ContentType: "text/plain")
    )
```

#### Content.Page

You can return full HTML pages, with managed dependencies using `Content.Page`. Here is a simple example:

```csharp
using static WebSharper.UI.Next.CSharp.Html;

new SiteletBuilder()
    .With<T>((ctx, endpoint) =>
        Content.Page(
            Title: "Welcome!",
            Head: link(attr.href("/css/style.css"), attr.rel("stylesheet")),
            Body: doc(
                h1("Welcome to my site."),
                p("It's great, isn't it?")
            )
        )
    )
```

The optional named arguments `Title`, `Head`, `Body` and `Doctype` set the corresponding elements of the HTML page. To learn how to create HTML elements for `Head` and `Body`, see [the HTML combinators documentation](HtmlCombinators.md).

<a name="json-response"></a>
#### Content.Json

If you are creating a web API, then Sitelets can automatically generate JSON content for you based on the type of your data. Simply pass your value to `Content.Json`, and WebSharper will serialize it. The format is the same as when parsing requests. [See here for more information about the JSON format.](Json.md)

```csharp
[EndPoint("/article")]
class GetArticle
{
    int id;
}

[Serializable]
class GetArticleResponse
{
    int id;
    string slug;
    string title;
}

new SiteletBuilder()
    .With<GetArticle>((ctx, endpoint) =>
        Content.Json(
            new GetArticleResponse {
                id = endpoint.id,
                slug = "some-blog-article",
                title = "Some blog article!"
            }
        )
    )
    .Install()

// Accepted Request:    GET /article/1423
// Parsed Endpoint:     new GetArticle { id = 1423 }
// Returned Content:    {"id": 1423, "slug": "some-blog-article", "title": "Some blog article!"}
```

#### Content.Custom

`Content.Custom` can be used to output any type of content. It takes three optional named arguments that corresponds to the aforementioned elements of the response:

* `Status` is the HTTP status code. It can be created using the function `Http.Status.Custom`, or you can use one of the predefined statuses such as `Http.Status.Forbidden`.

* `Headers` is the HTTP headers. You can create them using the function `Http.Header.Custom`.

* `WriteBody` writes the response body.

```csharp
new SiteletBuilder()
    .With("/someTextFile.txt", ctx =>
        Content.Custom(
            Status: Http.Status.Ok,
            Headers: new[] { Http.Header.Custom("Content-Type", "text/plain") },
            WriteBody: stream =>
            {
                using (var w = new System.IO.StreamWriter(stream))
                {
                    w.Write("The contents of the text file.");
                }
            }
        )
    )

// Accepted Request:    GET /someTextFile.txt
// Returned Content:    The contents of the text file.
```

### Helpers

In addition to the four standard Content families above, the `Content` module contains a few helper functions.

* Redirection:

```csharp
static class Content {
    /// Permanently redirect to an endpoint. (HTTP status code 301)
    static Task<Content> RedirectPermanent(object endpoint);

    /// Permanently redirect to a URL. (HTTP status code 301)
    static Task<Content> RedirectPermanentToUrl(string url);

    /// Temporarily redirect to an endpoint. (HTTP status code 307)
    static Task<Content> RedirectTemporary(object endpoint);

    /// Temporarily redirect to a URL. (HTTP status code 307)
    static Task<Content> RedirectTemporaryToUrl(string url);
}
```

* Response mapping: if you want to return HTML or JSON content, but further customize the HTTP response, then you can use one of the following:

```csharp
static class Content {
    /// Set the HTTP status of a response.
    static Task<Content> SetStatus(this Task<Content> content, Http.Status status);

    /// Add headers to a response.
    static Task<Content> WithHeaders(this Task<Content> content, IEnumerable<Header> headers);

    /// Replace the headers of a response.
    static Task<Content> SetHeaders(this Task<Content> content, IEnumerable<Header> headers);
}

// Example use
new SiteletBuilder()
    .With("/", ctx =>
        Content.Page(
            Title: "No entrance!",
            Body: text("Oops! You're not supposed to be here."))
            .SetStatus(Http.Status.Forbidden)
            .WithHeaders(new[] { Http.Header.Custom("Content-Language", "en") })
    )
```

<a name="context"></a>
## Using the Context

The method `SiteletBuilder.With()` provides a context of type `Context`. This context can be used for several purposes; the most important are creating internal links and managing user sessions.

<a name="linking"></a>
### Creating links

Since every accepted URL is uniquely mapped to an action value, it is also possible to generate internal links from an action value. For this, you can use the function `context.Link`.

```csharp
[EndPoint("/article")]
class Article
{
    public int id;
    public string slug;
}

new SiteletBuilder()
    .With<Article>((context, endpoint) =>
        Content.Page(
            Title: "Welcome!",
            Body: doc(
                h1("Index page"),
                a(attr.href(context.Link(new Article { id = 1423, slug = "some-article-slug" })),
                    "Go to some article"),
                br(),
                a(attr.href(context.ResolveUrl("~/Page2.html")), "Go to page 2")
            )
        )
    )
```

Note how `context.Link` is used in order to resolve the URL to the `Article` endpoint.  Endpoint URLs are always constructed relative to the application root, whether the application is deployed as a standalone website or in a virtual folder.  `context.ResolveUrl` helps to manually construct application-relative URLs to resources that do not map to sitelet endpoints.

### Managing User Sessions

Since `Context` implements the interface `WebSharper.Web.IContext`, it can be used to access the currently logged in user. [See here for more information about user sessions.](WebContext.md)

### Other Context members

* `context.ApplicationPath` returns the web root of the application. Most of the time this will be `"/"`, unless you use a feature such as an ASP.NET virtual directory.

* `context.Request` returns the `Http.Request` being responded to. This is useful to access elements such as HTTP headers, posted files or cookies.

* `context.ResolveUrl` resolves links to static pages in your application. A leading `~/` character is translated to the `ApplicationPath` described above.

* `context.RootFolder` returns the physical folder on the server machine from which the application is running.


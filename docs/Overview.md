# Overview of WebSharper

WebSharper is a framework and toolset for developing web/mobile applications and web services 
entirely in C# or F# (of a mix of the two languages) with strongly-typed client-server 
communication and site navigation. 
It provides powerful server-side capabilities *and* a compiler to JavaScript with
a whole set of client-side functional abstractions.

Of course, it is possible to use WebSharper only for its server-side
functionality, or conversely, purely as a C#/F#-to-JavaScript compiler that comes
with powerful libraries. But by taking advantage of both aspects, you can also
benefit from the facilities provided by WebSharper to allow client-server
interaction with minimal boilerplate.

Here is an overview of WebSharper's capabilities.

## Sitelets: server-side functionality

WebSharper Sitelets provide server-side functionality to parse HTTP requests
and serve content. HTTP endpoints are represented by values of a user-defined
EndPoint type, which are parsed from requests based on their shape and some
attributes. With Sitelets, you can:

* Discriminate endpoints based on the HTTP method, URL path, query arguments,
  and request body (JSON or form body).
* Parse and generate JSON based on the shape and attributes of your data type,
  for easy REST APIs.
* Generate links from EndPoint values, practically eliminating the risk of
  internad dead links.
* Generate HTML content from template files or directly in C#/F# with a clean
  syntax.

Learn more about Sitelets: [C#](Sitelets-CSharp.md)/[F#](Sitelets.md).

## JavaScript compiler and client-side abstractions

WebSharper can compile all your C# and/or F# source code to JavaScript.
Full interoperability is supported if you use both languages.
Unlike .NET-to-JavaScript compilers, you can tell WebSharper what parts of your code
should or shouldn't be compiled to JavaScript. This allows you to keep together
related server-side and client-side functionality in a single cohesive code
base. Adding a new feature requiring client-server communication has never been
so safe and swift. 

* Take advantage of C# language constructs on the client side, such as
  [LINQ](Linq-CSharp.md).
* Use powerful F# language features like pattern matching and type providers on the client side.
* Write in a mix of both languages to get the best of both worlds.
* Use a functional and reactive programming style with
  UI.Next [C#](UINext-CSharp.md)/[F#](UINext.md) to let the data flow through your UI.
* Write full web forms in a couple lines of code using
  [WebSharper.Forms](Introduction.md) or
  [WebSharper.Formlets](Formlets.md). (Currently available for F# only)
* Develop libraries with self-contained client and/or server functionality to reuse in multiple projects.
* Many JavaScript libraries has typed interfaces for WebSharper available on NuGet, 
  or write your own using a concise [F# DSL](InterfaceGenerator.md).

## Client-server interaction facilities

Including client-side controls inside server-rendered pages and interacting
between the client and the server has never been easier.

* Keep your code base consistent by using the same data types on the server and
  the client.
* Share code between tiers: JavaScript-compiled code is also compiled normally
  to .NET, so you can write a function once and use it directly both on the
  server and on the client.
* Include [client-side generated controls](CS-Controls.md) directly
  inside your page without any indirection.
* Alternatively, you can also include WebSharper client controls inside [ASPX or
  Razor pages (TODO)](CS-AspNet.md).
* Use automated remoting [C#](Remoting-CSharp.md)/[F#](Remoting.md): doing an AJAX request is as simple
  as `await`ing a call to your server-side function.
* Perform JSON serialization on the client using the same typed format as
  Sitelets, allowing you to call your REST APIs without worrying about the
  request format.
* Communicate between the server and the client using
  WebSockets, with automatically serialized
  messages.

## Extra features

* [Source mapping](SourceMapping.md).
* Analyzer for C#, showing WebSharper-specific translation errors as you code.
* Metaprogramming: translate calls to specific methods with your custom logic or 
easily include JavaScript code generated at compile-time.
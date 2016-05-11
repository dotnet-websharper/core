Introducing WebSharper for C#

WebSharper is a web development framework for the C# and F# languages.
Write full stack web applications in a single language or mixing the two .NET languages.
Client-side JavaScript code are generated from the original C# and F# source.

Additional features:

* Transparent asynchronous remoting. 
Just mark a `static async` method with `[Remote]` and you will be able to call it from
`JavaScript` annotated methods, no extra plumbing required.
* Type-safe site maps and links.
With the Sitelets API, you can define the pages of your website with a class hierarchy and URLs will be inferred for you, or you can customize the mapping.
* Efficient JavaScript code with type erasure.
WebSharper compilation does not try to replicate all .NET behavior in the browser by tracking the types of all objects.
Instead it uses compile-time type information to generate efficient JavaScript code.
* Typed interfaces to a growing number of popular JavaScript libraries. See the full list at the [WebSharper downloads](http://websharper.com/downloads) page.
* Useful abstractions for reactive forms and UI.
* Metaprogramming with macros and generators: modify translation of specific calls by custom logic or generate JavaScript function bodies programmatically.

The related compilers for C# and F# are called Zafir.
To use the Zafir compiler in your C# project, add the `Zafir` main library and `Zafir.CSharp` compiler tools NuGet packages.
Or download the [vsix installer](...) to gain access to a number of project templates in Visual Studio.
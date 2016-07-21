Introducing WebSharper for C#

WebSharper is a web development framework for the C# and F# languages.
Write full stack web applications in a single language or mixing the two .NET languages.
Client-side JavaScript code are generated from the original C# source where annotated.
Calling the server asynchronously is as simple as calling a `Remote`-annotated `static async` method from client-side code, no extra plumbing required.

Additional features:

* Analyzer for continous code assistance. See WebSharper-specific warnings and errors quickly as you code.
* Type-safe site maps and links.
With the Sitelets API, you can define the pages of your website with a class hierarchy and URLs will be inferred for you, or you can customize the mapping.
* Efficient JavaScript code with type erasure.
WebSharper compilation does not try to replicate all .NET behavior in the browser by tracking the types of all objects and having a large runtime.
Instead it uses compile-time type information to generate efficient JavaScript code.
You still get the power of type safety and generics while producing performant code.
* WebSharper extensions are typed interfaces for a growing number of popular JavaScript libraries. See the full list at the [WebSharper downloads](http://websharper.com/downloads) page.
* Useful abstractions for reactive forms and UI.
* Metaprogramming with macros and generators: modify translation of specific calls by custom logic or generate JavaScript function bodies programmatically.

To use WebSharper 4 (beta codename Zafir) in your C# project, add the `Zafir` and `Zafir.CSharp` NuGet packages.
It is also recommended to download a [vsix installer](...) which adds WebSharper 4 project templates in Visual Studio (under the section Zafir).
#WebSharper 3 to 4-alpha update guide

## Changes

WebSharper 4 was designed to be as compatible as possible with code written for WebSharper 3 while moving
closer to .NET semantics, and adding C# support.
Most code should compile and run without errors.

## NuGet packages

Uninstall the `WebSharper` package and all extensions.
Install `Zafir` and `Zafir.FSharp`, and `Zafir` versions of all extensions (check the `Include prerelease`). 

### JavaScript Attribute changes

The `JavaScript` attribute is no longer an alias for `ReflectedDefinition`.
New features: you can use the attribute on assembly level: `[<assembly: JavaScript>]`. 
Also `[JavaScript(false)]` removes a module, type or member from the scope of the JavaScript translation.

### Extra F# language features

Syntax forms previously disallowed by the limitations of ReflectedDefinition becomes available to use for client-side code.
These are object expressions, byref and `&` operator, inner generic functions, pattern matching on arrays, statically resolved type parameters.
Also object-oriented features has been expanded, see below.

### Object-oriented features

WebSharper now supports .NET semantics for method overrides, interface implementations, static constructors,
base calls, having no implicit constructor.
Previous workarounds for these missing features may be breaking:
JavaScript-compiled names of implementation and override methods are now resolved automatically, and using the `Name` attribute on them is disallowed.
You can specify the JavaScript-compiled name at the interface or abstract method declaration.

Module-bound `let` values are no more initialized on page load, but on the first access of any value in the same source file (as in .NET).

### Single Page Application changes

Bundling now creates minimal code using code path exploration.
You have to mark the entry point with the new `[<SPAEntryPoint>]` attribute.
This must be a single static method with no arguments.

### Translation changes

These changes may be breaking if some of your code relies on exact JavaScript form of some translated code.

* Union types now respect the `UseNullAsTrueValue` flag.
For example the `None` value is now translated to `null`.
* Delegates are now having proper proxies, `Combine`, `Remove`, `Target` and `GetInvocationList` are usable on client-side.
Previously delegates were a shortcut to create multiple-argument JavaScript functions (already deprecated in WebSharper 3 but not removed). 

### WebSharper Extension changes

C#-friendly overloads (using delegates and new function wrapper types) has been added to all libraries created by the WebSharper Interface Generator.
In some cases where the overloads would be ambigous with the F# versions (function with 0 or 1 arguments), the F# style overload has been removed.
You may need to convert to a delegate explicitly when using these methods. 

### Macros and generators

As the compiler pipeline of WebSharper has been replaced, the intermediate AST representation has changed.
Macros and generators also gained new features.
Full API documentation will be available later.

### Macros relying on type information

Some built-in macros like the client-side JSON serialization relies on compile-time type information to generate JavaScript code.
For example `WebSharper.Json.Encode` is a function that previously could be called only with fully-specified type argument.
Now, you can use it in a generic function if you mark that function `[<Inline>]`.
In general, tranlation that relies on type information is delayed within inlines until the type arguments has been resolved at the call point.

## Missing features in first alpha release compared to WebSharper 3 (to be added)

* TypeScript definition output
* Clean does not remove unpacked code 
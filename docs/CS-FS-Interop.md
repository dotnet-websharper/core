# C#/F# interoperability

There are helper classes included in WebSharper for most common conversions between `FSharp.Core` and standard .NET types.
These conversions work both on server and client side.

## Create F# values from standard .NET counterparts

The `WebSharper.FSharpConvert` static class contains static members to create values used commonly in F#.

* `FSharpConvert.Fun` overloads create curried F# functions for 1-8 arguments from `System.Action` and `System.Func` delegate values.
* `FSharpConvert.Option` overloads create an `FSharpOption<T>` value from a value of a .NET reference type or `Nullable` value type. (To create a 1)
* `FSharpConvert.List` overloads create an `FSharpList`.
* `FSharpConvert.Async` overloads create an `FSharpAsync<TResult>` from a `Task` or `Task<TResult>`.
* `FSharpConvert.Ref` overloads create an `FSharpRef` object with specified initial value or default value for a type.
* `Option.None` and F# `unit` values can be just `null`.

## Consume F# values from C#

Everything works as in .NET, there are no extra helpers provided by WebSharper.

* To call an `FSharpFunc` function, use its `Invoke` method. When calling curried F# functions, you have to chain calling `Invoke` to pass all arguments separately.
* Create F# union values using the `.New...` static method in the union type (or property for a union case with no fields).
* Create F# record values by using its constructor.
* Separate union cases by using the `Is...` methods, or the `Tag` property. Whenever a union has `null` as possible value, these methods are static.
* For the `FSharpOption` type, you can use `x?.Value`) to get the value or null.
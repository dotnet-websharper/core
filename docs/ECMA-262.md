# JavaScript Standard library

This extension enables you to use standard EcmaScript features in a
direct, type-safe way without inlining actual EcmaScript code. You can
use this extension to compile Standard ECMA-262 compliant code
directly from F#.

This extension implements the full 5th edition of the ECMA standard.

The ECMA code is available in the
`WebSharper.JavaScript` namespace.  The implementation
closely follows the standard, covering the following ECMA objects:

* `Global`
* `Object`
* `Function`
* `Array`
* `String`
* `Boolean`
* `Number`
* `Math`
* `Date`
* `RegExp`
* `Error`
* `JSON`

Each object has the methods defined in the ECMA 262 standard.  This
extension does not contain any browser-specific objects or methods.

## Extra features and differences

### Generic Object type

The `Object` class has a generic version, useful if you want to constrain 
all the object fields to a single type.
You can access a field in a typed manner by using an indexer with a 
string parameter: `myObj.["someField"]`.

### Strongly-typed functions

The `WebSharper.JavaScript.Function` is the base class for JavaScript functions, 
which has methods named `ApplyUnsafe`, `BindUnsafe`, `CallUnsafe` so that they do not 
have overloads with the strongly typed versions inheriting it.

F# functions always translate to a JavaScript function taking 0 or 1 arguments and
do not use the `this` value.
If you want to create any other kind of JavaScript function, use the
`WebSharper.JavaScript.FuncWith*` family of types as follows:

* `FuncWithThis<'This, 'Func>` for functions that use `this`. 
`'Func` can be a straight F# function or another interop type.

* `FuncWithArgs<'Args, 'Result>` for functions with n arguments that do not use `this`.
`'Args` must be a tuple type, this is checked at compile time.

* `FuncWithRest<..., 'Rest, 'Result>` for functions that take some fixed arguments,
and then a variadic array of the remaining arguments.
The number of fixed arguments can be between 0 and 6.

* `FuncWithArgsRest<'Args, 'Rest, 'Result>` for the rare case of a variadic function
with more than 6 fixed arguments.
`'Args` is a tuple type of the fixed arguments, length checked at compile time.

### Casting between .NET and JavaScript types

Ecma types in `WebSharper.JavaScript` all have a `.Self` property which returns it
as a standard .NET type where possible.

If you open the `WebSharper.JavaScript` namespace, .NET objects get an extension
property called `.JS` for casting them safely to their EcmaScript equivalent.

Example:

    // use this only in client only code
    let arr = [||] : int[]
    arr.JS.Push(1)

If you want to cast an F# function to a `WebSharper.JavaScript.Function`, you can use
the `Function.As` static method.

## More Examples

You have access to the `Math` object with all of its constants and
methods:

    let pi = Math.PI
    let sq25 = Math.Sqrt 25.

Strings can be manipulated with the ECMA `String` object and its
methods:

    let str = new String("a lowercase string")
    let upperstr = str.ToUpperCase() // "A LOWERCASE STRING"
    let tenthchar = str.CharAt(10) // "e"
    let substring = str.Substring(2,11) // "lowercase"

`RegExp` objects can be used to manipulate text:

    let str = new String("Bob likes pineapples.")
    let regex = new RegExp("^\w+") // matches the first word
    let newstr = str.Replace(regex,"Alice")

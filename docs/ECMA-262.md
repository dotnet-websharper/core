# WebSharper Extensions for EcmaScript (ECMA-262)

This extension enables you to use standard EcmaScript features in a
direct, type-safe way without inlining actual EcmaScript code. You can
use this extension to compile Standard ECMA-262 compliant code
directly from F#.

This extension implements the full 5th edition of the ECMA standard.

The ECMA code is available in the
`IntelliFactory.WebSharper.EcmaScript` namespace.  The implementation
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

## Examples

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

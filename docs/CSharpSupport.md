# C# language support

### Supported C# syntax

* anonymous functions
* operators (including user-defined operator overloading)
* await on `Task`, `Task<TResult>`
* conditional access (`?.` operator)
* assignments (including composite assignment operators)
* literals
* `ref` and `out` parameters
* optional parameters, default value
* delegates (having `Combine`, `Remove`, `Target`)
* implicit and explicit casts (including user-defined implicit/explicit operators), missing: numeric explicit casts (see planned) properly truncating value
* collection initializers (including dictionary initializers)
* anonymous objects
* query expressions - `from ... select ...`
* string interpolation (missing: padding)
* control flow: labels/`goto`, `break`, `continue`, `return`, `throw`, `yield return`, `while`, `do`/`while`, `for`, `foreach`, `using`, `if`, `switch`, `try`/`catch`/`finally`
* generator methods (with limitations: see planned)
* async methods returning `Task`, `Task<TResult>` (with limitations: see planned)

### C# features and fixes planned

These features and fixes are planned for stable release.

* struct definitions
* numeric explicit casts truncating value (currently does nothing, it is recommended to use `System.Math` methods instead)
* `checked`/`unchecked`
* padding in string interpolation
* resolving current limitations of generator and async methods: block-scoped variables and try/finally does not currently work inside a method transformed to a state-machine
* nullable operators should be absorbing (`null + 1` equals `null` in .NET)

### C# keywords not available for JavaScript

These keywords are used for interoperability with unmanage code or in multi-threaded environment which is not

* `typeof`
* `sizeof`
* `fixed`
* `unsafe`
* `lock`
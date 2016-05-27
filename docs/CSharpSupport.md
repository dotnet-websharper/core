### C# syntax supported:

* anonymous functions
* operators (including user-defined operator overloading)
* await on `Task`, `Task<_>`
* conditional access (`?.` operator)
* assignments (including composite assignment operators)
* literals
* ref and out parameters
* optional parameters, default value
* delegates (including Combine, Remove, Target)
* implicit and explicit casts (including user-defined implicit/explicit operators) missing numeric explicit casts (see planned) properly downsizing value
* collection initializers (including dictionary initializers)
* anonymous objects
* query expressions - `from ... select ...`
* string interpolation (missing padding)
* control flow: variable declarations with block scoping, labels/goto, break, continue, return, throw, yield return, while, do/while, for, foreach, using, if, switch, try/catch/finally
* generator methods (with limitations: see planned)
* async methods returning Task, Task`1 (with limitations: see planned)

### C# features planned:
* struct definitions
* numeric explicit casts properly transforming value - to be consistent, F# should check it better too, for example `byte 256` evaluating to `0` on the client-side too
* padding in string interpolation
* resolving current limitations of generator and async methods: block-scoped variables and try/finally does not work currently
* nullable operators should be absorbing (`null + 1` equals `null` in .NET)

### C# keywords not supported:

* checked
* typeof
* sizeof
* fixed
* unsafe
* lock
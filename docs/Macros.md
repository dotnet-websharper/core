* WebSharper.Core.Macro type
* WebSharper.Core.AST namespace
* Macro fallback and chaining
* Macros using type argument
* Parameter

Note: using a macro defined in the same project it is applied in requires the `useLocalReflection` wsconfig setting (default `false`); see [wsconfig.md](../../docs/content/core/wsconfig.md#useLocalReflection). Macros from a referenced assembly are unaffected.
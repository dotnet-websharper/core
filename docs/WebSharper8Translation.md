# WebSharper 8 translation

WebSharper reads F# or C# source code and outputs modern module-based JavaScript code. Instead of translating an entire project or files, WebSharper only looks at code that are annotated to be used client-side. This allows full-stack applications within a single project, sharing data types and code between layers.

### Scope of the JavaScript translation

Most common method of annotating for translation is using the `[<JavaScript>]` attribute on a class, F# module, or member. `[<JavaScript(false)>]` will revert the effect and exclude the annotated scope from translation.

Also, `[<JavaScript("fileName")>]` and `[<JavaScript("typeName")>]` can be used on the assembly level to annotate whole files or types without changing the target file themselves.

Lastly, in `wsconfig.json`, the `"javascript"` setting can be a bool or an array of strings. `"javascript": true` will annotate the whole project for translation without changing any files, this is a useful feature for client-only project. Or an array of strings can contain both file and type names, to describe the scope of translation.

### Proxies

Sometimes it is preferred that the same class have different implementation on the server and the client. In this case a `[<Proxy(typeof<TargetType>)>]`  or `[<Proxy("fullyQualifiedTypeName")>]` attribute on the client-side implementation will tell WebSharper, that in any translation, treat the two types as equivalent.

Also, this can be used to implement a client-side for standard library .NET types. WebSharper provides a good number of these proxies out of the box, for example may basic `System`, `System.Collections.Generic`, and `FSharp.Core` types are supported.

Use the `InternalProxy` attribute instead to limit the effect of a proxy to the current project only.

### Overview

Below is a quick overview of the translation process for reference.

- Source code is analyzed by FSharp.Compiler.Services or Microsoft.CodeAnalysis.CSharp respectively, client-facing code is read. Language-specific optimizations, like tail call elimination for F# are done.
- Mappings from .NET to JavaScript names are calculated. WebSharper supports overloads and overrides by making sure JavaScript naming lines up semantically with the original source. The `[<Name>]` attribute can be used to override default JavaScript naming, if it would lead to an impossible to resolve scenario, a compile-time error is thrown.
- Expressions like method calls are translated from their .NET information to their JavaScript equivalent. Also at this step, metaprogramming can be applied, to use type informatio to guide output, this functionality is called macros.
- The resulting code is optimized and transformed into proper JavaScript statements and expressions.
- The code is down into files for every class or dead code eliminated for bundles and written to JavaScript.

First we look at these two final output modes.

### One class per file output
This mode is intended for debugging, creating code for npm, or for bundling with external tool. This is the default output mode of WebSharper, unless `"preBundle": true` is set in `wsconfig.json`. One .NET class will create one output file. 

For website projects without `"preBundle": true` (for example for Debug mode), a `root.js` file is created that re-exports all code entry points needed for the website. This can be passed to tools like vite.

### Bundled output
For web (Sitelet) and SPA projects, WebSharper can use source code type information to create dead code eliminated bundles.

By default, a single bundle called `all.js` is created for a web application which contains all necessary code for all pages. If using `Content.Page` to create responses, use the `Bundle` argument to set a name for the bundle to be pre-created for the page. WebSharper creates these bundles by compile-time analysis of the client-side code required to render the page that is passed as the `Body` argument. If you want to send code to a certain bundle without wrapping it in a `Content` object yet, use the `Content.Bundle` helper, which uses the same compile-time analysis.

The `all.js` bundle is always created and serves as a fallback if a page would require code that is not well-contained in any other single bundle. For large sites with different code used accross many pages,

## Core proxies

Some of the default type proxies are as follows:

- Numeric values except `int64` and `uint64` are translated to `Number`. Also `DateTime` and `TimeSpan` are translated to `Number` equivalent to their JavaScript Date ticks.
- The large numeric types as well as `bigint` is translated to `BigInt`.
- `string`, `char`, and `Guid`s are translated to `String`.
- Both delegates and F# functions are translated to JavaScript functions.
- For `decimal` support, install the `WebSharper.MathJS` NuGet package as they are handled by `mathjs`.
- `System.Exception` is translated to `Error`.
- Arrays and `System.Collections.Generic.List` as well as `Queue` and `Stack` are translated to a JavaScript `Array`.
- Many more `System` and `FSharp.Core` types are translated to custom objects.
- See structured types below.

## F# translation

Below is an overview of how F# language features are transpiled to JavaScript.

### Code organization

Websharper treats classes as units of code organization, .NET namespaces are erased in the code output except in the one class per file output file names.

F# modules containing module-level `let` bindings and functions yield two files, one for the static initializer and one for functions, this matches how F# modules are handled by the F# compiler.

Inlining is treated separately from F# `inline let` functions, guided by WebSharper's `[<Inline>]` attribute. Local `inline let` expressions are not supported at the moment, although this is planned to change soon.

### Expressions and type information

F# is an expression-based language, while JavaScript is statement-based, so some F# expressions will be translated to statements depending on their contents. For example `if/then/else` is translated to either the conditional ternary operator `? :` or `if/then/else` depending on if the "then" and "else" branches are possible to translate as expressions or not. Similarly simple `match` expressions will be translated to conditional operators or `switch` depending on context.
Generally, expressions stick to their .NET semantics, for example loops over collections will use the `GetEnumerator` and `MoveNext` methods. A couple exceptions:
* F# code quotations are erased, they are translated as if they were their value. This is so that some helpers can use code quotations in .NET to take client-side expressions to encode, while on the client the argument is executable.
* WebSharper does not support reflection, its goal is to create efficient JavaScript with minimal overheads.  Type checks work as far as types are the same in JavaScript. You cannot do type checks on generics.
* Casting and conversions are happening in JavaScript too when the target types require it. Use the `As` helper for an unsafe cast from any type to any other.

### Structured types

- F# tuples and .NET `ValueTuple`s both get translated to JavaScript `Array`.
- F# unions (including `option` and `list`) are translated to classes or plain objects with property `$` containing the union case index, while properties `$1`, `$2`, etc. are matching the union fields. If the union type has no members, then it will be translated to a plain object, otherwise a class. The `Prototype` attribute can be used to override this, `[<Prototype>]` will force the output to be a JavaScript class, while `[<Prototype(false)>]` will force the output to be a plain object and translate methods to functions. A constructor function or static member is emitted for each union case.
  F# code:
  ```fsharp
  [<JavaScript>]
  type PlainUnion = 
      | A of int 
      | B of string
  
  [<JavaScript>]
  type UnionWithMember = 
      | C of int 
      | D of string
      
      member this.Text = 
          match this with 
          | C x -> string x
          | D x -> x  
  ```
  JavaScript output:
  ```javascript
  // File: WebSharper.Tests.PlainUnion.js
  export function B(Item){
    return{$:1, $0:Item};
  }
  export function A(Item){
    return{$:0, $0:Item};
  }
  
  // File: WebSharper.Tests.UnionWithMember.js
  import { Create } from "../WebSharper.Core.JavaScript/Runtime.js"
  export default class UnionWithMember {
    static D(Item){
      return Create(UnionWithMember, {$:1, $0:Item});
    }
    static C(Item){
      return Create(UnionWithMember, {$:0, $0:Item});
    }
    get Text(){
      return this.$==1?this.$0:String(this.$0);
    }
  }

  ```
- F# `list`s and `option`s are built-in union types, translated similarly.
- F# records are similarly translated to plain objects or classes. A record constructor function or static member `New` is emitted. Similarly to unions, if the record type has no members, then it will be translated to a plain object, otherwise a class. The `Prototype` attribute can be used the same way too.
  F# code:
  ```fsharp
  [<JavaScript>]
  type PlainRecord = 
      {
          A: int
          B: int
      }
  
  [<JavaScript>]
  type RecordWithMember = 
      {
          C: int
          D: int
      }
    
      member this.Total = 
          this.C + this.D
  ```
  JavaScript code:
  ```javascript
  // File: WebSharper.Tests.PlainRecord
  export function New(A, B){
    return{A:A, B:B};
  }
    
  // File: WebSharper.Tests.RecordWithMember
  import { Create } from "../WebSharper.Core.JavaScript/Runtime.js"
  export default class RecordWithMember {
    C;
    D;
    get Total(){
      return this.C+this.D;
    }
    static New(C, D){
      return Create(RecordWithMember, {C:C, D:D});
    }
  }
  ```
- F# anonymous records are always translated to plain objects.
- Only read-only structs are supported.

### Objects

Classes are translated to JavaScript classes. Overloads are supported by automatic renaming, and overrides are tracking the mapping from signature to JavaScript name so that the correct translated methods are overridden. Use the `[<Name>]` attribute when you want to specify the exact translated names for overloads. Multiple class constructors are combined into a single one which takes an automatically generated name for constructor as the first parameter. Helper static methods are generated to call the constructors from outside without knowing the names.

F# code:
```fsharp
type BaseClass(x) =
    let mutable x = x
    new() = BaseClass(0)
    member this.X = x
    [<Name "Incr_i">]
    member this.Incr(i) = x <- x + i
    abstract Incr: unit -> unit
    default this.Incr() = this.Incr(1)

type SubClass(x) =
    inherit BaseClass(x)
    override this.Incr() = base.Incr(2)
```
JavaScript code:
```javascript
// File: WebSharper.Tests.BaseClass.js
import Object from "../WebSharper.StdLib/System.Object.js"
export default class BaseClass extends Object {
  x;
  Incr_i(i){
    this.x=this.x+i;
  }
  get X(){
    return this.x;
  }
  Incr(){
    this.Incr_i(1);
  }
  static New(){
    return new this("New");
  }
  static New_1(x){
    return new this("New_1", x);
  }
  constructor(i, _1){
    if(i=="New"){
      i="New_1";
      _1=0;
    }
    if(i=="New_1"){
      const x=_1;
      super();
      this.x=x;
    }
  }

// File: WebSharper.Tests.SubClass.js
import BaseClass from "./WebSharper.Tests.BaseClass.js"
export default class SubClass extends BaseClass {
  Incr(){
    super.Incr_i(2);
  }
  constructor(x){
    super("New_1", x);
  }
}
```

### Interfaces

To make interface methods uniquely identifyable, WebSharper gives them a long name that contains the interface type name. You can use the `Name` attribute to set shorter fixed translated names.

F# code:
```fsharp
type MyInterface =
    abstract Incr: int -> int
    [<Name "IncrTwo">]
    abstract IncrTwo: int -> int

[<JavaScript>]
module Inst =
    let getIntfObj() =
        { new MyInterface with 
            member this.Incr x = x + 1 
            member this.IncrTwo x = x + 2 
        }
```
JavaScript code:
```javascript
// File: WebSharper.Tests.MyInterface
export function isMyInterface(x){
  return"IncrTwo"in x&&"WebSharper_Tests_MyInterface$Incr"in x;
}

// File: WebSharper.Tests.Inst
export function getIntfObj(){
  return{WebSharper_Tests_MyInterface$Incr(x){
    return x+1;
  }, IncrTwo(x){
    return x+2;
  }};
}
```

## WebSharper attributes

The following is an overview of all attributes provided by WebSharper to guide translation.

### Standard attributes

- `JavaScript` - see above in scope section.
- `Constant` - provide a literal to stand in as the value in translation.
- `Inline` - without a string argument, it makes a function inlined, with a string argument, it emits the JavaScript code snippet provided at the call point. For the latter, you can use `$0`, `$1`, or `$parameterName` in your JS snippet.
- `Direct` - provide a JavaScript code snippet to serve as the body of the function.
- `Pure` - marks the function as ok to eliminate when result is ignored, it should be used only if the function is deterministic and has no side effect.
- `Warn` - Gives a warning if annotated method is used from JavaScript code, but not for .NET calls.
- `Macro` - used for metaprogramming, implement a subclass of `WebSharper.Core.Macro` type to translate calls based on compile-time type or other information.
- `Generated` - used for metaprogramming, implement a subclass of `WebSharper.Core.Generator` type to emit contents of a function programmatically.
- `Name` - used for setting translated name when applies.
- `Proxy` - see above in proxies section.
- `InternalProxy` - see above in proxies section.
- `Stub` - marks a type be a stand-in for a JavaScript type with every member mapping to a member of the same name.
- `OptionalField` - marks F# option fields for autoconversion from JavaScript undefined or existing value.
- `Prototype` - marks a union/record/class to have a JavaScript class declaration, or `Prototype(false)` converts them into a plain object.

### Dependency-specific attributes

- `Require` - for non-module JavaScript and other code requirements.
- `Import` - for adding module-based imports. 

### Remoting-specific attributes

- `Remote` - marks a server-side method for remoting.
- `RemotingProvider` - on a server side method, specifies a client-side wrapper object that remoting calls will go through.

### Bundle-specific attributes

- `JavaScriptExport` - in Single Page Application, or Library Bundle projects, adds annotated function/type to the JavaScript output independent of dead code elimination.
- `SPAEntryPoint` - the entry point of a Single Page Application or Library Bundle projects on which dead code elimination can act.

### JSON-specific attributes

- `NamedUnionCases` - for union serialization, customize the name of the discriminator field.
- `DateTimeFormat` - customize the string format of the serialized DateTime values.

### TypeScript-specific attributes

- `Type` - for use on proxies, sets the TypeScript type for the JavaScript proxy.

### Sitelets routing-specific attributes

- `EndPoint` - specifies an URL or URL fragment for Sitelets routing.
- `Method` - specifies the HTTP method.
- `Json` - marks a field to deserialize request body JSON data into.
- `Query` - marks a field to fill in from query parameter.
- `FormData` - marks a field to fill in from request body form data.
- `Wildcard` - marks a field that will take any unrecognized remaining part of an URL as string.
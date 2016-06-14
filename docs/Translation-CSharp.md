# Translation and metaprogramming

WebSharper provides several ways to customize the way functions and
values are compiled to JavaScript:

* [Directly providing JavaScript code](#javascript);
* [Customizing the compiled name of the value](#name);
* [Classes that directly represent JavaScipt build-in types](#types);
* [Access JavaScipt properties dynamically](#dynamic);
* [Transforming the code](#meta) during compilation, a concept
  known as metaprogramming.

<a name="javascript"></a>
## Embedding JavaScript

There are two ways of directly inserting JavaScipt code into a WebSharper project.

### JavaScript function body

The `Direct` attribute takes a JavaScript expression as a string parameter.
This will get parsed by the compiler and any syntax errors will be reported in a
compile error message.
This parsed expression will be used as a function body, `return` is automatically
added for the last value.

If you don't want the use function from .NET, the `WebSharper.JavaScript.Interop.X<T>()` method
throws an exception of type `WebSharper.JavaScript.ClientSideOnly` with the message
"This function is intended for client-side use only.".

You can use placeholders for the function or method arguments.
For named parameters, the name with a `$` prepended is recognised.
For example:

    [Direct("$x + $y")]
    public static int Add(int x, int y) => Interop.X<int>();

Also you can access parameters by index.
In let-bound functions in modules and static methods of classes, the parameters
are indexed from 0.

    [Direct("$0 + $1")]
    public static int Add(int x, int y) => Interop.X<int>();
    
In instance methods, `$0` translates to the self indentifier, and method parameters
are indexed from 1.
(You can also use `$this` for the self identifier, but this recommended against, as
a parameter named `this` can override it, and it does not work for extension members
which are actually static methods in translated form.)

    [Direct("Math.sqrt($0.x * $0.x + $0.y * $0.y)")]
    member this.GetLength() => Interop.X<float>();

### Inlined JavaScript code

The `Inline` attribute  takes a JavaScript expression as a string parameter.
(It can also be used together with the `JavaScript` attribute to inline a function
translated from F#.)
This will be parsed, and inlined to all call sites of this function.
Only a subset of JavaScript operators and keywords can be used which can be translated
to the "core" AST used internally by WebSharper to optimize output.

Parameter placeholders work exactly as with `Direct`. 

    [Inline("$x + $y")]
    public static int Add(int x, int y) => Interop.X<int>();

### Constant

The `Constant` attribute takes a literal value as parameter.
It can annotate a property or a union case which will be translated to the literal provided.

<a name="name"></a>
## Naming

The `Name` attribute takes a string parameter and allows specifying
the name of a function or class in the translation.
For example:

    [Name("add")]
    public static int OriginalNameForAdd(int x, int y) => x + y;

<a name="types"></a>
## Types for interacting with outside JavaScript code

There are classes in the `WebSharper.JavaScript` namespace that are direct representations of ECMA standard
library JavaScipt types.
The `WebSharper.JavaScript` namespace declares a `.ToJS()` extension method on all .NET types to safely
convert them to their JavaScipt representation.

For JavaScipt functions, the `Function` class is an untyped representation, but if you know the signature of
the JavaScipt function, there are more strongly typed alternatives:

* For any function, that do not care about the `this` argument, you can use delegates.
* For functions that work with the `this` argument, use `ThisAction` and `ThisFunc` classes.
They have a constructor that takes a delegate, for which the first argument will have the `this` value.

    var logger = new ThisAction<object>(x => WebSharper.JavaScript.Console.Log(x));

* For functions taking variadic arguments, use `ParamsAction` and `ParamsFunc` classes.
* Finally for functions using the `this` value and have variadic arguments, use `ThisParamsAction` and `ThisParamsFunc`

<a name="dynamic"></a>
## Access JavaScipt properties dynamically

The `WebSharper.JavaScript` namespace declares a `.GetJS` extension method, that can be used to get JavaScipt properties dynamically.
Example: `x.GetJS<int>("size", "width")` is translated to `x.size.width` and usable as an `int` value.
You can use `x.GetJS<T>()` to just use the value of `x` exposed as another .NET type `T`.

### C# dynamic
You can use the `dynamic` type to access JavaScipt properties and functions without any extra helpers:

    dynamic d = names;
	d.getItems()[3].name; // translates directly to `d.getItems()[3].name`
	
Also, operators on dynamic values are translated directly if there is a JavaScipt equivalent.

<a name="meta"></a>
## Metaprogramming

Macro and generator documentation will be available for stable release of WebSharper 4.
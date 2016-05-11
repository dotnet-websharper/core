# Attributes

## Attributes in `WebSharper` namespace

The `WebSharper` namespace contains a number of attributes that allows customizing
the JavaScript compilation.

Every attribute that takes a type as parameter argument also accepts it as a string
containing the assembly-qualified name for a type.

### JavaScript

Marks an assembly, classes or members to be compiled to JavaScript.
If a class is marked, it is automatically inherited to nested classes and all members.
`[JavaScript(false)]` can exclude a class or member.
If a class is marked with `[JavaScript(false)]`, you can re-include a member with `[JavaScript]`.

The `Proxy` attribute also marks a class for JavaScript translation,
using the `JavaScript` attribute in addition is allowed but has no extra effect.

### Constant

Compiles all calls to a property getter to a constant `bool`, `double`, `int` or `string` value.

### Inline

Marks members for inline compilation to JavaScript.
You can use this attribute with or without a string argument.
`[Inline]` compiles the method body from C#, but compiled form of the member not appear as a separate
function but is applied everywhere the member is called.

`Inline("...")` allows specifying a JavaScript expression or function body (statements with a `return`).
In this case, the C# function body is ignored in the JavaScript translation.
If you use JavaScript-specific functionality and don't intend to call the method from server-side code,
the `WebSharper.JavaScript.Pervasives.X` helper is available to throw a `ClientSideOnly` exception.

Inline strings work as JavaScript code templates, they are parsed and checked at compile-time by WebSharper.
The arguments of the method can be accessed in two different ways:

* by ordinal: `$0`, `$1` and so on. If it is an instance member, `$0` is the `this` argument, otherwise the first method argument.
* by name: `$this` for the `this` argument. `$a` where `a` is the name of an argument. `$value` for the `value` argument of a property setter.

Overrides and interface implementations can't be inlined.

Example:

	// using static WebSharper.JavaScript.Pervasives;
	[Inline("$a + $b")]
	public static int Add(int a, int b) => X<int>();

    // alternative:
	[Inline("$0 + $1")]
	public static int Add(int a, int b) => X<int>();

### Direct

The `Direct` attribute must have a string argument, a JavaScript code template in the same form as `Inline`.
It works the same as `Inline`, only the code is differently structured.
This creates the JavaScript function body from the provided code as a prototype member or globally accessible function
instead of inlining it at every call point.

### Macro

The `Macro` attribute takes a type (or assembly-qualified name) as argument and can annotate methods, constructors and classes.
Macros allow a custom compilation logic to be applied in every place a method or constructor is called.
If you annotate a class, macros will execute on calls to any method or constructor of the class which does not
have any other WebSharper transation defined.

See macro documentation for details.

### Generated

The `Generated` attribute takes a type (or assembly-qualified name) as argument and can annotate methods and constructors.
Generators can be used to create a JavaScript function dynamically. 

### Name

The `Name` attribute takes a string argument and specifies the name of the member or class in the
JavaScript translation.

When naming a class or static member, you can also provide the full address of it by providing
an argument that contains dot characters. Or alternatively you can use a string array as the argument.

Overrides and interface implementations can't be explicitly named as they inherit the name from
the abstract member they are overriding or implementing.

### Proxy

The `Proxy` attribute takes a type (or assembly-qualified name) as argument.
Proxying allows using .NET types not annotated with `[JavaScript]` in the client-side by
providing alternative implementation to all or some of the members of the class.
The WebSharper common libraries contain a number of proxies for .NET framework classes.

Use the `Proxy` attribute on your implementation class, specifying the type it will proxy (replace)
in client-side code.

Proxy classes are recommended to be private or internal otherwise the WebSharper compiler will give a warning.
You can use members from a proxy type directly in the same assembly that it is defined in, but other assemblies
should use the proxied type.

You can define multiple proxies for the same single type, but only if the first proxy contains instance methods.
All following proxies must have only static methods.

### Remote

Marks a server-side function to be invokable remotely from the client-side.
See remoting documentation.

### Require
Annotates members with dependencies. The type (or assembly-qualified name) passed to the constructor
must implement Resources.IResourceDefinition and a default constructor.

### Stub
Marks members that should be compiled by-name.

### RemotingProvider
Indicates the client-side remoting provider that should be used by calls to this RPC method.
See remoting documentation.

### OptionalField
If a property has an F# option (`FSharpOption`) type, then adds automatic inlines 
so that a missing JavaScript field is converted to `None` (`null` in C#), otherwise `Some fieldValue`.

### DateTimeFormat
Defines the format used to de/serialize a DateTime field or union case argument.
The default is `"o"` (ISO 8601 round-trip format) for JSON serialization,
and `"yyyy-MM-dd-HH.mm.ss"` for URL parsing.

### SPAEntryPoint
Marks the entry point of a Single Page Application.
Must be a static method without arguments.

### NamedUnionCases
This is an F# only attribute, 

## Attributes in `WebSharper.Sitelets` namespace

With Sitelets, you can define the sitemap of your web application with a class hierarchy.
Attributes can be used to customize how the default router works.

### EndPoint
Indicates the URL fragment parsed by this action type.

### Method
Indicates the HTTP method used for the endpoint.

### Json
Indicates that the arguments for this endpoint class are parsed from the JSON in the request
body instead of from the URL path.
The value must be a primitive value, or an `FSharpOption` of it.

### Query
Indicates that a field or property properties value are parsed from the query parameters instead of from the URL path.

### FormData
Indicates that a field or property must be parsed from the request's body in 
form post syntax, ie. with the Content-Type being either `application/x-www-form-urlencoded` or `multipart/form-data`.
The value must be a primitive value, or an `FSharpOption` of it.

### Wildcard
Indicates that the last field or property parses all the remaining
path segments into an array.
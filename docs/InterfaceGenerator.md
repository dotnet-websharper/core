# WebSharper Interface Generator

The WebSharper Interface Generator (WIG) is a tool (build task) for generating
WebSharper bindings to JavaScript libraries.  Bindings allow
developers to access these libraries from typed F# code that gets
compiled to JavaScript by WebSharper.  While it is possible to create
bindings manually, WIG allows to write the binding definitions in F#,
making full use of the language to streamline repetitive tasks.

Simply put, WIG takes an F# value
representing a set of classes, interfaces, and members together with
their documentation and mappings to JavaScript, and generates a
binding assembly from that definition.  The binding is a .NET assembly
containing the generated types and method stubs annotated with raw
JavaScript code using the `InlineAttribute` custom attribute.

## Getting Started

WIG is included with the WebSharper installer.
To create a new project select the "Extension" template from WebSharper templates.
This project file contains the line required for the WIG build task to run.

    <WebSharperProject>InterfaceGenerator</WebSharperProject>

You get a small example in the `Main.fs` file, which ends in this:

    [<Sealed>]
    type Extension() =
        interface IExtension with
            member ext.Assembly =
                Definition.Assembly

    [<assembly: Extension(typeof<Extension>)>]
    do ()

This exposes the value defined by `Definition.Assembly` to the WIG compiler.
The library that you write is used just as a generator for this value,
any other code that it has will have no effect on the final assembly produced by the
WIG build task.

## Constructing Types

Defining classes, interfaces and member signatures requires an
abstraction for types.  Types are represented as
`WebSharper.InterfaceGenerator.Type.IType` values.
These values can describe both types from other assemblies (external) or
type definitions in current WIG project.

### Immutability and Identity

Adding members or attributes to a type declaration is mutable and returns the
type declaration for chaining.
However, all operators and helper functions work non-desctructively on member,
attribute and resource definitions.
This allows mutual recursion between types:

    let A = Class "A"
    let B = Class "B"

    A |+> Instance [
        "getB" => T<unit> ^-> B
    ] |> ignore // "GetB" method was addded to method list of A

    B |+> Instance [
        "getA" => T<unit> ^-> A
    ] |> ignore // "GetA" method was addded to method list of B

Example for immutability of members:

    let GetCount = Method "getCount" (T<unit> ^-> T<int>)
    let GetCountObs = GetCount |> Obsolete

    A |+> Instance [ GetCount ]
    |> ignore // A will have "GetCount" without the Obsolete attribute

    B |+> Instance [ GetCountObs ]
    |> ignore // B will have "GetCount" with the Obsolete attribute

### Operator Reference

<table>
    <thead>
        <tr>
            <td>Function</td>
            <td>Operator</td>
            <td>Description</td>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td><code>Method</code></td>
            <td><code>=></code></td>
            <td>Defines a method from name and signature</td>
        </tr>
        <tr>
            <td><code>Property</code></td>
            <td><code>=&#64;</code></td>
            <td>Defines a property with a getter and setter</td>
        </tr>
        <tr>
            <td><code>Getter</code></td>
            <td><code>=?</code></td>
            <td>Defines a read-only property</td>
        </tr>
        <tr>
            <td><code>Setter</code></td>
            <td><code>=!</code></td>
            <td>Defines a write-only property</td>
        </tr>
        <tr>
            <td></td>
            <td><code>?</code></td>
            <td>Defines a named parameter</td>
        </tr>
        <tr>
            <td></td>
            <td><code>^-></code></td>
            <td>Defines a function type</td>
        </tr>
        <tr>
            <td></td>
            <td><code>-*</code></td>
            <td>Defines the type of the <code>this</code> parameter on a function</td>
        </tr>
        <tr>
            <td></td>
            <td><code>*+</code></td>
            <td>Defines the <code>rest</code> parameter (ParamArray in .NET)</td>
        </tr>
        <tr>
            <td><code>Type.ArrayOf</code></td>
            <td><code>!|</code></td>
            <td>Defines an array type from its element type</td>
        </tr>
        <tr>
            <td></td>
            <td><code>!+</code></td>
            <td>Defines <code>arguments</code> parameter (single ParamArray in .NET)</td>
        </tr>
        <tr>
            <td></td>
            <td><code>!?</code></td>
            <td>Defines an optional parameter, property or return type</td>
        </tr>
        <tr>
            <td></td>
            <td><code>*</code></td>
            <td>Defines a tuple type or joins parameters</td>
        </tr>
        <tr>
            <td></td>
            <td><code>+</code></td>
            <td>Defines an overloaded parameter or a <code>Choice</code> property or return type</td>
        </tr>
        <tr>
            <td></td>
            <td><code>|=></code></td>
            <td>Copies type definition identifier or applies attributes</td>
        </tr>
        <tr>
            <td></td>
            <td><code>|+></code></td>
            <td>Adds members to a type definition</td>
        </tr>
    </tbody>
</table>

### Side cases

* When defining tuples, `*` expands the tuple if the left hand argument is already a tuple.
If you want to define the type `(A * B) * C`, you must use `Type.Tuple [ A * B; C ]`.

* When defining functions like `A * B ^-> C`, the tuple on the left hand side is
automatically converted to a `Type.Parameters` which creates multiple arguments
from the tuple elements.
If you want to describe a JavaScript function that do take a single 2-length array
as argument, you must convert it to a single parameter explicitly:
`(A * B).Parameter ^-> C`.

### External Types

External types can be defined by using the `T` type function, for example
`T<int>`, `<int>`, `T<list<string>>`, `<list<string>>`, `T<MyOtherLibrary.SomeType>`.

### Type Combinators

Simpler types can be combined to form more complex types, including
arrays, tuples, function types, and generic instantiations.

    Type.ArrayOf T<int>
        // array, equivalent to T<int[]>
        // alternate syntax: !| T<int>

    T<int> * T<float> * T<string>
        // tuple, equivalent to T<int * float * string>
        // alternate syntax: Type.Tuple [T<int>; T<float>; T<string>]

    T<int> ^-> T<unit>
        // function, equivalent to T<int -> unit>

    T<System.Collections.Generic.Dictionary<_,_>>.[T<int>, MyTypeDef]
        // adding type parameters to a generic type
        // compile-time error if number of parameters do not match

In addition, delegate types can be formed.  WebSharper treats delegate
types specially: their are compiled to JavaScript functions accepting
the first argument through the implicit `this` parameter. For example
when this can be helpful, consider following JavaScript function:

    function iterate(callback, array) {
        for (var i = 0; i < array.length; i++) {
            callback.call(array[i], i);
        }
    }

To bind this function to WebSharper one needs to provide a type for
the `callback` parameter, which is a function called with an element
of the array passed through the `this` implicit parameter and the
array index passed through the first parameter.  This can be achieved
thus:

    let callbackType = T<obj> -* T<int> ^-> T<unit>
    let iterateType  = callbackType * Type.ArrayOf T<obj> ^-> T<unit>


The type of the callback is then compiled to a delegate type in F#,
`Func<obj,int,unit>`.

### Self Placeholder

The `TSelf` type value will be evaluated to the type the defined member is added to.
This allows creating a member, list of members or `ClassMembers` value (list of members
marked with `Instance` or `Static`) and reuse it by adding it to multiple type declarations.

## Defining Members

The primary use of type values is the definition of member signatures,
methods, constructors, properties and fields, are defined.

### Methods

Method representations are constructed using the `Method` (short form:
`=>`) combinator that takes the name of the method and the
corresponding functional type.  Some examples:

    let methods =
        [
            Method "dispose" T<unit -> unit>
            Method "increment" (T<int> ^-> T<int>)
            "add" => T<int> * T<int> ^-> T<int>
        ]

Void return types and empty parameter lists are indicated by the
`unit` type, multiple parameters are indicated by tuple types.  It is
an error to define a method with a non-functional type.

### Parameter Names

By default, method parameters get autogenerated names.  You can
customize parameter names as follows:

    let methods =
        [
            Method "dispose" (T<unit>?object ^-> T<unit>)
            Method "increment" (T<int>?value ^-> T<int>)
            "add" => T<int>?x * T<int>?y ^-> T<int>
        ]

### Variable-Argument Signatures

F# supports variable-argument methods via the
`System.ParamArrayAttribute` annotation. WebSharper understands this
annotation and compiles such methods and delegates to
variable-argument accepting functions in JavaScript.  Here is the
syntax to define a variable-argument signature:

    let methods =
        [
            "t1" => !+ T<obj> ^-> T<unit>
            "t2" => T<string> *+ T<obj> ^-> T<unit>
        ]

When compiled to F#, these methods will have the following signatures:

    val t1 : ([<System.ParamArray>] args: obj []) -> unit
    val t2 : string * ([<System.ParamArray>] args: obj []) -> unit

### Optional Parameters

Parameters can be made optional:

    Method "exit" (!? T<string>?reason ^-> T<unit>)

Signatures such as the one above generate multiple members by implicit
overloading (see below).

### Implicit Overloads

Type unions facilitate describing JavaScript methods that accept
arguments of either-or types.  Type unions are implemented by implicit
overloading of generated members.  For example:

    "add" => (T<int> + T<string>) * (T<obj> + T<string>) ^-> T<unit>

This method can accept either `string` or an `int` as the first
argument, and either an `obj` value or a `string` as the second.  Four
overloads are generated for this signature.

### Properties

Properties can be generated with a getter, a setter or both.  Below
are the full and abbreviated syntax forms:

    let properties =
        [
            Getter "ReadOnly" T<int>
            Setter "WriteOnly" T<int>
            Property "Mutable" T<int>
        ]

    let shorthand =
        [
            "ReadOnly"  =? T<int>
            "WriteOnly" =! T<int>
            "Mutable"   =@ T<int>
        ]

#### Indexed properties

Properties can have indexers. `"" =@ T<string> |> Indexed T<int>` creates
an indexed property where `x.[n] : string` translates to `x[n]`.
If the property name is not empty:
`"Lines" =@ T<string> |> Indexed T<int>` creates an indexed property where
`x.Lines.[n]` translates to `x.Lines[n]`.

If you define a custom inline, use `$index` to refer to the index parameter.

### Constructors

Constructors definitions are similar to methods but do not carry a
return type.  Examples:

    let constructors =
        [
            Constructor T<unit>
            Constructor (T<int>?width * T<int>?height)
        ]

#### JavaScript Object Expression

`ObjectConstructor (T<int>?x * T<int>?y)` defines a .NET constructor with
JavaScript inline `{ x = $x, y = $y }`.

### Interfaces

Interfaces are defined using the `Interface` keyword and then extended
with members.

    Interface "IAccessible"

### Member Definitions

Member definitions are appended using the `|+>` combinator, for
example:

    Interface "IAccessible"
    |+> [
        "Access" => T<unit->unit>
        "LastAccessTime" =? T<System.DateTime>
    ]

Interface definitions take a `list<CodeModel.IInterfaceMember>`.
Class definitions take a `CodeModel.ClassMembers` value, which can be
constructed from a `list<CodeModel.IClassMember>` using the `Instance`
and `Static` functions.

### Inheritance

Interfaces can inherit or extend multiple other interfaces.  The
syntax is as follows:

    Interface "IAccessible"
    |=> Extends [T<System.IComparable>; T<System.IEnumerable<int>>]
    |+> [
        "Access"         => T<unit->unit>
        "LastAccessTime" =? T<System.DateTime>
    ]

### Classes

Class definition is very similar to interface definition.  It starts
with the `Class` keyword:

    let Pear =
        Class "Pear"
        |+> Static [
            "Create" => T<unit> ^-> TSelf
        ]
        |+> Instance [
            "Eat"     => T<unit->unit>
            "IsEaten" =? T<bool>
        ]

### Class Inheritance

The syntax for class inheritance is as follows:

    Class "ChildClass"
    |=> Inherits BaseClass

### Interface Implementation

The syntax for class inheritance is as follows:

    Class "MyClass"
    |=> Implements [T<System.IComparable>]

### Nested Classes

Class nesting is allowed:

    Class "MyClass"
    |=> Nested [
        Class "SubClass"
    ]

### Generics

#### Generic Types

Generic types and interfaces are defined by prefixing the definition
with the code of the form `Generic - fun t1 t2 .. tN ->`.  The
`t1..tN` parameters can be used as types in the definition and
represent the generic parameters.  For example:

    Generic - fun t1 t2 ->
        Interface "IDictionary"
        |+> [
            "Lookup"      => t1 ^-> t2
            "ContainsKey" => t1 -> T<bool>
            "Add"         => t1 * t2 -> T<unit>
            "Remove"      => t1 -> T<unit>
        ]

This compiles to the following signature:

    type IDictionary<'T1,'T2> =
        abstract member Lookup : 'T1 -> 'T2
        abstract member ContainsKey : 'T1 -> bool
        abstract member Add : 'T1 * 'T2 -> unit
        abstract member Remove : 'T1 -> unit

This syntax can produce up to 4 type parameters.
To have more, the `Generic -` helper can be nested, or
use `GenericN n - fun [t1; t2; .. tN] ->`.
Although this gives an incomplete pattern match warning, there will be no
runtime errors if the matching the list for the provided length `n`.

#### Generic Methods

Similarly, generic methods are generated using lambda expressions, for example:

    Generic - fun t ->
        "length" => T<list<_>>.[t] ^-> T<int>

This code would generate the following F# signature:

    val Length<'T> : list<'T> -> int

You can use `Generic %` to add the same generics to a list of members,
`Generic *` to add the same generics to a `ClassMembers` value.
Also `Generic + ["a"; "b"; ...] - ...` specifies the names of the type parameters.

#### Type Constraints

You can now set type constraints on parameters using `p.Constraints <- [...]` inside the lambda passed to `Generic -`. Previous `WithConstraints` helper is removed as we want to have all helper functions named `With...` to be non-destructive.

## Modifiers

### Documentation Comments

Documentation comments can be added using the `WithComment` function.

## Customizing JavaScript

By default, inline JavaScript definitions are inferred for all methods
and properties from their names.  This is intuitive and convenient but
not fully general.  Is is therefore possible to bypass the inferred
inlines and customize the generated bindings.

### Default Inline Generation

Default bindings are name-based.  For example, a static function
called `foo` with three arguments on a class called `Bar`, produces
the JavaScript inline `Bar.foo($0,$1,$2)`.

Generated .NET names are automatically capitalized, so that this
function is accessible as `Bar.Foo` from F#.

Qualified names can be used on classes that are accessible in
JavaScript with a qualified name, for example:

    Class "geometry.Point"

This generates a .NET class `Point` which binds all static members as
`geometry.Point.foo()` in JavaScript.

### Inline Transformations

#### Functions

The `FSharpFunc<'TArg, 'TRes>` type (which is used for lambdas by the F# compiler)
always take one parameter (which can be a tuple).
In WebSharper translation, these become JavaScript functions taking a single
fixed-length array (although curried functions only used in local scope are optimized).
However, it is often required that we pass functions defined in F# to a JavaScript
library (for example event handlers, callbacks, functional-style libraries).
WIG automatically converts between these function calling conventions.

#### Choice

Union types (for example `T<int> + T<string>`) can create method overloads,
but also `Choice` typed properties or method return types when the cases can be
distinguished in JavaScript using the `typeof`, `Array.isArray` and the
`arr.length` functions.
In F# this means either at most one array case or possibly multiple tuple cases
with all different length, at most one number type (including `DateTime` and
`TimeSpan`, which are proxied as a Number), `string`, `bool`, and at most one other
object type.
If there are cases which can't be separated, the type will default to `obj`.

#### Option

By using the `!?` operator on the type of a property, it will be an option
type in F# which is converted to and from an existing or missing field on a
JavaScript object.
On method returns, undefined is converted to `None`, all other values
(including `null`) to `Some ...`.

#### Custom Inline Transformations

You can add a custom defined inline transformation with the `WithInterop` helper.
This takes a record with an `In` and an `Out` field, both `string -> string`.
For example:

    let Index =
        T<int> |> WithInterop {
            In  = fun s -> s + " - 1"
            Out = fun s -> s + " + 1"
        }

Use this `Type.Type` value instead of `T<int>` in your member
declarations where you want to handle an index as 1-based in your code,
but pass it to and get it from a 0-based value in a JavaScript library.
On method parameters and property setters the `In` function will be used on the
parameter or property value in the automatic inline. On method return values and
property getters the `Out` function will be used on the whole inline of the
method or property getter.

#### Erasing Inline Transformations

Use the `WithNoInterop` helper to clean any automatic and custom inline
transformations from a `Type.Type` value.

### Customizations

#### Custom Names

The simplest form of customization allows to decouple the .NET name of
a member from the name used by the inline generation process.  This is
done by the `WithSourceName` function.  For example:

    "ClonePoint" => Point ^-> Point
    |> WithSourceName "clone"

This generates a method that is available as `ClonePoint` from .NET
but calls `clone` in JavaScript.

#### Custom Inline Methods

The method and constructor inlines can be set explicitly by using
`WithInline`.

#### Custom Inline Properties

Properties have separate inlines for the getter and the setter
methods.  These can be set explicitly by using `WithGetterInline` and
`WithSetterInline` respectively.

#### Custom Inlines Using Transformations

To define a custom inline for a method that still makes use of the default or
custom inline transformations on parameters and return value, use the
`WithInteropInline` helper.
It takes a function typed `(string -> string) -> string`, use the provided
function on a parameter name or index to get its transformed inline.
For example, defining an `on` event setter with function argument detupling:

    "onChange" => (T<int> * T<obj> ^-> T<unit>)?callback ^-> T<unit>
    |> WithInteropInline (fun tr -> "$this.on('change', " + tr "callback" + ")"

Similar helpers exists for property getters and setters:
`WithInteropGetterInline` and `WithInteropSetterInline`.
For getters, provided function is only usable for transforming `"index"`
in the case of an indexed property, and for setters transforming `"value"` or `"index"`.

#### Obsoleted Members and Types

Use the `Obsolate` helper to mark a type or member definition with
`System.ObsoleteAttribute`.
`ObsolateWithMessage` also sets a custom warning message.

## Best Practices

The benefit of using F# for generating the bindings is that repetitive
tasks or patterns can be distributed as functions.  Several such
patterns are pre-defined in the standard library.

### Configuration Class Patterns

These patterns for constructing member lists are useful for describing
JavaScript configuarion objects.
Configuration objects typically are simple collections of fields, most
of them optional, that are used to describe how another object it to
be constructed or operate.  Let us take a simple example:

    let MyConfig : Class =
        Class "classname"
        |+> Pattern.RequiredFields [
                "name", T<string>
            ]
        |+> Pattern.OptionalFields [
                "width", T<int>
                "height", T<int>
            ]

This definition would produce a class useable from F# that would
compile to simple JavaScript object literals:

    MyConfig("Alpha", Width=140)

The `Width` property is write-only, but there is also a `WidthOpt`
property generated which allow access to the optional field typed as
an F# `option` value.

`Pattern.ObsoleteFields` can be used similarly, to create a list of
properties with the `Obsolete` attribute.

### Enumeration Patterns

JavaScript functions often accept only a fixed set of constants, such
as strings.  Typing such parameters with `string` would be misleading
to the user.  The enumeration pattern allows to generate a type that
is limited to a specific set of constants, specified as either inlines
or strings literals.  See `Pattern.EnumInlines` and
`Pattern.EnumStrings`, both of which generate `Class` values.

## Assembly and Namespaces

You have to provide the WIG compiler with a single `CodeModel.Assembly`
value as described in the "Getting Started" section.
Construct this with the `Assembly` helper which takes a list of
`CodeModel.Namespace` values which can be created with the `Namespace` helper
specifying its name and a list of `CodeModel.NamespaceEntity` values.
These latter can be type definitions or resource types.

### Missing Type Definitions

If you have a type definition which you refer to but does not get included in a
namespace in the assembly definition, when building the library, you will get
an error.

## Resources

You can define WebSharper resource classes using the `Resource` function.
Pass it to `AssemblyWide` to make the resource loaded if anything from the
currently created assembly is used.

    Namespace "WebSharper.JQuery.Resources" [
        Resource "JQuery" "http://code.jquery.com/jquery-1.11.2.min.js"
        |> AssemblyWide
    ]

Use the `Requires` helper to add a list of defined resources as a dependency to a
type definition or another resource definition.
Use the `RequiresExternal` to add a list of resource classes from another assembly
to a type definition or a resource definition.

# WebSharper Interface Generator

The WebSharper Interface Generator (WIG) is a tool for generating
WebSharper bindings to JavaScript libraries.  Bindings allow
developers to access these libraries from typed F# code that gets
compiled to JavaScript by WebSharper.  While it is possible to create
bindings manually, WIG allows to write the binding definitions in F#,
making full use of the language to streamline repetitive tasks.

WIG includes an assembly with binding-generating code and Visual
Studio project templates.  Simply put, WIG takes an F# value
representing a set of classes, interfaces, and members together with
their documentation and mappings to JavaScript, and generates a
binding assembly from that definition.  The binding is a .NET assembly
containing the generated types and method stubs annotated with raw
JavaScript code using the `InlineAttribute` custom attribute.

## Getting Started

WIG installs together with WebSharper.  To create a new project open
Visual Studio and select an "Extension" template from WebSharper
templates.

Below you will find a sample binding definition. For more examples,
please check out the
[open-source code](http://bitbucket.org/IntelliFactory/) by
IntelliFactory.

    module WebSharperExtension.Definition

    open IntelliFactory.WebSharper.InterfaceGenerator

    let I1 =
        Interface "I1"
        |+> [
            "test1" => T<string> ^-> T<string>
            "radius1" =@ T<float>
        ]

    let I2 =
        Generic / fun t1 t2 ->
            Interface "I2"
            |+> [
                Generic - fun m1 -> "foo" => m1 * t1 ^-> t2
            ]

    let C1 =
        let C1T = Type.New ()
        Class "C1"
        |=> C1T
        |+> Protocol [
            "foo" =% T<int>
        ]
        |+> [
            Constructor (T<unit> + T<int>)

            "mem" =>
                (T<unit> + T<int> ^-> T<unit>)

            "test2" =>
                (C1T -* T<int> ^-> T<unit>) * T<string> ^-> T<string>

            "radius2" =@ T<float>
            |> WithSourceName "R2"

            "length" =% T<int>
            |> WithSourceName "L2"
        ]

    let Assembly =
        Assembly [
            Namespace "MyNamespace" [
                I1
                Generic - I2
                C1
            ]
        ]

## Constructing Types

Defining classes, interfaces and member signatures requires an
abstraction for types.  Types are represented as
`IntelliFactory.WebSharper.InterfaceGenerator.Type.IType` values.
These values describe system, user-defined, existing, and generated
types.

### Existing Types

Existing system and user-defined types can be defined by using the `T`
combinator with a generic parameter:

    let types : list<Type.IType> =
        [
            T<int>
            T<list<string>>
            T<MyType>
        ]

### Type Combinators

Simpler types can be comined to form more complex types, including
arrays, tuples, function types, and generic instantiations.

    [
        Type.ArrayOf T<int>
        T<int> * T<float> * T<string>
        Type.Tuple [T<int> * T<float>; T<string>]
        T<int> ^-> T<unit>
        T<System.Collections.Generic.Dictionary<_,_>>.[T<int>,T<string>]
    ]

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

### New Types

In addition to existing user-defined and system types, there are new
types that correspond to the classes and interfaces being defined.
Class and interface definitions implement the `Type.IType` interface
and can be used where types are expected.  When this is inconvenient,
as it often is, for example, with mutually recursive classes, new type
values can be constructed and used before being associated with a
particular class or interface definition:

    let Widget   = Type.New()
    let Callback = Widget ^-> T<unit>

    let WidgetClass =
        Class "Widget"
        |=> Widget

The last line associates the `Widget` type with the `WidgetClass`
definition.

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

Due to their prevalence in F#, by default all methods are generated as
static and public.  See Instance Member Definitions, Access Modifiers,
Static and Instance Modifiers.

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

### Constructors

Constructors definitions are similar to methods but do not carry a
return type.  Examples:

    let constructors =
        [
            Constructor T<unit>
            Constructor T<int>?width * T<int>?height
        ]

### Fields

Fields are generated using a similar syntax to properties:

    let fields =
        [
            Field "width" T<int>
            Field "height" T<int>
        ]

    let shorthand =
        [
            "width"  =% T<int>
            "height" =% T<int>
        ]

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

### Inheritance

Interfaces can inherit or extend multiple other interfaces.  The
syntax is as follows:

    Interface "IAccessible"
    |=> Extends [T<System.IComparable>; T<System.IEnumerable<int>>]
    |+> [
        "Access"         => T<unit->unit>
        "LastAccessTime" =? T<System.DateTime>
    ]

### Clases

Class definition is very similar to interface definition.  It starts
with the `Class` keyword:

    Class "Pear"

### Static Member Definitions

Static members are added using the `|+>` combinator:

    let Pear =
        let Pear = Type.New()
        Class "Pear"
        |=> Pear
        |=> [
            "Create" => T<unit> ^-> Pear
        ]

### Instance Member Definitions

Instance members are usually added using the `Protocol` combinator:

    Class "Pear"
    |+> Protocol [
        "Eat"     => T<unit->unit>
        "IsEaten" =? T<bool>
    ]

The use of the `Protocol` function is equivalent to transforming the
methods with the `Instance` function prior to inclusion.

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
with the code of the form `Generic / fun t1 t2 .. tN ->`.  The
`t1..tN` parameters can be used as types in the definition and
represent the generic parameters.  For example:

    Generic / fun t1 t2 ->
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

#### Generic Methods

Similarly, generic methods are generated using lambda expressions, but
the syntax is now `Generic - fun t1 .. tN ->`, for example:

    Generic - fun t ->
        "length" => T<list<_>>.[t] ^-> T<int>

This code would generate the following F# signature:

    val Length<'T> : list<'T> -> int

## Modifiers

### Access Modifiers

By default, all members and types are generated with the public access
modifier.  This can be changed by applying one of the four access
modifier setters: `Public`, `Internal`, `Protected` and `Private`.

### Static and Instance Modifiers

By default, all members are generated static.  Members can be made
static or instance by using the `Static` or `Instance` functions.
Interface definitions automatically apply the `Instance` function.

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

Properties have separete inlines for the getter and the setter
methods.  These can be set explicitly by using `WithGetterInline` and
`WithSetterInline` respectively.

## Best Practices

The benefit of using F# for generating the bindings is that repetitive
tasks or patterns can be distributed as functions.  Several such
patterns are pre-defined in the standard library.

### Configuration Class Pattern

This pattern is useful for describing JavaScript configuarion objects.
Configuration objects typically are simple collections of fields, most
of them optional, that are used to describe how another object it to
be constructed or operate.  Let us take a simple example:

    let MyConfig : Class =
        Pattern.Config "classname" {
            Required =
            [
                "name", T<string>
            ]
            Optional =
            [
                "width", T<int>
                "height", T<int>
            ]
        }

This definition would produce a class useable from F# that would
compile to simple JavaScript object literals:

    MyConfig("Alpha", Width=140)

#### Enumeration Pattern

JavaScript functions often accept only a fixed set of constants, such
as strings.  Typing such parameters with `string` would be misleading
to the user.  The enumeration pattern allows to generate a type that
is limited to a specific set of constants, specified as either inlines
or strings literals.  See `Pattern.EnumInlines` and
`Pattern.EnumStrings`, both of which generate `Class` values.


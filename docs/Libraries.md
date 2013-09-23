# Developing JavaScript Libraries

Developing JavaScript libraries with WebSharper is as simple as
writing F# code, annotating it with a few custom attributes, compiling
it with F#, and running WebSharper to generate the JavaScript:

    module MyModule =

      [<JavaScript>]
      let rec Factorial n =
        match n with
        | 0 -> 1
        | n -> n * Factorial (n - 1)

This section provides the overview of the required custom attributes,
the F# language features and the F# standard library functions
supported by WebSharper in the JavaScript environment.

## Member Annotations

There are required and optional custom attribute annotations that
influence how F# code gets compiled to JavaScript.  To be useable from
the client-side code, any member must be annotated with either the
`JavaScript` attribute or one of the attributes inheriting from
`AbstractInlineAttribute`.  To customize the name in the compiled
JavaScript output, it might also be annotated with an attribute
inheriting from `AbstractNameAttribute`.

### JavaScriptAttribute

`JavaScriptAttribute` marks members for compilation into
JavaScript. It is the single most important attribute in
WebSharper. The annotated members are translated to JavaScript by the
WebSharper compiler by inspecting and translating their F# bodies.

For example:

    module MyModule =

      [<JavaScript>]
      let rec Factorial n =
        match n with
        | 0 -> 1
        | n -> n * Factorial (n - 1)

The attribute is implemented as an alias for the
`ReflectedDefinitionAttribute` that comes with F#.  The F# comiler
recognizes members marked with this attribute and stores their
reflected (abstract syntax tree or quotation) form within the
resulting assembly, in addition to compiling them to IL as regular
members. WebSharper compiler is then able to find the quoted form of
the members and translate them to JavaScript.

### Naming Attributes

These attributes influence the member names as in JavaScript. The base
class, `Naming.AbstractNameAttribute`, allows to create custom
attributes with arbitrary logic for determining the compiled name.
This is useful to avoid naming clashes.

A simple implementation, the `NameAttribute`, explicity sets the
JavaScript-compiled names of members and types.  For example:

    [<Stub>]
    [<Name "my.package.Date">]
    type Date =
        /// Returns the day of the month.
        [<Name "getDate">]
        member this.GetDate() = 0

### Inlining Attributes

Inlining attributes mark functions for inline compilation to
JavaScript. The base class, `Inlining.AbstractInlineAttribute`, allows
to create custom attributes with arbtirary macro-expansion logic.
Three common forms are provided: `InlineAttribute`,
`ConstantAttribute` and `StubAttribute`.

#### InlineAttribute

`InlineAttribute` is a simple attribute that specifies that members
are to be compiled inline.  This attirbute either complements the
`JavaScriptAttribute`, or serves standalone with a JavaScript template
string. The following two forms are equivalent:

    [<Inline>]
    [<JavaScript>]
    let Add (x: int) (y: int) = x + y

    [<Inline "$x + $y">]
    let Add (x: int) (y: int) = 0

The sytnax of the template string is regular JavaScript. Variables
that start with `$` are treated as placeholders.  There are named
(`$x`), positional (`$0`), and special (`$this`, `$value`)
placeholders.  To use an actual variable that starts with a `$` sign,
duplicate the sign, as in `$$x`.

#### ConstantAttribute

`ConstantAttribute` allows members to compile to constant
literals. Its most common use is to annotate union cases.  For
example:

    type Align =
      | [<Constant "left">]   Left
      | [<Constant "center">] Center
      | [<Constant "right">]  Right

With these annotations, `Align.Left` is compiled as literal `"left"`,
and pattern-matching against any union case is compiled as an equality
test against the corresponding literal.

This pattern is useful for providing type safety for JavaScript code.

### StubAttribute

This attribute commonly marks types that expose JavaScript-implemented
functionality to WebSharper.  `StubAttribute` is useful for enabling
WebSharper code to consume and interoperate with legacy and
third-party JavaScript code.

Methods and fields on types marked with `StubAttribute` that are not
marked with special translation attributes such as
`JavaScriptAttribute` are translated by-name.  Methods do not have to
have a meaningful body, but should be correctly typed.

Sample usage:

    [<Name [| "Date" |]>]
    [<Stub>]
    type Date() =

      /// Returns the day of the month.
      member this.getDate() = 0

      /// Returns the day of the week.
      member this.getDay() = 0

      /// Returns the year.
      member this.getFullYear() = 0

The above example exposes to F# code some of the functionality of the
`Date` object as present in most JavaScript environments (and
specified in ECMA-262 3rd ed.

## F# Language Coverage

Most of the F# language features are directly supported by WebSharper.
The philosophy is to produce readable, straightforward JavaScript
code, making it possible to analyze the output and use it from
external JavaScript code or apply JavaScript-targeting tools.

### Data Representation

In general, matching JavaScript data types are reused, where possible,
to represent F# data types:

* F# numbers, booleans, strings, and arrays are represented
  directly as their JavaScript counterparts.
* F# lambda expressions are directly compiled to JavaScript lambda
  expressions.
* F# algebraic data types are represented as JavaScript objects.  In
  particular, tuples are represented as arrays, records as objects
  with matching field names, and unions as objects with field names of
  the form `$n` where `n` is the field position.
* F# objects are represented as JavaScript objects, with fields and
  members as JavaScript fields.

JavaScript representations of F# data types:

| F#            | JavaScript                                     |
|---------------|------------------------------------------------|
| null          | null                                           |
| "foo"         | "foo"                                          |
| true          | true                                           |
| 1             | 1                                              |
| 1.25          | 1.25                                           |
| (1, 2)        | `[1, 2]`                                       |
| `[| 1 |]`     | `[1]`                                          |
| None          | `{$: 0}`                                       |
| Some 1        | `{$: 0, $0: 1}`                                |
| `[1; 2]`      | `{$: 1, $0: 1, $1: {$: 1, $0: 2, $1: {$: 0}}}` |
| `{A = 1}`     | `{A: 1}`                                       |

#### Arrays

Arrays are represented directly as JavaScript arrays.
Multi-dimensional arrays are not currently supported.

#### Tuples

Tuples are represented as JavaScript arrays.

#### F# Records

Records are represented as JavaScript objects.

For example, consider this code:

    type R =
      {
        a: int
        b: string
      }

    [<JavaScript>]
    let F() =
      { a = 1; b = "foo" }

This will compile to:

    function F() {
      return {a: 1, b: "foo"};
    }

#### F# Unions

Unions are represented as JavaScript objects, with fields for the
union case tag and every field.  Consider:

    type U =
        | A
        | B of int * string

    [<JavaScript>]
    let F() = [| U.A; U.B(1, "!") |]

This will compile to:

    function F() {
      return [{$: 0}, {$: 1, $1: 1, $2: "!"}];
    }

#### Enumerations

Enumerations with integer values are supported for both construction
and pattern-matching.

For example, consider this code:

    type E =
      | One = 1
      | Two = 2

    [<JavaScript>]
    let F () = E.One

It will compile to:

    function F() {
      return 1;
    }

Enumerations do not require any annotations. Enumeration support
includes standard enumerations such as `System.DayOfWeek`.

### Functional Features

JavaScript is a functional language and therefore allows most of the
F# features to be represented directly.  Two notable omissions are its
lack of support for static typing and tail-call optimization.

First-class functions and closures map directly to their JavaScript
counterparts.

Methods are uncurried during compilation. For example:
`let f x y z = ...` at module level translates to the equivalent of
`function f(x, y, z) ...`

Curried lambda functions a translated directly:

    (fun x y -> ...)

Translates to:

    function (x) { return function (y) { ... }}

Lambda functions that accept tuples are compiled to accept either a
single or multiple arguments.  When called with a single argument, the
function expects the argument to be an array representing the tuple.
When called with multiple arguments, it assumes the arguments to be
the tuple components.  For example:

    let f = (fun (x, y) -> ...)

Given the above definition, JavaScript can call `f` in two ways:

    f(1, 2)
    f([1, 2])

Types are erased during compilation.

Tail call optimization is not currently supported. Future versions of
WebSharper will support it with a combination of local optimizations
that transform recursive functions to loops and a global optimization
with trampolining.

### Object-Oriented Features

Inheritance is modelled with JavaScript prototype chains. For example:

    type A [<JavaScript>]() =
        class end

    type B [<JavaScript>] () =
      inherit A()

Translates to the equivalent of:

    function A() {...}
    function B() {...}
    B.prototype = new A();

Chaining the prototypes allows JavaScript objects to inherit instance
members and interface implementations from the superclasses.

Interfaces are supported structurally. A JavaScript object is assumed
to implement an interface if it has methods with matching names. Just
as types, interfaces are therefore an F#-level concept that gets
erased during compilation. Type tests against interfaces are not
compiled. While F# allows a class to implement two interfaces with
clashing method names, using distinct method implementations for each,
it is an error to do so in WebSharper.

#### Equality and Hashing

JavaScript notion of pointer equality does not match structural
equality required by F#.  Moreover, JavaScript does not provide a
generic hash primitive, `obj -> int`.  To model these F# features,
WebSharper:

1. Implements object hashing by destructively modifying the hashed
   object and assigning a freshly generated unique hash to one of the
   object's fields.  Subsequent calls to the `hash` function will read
   the field.

2. Implements custom (structural) hashing by overriding `GetHashCode`
   for datatypes that require it.  The `hash` function always checks
   for the presence of `GetHashCode` before falling back to the
   generic implementation.

3. Implements a generic equality algorithm that recursively traverses
   and compares all fields of the two objects being compared.

4. Allows to override `Equals` and provide a custom equality
   logic. The equality primitive always checks for the presence
   `Equals` before falling back to the generic implementation.

#### Comparisons

Structural comparisons are modelled in a manner similar to equality
and hashing.  A generic implementation works for all objects by
recursively comparing their fields, respecting `IComparable`
implementations when those are provided by the user.

### Limitations

This section documents the limitations of F# language support in
@WebSharper{} and possible workarounds to these limitations.

#### Inner Generic Functions

Due to F# quotations limitations, the following code does not compile
under F#:

    [<JavaScript>]
    let F() =
      let id x = x
      id 5

The workaround is to specialize the generic function to a concrete
type, or lift it to the module level:

    [<JavaScript>]
    let id x = x

    [<JavaScript>]
    let F() = id 5

#### Anonymous Interface Implementations

Another limitation of F# quotations prohibits the following code:

    [<JavaScript>]
    let F () =
      {
        new System.IDisposable with
          member this.Dispose() = ()
      }

The workaround is to provide an explicit name to the class:

    type MyDisposable = | D with
      interface System.IDisposable with
        [<JavaScript>]
        member this.Dispose() = ()

    [<JavaScript>]
    let F () = D :> System.IDisposable

#### Record Expressions in Constructors

F# reflected definitions provide insufficient information about the
record expressions in object constructors, preventing `WebSharper`
from correctly compiling them.

A simple example:

    type T =
      [<JavaScript>]
      new () = {}

Workaround: avoid record expressions, use simple constructors with
overloads if necessary.

    type T [<JavaScript>] () =
      class end

#### Operator Overloading

This feature is not currently fully supported.  For example, `a + b`
expression translation ignores static `op_Addition` methods on the
type of `a`.

#### Units of Measure

This feature is not currenty supported.

#### Recursive Values

The following does not translate:

    type R = { R : R }
    let rec r : R = { R = r }

#### Generics Limitations

Certain uses of generic arguments are invalid because of type erasure,
for instance:

    let F<'T>() =
        try () with :? 'T -> ()

For the same reason, JSON serialization fails with generic functions,
for example:

    [<Rpc>]
    let F<'T> x = x

    [<JavaScript>]
    let G<'T> x = F x

This will fail to compile because the concrete type `'T` is not
statically known.

## F# and .NET Library Coverage

WebSharper includes a reasonably comprehensive F# and .NET standard
library coverage, allowing to use the familiar APIs in JavaScript,
including such modules and classes as `List`, `Array`, `Map`, `Set`,
`Async`, `Event`, `DateTime`, `TimeSpan`, `Dictionary`, `Stack`,
`Queue`. The support for these classes is sometimes incomplete, with a
focus on functionality that is reasonable to implement and useful to
have available on the client.  `WebSharper` will warn you if you
attempt to use a feature that is not supported.

## JavaScript Library Coverage

WebSharper makes it easy to access JavaScript APIs in a typed way from
F# by shipping bindings to JavaScript libraries. While a lot of these
are available as WebSharper, the standard distribution ships:

* The `IntelliFactory.WebSharper.JavaScript` module with common
  utilities, such as getting or setting fields on JavaScript objects,
  doing `alert` or `setTimeout` calls, and the like.

* JavaScript standard library bindings based on the ECMA 262 3rd
  edition. Consult the `IntelliFactory.WebSharper.EcmaScript`
  namespace for details.

* [DOM level 3](http://www.w3.org/DOM/) bindings.  Consult the
  `IntelliFactory.WebSharper.Dom` namespace for details.

* [jQuery](http://jquery.com) bindings. Consult the
  `IntelliFactory.WebSharper.JQuery` namespace for details.



# Developing Bindings to JavaScript Libraries

WebSharper is designed to enable the programmer to easily interoperate
with existing JavaScript code. This section documents the techniques
and guidelines for creating F# bindings to existing JavaScript
libraries.

The development of WebSharper bindings to JavaScript involves:

* Writing class and function stubs.
* Providing static types for the stubs, possibly writing wrapper types
  or using inlining where appropriate.
* Defining CSS, JavaScript and other resources and packaging the
  bindings library.
* Providing configuration sections for the library users.

## Stubs

A stub is an F# class or a function that has no F# implementation, but
instead represents existing JavaScript code. The stubs provide F#
identity, type safety and code completion to JavaScript-implemented
functionality.

For example, assume a JavaScript file with the following declarations:

    function counter() {
        var value = 0;
        return function () { return value++; };
    }

    function Counter() {
      this.value = 0;
      this.next = function () {
        this.value ++;
        return this.value;
      };
    }

The corresponding F# module with stubs might look like this:

	module Counter =

	  [<Name "counter">]
	  [<Stub>]
	  let makeCounter () : (unit -> int) = X<_>

	  [<Stub>]
	  type Counter() =

		[<DefaultValue>]
		val mutable value: int

		[<Name "next">]
		[<Stub>]
		member this.Next() : int = 0

Given the stub definitions, the following F# code:

	open Counter
	let c = new Counter()
	c.Next()

Will translate to the equivalent of:

	var c = new Counter()
	c.next()

Things to note:

* Marking a class declaration with `StubAttribute` enables all its
  members for use from F#.

* `StubAttribute`-annotated members do not have to have meaningful
  bodies if they are not intended to be called on the server
  side. `Unchecked.defaultof<_>` or its shorthand `X<_>` is acceptable
  as a body of any such member.

* `NameAttribute` can be provided when the inferred names (F# names)
  do not match the desired compiled names.

## Static Typing

Stubs must provide types for all represented methods. The bindings
author is expected to pick or construct types that reflect the
expectations of the JavaScript code. To do this effectively requires
an awareness of how WebSharper represents F# data in JavaScript.

For cases when static typing is difficult or impossible to determine,
it is always legal to give all argument and return parameters the
`obj` type. This approach gives the least information to the user, but
is viable if the user is to properly cast the parameters. Note that
`box`, `unbox`, `upcast` and `downcast` are all implemented as the
identity function in WebSharper, and are safe when the source and
target types of the cast have the same data representation, as all
type information is erased during compilation.

Common scenarios for providing static typing to stubs include:

* JavaScript code expects a scalar, such as a string, a number, or a
  boolean). Use the matching F# type `string`, `int`, `double`,
  `bool`.
* JavaScript code expects an array. If the array has a fixed known
  length, use an F# tuple. If the array varies in length and is
  monomorphic, use an F# array. If the array varies in length and is
  polymorphic, use `obj []`.
* JavaScript code expects a first-class function}.  If at all
  possible, determine the parameter and return types and use
  `p1 * p2 * ... * pn -> r`.
* JavaScript code expects a string from a fixed set of possible
  values.  Use `ConstantAttribute` to create a new union type to
  represent this fixed set of values.
* JavaScript code expects an object with a certain field structure.
  Express the structure as a new record type and use it as the type of
  the parameter.
* JavaScript code expects either a value of type `A`, or a value of
  type `B`. Use member overloads where appropriate.
* JavaScript code expects an object with optional fields.  This is a
  common idiom for configuration objects in graphical user interface
  frameworks. The recommended way to expose the assumptions about
  these objects to F# is to create a class with `DefaultValue` fields,
  which will be left undefined if not explicitly set:


```
type ButtonConfiguration [<Inline "{}">] () =
    [<DefaultValue>]
    val mutable label: string

    [<DefaultValue>]
    val mutable onclick: unit -> unit

    ...

let cfg = new ButtonConfiguration(label = "Click me!")
```

## Packaging and Configuration

The F# code written against stub classes can only work if the
implementing JavaScript is available in the runtime environment.  The
recommended way to do it is to annotate all stub modules and classes
with the `RequireAttribute` and define resource classes to include the
necessary files. If that is properly done, WebSharper automatically
includes the relevant JavaScript and CSS links on the page when any of
the stubbed functionality is used, freeing the library user from
manually keeping track of these assumptions.

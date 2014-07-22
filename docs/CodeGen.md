# Code Generation

Currently starts from F# quotations, goes to intermediate form, then
to JavaScript.  Optimizations are done on the intermediate form.

Optimizatoins are needed primarily to remove some of the higher-order
overhead from F# quotations (intermediate lambda forms, let
expressions).  Quotations are not optimized in any way by F#.

Currently generated code is organized in units of 1 JS file per one
DLL.  Would be better to support finer granularity and a JS module
system.  The production build would then eliminate more dead code.

Currently all top-level F# members (methods, properties, etc) are made
visible / accessible from JS.  A better strategy would be to make only
public members visible, which would allow to compile private members
differently and reduce code size.

Instance methods are compiled to mimick JS conventions `x.foo()`.
This can be improved for non-virtual methods for private types, which
are not exposed to JS. By converting them to static dispatch `foo(x)`,
the performance will likely improve.

A big problem is with lambdas that have a tuple domain `fun (x, y) ->
..`.  We fixed the representation to something callable from
JavaScript as `f(1, 2)` and `f([1, 2])`, both in fact.  This makes
WebSharper inject `Runtime.Tupled` adapters around such functions. It
is perhaps a design mistake.  Better distinguishing between FFI
functionality (talking to JavaScript) and internal use, would allow to
fix the convention, and reduce runtime overhead in many cases.

Currying poses a problem as well. For top-level `let` declarations, it
is OK, as F# takes care of it.  Within sub-functions, we try to
optimize currying but the optimizer does not systematically always
remove it.  This ends up with `function (f) { return function (x) ..`
code sometimes that a better optimizer would avoid (and improve
performance).

Tail-calls are optimized away for self-calling local functions and
groups of such functions. However, there are improvements to be had
there as well.  Perhaps the most basic improvement is to optimize
self-calling top-level function, which currently is not done.

Interfaces are compiled in "JS-natural" way, which breaks the .NET
semantics, when .Foo method is different depending on whether it
implements an interface, or is a private class method, and also
which interface it implements.  In our translation, it is always .Foo.
This is probably a design mistake, again need to
distinguish between interfaces used for FFI and for internal code.

Note that it would be better to start from CIL rather than quotations.
This would support C# and other CLR languages, and it would also
re-use many more optimizations already performed by FSC and other
compilers.

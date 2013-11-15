# Asynchronous Workflows

WebSharper supports [F# asynchronous workflows][asyncs] on the client,
implementing them with JavaScript callbacks.

As in F#, a workflow of type `async<'T>` represents a program that can
be invoked to either succeed with a result `'T` asynchronously or fail
with an exception.  The limiations on the client are:

* Cancellation is not supported.

* All parallelism is cooperative.  You have to yield control inside a
  workflow to let other workflows execute.  In current implementation,
  yielding of control may happen implicitly every time you use `let!`
  operator.

* There is no way to `Async.RunSynchronously`.

[asyncs]: http://msdn.microsoft.com/en-us/library/dd233250.aspx

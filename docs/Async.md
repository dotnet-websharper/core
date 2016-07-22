# Asynchronous Workflows in F#

WebSharper supports [F# asynchronous workflows][asyncs] on the client,
implementing them with JavaScript callbacks.

As in F#, a workflow of type `async<'T>` represents a program that can
be invoked to either succeed with a result `'T` asynchronously or fail
with an exception.  The limitations on the client are:

* All parallelism is cooperative as the JavaScript runtime is single-threaded. 
  You have to yield control inside a
  workflow to let other workflows execute.
  Yielding of control happens implicitly every time whenever F# inserts
  a call to an `AsyncBuilder` call.
  This includes where you use a `let` or `let!`, every iteration of a `for` or `while` loop, 
  or between consecutive statements on the top level of an async block.

* There is no way to use `Async.RunSynchronously`.

* Cancellation is supported, in standard .NET/F# ways. If a cancellation occurs
  when waiting on an aynchronous remote call, the response from the server gets
  discarded without converting the JSON back to an object graph.

[asyncs]: http://msdn.microsoft.com/en-us/library/dd233250.aspx

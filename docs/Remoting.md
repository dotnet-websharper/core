# Communicating With the Server

WebSharper supports remote procedure calls from the client
(JavaScript environment) to the server (ASP.NET or other hosting
environment). Remoting is designed to be safe and efficient while
requiring as little boilerplate code as possible.

Here is a simple example of a client-side and a server-side function
pair that communicate with RPC:

```fsharp
module Server =
    
    [<Rpc>]
    let GetBlogsByAuthor author =
        use db = new DbContext()
        let blogs = db.GetBlogsByAuthor author
        async { return blogs }
    
[<JavaScript>]
module Client =

    let GetBlogsByAuthor (author: Author) (callback: Blog [] -> unit) =
        async {
            let! blogs = Server.GetBlogsByAuthor author
            return callback blogs
        }
        |> Async.Start
```

The conceptual model assumed by WebSharper is this: the client always
has the control, and calls the server when necessary.

The remoting component also assumes that:

* RPC-callable methods are marked with `RemoteAttribute`.

* RPC-callable methods are safe to call from the web by an
  unauthenticated client.
  
* RPC-callable methods have argument types that are serializable to
  JSON.

* RPC-callable methods have a return type that is serializable to
  JSON, or are of type `Async<'T>` where `'T` is such a type.

JSON serializers are automatically derived for the following types,
where `'T1, 'T2, ...` are arbitrary JSON-serializable types:

* `unit`
* `bool`
* `int`
* `int64`
* `double`
* `string`
* `System.DateTime`
* `'T []`
* `'T1 * 'T2 * ... * 'Tn`
* unions (including `option` and `list`)
* records
* classes with a default constructor

For records, unions and classes to be JSON-serializable, all their
fields must also be JSON-serializable.

The remoting mechanism supports three different ways of doing a remote
call: message-passing, synchronous and asynchronous.

### Message-Passing Calls

Message-passing calls are similar to RPC calls but they do not lock
the browser, returning immediately on the client. If an RPC function
has the return type of `unit`, calls to this function are
message-passing calls.

```fsharp
[<Remote>]
let Log (msg: string) =
    System.Diagnostics.Debug.Write("MSG: {0}", msg)
```

With these definitions, a call to `Log "foo"` proceeds as follows:

* The client serializes `"foo"` to JSON.

* The client sends a request to the server.

* The client returns `unit` immediately.

* The server parses the request.

* The server binds to and calls the requested method with the
  arguments deserialized from JSON.

### Asynchronous Calls

These calls allow for asynchronous, callback-based processing of the
server response.  They utilize the `Async<'T>` abstraction from F# to
express multi-step asynchronous workflows.  The implementation uses
nested JavaScript callbacks.

For example:

```fsharp
[<Remote>]
let Increment(x: int) =
    async {
        return! x + 1
    }

[<JavaScript>]
let Foo (callback: int -> unit) =
    async {
        let! x = Increment 0
        let! y = Increment x
        let! z = Increment y
        return callback z
    }
    |> Async.Start
```

With these definitions, a call to `Foo f` proceeds as follows:

* The client sends `0` to the server and registers a callback,
  proceeding immediately.

* The server replies with `1` and the browser invokes the callback
  from step 1, binding `x` to `1`.

* The client sends `1` to the server and registers another
  callback. These asynchronous steps repeat according to the workflow,
  until the line `return callback z` is reached, with `z` being bound
  to `3`.

* `f 3` is called.

The mechanics of individual calls are similar to the message-passing
calls.

Note that using `Async` on the server side means that your code can
switch threads.  Extra care should be taken to acquire references to
thread-local objects such as `System.Web.HttpContext.Current` before
entering the async expression.

### Synchronous Calls

Synchronous RPC calls block the browser until the server's reply is
available. Their use is not recommended. For the user they look just
like ordinary client-side function calls.

Example:

```fsharp
[<Remote>]
let Increase(x: int) = x + 1
```

With these definitions, a client call to `Increase 0` proceeds as
follows:

* The client serializes `0` to JSON.

* The client sends a RESTful request to the server. The request
  contains information on which method to call, and its
  JSON-serialized arguments (`0`)}

* The client blocks the browser.

* The server (in ASP.NET context, the WebSharper handler) parses the
  request and looks up the requested method.

* The server makes sure the method is marked with `RpcAttribute`.

* The server binds to the method, deserializes the arguments from
  JSON, and calls it.

* The server serializes the method's response to JSON and responds to
  the request.

* The client deserializes the response `1` from JSON and returns it.

* The client unblocks the browser.

## Handler Objects

WebSharper 2.0 introduces the ability to use instance methods for
client-server communication. The syntax for this is:

```fsharp
Remote<MyClass>.MyMethod(...)
```

The method invoked should be annotated with the `RpcAttribute` and
follows the same convention as static methods:

```fsharp
type MyType(..) =
    [<Remote>]
    member this.MyMethod(..) = ..
```

When the server receives such a request, it obtains an instance of
`MyClass` via an `IRpcHandlerFactory`, and invokes the instance method
on the obtained object:

```fsharp
type IRpcHandlerFactory =
    abstract member Create : Type -> option<obj>
```

The default `IRpcHandlerFactory` always returns `None`, refusing to
create instances. This can be customized by using the following method
during the web application startup (such as in `Global.asax`):

```fsharp
SetRpcHandlerFactory : IRpcHandlerFactory -> unit
```

The support for handler objects makes it more natural to use
WebSharper remote procedure calls in object-oriented server-side
frameworks, such as ASP.NET MVC. This approach also lends itself to
the use of Inversion of Control containers to implement
`IRpcHandlerFactory`.

## Communication Protocol

The communication protocol used by WebSharper is a custom protocol
built on top of HTTP and JSON. The client sends HTTP POST requests
marked with a special HTTP headers to the current URL of the page
(`?`), with the bodies of the requests containing the JSON-serialized
method arguments. The server responds with a JSON reply.

The URL to which the requets are sent can be customized by subclassing
from the `RpcAttribute`.

--------

See also:

* [Manual TOC](WebSharper.md)
* [Hosting in IIS](IIS.md)



# Developing Proxies to .NET Libraries

__Proxying__ in WebSharper is the process of providing
JavaScript-compilable F# implementations for classes and modules that
were compiled without WebSharper, for example the Base Class Library
classes such as `Dictionary`. The proxying graph relates proxied types
to proxy types. The proxy graph is constructed by consulting all
`Proxying.AbstractProxyAttribute` in an assembly and its references.

The simple implementation, `ProxyAttribute`, is applied to the proxied
type and should reference the proxy type.  Sample usage:

    open System.Collections.Generic

    [<Proxy(typeof<Dictionary<_, _>)>]
    [<JavaScript>]
    type MyDictionary<'K, 'V> () =

        let mutable count = 0

        member this.Count = count

WebSharper projects that reference the DLL containing the above code
can use the standard type `Dictionary<_, _>` in client-side code, and
the above implementation will be called instead.

Sometimes it is not possible to use `typeof`, for example when
proxying a module. In this case, you can use the type or module's full
name instead:

    [<Proxy "Microsoft.FSharp.Collections.ArrayModule, \
          FSharp.Core, \
          Version=2.0.0.0, \
          Culture=neutral, \
          PublicKeyToken=b03f5f7f11d50a3a">]
    module MyArrayModule =

        [<Inline "$0.length">]
        let length<'T> (arr : 'T []) = 0

There are several things to remember when developing .NET proxy code:

* The string parameter to the `ProxyAttribute` must exactly match the
  `FullName` property of the generic definition of the target .NET
  type.

* The name, type, number of arguments and the calling convention of a
  proxy member must match exactly those of the member being proxied.

* It is legal for a proxy to be partial. If the proxied type doesn't
  implement all members, then client-side calls to proxied members
  will succeed and client-side calls to non-proxied members will fail.
  For example, WebSharper contains a proxy for the module `Async`, but
  does not implement certain methods such as `RunSynchronously`.
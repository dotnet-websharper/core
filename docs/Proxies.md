# Developing Proxies to .NET Libraries

__Proxying__ in WebSharper is the process of providing
JavaScript-compilable F# implementations for classes and modules that
were compiled without WebSharper, for example the Base Class Library
classes such as `Dictionary`. The proxying graph relates proxied types
to proxy types. The proxy graph is constructed by consulting all
`Proxying.AbstractProxyAttribute` in an assembly and its references.

The simple implementation, `ProxyAttribute`, is applied to the proxied
type and should reference the proxy type.  Sample usage:

    [<Proxy "Microsoft.FSharp.Collections.ArrayModule, \
          FSharp.Core, \
          Version=2.0.0.0, \
          Culture=neutral, \
          PublicKeyToken=b03f5f7f11d50a3a">]
    module MyArrayModule =

        [<Inline "$0.length">]
        let length<'T> (arr : 'T []) = 0

WebSharper projects that reference the DLL containing the above code
can use `Array.length` in client-side code.

There are several things to remember when developing .NET proxy code:

* The string parameter to the `ProxyAttribute` must exactly match the
  `FullName` property of the generic definition of the target .NET
  type.

* The name, type, number of arguments and the calling convention of a
  proxy member must match exactly those of the member being proxied.

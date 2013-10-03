# Hosting WebSharper in IIS

**NOTE**: If you are using ASP.NET MVC-style application, there is
simpler way to host WebSharper sitelets - see the
[WebSharper.WebApi](http://github.com/intellifactory/websharper.webapi)
project.

WebSharper applications need to install two components in a web
request processing pipeline:

* [Remote procedure call](Remoting.md) handler
* [Sitelets](Sitelets.md) handler

The default templates ("Sitelet Host Website") accomplish this by
registering IIS modules in `Web.config:

```xml
<configuration>
  <system.webServer>
    <modules>
      <add name="WebSharper.RemotingModule"
           type="IntelliFactory.WebSharper.Web.RpcModule,
                 IntelliFactory.WebSharper.Web" />
      <add name="WebSharper.Sitelets"
           type="IntelliFactory.WebSharper.Sitelets.HttpModule,
                 IntelliFactory.WebSharper.Sitelets" />
```

These modules are installed into the request processing pipele and
handle matching requests automatically. Sitelets are picked up by
locating assembly attributes in assemblies under `~/bin`:

```fsharp
[<assembly: Website(...)>]
do ()
```

See also:

* [Manual Home](WebSharper.md)
* [Sitelets](Sitelets.md)
* [Remoting](Remoting.md)

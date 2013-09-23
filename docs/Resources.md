# Managing Resource Dependencies

WebSharper automates the management of resource dependencies.  For the
purposes of WebSharper, a __resource__ is any HTML code that can be
rendered to the `<head>` section of a page, for example a CSS or a
JavaScript reference.  Pages written with WebSharper infer their
minimal necessary resource set and the correct resource ordering.

## Declaring Resources

The code necessary to declare resources can be found in the
`Resources` module under the `IntelliFactory.WebSharper.Core`
namespace:

    module R = IntelliFactory.WebSharper.Core.Resources

To declare a resource, you create a new type with a default
constructor and implement the `IResource` interface:

    type MyResource() =
        interface R.IResource with
            member this.Render ctx writer = ..

You can emit arbitrary HTML in the `Render` method.  It will appear in
the `<head/>` section of pages that depend on this resource.

A convenient way to do declare resources is to derive from the
`BaseResource` class:

    type MyResource() =
        inherit Resources.BaseResource("http://my.cdn.net",
           "file1.js", "file2.js", "file3.css")

The above code declares a resource that renders to this HTML:

    <script type="text/javascript"
            src="http://my.cdn.net/file1.js"></script>
    <script type="text/javascript"
            src="http://my.cdn.net/file2.js"></script>
    <link rel="stylesheet" type="text/css"
          href="http://my.cdn.net/file1.css" />

The ID of the resource is set to `MyNamespace.MyResource`, the
`FullName` of the class.  The base URL (`http://my.cdn.net`) can be
overriden by providing an application setting with the key
`MyNamespace.MyResource`.

`BaseResource` can also declare resources embedded into the current
assembly:

    [<assembly: System.Web.UI.WebResource("My.js", "text/javascript")>]
    do ()

    type MyEmbeddedResource() =
        inherit Resources.BaseResource("My.js")

## Declaring Resource Dependencies

A resource dependency can be declared on a type or a member by
annotating it with `RequireAttribute`. It is parameterized by the type
of the resource to require:

    [<Require(typeof<MyResource>)>]
    type MyWidget() = ...

    [<Require(typeof<MyResource>)>]
    let F (..) = ..

Types, modules and static methods can be annotated with dependencies.
All code that calls the annotated methods is assumed to depend on the
resource.

## Dependency Graph

When constructing a page, `WebSharper` infers the set of resources to
include in the `<head/>` section.  It starts by looking at the set of
controls present on the page.  Every control has a type, and this type
corresponds to a node in the dependency graph.  WebSharper computes
the set of all nodes in the graph reachable from the control nodes.
It then renders all resources found in this set.

The dependency graph is a directed graph with .NET classes and static
methods as nodes.  An edge from A to B singifies that A depends on B.
The graph is partially inferred and partially specified by the user:

* Graph edges are inferred from the call graph.  For an example, if a
  function `f` calls a function `g`, then there is an edge from `f` to
  `g`.  There are also structural rules, such as classes depending on
  their base classes, or methods and modules depending on the modules
  that declared them.

* Graph edges are also declared by using the `RequireAttribute`.

## Resource Implementation

The resource graphs are constructed for every WebSharper-processed
assembly and are serialized to binary. They are stored within the
assembly itself.  At runtime all the graphs of all the referenced
assemblies are deserialized and merged into a single graph.

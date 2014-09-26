# Managing Resource Dependencies

WebSharper automates the management of resource dependencies.  For the
purposes of WebSharper, a __resource__ is any HTML code that can be
rendered to the `<head>` section of a page, for example a CSS or a
JavaScript reference.  Pages written with WebSharper infer their
minimal necessary resource set and the correct resource ordering.

Most resources are declared as subclasses of the `Core.Resources.BaseResource`
class (see "Declaring Resources and Dependencies" below for the different
ways to declare a resource). Such a resource is declared with an associated
path. The path is resolved by WebSharper as follows:

1. If the assembly contains an embedded resource file whose name
corresponds to the resource's path, then WebSharper:

    * extracts this file into the relevant subfolder of your application
      (`/Scripts/WebSharper/<path>` for JavaScript files,
      `/Content/WebSharper/<path>` for other resources)

    * adds the correct tag to `<head>` (`<script>` for JavaScript files,
      `<link>` for CSS files) with the corresponding path.
      
2. If there is no such embedded resource file, then WebSharper simply
adds the correct tag to `<head>` without altering the path. This means
that for such resources, you *should* use absolute URLs, either inside
the application (for example `/some/path.js`) or to an external website
(for example `//my.cdn.net/file1.js`).

A resource of type `BaseResource` can also be declared with a base path
and a set of subpaths. This is useful for a library consisting of several
files that need to be loaded eg. from a CDN. In this case, step 1 above
is skipped, and a tag is added for each subpath by combining it with the
base path.

## Dependency resolution

A resource is only included by WebSharper if it is required by a
client-side element. This means that each page of your website only
contains the minimum set of resources that its contents need.

An assembly, a type, a module, a static member or a module `let`
declaration can be marked as requiring a resource (see "Declaring
Resources and Dependencies" below for the different ways to declare
a dependency). Any page that calls this item's client-side code
will have the given resource included.

A resource B can also be required by another resource A. In this case,
any code that requires A will also include B. WebSharper ensures that
B is located before A in the `<head>`.

## Declaring Resources and Dependencies

### In WebSharper Interface Generator

To declare a resource in WIG, you can use one of the following
functions:

* `Resource` declares a `BaseResource` with a single path.

```fsharp
let R1 = Resource "ResourceClassName" "path.js"
```

* `Resources` declares a `BaseResource` with a base path and multiple
subpaths.

```fsharp
let R2 =
    Resources "ResourceClassName2"
        "//my.cdn.net" ["file1.js"; "file2.js"; "file1.css"]
```

In either case, your resource must be included in the `Assembly`
declaration. A common idiom is to create a sub-namespace called
`Resources` and to include all resources in it:

```fsharp
let Assembly =
    Assembly [
        Namespace "My.Library" [
            // Library classes...
        ]
        Namespace "My.Library.Resources" [
            R1
            R2
        ]
    ]
```

To declare that a class or an assembly depends on a given resource,
you can use one of the following functions:

* `Requires` declares a dependency on resources declared in this
assembly.

```fsharp
let C =
    Class "My.Library.C"
    |+> (* members... *)
    |> Requires [R1; R2]
    
let Assembly =
    Assembly [
        // ...
    ]
    |> Requires [R3]
```

* `RequiresExternal` declares a dependency on resources declared
outside of this assembly.

```fsharp
let C2 =
    Class "My.Library.C2"
    |+> (* members... *)
    |> RequiresExternal [typeof<Other.Library.Resources.R4>]
```

### In WebSharper Libraries, Applications and Manual Bindings

To declare a resource in a WebSharper library or application,
you can simply declare a class inheriting from `BaseResource`.
Use the constructor with a single argument for single paths,
and multiple arguments for a base path and a set of subpaths.

```fsharp
module Resources =

    open IntelliFactory.WebSharper.Core.Resources

    type R1() =
        inherit BaseResource("path.js")
        
    type R2() =
        inherit BaseResource("//my.cdn.net",
            "file1.js", "file2.js", "file3.css")
```

You can also implement more complex resources (for example,
resources that require a bit of inline JavaScript) by directly
implementing the `IResource` interface. You can emit arbitrary
HTML in the `Render` method using the provided `HtmlTextWriter`.

```fsharp
type R3() =
    interface R.IResource with
        member this.Render ctx writer = ...
```

A resource dependency can be declared on a type, a member or an
assembly by annotating it with `RequireAttribute`. It is parameterized
by the type of the resource to require:

```fsharp
[<Require(typeof<R1>)>]
type MyWidget() = ...

[<Require(typeof<R2>)>]
let F x = ...

[<assembly:Require(typeof<R3>)>]
do()
```

## Resource Implementation

The resource dependency graphs are constructed for every WebSharper-processed
assembly and are serialized to binary. They are stored within the
assembly itself.  At runtime all the graphs of all the referenced assemblies
are deserialized and merged into a single graph.
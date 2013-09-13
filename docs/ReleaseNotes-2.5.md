# Release Notes - 2.5

Note: this is a draft document for the upcoming 2.5 release.

## New features

* Support for .NET 4.5 and F# 3.0.

* Support for free Visual Studio tools (VS Express For Web + F# tools)

* [NuGet](http://nuget.org)-based distribution model.

* Interface Generator has been reworked internally, and no longer
  relies on the C# compiler to generate code.  This requires some
  small changes to existing extension code using this tool.

* A simple form of [bundling](Bundling.md) is now available to
  generate standalone JavaScript packages with WebSharper.

* There is now an experimental [TypeScript
  generation](TypeScriptOutput.md) feature for producing
  [TypeScript](http://typescriptlang.org) definition files
  corresponding to WebSharper JavaScript output.  This is useful for
  developing libraries in WebSharper for users to consume in
  TypeScript.

## Bug fixes

Issues fixed:

* Issue #28
* Issue #87
* Issue #90
* Issue #101
* Issue #107
* Issue #109
* Issue #110
* Issue #111
* Issue #112
* Issue #114
* Issue #117
* Issue #117
* Issue #123
* Issue #125
* Issue #126

## Breaking changes

### Extensions

WebSharper extension code created using the Interface Generator needs
to be updated.  Once you compose a first-class value describing the
generated assembly bindings, you need to tag it to the assembly
metadata using the following snippet:

    open IntelliFactory.WebSharper.InterfaceGenerator

    [<Sealed>]
    type MyExtension() =
        interface IExtension with
            member x.Assembly = ...

    [<assembly: Extension(typeof<MyExtension>)>]
    do ()

This code should replace the "Main" method that used to call IG API
directly.  Also, since C# files are no longer processed, extensions
that relied on specifying resources and dependencies in C# files
should be updated to use the new API available in the
`InterfaceGenerator` namespace.

### Recompilation requirement

As the internal metadata format has been amended, all WebSharper code
needs to be recompiled using the latest compiler.


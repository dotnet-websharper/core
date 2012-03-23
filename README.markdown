# WebSharper

WebSharper is an F#-based web programming platform. It lets you
develop web applications from a single F# codebase, to be run in the
.NET environment on the server, the JavaScript environment on the
browser, or both.  WebSharper enjoys type safety of F# and code
completion of Microsoft Visual Studio, making it a very productive
envionment for developing JavaScript. WebSharper server-side
components make communicating with the server is as easy as calling a
function.

WebSharper includes:

* A compiler from assemblies to JavaScript.

* Extensible support for a large part of the F# and .NET standard
  library (including sequences, events and asynchronous workflows) on
  the client.

* Support for type-safe programming with the standard JavaScript
  library and DOM.

* F# bindings to a number of third-party libraries including jQuery.

* Formlets - an innovative library for type-safe web formcombinators.

* Support for seamless client/server communication.

* Tools for embedding raw JavaScript code and binding to external
  JavaScript codebases from F#.

* Integration with ASP.NET andMicrosoft Visual Studio.

* Resource dependency management for CSS, image and other supporting
  files.

## Copying

Since January 2012, WebSharper source code is available under GNU
Affero GPL, see LICENSE.txt in the source.  Alternative licensing
schemes are available for users who find AGPL terms too restrictive,
see the [website][ws] for details.

## Installation

The easiest way to obtain WebSharper is by using the binary installer
(see [websharper.com][ws] or the Downloads section of this project).
The installer adds templates to Visual Studio and makes it easy to get
started.

## Building

Build requirements:

* Microsoft .NET Framework 4.0

* F#

To compile, run `MSBuild.exe` in the root directory of the checkout.
This will download and reference required Mono.Cecil libraries and
Microsoft Ajax Minifier tasks automatically.

For example, using PowerShell:

    set-alias msb C:\WINDOWS\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe
    msb /P:Platform=AnyCPU /Configuration:Release

Solutions provided for Microsoft Visual Studio 2010 and 2011 under the
`build` directory in the source, but these products are not required
to build WebSharper.

## Documentation

Please refer to the WebSharper [website][ws] for tutorials and
samples.  The website also distributes a
[manual](http://websharper.com/WebSharper.pdf) in PDF format.  There
are plans to make the manual, tutorial and sample sources available as
part of this repository in the future.

## Extensions

The WebSharper [website][ws] provides several extensions that make
third-party JavaScript frameworks available for easy use in WebSharper
projects (see the Download section).  The source code of the
extensions will be made available as well, as time permits.

## Bugs

The preferred way to report bug and request enhancements is through
the built-in [issue
service](http://bitbucket.org/IntelliFactory/websharper/issues).

## Contact

WebSharper is being developed by IntelliFactory.  Please feel free to
[contact us](http://websharper.com/contact).

For public discussions we also recommend using
[FPish](http://fpish.net/topics), the functional programming community
site built with WebSharper.

[ws]: http://websharper.com

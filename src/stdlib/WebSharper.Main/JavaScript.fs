// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

namespace WebSharper.JavaScript

open WebSharper
module Re = WebSharper.Core.Resources

/// Defines common JavaScript operations.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JS =

    /// The JavaScript "undefined" value.
    [<Inline "undefined">]
    let Undefined<'T> = X<'T>

    [<Inline "this">]
    let This<'T> = X<'T>

    /// Enumerates JavaScript value kinds.
    type Kind =
        | [<Constant "boolean">]   Boolean
        | [<Constant "function">]  Function
        | [<Constant "number">]    Number
        | [<Constant "string">]    String
        | [<Constant "object">]    Object
        | [<Constant "undefined">] Undefined

    /// Represents a timer handle.
    [<Sealed>]
    type Handle = class end

/// Defines common JavaScript operations.
type JS =

    /// The global JavaScript object.
    [<Inline "$global">]
    static member Global = X<obj>

    /// The global Window object.
    [<Inline "$global">]
    static member Window = X<Window>

    /// The JavaScript "undefined" value.
    [<Inline "undefined">]
    static member Undefined = X<obj>

    /// The JavaScript "this" value.
    [<Inline "this">]
    static member this = X<obj>

    /// Returns the reference to the global document.
    [<Inline>]
    static member Document = JS.Window.Document

    /// Evaluates JavaScript code.
    [<Inline>]
    static member Eval x = JS.Window.Eval x

    /// Ignores the value of an expression.
    [<Inline "void $x">]
    static member Void (x: obj) = X<unit>

    /// The "Not-a-Number" float value.
    [<Inline>]
    static member NaN = JS.Window.NaN

    /// The Infinity float value.
    [<Inline>]
    static member Infinity = JS.Window.Infinity

    /// Parse a string as an integer.
    [<Inline>]
    static member ParseInt(s: string) = JS.Window.ParseInt(s)

    /// Parse a string as an integer with the given radix.
    [<Inline>]
    static member ParseInt(s: string, radix: int) = JS.Window.ParseInt(s, radix)

    /// Parse a string as a float.
    [<Inline>]
    static member ParseFloat(s: string) = JS.Window.ParseFloat(s)

    /// Checks whether a float is "Not-a-Number".
    [<Inline>]
    static member IsNaN(n: obj) = JS.Window.IsNaN(n)

    /// Checks whether a number is finite.
    [<Inline>]
    static member IsFinite(n: float) = JS.Window.IsFinite(n)

    /// Checks whether a number is finite.
    [<Inline>]
    static member IsFinite(n: int) = JS.Window.IsFinite(n)

    /// Percent-encode a URI while leaving special URI characters such as '/' and '?' unencoded.
    [<Inline>]
    static member EncodeURI(uri: string) = JS.Window.EncodeURI(uri)

    /// Percent-decode a URI while leaving special URI characters such as '/' and '?' encoded.
    [<Inline>]
    static member DecodeURI(uri: string) = JS.Window.DecodeURI(uri)

    /// Percent-encode a URI, including special URI characters such as '/' and '?'.
    [<Inline>]
    static member EncodeURIComponent(uri: string) = JS.Window.EncodeURIComponent(uri)

    /// Percent-decode a URI, including special URI characters such as '/' and '?'.
    [<Inline>]
    static member DecodeURIComponent(uri: string) = JS.Window.DecodeURIComponent(uri)

    /// Parse and inline JavaScript code.
    /// Replaces variables $0, $1, etc with the provided arguments.
    [<Macro(typeof<WebSharper.Core.Macros.InlineJS>)>]
    static member Inline<'T>(inlineString: string, [<System.ParamArray>] args: obj[]) = X<'T>

    /// Exposes the JavaScript `+x` operator.
    [<Inline "+ $x">]
    static member Plus<'T> (x: obj) = X<'T>

    /// Exposes the JavaScript `-x` operator.
    [<Inline "- $x">]
    static member Minus<'T> (x: obj) = X<'T>

    /// Exposes the JavaScript `~x` operator.
    [<Inline "~ $x">]
    static member BitwiseNot (x: obj) = X<'T>

    /// Exposes the JavaScript `!x` operator.
    [<Inline "! $x">]
    static member Not (x: obj) = X<bool>

    /// Displays a popup dialog.
    [<Inline "alert($message)">]
    static member Alert (message: string) = X<unit>

    /// Displays a popup dialog with Yes and No buttons, returns the choice.
    [<Inline "confirm($message)">]
    static member Confirm (message: string) = X<bool>

    /// Displays a popup dialog with a text input box, returns the value entered by the user.
    [<Inline "prompt($message, $value)">]
    static member Prompt (message: string) (value: string) = X<string>

    /// Schedules the function for execution in the
    /// given number of microseconds.
    [<Inline "setTimeout($f,$msec)">]
    static member SetTimeout (f: unit -> unit) (msec: int) = X<JS.Handle>

    /// Schedules the function for execution once every
    /// given number of microseconds.
    [<Inline "setInterval($f,$msec)">]
    static member SetInterval (f: unit -> unit) (msec: int) = X<JS.Handle>

    /// Clears a scheduled timeout function.
    [<Inline "clearTimeout($handle)">]
    static member ClearTimeout (handle: JS.Handle) = X<unit>

    /// Clears a scheduled interval function.
    [<Inline "clearInterval($handle)">]
    static member ClearInterval (handle: JS.Handle) = X<unit>

    /// Performs JavaScript function application.
    [<Inline "$wsruntime.Apply($x[$func], $x, $args)">]
    static member Apply<'T> (x: obj) (func: string) (args: obj []) = X<'T>

    /// Deletes a field from a JavaScript object.
    [<Inline "delete $x[$field]">]
    static member Delete (x: obj) (field: string) = X<unit>

    /// Tests if the object contains a property.
    [<Inline "$x.hasOwnProperty($prop)">]
    static member HasOwnProperty (x: obj) (prop: string) = X<bool>

    /// Tests if the object has or inherits a property.
    /// Implemented with the "in" operator.
    [<Inline "$field in $x">]
    static member In (field: string) (x: obj) = X<bool>

    /// Retrieves all proper fields from an object.
    [<Direct "var r=[]; for(var k in $o) r.push([k,$o[k]]); return r">]
    static member GetFields (o: obj) = X<(string * obj)[]>

    /// Retrieves the names of all proper fields from an object.
    [<Direct "var r=[]; for(var k in $o) r.push(k); return r">]
    static member GetFieldNames (o: obj) = X<string[]>

    /// Retrieves the values of all proper fields from an object.
    [<Direct "var r=[]; for(var k in $o) r.push($o[k]); return r">]
    static member GetFieldValues (o: obj) = X<obj[]>

    /// Constructs a new object with a given constructor function.
    [<Inline "new $x()">]
    static member New (x: obj) : 'T = X<'T>

    /// Applies the JavaScript `typeof` operator.
    [<Inline "typeof $x">]
    static member TypeOf (x: obj) = X<JS.Kind>

    /// Iterates over the fields of a JavaScript object.
    /// Iteration can be terminated by returning `true`.
    [<Inline "for (var k in $x) { if ($iter(k)) break; }">]
    static member ForEach (x: obj) (iter: string -> bool) = X<unit>

    /// Tests if an object is an instance of a given class.
    [<Inline "$x instanceof $cl">]
    static member InstanceOf (x: obj) (cl: obj) = X<bool>

    /// Logs the given object to console if one is defined.
    [<Inline "if (console) console.log($x)">]
    [<System.Obsolete "Use Console.Log instead.">]
    static member Log (x: obj) = X<unit>

    /// Logs an array or tuple to console if one is defined.
    [<Inline "if (console) console.log.apply(console, $args)">]
    [<System.Obsolete "Use Console.Log instead.">]
    static member LogMore args = X<unit>

    /// Gets a given field from an object.
    [<Inline "$target[$field]">]
    static member Get<'T> (field: string) (target: obj) = X<'T>

    /// Sets a given field on an object.
    [<Inline "void ($target[$field] = $v)">]
    static member Set (target: obj) (field: string) (v: obj) = X<unit>

    /// Requests a function to be called on the next animation frame.
    [<Inline "requestAnimationFrame($f)">]
    static member RequestAnimationFrame (f: float -> unit) = X<JS.Handle>

    /// Cancels an animation frame request.
    [<Inline "cancelAnimationFrame($handle)">]
    static member CancelAnimationFrame (handle: JS.Handle) = X<unit>

    /// Requests the given URL.
    [<Inline>]
    static member Fetch(url: string) = JS.Window.Fetch(url)

    /// Requests the given URL with options.
    [<Inline>]
    static member Fetch(url: string, options: RequestOptions) = JS.Window.Fetch(url, options)

    /// Performs the given request.
    [<Inline>]
    static member Fetch(request: Request) = JS.Window.Fetch(request)

    /// Imports a single given export from an ES6 module.
    [<Macro(typeof<WebSharper.Core.Macros.ImportJS>)>]
    static member Import<'T> (export: string, from: string) = X<'T>

    /// Imports an entire ES6 module.
    [<Macro(typeof<WebSharper.Core.Macros.ImportJS>)>]
    static member ImportAll<'T> (from: string) = X<'T>

    /// Imports default export from an ES6 module.
    [<Macro(typeof<WebSharper.Core.Macros.ImportJS>)>]
    static member ImportDefault<'T> (from: string) = X<'T>

    [<Macro(typeof<WebSharper.Core.Macros.ImportJS>)>]
    static member ImportFile (from: string) = X<unit>

    [<Inline "import($moduleName)">]
    static member ImportDynamic<'T> (moduleName: string) = X<Promise<'T>>

    /// Unchecked JS expression that makes it into output .js unchanged.
    /// String interpolation is supported to include expressions.
    [<Macro(typeof<WebSharper.Core.Macros.JSVerbatim>)>]
    static member Verbatim<'T> (js: string) = X<'T>

    /// Unchecked JSx expression that makes it into output .jsx unchanged.
    /// String interpolation is supported to include expressions.
    [<Macro(typeof<WebSharper.Core.Macros.JSHtml>)>]
    static member Html<'T> (jsx: string) = X<'T>

    /// Unchecked JSx expression that makes it into output .jsx unchanged.
    /// String interpolation is supported to include expressions.
    [<Macro(typeof<WebSharper.Core.Macros.JSHtml>)>]
    static member Jsx<'T> (jsx: string) = X<'T>

    /// Unchecked JSx expression that makes it into output .jsx unchanged.
    /// String interpolation is supported to include expressions.
    [<Macro(typeof<WebSharper.Core.Macros.JSHtml>)>]
    static member jsx<'T> (jsx: string) = X<'T>

    /// Unchecked JSx expression that makes it into output .jsx unchanged.
    /// String interpolation is supported to include expressions.
    [<Macro(typeof<WebSharper.Core.Macros.JSHtml>)>]
    static member html<'T> (jsx: string) = X<'T>

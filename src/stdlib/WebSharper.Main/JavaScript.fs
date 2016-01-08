// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

/// Defines common JavaScript operations.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WebSharper.JavaScript.JS

module A = WebSharper.Core.Attributes
module Re = WebSharper.Core.Resources

type AnimationFrameResource() =
    interface Re.IResource with
        member this.Render ctx html =
            let html = html Re.Scripts
            html.WriteLine "<!--[if lte IE 9.0]>"
            let name = if ctx.DebuggingEnabled then "AnimFrame.js" else "AnimFrame.min.js"
            let ren = Re.Rendering.GetWebResourceRendering(ctx, typeof<AnimationFrameResource>, name)
            ren.Emit(html, Re.Js)
            html.WriteLine "<![endif]-->"

/// Constructs a JavaScript "undefined" value.
[<A.Inline "undefined">]
let Undefined<'T> = raise ClientSideOnly : 'T

/// Returns the reference to the global JavaScript object.
[<A.Inline "$global">]
let Global = obj ()

/// Ignores the value of an expression.
[<A.Inline "void $x">]
let Void (x: obj) = raise ClientSideOnly : unit

/// Exposes the JavaScript `+x` operator.
[<A.Inline "+ $x">]
let Plus<'T> (x: obj) = raise ClientSideOnly : 'T

/// Exposes the JavaScript `-x` operator.
[<A.Inline "- $x">]
let Minus<'T> (x: obj) = raise ClientSideOnly : 'T

/// Exposes the JavaScript `~x` operator.
[<A.Inline "~ $x">]
let BitwiseNot (x: obj) = raise ClientSideOnly : 'T

/// Exposes the JavaScript `!x` operator.
[<A.Inline "! $x">]
let Not (x: obj) = raise ClientSideOnly : bool

/// Enumerates JavaScript value kinds.
type Kind =
    | [<A.Constant "boolean">]   Boolean
    | [<A.Constant "function">]  Function
    | [<A.Constant "number">]    Number
    | [<A.Constant "string">]    String
    | [<A.Constant "object">]    Object
    | [<A.Constant "undefined">] Undefined

/// Displays a popup dialog.
[<A.Inline "alert($message)">]
let Alert (message: string) = raise ClientSideOnly : unit

/// Displays a popup dialog with Yes and No buttons, returns the choice.
[<A.Inline "confirm($message)">]
let Confirm (message: string) = raise ClientSideOnly : bool

/// Displays a popup dialog with a text input box, returns the value entered by the user.
[<A.Inline "prompt($message, $value)">]
let Prompt (message: string) (value: string) = raise ClientSideOnly : string

/// Represents a timer handle.
[<Sealed>]
type Handle = class end

/// Schedules the function for execution in the
/// given number of microseconds.
[<A.Inline "setTimeout($f,$msec)">]
let SetTimeout (f: unit -> unit) (msec: int) = raise ClientSideOnly : Handle

/// Schedules the function for execution once every
/// given number of microseconds.
[<A.Inline "setInterval($f,$msec)">]
let SetInterval (f: unit -> unit) (msec: int) = raise ClientSideOnly : Handle

/// Clears a scheduled timeout function.
[<A.Inline "clearTimeout($handle)">]
let ClearTimeout (handle: Handle) = raise ClientSideOnly : unit

/// Clears a scheduled interval function.
[<A.Inline "clearInterval($handle)">]
let ClearInterval (handle: Handle) = raise ClientSideOnly : unit

/// Performs JavaScript function application.
[<A.Inline "$x[$func].apply($x,$args)">]
let Apply<'T> (x: obj) (func: string) (args: obj []) = raise ClientSideOnly : 'T

/// Deletes a field from a JavaScript object.
[<A.Direct "delete $x[$field]">]
let Delete (x: obj) (field: string) = raise ClientSideOnly : unit

/// Tests if the object contains a property.
[<A.Inline "$x.hasOwnProperty($prop)">]
let HasOwnProperty (x: obj) (prop: string) = raise ClientSideOnly : bool

/// Tests if the object has or inherits a property.
/// Implemented with the "in" operator.
[<A.Inline "$field in $x">]
let In (field: string) (x: obj) = raise ClientSideOnly : bool

/// Retrieves all proper fields from an object.
[<A.Direct "var r=[]; for(var k in $o) r.push([k,$o[k]]); return r">]
let GetFields (o: obj) = raise ClientSideOnly : (string * obj)[]

/// Retrieves the names of all proper fields from an object.
[<A.Direct "var r=[]; for(var k in $o) r.push(k); return r">]
let GetFieldNames (o: obj) = raise ClientSideOnly : string[]

/// Retrieves the values of all proper fields from an object.
[<A.Direct "var r=[]; for(var k in $o) r.push($o[k]); return r">]
let GetFieldValues (o: obj) = raise ClientSideOnly : obj[]

/// Constructs a new object with a given constructor function.
[<A.Inline "new $x()">]
let New (x: obj) : 'T = raise ClientSideOnly : 'T

/// Applies the JavaScript `typeof` operator.
[<A.Inline "typeof $x">]
let TypeOf (x: obj) = raise ClientSideOnly : Kind

/// Iterates over the fields of a JavaScript object.
/// Iteration can be terminated by returning `true`.
[<A.Direct "for (var k in $x) { if ($iter(k)) break; }">]
let ForEach (x: obj) (iter: string -> bool) = raise ClientSideOnly : unit

/// Tests if an object is an instance of a given class.
[<A.Inline "$x instanceof $cl">]
let InstanceOf (x: obj) (cl: obj) = raise ClientSideOnly : bool

/// Logs the given object to console if one is defined.
[<A.Direct "if (console) console.log($x)">]
[<System.Obsolete "Use Console.Log instead.">]
let Log (x: obj) = raise ClientSideOnly : unit

/// Logs an array or tuple to console if one is defined.
[<A.Direct "if (console) console.log.apply(console, $args)">]
[<System.Obsolete "Use Console.Log instead.">]
let LogMore args = ()

/// Gets a given field from an object.
[<A.Inline "$target[$field]">]
let Get<'T> (field: string) (target: obj) = raise ClientSideOnly : 'T

/// Sets a given field on an object.
[<A.Inline "void ($target[$field] = $v)">]
let Set (target: obj) (field: string) (v: obj) = raise ClientSideOnly : unit

/// Requests a function to be called on the next animation frame.
[<A.Require(typeof<AnimationFrameResource>)>]
[<A.Inline "requestAnimationFrame($f)">]
let RequestAnimationFrame (f: float -> unit) = raise ClientSideOnly : Handle

/// Cancels an animation frame request.
[<A.Require(typeof<AnimationFrameResource>)>]
[<A.Inline "cancelAnimationFrame($handle)">]
let CancelAnimationFrame (handle: Handle) = raise ClientSideOnly : unit

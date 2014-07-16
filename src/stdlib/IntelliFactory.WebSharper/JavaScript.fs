// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

/// Defines common JavaScript operations.
module IntelliFactory.WebSharper.JavaScript

module A = IntelliFactory.WebSharper.Core.Attributes

/// Thrown on the server when client-side code is being executed.
[<Sealed>]
type ClientSideCodeException() =
    inherit exn()

/// Constructs a null or default value for a client-side stub.
let ClientSide<'T> : 'T =
    let mutable count = 0
    count <- count (* prevent inlining that crashes compilation *)
    Unchecked.defaultof<'T>

/// Constructs a JavaScript "undefined" value.
[<A.Inline "undefined">]
let Undefined<'T> = ClientSide<'T>

/// Returns the reference to the global JavaScript object.
[<A.Inline "$global">]
let Global = obj ()

/// Ignores the value of an expression.
[<A.Inline "void $x">]
let Void (x: obj) = ClientSide<unit>

/// Exposes the JavaScript `+x` operator.
[<A.Inline "+ $x">]
let Plus<'T> (x: obj) = ClientSide<'T>

/// Exposes the JavaScript `-x` operator.
[<A.Inline "- $x">]
let Minus<'T> (x: obj) = ClientSide<'T>

/// Exposes the JavaScript `~x` operator.
[<A.Inline "~ $x">]
let BitwiseNot (x: obj) = ClientSide<'T>

/// Exposes the JavaScript `!x` operator.
[<A.Inline "! $x">]
let Not (x: obj) = ClientSide<bool>

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
let Alert (message: string) = ClientSide<unit>

/// Displays a popup dialog with Yes and No buttons, returns the choice.
[<A.Inline "confirm($message)">]
let Confirm (message: string) = ClientSide<bool>

/// Represents a timer handle.
[<Sealed>]
type Handle = class end

/// Schedules the function for execution in the
/// given number of microseconds.
[<A.Inline "setTimeout($f,$msec)">]
let SetTimeout (f: unit -> unit) (msec: int) = ClientSide<Handle>

/// Schedules the function for execution once every
/// given number of microseconds.
[<A.Inline "setInterval($f,$msec)">]
let SetInterval (f: unit -> unit) (msec: int) = ClientSide<Handle>

/// Clears a scheduled timeout function.
[<A.Inline "clearTimeout($handle)">]
let ClearTimeout (handle: Handle) = ClientSide<unit>

/// Clears a scheduled interval function.
[<A.Inline "clearInterval($handle)">]
let ClearInterval (handle: Handle) = ClientSide<unit>

/// Performs JavaScript function application.
[<A.Inline "$x[$func].apply($x,$args)">]
let Apply<'T> (x: obj) (func: string) (args: obj []) = ClientSide<'T>

/// Deletes a field from a JavaScript object.
[<A.Direct "delete $x[$field]">]
let Delete (x: obj) (field: string) = ClientSide<unit>

/// Tests if the object contains a property.
[<A.Inline "$x.hasOwnProperty($prop)">]
let HasOwnProperty (x: obj) (prop: string) = ClientSide<bool>

/// Tests if the object has or inherits a property.
/// Implemented with the "in" operator.
[<A.Inline "$field in $x">]
let In (field: string) (x: obj) = ClientSide<bool>

/// Retrieves all proper fields from an object.
[<A.Direct "var r=[]; for(var k in $o) r.push([k,$o[k]]); return r">]
let GetFields (o: obj) = ClientSide<(string * obj)[]>

/// Retrieves the names of all proper fields from an object.
[<A.Direct "var r=[]; for(var k in $o) r.push(k); return r">]
let GetFieldNames (o: obj) = ClientSide<string[]>

/// Retrieves the values of all proper fields from an object.
[<A.Direct "var r=[]; for(var k in $o) r.push($o[k]); return r">]
let GetFieldValues (o: obj) = ClientSide<obj[]>

/// Constructs a new object with a given constructor function.
[<A.Inline "new $x()">]
let New (x: obj) : 'T = ClientSide<'T>

/// Applies the JavaScript `typeof` operator.
[<A.Inline "typeof $x">]
let TypeOf (x: obj) = ClientSide<Kind>

/// Iterates over the fields of a JavaScript object.
/// Iteration can be terminated by returning `true`.
[<A.Direct "for (var k in $x) { if ($iter(k)) break; }">]
let ForEach (x: obj) (iter: string -> bool) = ClientSide<unit>

/// Tests if an object is an instance of a given class.
[<A.Inline "$x instanceof $cl">]
let InstanceOf (x: obj) (cl: obj) = ClientSide<bool>

/// Logs the given object to console if one is defined.
[<A.Direct "if (console) console.log($x)">]
let Log (x: obj) = ClientSide<unit>

/// Gets a given field from an object.
[<A.Inline "$target[$field]">]
let Get<'T> (field: string) (target: obj) = ClientSide<'T>

/// Sets a given field on an object.
[<A.Inline "void ($target[$field] = $v)">]
let Set (target: obj) (field: string) (v: obj) = ClientSide<unit>

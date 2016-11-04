// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

/// Defines operators and functions that are automatically available whenever
/// `WebSharper` is open.
[<AutoOpen>]
module WebSharper.JavaScript.Pervasives

open WebSharper
module M = WebSharper.Macro

/// Casts an object to the desired type.
[<Inline "$x">]
let As<'T> (x: obj) = X<'T>

[<Inline "$x * $y">]
let ( *. ) x y = X<obj>

[<Inline "$x / $y">]
let ( /. ) x y = X<obj>

[<Inline "$x % $y">]
let ( %. ) x y = X<obj>

[<Inline "$x + $y">]
let ( +. ) x y = X<obj>

[<Inline "$x - $y">]
let ( -. ) x y = X<obj>

[<Inline "$x << $y">]
let ( <<. ) x y = X<obj>

[<Inline "$x >> $y">]
let ( >>. ) x y = X<obj>

[<Inline "$x >>> $y">]
let ( >>>. ) x y = X<obj>

[<Inline "$x < $y">]
let ( <. ) x y = X<bool>

[<Inline "$x > $y">]
let ( >. ) x y = X<bool>

[<Inline "$x >= $y">]
let ( >=. ) x y = X<bool>

[<Inline "$x <= $y">]
let ( <=. ) x y = X<bool>

[<Inline "$x == $y">]
let ( ==. ) x y = X<bool>

[<Inline "$x === $y">]
let ( ===. ) x y = X<bool>

[<Inline "$x != $y">]
let ( !=. ) x y = X<bool>

[<Inline "$x !== $y">]
let ( !==. ) x y = X<bool>

[<Inline "$x | $y">]
let ( |. ) x y = X<obj>

[<Inline "$x & $y">]
let ( &. ) x y = X<obj>

[<Inline "$x ^ $y">]
let ( ^. ) x y = X<obj>

[<Inline "$obj[$field]">]
let ( ? ) (obj: obj) (field: string) = X<'T>

[<Inline "void ($obj[$key] = $value)">]
let ( ?<- ) (obj: obj) (key: string) (value: obj) = X<unit>

[<Inline "[$x,$y]">]
let ( => ) (x: string) (y: obj) = (x, y)

[<JavaScript>]
let private NewFromSeq<'T> (fields: seq<string * obj>) : 'T =
    let r = obj ()
    for (k, v) in fields do
        (?<-) r k v
    As r

/// Constructs a new object as if an object literal was used.
[<Macro(typeof<M.New>); Inline>]
let New<'T> (fields: seq<string * obj>) = NewFromSeq<'T> fields

type JS =
    /// Parses and inlines JavaScript code
    [<Macro(typeof<M.InlineJS>)>]
    static member Inline<'T> (inlineString: string, [<System.ParamArray>] args: obj[]) = X<'T>

/// Constructs an proxy to a remote object instance.
[<Constant null>]
let Remote<'T> = X<'T>

/// Gets JavaScript properties in sequence dynamically from an object.
[<JavaScript; Macro(typeof<M.GetJS>)>]
let GetJS<'T> (x: obj) (items: seq<string>) =
    let mutable x = x
    for i in items do
        x <- x?(i)
    As<'T> x    

[<Macro(typeof<M.DefaultToUndefined>)>]
let DefaultToUndefined<'T> (x: 'T) = x

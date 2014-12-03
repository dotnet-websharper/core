// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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
/// `IntelliFactory.WebSharper` is open.
[<AutoOpen>]
module IntelliFactory.WebSharper.Pervasives

module A = IntelliFactory.WebSharper.Core.Attributes
module J = IntelliFactory.WebSharper.JavaScript
module M = IntelliFactory.WebSharper.Macro

/// Returns null or some other default value of the given type.
/// Note: a short-hand for "Unchecked.defaultof<'T>".
let X<'T> = Unchecked.defaultof<'T>

let private binary (x: obj) (y: obj) = J.ClientSide<obj>
let private binaryAs<'T> (x: obj) (y: obj) = J.ClientSide<'T>

type ConstantAttribute = A.ConstantAttribute
type DirectAttribute = A.DirectAttribute
type InlineAttribute = A.InlineAttribute
type JavaScriptAttribute = A.JavaScriptAttribute
type MacroAttribute = A.MacroAttribute
type NameAttribute = A.NameAttribute
type ProxyAttribute = A.ProxyAttribute
type RemoteAttribute = A.RemoteAttribute
type RequireAttribute = A.RequireAttribute
type RpcAttribute = A.RemoteAttribute
type StubAttribute = A.StubAttribute

/// Casts an object to the desired type.
[<Inline "$x">]
let As<'T> (x: obj) = J.ClientSide<'T>

/// Implements piping with mutation.
[<Inline "($f($x), $x)">]
let ( |>! ) x f = f x; x

[<Inline "$x * $y">]
let ( *. ) x y = binary x y

[<Inline "$x / $y">]
let ( /. ) x y = binary x y

[<Inline "$x % $y">]
let ( %. ) x y = binary x y

[<Inline "$x + $y">]
let ( +. ) x y = binary x y

[<Inline "$x - $y">]
let ( -. ) x y = binary x y

[<Inline "$x << $y">]
let ( <<. ) x y = binary x y

[<Inline "$x >> $y">]
let ( >>. ) x y = binary x y

[<Inline "$x >>> $y">]
let ( >>>. ) x y = binary x y

[<Inline "$x < $y">]
let ( <. ) x y = binaryAs<bool> x y

[<Inline "$x > $y">]
let ( >. ) x y = binaryAs<bool> x y

[<Inline "$x >= $y">]
let ( >=. ) x y = binaryAs<bool> x y

[<Inline "$x <= $y">]
let ( <=. ) x y = binaryAs<bool> x y

[<Inline "$x == $y">]
let ( ==. ) x y = binaryAs<bool> x y

[<Inline "$x === $y">]
let ( ===. ) x y = binaryAs<bool> x y

[<Inline "$x != $y">]
let ( !=. ) x y = binaryAs<bool> x y

[<Inline "$x !== $y">]
let ( !==. ) x y = binaryAs<bool> x y

[<Inline "$x | $y">]
let ( |. ) x y = binary x y

[<Inline "$x & $y">]
let ( &. ) x y = binary x y

[<Inline "$x ^ $y">]
let ( ^. ) x y = binary x y

[<Inline "$obj[$field]">]
let ( ? ) (obj: obj) (field: string) = J.ClientSide<'T>

[<Inline "void ($obj[$key] = $value)">]
let ( ?<- ) (obj: obj) (key: string) (value: obj) = J.ClientSide<unit>

[<Inline "[$x,$y]">]
let ( => ) (x: string) (y: obj) = (x, y)

[<JavaScript>]
let private NewFromList<'T> (fields: seq<string * obj>) : 'T =
    let r = obj ()
    for (k, v) in fields do
        (?<-) r k v
    As r

/// Constructs a new object as if an object literal was used.
[<Macro(typeof<M.New>)>]
let New<'T> (fields: seq<string * obj>) = X<'T>

/// Re-exports Remoting.IRpcHandlerFactory.
type IRpcHandlerFactory =
    IntelliFactory.WebSharper.Core.Remoting.IHandlerFactory

/// Re-exports Remoting.SetHandlerFactory.
let SetRpcHandlerFactory (factory: IRpcHandlerFactory) =
    IntelliFactory.WebSharper.Core.Remoting.SetHandlerFactory factory

/// Constructs an proxy to a remote object instance.
[<Inline "null">]
let Remote<'T> = JavaScript.ClientSide<'T>

module F = IntelliFactory.WebSharper.Core.Functions

type Func<'T1,'T2> = F.Func<'T1,'T2>
type Func<'T1,'T2,'T3> = F.Func<'T1,'T2,'T3>
type Func<'T1,'T2,'T3,'T4> = F.Func<'T1,'T2,'T3,'T4>
type Func<'T1,'T2,'T3,'T4,'T5> = F.Func<'T1,'T2,'T3,'T4,'T5>
type Func<'T1,'T2,'T3,'T4,'T5,'T6> = F.Func<'T1,'T2,'T3,'T4,'T5,'T6>
type Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7> = F.Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7>
type Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8> = F.Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8>
type Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9> = F.Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9>


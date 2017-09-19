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

[<WebSharper.Proxy
    "Microsoft.FSharp.Linq.NullableOperators, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.NullableOperatorsProxy 

open System
open WebSharper.JavaScript

type N<'T when 'T: (new: unit -> 'T) and 'T: struct and 'T :> ValueType> = Nullable<'T> 

module M = WebSharper.Core.Macros

[<Macro(typeof<M.Arith>)>]
let (/?) (a: 'T0) (b: N<'T1>) = X<N<'T2>>

[<Macro(typeof<M.Comp>)>]
let (=?) (a: 'T) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (>=?) (a: 'T) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (>?) (a: 'T) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (<=?) (a: 'T) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (<>?) (a: 'T) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (<?) (a: 'T) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Arith>)>]
let (-?) (a: 'T0) (b: N<'T1>) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let ( *?) (a: 'T0) (b: N<'T1>) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (%?) (a: 'T0) (b: N<'T1>) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (+?) (a: 'T0) (b: N<'T1>) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (?/) (a: N<'T0>) (b: 'T1) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (?/?) (a: N<'T0>) (b: N<'T1>) = X<N<'T2>>

[<Macro(typeof<M.Comp>)>]
let (?=) (a: N<'T>) (b: 'T) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?=?) (a: N<'T>) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?>) (a: N<'T>) (b: 'T) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?>=) (a: N<'T>) (b: 'T) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?>=?) (a: N<'T>) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?>?) (a: N<'T>) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?<) (a: N<'T>) (b: 'T) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?<=) (a: N<'T>) (b: 'T) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?<=?) (a: N<'T>) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?<>) (a: N<'T>) (b: 'T) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?<>?) (a: N<'T>) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Comp>)>]
let (?<?) (a: N<'T>) (b: N<'T>) = X<bool>

[<Macro(typeof<M.Arith>)>]
let (?-) (a: N<'T0>) (b: 'T1) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (?-?) (a: N<'T0>) (b: N<'T1>) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (?*) (a: N<'T0>) (b: 'T1) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (?*?) (a: N<'T0>) (b: N<'T1>) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (?%) (a: N<'T0>) (b: 'T1) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (?%?) (a: N<'T0>) (b: N<'T1>) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (?+) (a: N<'T0>) (b: 'T1) = X<N<'T2>>

[<Macro(typeof<M.Arith>)>]
let (?+?) (a: N<'T0>) (b: N<'T1>) = X<N<'T2>>

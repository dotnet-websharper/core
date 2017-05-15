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

namespace WebSharper

open WebSharper.JavaScript

[<Proxy(typeof<System.Tuple>)>]
type internal TupleStaticProxy =
    [<Inline "[$0]">] 
    static member Create<'T1>(_: 'T1) = X<System.Tuple<'T1>>
    [<Inline "[$0, $1]">]
    static member Create<'T1, 'T2>(_: 'T1, _: 'T2) = X<System.Tuple<'T1, 'T2>>
    [<Inline "[$0, $1, $2]">]
    static member Create<'T1, 'T2, 'T3>(_: 'T1, _: 'T2, _: 'T3) = X<System.Tuple<'T1, 'T2, 'T3>>
    [<Inline "[$0, $1, $2, $3]">]
    static member Create<'T1, 'T2, 'T3, 'T4>(_: 'T1, _: 'T2, _: 'T3, _: 'T4) = X<System.Tuple<'T1, 'T2, 'T3, 'T4>>
    [<Inline "[$0, $1, $2, $3, $4]">]
    static member Create<'T1, 'T2, 'T3, 'T4, 'T5>(_: 'T1, _: 'T2, _: 'T3, _: 'T4, _: 'T5) = X<System.Tuple<'T1, 'T2, 'T3, 'T4, 'T5>>
    [<Inline "[$0, $1, $2, $3, $4, $5]">]
    static member Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>(_: 'T1, _: 'T2, _: 'T3, _: 'T4, _: 'T5, _: 'T6) = X<System.Tuple<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>>
    [<Inline "[$0, $1, $2, $3, $4, $5, $6]">]
    static member Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>(_: 'T1, _: 'T2, _: 'T3, _: 'T4, _: 'T5, _: 'T6, _: 'T7) = X<System.Tuple<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>>
    [<Inline "[$0, $1, $2, $3, $4, $5, $6].concat($7)">]
    static member Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest>(_: 'T1, _: 'T2, _: 'T3, _: 'T4, _: 'T5, _: 'T6, _: 'T7, _: 'TRest) = X<System.Tuple<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest>>

[<Proxy(typeof<System.Tuple<_>>)>]
type internal TupleProxy<'T1> 
    [<Inline "[$0]" >] (a: 'T1) =
    [<Inline "$0[0]" >] member this.Item1 = X<'T1>

[<Proxy(typeof<System.Tuple<_,_>>)>]
type internal TupleProxy<'T1, 'T2> 
    [<Inline "[$0, $1]" >] (a: 'T1, b: 'T2) =
    [<Inline "$0[0]" >] member this.Item1 = X<'T1>
    [<Inline "$0[1]" >] member this.Item2 = X<'T2>

[<Proxy(typeof<System.Tuple<_,_,_>>)>]
type internal TupleProxy<'T1, 'T2, 'T3> 
    [<Inline "[$0, $1, $2]" >] (a: 'T1, b: 'T2, c: 'T3) =
    [<Inline "$0[0]" >] member this.Item1 = X<'T1>
    [<Inline "$0[1]" >] member this.Item2 = X<'T2>
    [<Inline "$0[2]" >] member this.Item3 = X<'T3>

[<Proxy(typeof<System.Tuple<_,_,_,_>>)>]
type internal TupleProxy<'T1, 'T2, 'T3, 'T4> 
    [<Inline "[$0, $1, $2, $3]" >] (a: 'T1, b: 'T2, c: 'T3, d: 'T4) =
    [<Inline "$0[0]" >] member this.Item1 = X<'T1>
    [<Inline "$0[1]" >] member this.Item2 = X<'T2>
    [<Inline "$0[2]" >] member this.Item3 = X<'T3>
    [<Inline "$0[3]" >] member this.Item4 = X<'T4>

[<Proxy(typeof<System.Tuple<_,_,_,_,_>>)>]
type internal TupleProxy<'T1, 'T2, 'T3, 'T4, 'T5> 
    [<Inline "[$0, $1, $2, $3, $4]" >] (a: 'T1, b: 'T2, c: 'T3, d: 'T4, e: 'T5) =
    [<Inline "$0[0]" >] member this.Item1 = X<'T1>
    [<Inline "$0[1]" >] member this.Item2 = X<'T2>
    [<Inline "$0[2]" >] member this.Item3 = X<'T3>
    [<Inline "$0[3]" >] member this.Item4 = X<'T4>
    [<Inline "$0[4]" >] member this.Item5 = X<'T5>

[<Proxy(typeof<System.Tuple<_,_,_,_,_,_>>)>]
type internal TupleProxy<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> 
    [<Inline "[$0, $1, $2, $3, $4, $5]" >] (a: 'T1, b: 'T2, c: 'T3, d: 'T4, e: 'T5, f: 'T6) =
    [<Inline "$0[0]" >] member this.Item1 = X<'T1>
    [<Inline "$0[1]" >] member this.Item2 = X<'T2>
    [<Inline "$0[2]" >] member this.Item3 = X<'T3>
    [<Inline "$0[3]" >] member this.Item4 = X<'T4>
    [<Inline "$0[4]" >] member this.Item5 = X<'T5>
    [<Inline "$0[5]" >] member this.Item6 = X<'T6>

[<Proxy(typeof<System.Tuple<_,_,_,_,_,_,_>>)>]
type internal TupleProxy<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> 
    [<Inline "[$0, $1, $2, $3, $4, $5, $6]" >] (a: 'T1, b: 'T2, c: 'T3, d: 'T4, e: 'T5, f: 'T6, g: 'T7) =
    [<Inline "$0[0]" >] member this.Item1 = X<'T1>
    [<Inline "$0[1]" >] member this.Item2 = X<'T2>
    [<Inline "$0[2]" >] member this.Item3 = X<'T3>
    [<Inline "$0[3]" >] member this.Item4 = X<'T4>
    [<Inline "$0[4]" >] member this.Item5 = X<'T5>
    [<Inline "$0[5]" >] member this.Item6 = X<'T6>
    [<Inline "$0[6]" >] member this.Item7 = X<'T7>

[<Proxy(typeof<System.Tuple<_,_,_,_,_,_,_,_>>)>]
type internal TupleProxy<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> 
    [<Inline "[$0, $1, $2, $3, $4, $5, $6].concat($7)" >] (a: 'T1, b: 'T2, c: 'T3, d: 'T4, e: 'T5, f: 'T6, g: 'T7, h: 'TRest) =
    [<Inline "$0[0]" >] member this.Item1 = X<'T1>
    [<Inline "$0[1]" >] member this.Item2 = X<'T2>
    [<Inline "$0[2]" >] member this.Item3 = X<'T3>
    [<Inline "$0[3]" >] member this.Item4 = X<'T4>
    [<Inline "$0[4]" >] member this.Item5 = X<'T5>
    [<Inline "$0[5]" >] member this.Item6 = X<'T6>
    [<Inline "$0[6]" >] member this.Item7 = X<'T7>
    [<Inline "$0.slice(7)" >] member this.Rest = X<'TRest>

[<Proxy(typeof<System.ValueTuple<_>>)>]
type internal ValueTupleProxy<'T1> 
    [<Inline "[$0]" >] (a: 'T1) =
    member this.Item1 
        with [<Inline "$0[0]" >] get () = X<'T1>
        and  [<Inline "$0[0] = $v" >] set (v: 'T1) = ()

[<Proxy(typeof<System.ValueTuple<_,_>>)>]
type internal ValueTupleProxy<'T1, 'T2> 
    [<Inline "[$0, $1]" >] (a: 'T1, b: 'T2) =
    member this.Item1 
        with [<Inline "$0[0]" >] get () = X<'T1>
        and  [<Inline "$0[0] = $v" >] set (v: 'T1) = ()
    member this.Item2 
        with [<Inline "$0[1]" >] get () = X<'T2>
        and  [<Inline "$0[1] = $v" >] set (v: 'T2) = ()

[<Proxy(typeof<System.ValueTuple<_,_,_>>)>]
type internal ValueTupleProxy<'T1, 'T2, 'T3> 
    [<Inline "[$0, $1, $2]" >] (a: 'T1, b: 'T2, c: 'T3) =
    member this.Item1 
        with [<Inline "$0[0]" >] get () = X<'T1>
        and  [<Inline "$0[0] = $v" >] set (v: 'T1) = ()
    member this.Item2 
        with [<Inline "$0[1]" >] get () = X<'T2>
        and  [<Inline "$0[1] = $v" >] set (v: 'T2) = ()
    member this.Item3 
        with [<Inline "$0[2]" >] get () = X<'T3>
        and  [<Inline "$0[2] = $v" >] set (v: 'T3) = ()

[<Proxy(typeof<System.ValueTuple<_,_,_,_>>)>]
type internal ValueTupleProxy<'T1, 'T2, 'T3, 'T4> 
    [<Inline "[$0, $1, $2, $3]" >] (a: 'T1, b: 'T2, c: 'T3, d: 'T4) =
    member this.Item1 
        with [<Inline "$0[0]" >] get () = X<'T1>
        and  [<Inline "$0[0] = $v" >] set (v: 'T1) = ()
    member this.Item2 
        with [<Inline "$0[1]" >] get () = X<'T2>
        and  [<Inline "$0[1] = $v" >] set (v: 'T2) = ()
    member this.Item3 
        with [<Inline "$0[2]" >] get () = X<'T3>
        and  [<Inline "$0[2] = $v" >] set (v: 'T3) = ()
    member this.Item4 
        with [<Inline "$0[3]" >] get () = X<'T4>
        and  [<Inline "$0[3] = $v" >] set (v: 'T4) = ()

[<Proxy(typeof<System.ValueTuple<_,_,_,_,_>>)>]
type internal ValueTupleProxy<'T1, 'T2, 'T3, 'T4, 'T5> 
    [<Inline "[$0, $1, $2, $3, $4]" >] (a: 'T1, b: 'T2, c: 'T3, d: 'T4, e: 'T5) =
    member this.Item1 
        with [<Inline "$0[0]" >] get () = X<'T1>
        and  [<Inline "$0[0] = $v" >] set (v: 'T1) = ()
    member this.Item2 
        with [<Inline "$0[1]" >] get () = X<'T2>
        and  [<Inline "$0[1] = $v" >] set (v: 'T2) = ()
    member this.Item3 
        with [<Inline "$0[2]" >] get () = X<'T3>
        and  [<Inline "$0[2] = $v" >] set (v: 'T3) = ()
    member this.Item4 
        with [<Inline "$0[3]" >] get () = X<'T4>
        and  [<Inline "$0[3] = $v" >] set (v: 'T4) = ()
    member this.Item5 
        with [<Inline "$0[4]" >] get () = X<'T5>
        and  [<Inline "$0[4] = $v" >] set (v: 'T5) = ()

[<Proxy(typeof<System.ValueTuple<_,_,_,_,_,_>>)>]
type internal ValueTupleProxy<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> 
    [<Inline "[$0, $1, $2, $3, $4, $5]" >] (a: 'T1, b: 'T2, c: 'T3, d: 'T4, e: 'T5, f: 'T6) =
    member this.Item1 
        with [<Inline "$0[0]" >] get () = X<'T1>
        and  [<Inline "$0[0] = $v" >] set (v: 'T1) = ()
    member this.Item2 
        with [<Inline "$0[1]" >] get () = X<'T2>
        and  [<Inline "$0[1] = $v" >] set (v: 'T2) = ()
    member this.Item3 
        with [<Inline "$0[2]" >] get () = X<'T3>
        and  [<Inline "$0[2] = $v" >] set (v: 'T3) = ()
    member this.Item4 
        with [<Inline "$0[3]" >] get () = X<'T4>
        and  [<Inline "$0[3] = $v" >] set (v: 'T4) = ()
    member this.Item5 
        with [<Inline "$0[4]" >] get () = X<'T5>
        and  [<Inline "$0[4] = $v" >] set (v: 'T5) = ()
    member this.Item6 
        with [<Inline "$0[5]" >] get () = X<'T6>
        and  [<Inline "$0[5] = $v" >] set (v: 'T6) = ()

[<Proxy(typeof<System.ValueTuple<_,_,_,_,_,_,_>>)>]
type internal ValueTupleProxy<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> 
    [<Inline "[$0, $1, $2, $3, $4, $5, $6]" >] (a: 'T1, b: 'T2, c: 'T3, d: 'T4, e: 'T5, f: 'T6, g: 'T7) =
    member this.Item1 
        with [<Inline "$0[0]" >] get () = X<'T1>
        and  [<Inline "$0[0] = $v" >] set (v: 'T1) = ()
    member this.Item2 
        with [<Inline "$0[1]" >] get () = X<'T2>
        and  [<Inline "$0[1] = $v" >] set (v: 'T2) = ()
    member this.Item3 
        with [<Inline "$0[2]" >] get () = X<'T3>
        and  [<Inline "$0[2] = $v" >] set (v: 'T3) = ()
    member this.Item4 
        with [<Inline "$0[3]" >] get () = X<'T4>
        and  [<Inline "$0[3] = $v" >] set (v: 'T4) = ()
    member this.Item5 
        with [<Inline "$0[4]" >] get () = X<'T5>
        and  [<Inline "$0[4] = $v" >] set (v: 'T5) = ()
    member this.Item6 
        with [<Inline "$0[5]" >] get () = X<'T6>
        and  [<Inline "$0[5] = $v" >] set (v: 'T6) = ()
    member this.Item7 
        with [<Inline "$0[6]" >] get () = X<'T7>
        and  [<Inline "$0[6] = $v" >] set (v: 'T7) = ()

[<Proxy(typeof<System.ValueTuple<_,_,_,_,_,_,_,_>>)>]
type internal ValueTupleProxy<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> 
    [<Inline "[$0, $1, $2, $3, $4, $5, $6].concat($7)" >] (a: 'T1, b: 'T2, c: 'T3, d: 'T4, e: 'T5, f: 'T6, g: 'T7, h: 'TRest) =
    member this.Item1 
        with [<Inline "$0[0]" >] get () = X<'T1>
        and  [<Inline "$0[0] = $v" >] set (v: 'T1) = ()
    member this.Item2 
        with [<Inline "$0[1]" >] get () = X<'T2>
        and  [<Inline "$0[1] = $v" >] set (v: 'T2) = ()
    member this.Item3 
        with [<Inline "$0[2]" >] get () = X<'T3>
        and  [<Inline "$0[2] = $v" >] set (v: 'T3) = ()
    member this.Item4 
        with [<Inline "$0[3]" >] get () = X<'T4>
        and  [<Inline "$0[3] = $v" >] set (v: 'T4) = ()
    member this.Item5 
        with [<Inline "$0[4]" >] get () = X<'T5>
        and  [<Inline "$0[4] = $v" >] set (v: 'T5) = ()
    member this.Item6 
        with [<Inline "$0[5]" >] get () = X<'T6>
        and  [<Inline "$0[5] = $v" >] set (v: 'T6) = ()
    member this.Item7 
        with [<Inline "$0[6]" >] get () = X<'T7>
        and  [<Inline "$0[6] = $v" >] set (v: 'T7) = ()
    member this.Rest 
        with [<Inline "$0.slice(7)" >] get() = X<'TRest>
        and  [<Inline "Array.prototype.splice.apply($0, [7, $0.length - 7].concat($v))" >] set (v: 'TRest) = ()

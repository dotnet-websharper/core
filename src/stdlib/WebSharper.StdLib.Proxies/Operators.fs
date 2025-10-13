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

[<WebSharper.Name "Operators">]
[<WebSharper.Proxy "Microsoft.FSharp.Core.Operators, FSharp.Core">]
module private WebSharper.OperatorsProxy

#nowarn "86"

open WebSharper.JavaScript

module M = WebSharper.Core.Macros

[<JavaScript>]
[<Name "range">]
[<Macro(typeof<M.Range>)>]
let ( .. ) (min: 'T) (max: 'T) : seq<'T> =
    let count = 1 + As max - As min
    if count <= 0 then Seq.empty
    else Seq.init count (fun x -> As (x + As min))

[<JavaScript>]
[<Name "step">]
[<Macro(typeof<M.Step>)>]
let ( .. .. ) (min: 'T1) (step: 'T2) (max: 'T1) : seq<'T1> =
    let s = sign (As<int> step)
    Seq.initInfinite (fun k -> As<int> min + k * As<int> step)
    |> Seq.takeWhile (fun k -> s * (As<int> max - As<int> k) >= 0)
    |> As

[<Inline "$r[0]">]
let ( ! ) (r: ref<'T>) = X<'T>

[<Macro(typeof<M.Arith>)>]
let ( % ) (a: 'T1) (b: 'T2) = X<'T3>

[<Macro(typeof<M.Arith>)>]
let ( &&& ) (a: 'T1) (b: 'T1) = X<'T1>

[<Macro(typeof<M.Arith>)>]
let ( * ) (a: 'T1) (b: 'T2) = X<'T3>

[<Macro(typeof<M.Arith>)>]
let ( ** ) (a: 'T1) (b: 'T2) = X<'T1>

[<Inline "Math.pow($a, $p)">]
let PowInteger (a: 'T, p: int) = X<'T>

[<Macro(typeof<M.Arith>)>]
let ( + ) (a: 'T1) (b: 'T2) = X<'T3>

[<Macro(typeof<M.Arith>)>]
let ( - ) (a: 'T1) (b: 'T2) = X<'T3>

[<Macro(typeof<M.Arith>)>]
let ( / ) (x: 'T1) (y: 'T2) = X<'T3>

[<Inline "void ($a[0] = $b)">]
let ( := ) (a: ref<'T>) (b: 'T) = X<unit>

[<Inline>]
let ( << ) (f: 'T1 -> 'T2) (g: 'T3 -> 'T1) : 'T3 -> 'T2 = 
    ()
    fun x -> f (g x)

[<Macro(typeof<M.Arith>)>]
let inline ( <<< ) (a: 'T) (b: int) = X<'T>

[<Inline>]
let ( <| ) (f: 'T -> 'TR) (x: 'T) : 'TR = f x

[<Inline>]
let ( <|| ) (f: 'T1 -> 'T2 -> 'TR) (x: 'T1, y: 'T2) : 'TR = f x y

[<Inline>]
let ( <||| ) (f: 'T1 -> 'T2 -> 'T3 -> 'TR)
             (x: 'T1, y: 'T2, z: 'T3) : 'TR = f x y z

[<Macro(typeof<M.Comp>)>]
let ( = ) (a: 'T) (b: 'T) = X<bool>

[<Macro(typeof<M.Comp>)>]
let ( <> ) (a: 'T) (b: 'T) =  X<bool>

[<Macro(typeof<M.Comp>)>]
let ( < ) (a: 'T) (b: 'T) = X<bool>

[<Macro(typeof<M.Comp>)>]
let ( > ) (a: 'T) (b: 'T) = X<bool>

[<Macro(typeof<M.Comp>)>]
let ( <= ) (a: 'T) (b: 'T) = X<bool>

[<Macro(typeof<M.Comp>)>]
let ( >= ) (a: 'T) (b: 'T) = X<bool>

[<Inline>]
let ( >> ) (f: 'T1 -> 'T2) (g: 'T2 -> 'T3): 'T1->'T3 = 
    ()
    fun x -> g (f x)

[<Macro(typeof<M.Arith>)>]
let inline ( >>> ) (a: 'T) (b: int) : 'T = X<'T>

[<Inline>]
let ( @ ) a b = List.append a b

[<Inline "$a + $b">]
let ( ^ ) (a: string) (b: string) : string = a + b

[<Macro(typeof<M.Arith>)>]
let ( ^^^ ) (a: 'T) (b: 'T) = X<'T>

[<Inline>]
let ( |> ) (x: 'T1) (f: 'T1 -> 'T2) : 'T2 = f x

[<Inline>]
let ( ||> ) (x: 'T1, y: 'T2) (f: 'T1 -> 'T2 -> 'TR) : 'TR = f x y

[<Macro(typeof<M.Arith>)>]
let ( ||| ) (a: 'T) (b: 'T) = X<'T>

[<Inline>]
let ( |||> ) (x: 'T1, y: 'T2, z: 'T3)
             (f: 'T1 -> 'T2 -> 'T3 -> 'TR) : 'TR = f x y z

[<Macro(typeof<M.Arith>)>]
let ( ~+ ) (x: 'T) = X<'T>

[<Macro(typeof<M.Arith>)>]
let ( ~- ) (x: 'T) = X<'T>

[<Macro(typeof<M.Arith>)>]
let ( ~~~ ) (x: 'T) = X<'T>

[<Name "bigIntAbs">]
let BigIntAbs (x: int64) =
    match x with
    | n when n < 0L -> -x
    | _             -> x

[<Macro(typeof<M.Abs>)>]
[<Inline "Math.abs($x)">]
let Abs (x: 'T) = X<'T>

[<Macro(typeof<M.Op>)>]
[<Inline "Math.acos($x)">]
let Acos (x: 'T) = X<'T>

[<Macro(typeof<M.Op>)>]
[<Inline "Math.asin($x)">]
let Asin (x: 'T) = X<'T>

[<Macro(typeof<M.Op>)>]
[<Inline "Math.atan($x)">]
let Atan (x: 'T) = X<'T>

[<Macro(typeof<M.Op>)>]
[<Inline "Math.atan2($x, $y)">]
let Atan2 (x: 'T1) (y: 'T1) = X<'T2>

[<Inline "$x">]
let Box (x: 'T) = X<obj>

[<Macro(typeof<M.Op>)>]
[<Inline "Math.ceil($x)">]
let Ceiling (x: 'T) = X<'T>

[<Macro(typeof<M.Char>)>]
let ToChar (x: 'T) = X<char>

[<Macro(typeof<M.Conversion>)>]
let ToByte (x: 'T) = X<byte>

[<Macro(typeof<M.Conversion>)>]
let ToSByte (x: 'T) = X<sbyte>

[<Inline>]
let Compare<'T> (a: 'T) (b: 'T) = Unchecked.compare a b

[<Macro(typeof<M.Op>)>]
[<Inline "Math.cos($x)">]
let Cos (x: 'T) = X<'T>

[<Macro(typeof<M.Op>)>]
[<Inline "(Math.exp($x)+Math.exp(-$x))/2">]
let Cosh<'T> (x: 'T) = X<'T>

[<Inline "void ($x[0]--)">]
let Decrement (x: ref<int>) = ()

[<Inline>]
let DefaultArg x d =
    match x with
    | Some x -> x
    | None   -> d

[<Inline>]
let DefaultValueArg x d =
    match x with
    | ValueSome x -> x
    | ValueNone   -> d

[<Inline "$x">]
let Enum<'T when 'T : enum<int>> (x: 'T) = X<'T>

[<Macro(typeof<M.Conversion>)>]
let ToDecimal (x: 'T) = X<decimal>

[<Macro(typeof<M.Conversion>)>]
let ToDouble (x: 'T) = X<double>

[<Macro(typeof<M.Op>)>]
[<Inline "Math.exp($x)">]
let inline Exp (x: 'T) = X<'T>

let FailWith (msg: string) : 'T = raise (exn msg)

[<Macro(typeof<M.Conversion>)>]
let ToFloat (x: 'T) = X<float>

[<Macro(typeof<M.Op>)>]
[<Inline "Math.floor($x)">]
let Floor (x: 'T) = X<'T>

[<Inline "$x[0]">]
let Fst (x: TupleProxy<'T1,'T2>) = X<'T1>

[<Inline>]
let Hash<'T when 'T : equality> (x: 'T) = Unchecked.hash x

[<Inline "$x">]
let Identity (x: 'T) = X<'T>

[<Inline "void $x">]
let Ignore (x: 'T) = X<unit>

[<Inline "void ($x[0]++)">]
let Increment (x: ref<int>) = ()

[<Inline "Infinity">]
let Infinity = Unchecked.defaultof<double>

let InvalidOp (msg: string) : 'T = raise (System.InvalidOperationException(msg))

let InvalidArg (arg: string) (msg: string) : 'T = raise (System.ArgumentException(arg, msg))

[<Inline>]
let Lock (o: 'TLock) (act: unit -> 'T) = act()

[<Macro(typeof<M.Conversion>)>]
let ToInt (x: 'T) = X<int>

[<Macro(typeof<M.Conversion>)>]
let ToInt16 (x: 'T) = X<int16>

[<Macro(typeof<M.Conversion>)>]
let ToSingle (x: 'T) = X<single>

[<Macro(typeof<M.Conversion>)>]
let ToInt32 (x: 'T) = X<int32>

let toUInt (x: float) : int =
    (if x < 0. then Math.Ceil(x) else Math.Floor(x)) >>>. 0 |> As<int>

[<Inline "4294967296">]
let intRange = X<int>

let toInt (x: float) : int =
    let u = toUInt x
    if u > 2147483647 then u - As<int> intRange else u

[<Inline "$x">]
let ToEnum<'T> (x: int) = X<'T>

[<Macro(typeof<M.Conversion>)>]
let ToInt64 (x: 'T) = X<int64>

[<Macro(typeof<M.Conversion>)>]
let ToUInt16 (x: 'T) = X<uint16>

[<Macro(typeof<M.Conversion>)>]
let ToUInt32 (x: 'T) = X<uint32>

[<Macro(typeof<M.Conversion>)>]
let ToUInt64 (x: 'T) = X<uint64>

[<Inline "Math.log($x)">]
let Log (x: 'T) = X<'T>

[<Inline "Math.log($x)/Math.log(10)">]
let Log10 (x: 'T) = X<'T>

[<Inline>]
let Max<'T when 'T : comparison> (a: 'T) (b: 'T) =
    if a > b then a else b

[<Inline>]
let Min<'T when 'T : comparison> (a: 'T) (b: 'T) =
    if a < b then a else b

[<Inline "Infinity">]
let InfinitySingle = single infinity

[<Inline "NaN">]
let NaNSingle = single nan

[<Inline "NaN">]
let NaN = nan

[<Inline "!$x">]
let Not (x: bool) = X<bool>

[<Inline "throw $e">]
let Raise (e: exn) = X<'T>

[<Inline "[$x]">]
let Ref (x: 'T) = X<ref<'T>>

[<Inline "Math.round($x)">]
let Round (x: 'T) = X<'T>

[<Inline "$x">]
let CreateSequence (x: seq<'T>) = X<seq<'T>>

[<Name "bigIntSign">]
let BigIntSign (x: int64) =
    match x with
    | 0L            -> 0
    | n when n < 0L -> -1
    | _             -> 1

[<Macro(typeof<M.Sign>); JavaScript>]
let Sign<'T> (x: 'T) =
    match As<int> x with
    | 0            -> 0
    | n when n < 0 -> -1
    | _            -> 1

[<Macro(typeof<M.Op>)>]
[<Inline "Math.sin($x)">]
let Sin (x: 'T) = X<'T>

[<Macro(typeof<M.Op>)>]
[<Inline "(Math.exp($x)-Math.exp(-$x))/2">]
let Sinh (x: 'T) = x

[<Inline "$x[1]">]
let Snd (x: TupleProxy<'T1,'T2>) = X<'T2>

[<Inline "Math.sqrt($x)">]
let Sqrt (x: 'T1) = X<'T2>

[<Macro(typeof<M.String>)>]
let ToString (x: 'T) = X<string>

[<Macro(typeof<M.Op>)>]
[<Inline "Math.tan($x)">]
let inline Tan (x: 'T) = X<'T>

[<Macro(typeof<M.Op>)>]
[<Inline "(Math.exp(2*$x)-1)/(Math.exp(2*$x)+1)">]
let Tanh (x: 'T) = X<'T>

[<Inline>]
let inline Truncate<'T> (x: 'T) =
    if x <. 0 then Ceiling x else Floor x

[<Inline "$x">]
let Unbox (x: obj) = X<'T>

[<Inline "$x == null">]
let IsNull (x: 'T) = X<bool>

[<Inline>]
let Using t f =
    try f t finally (t :> System.IDisposable).Dispose()

[<Name "KeyValue">]
let KeyValuePattern (kvp: System.Collections.Generic.KeyValuePair<_,_>) =
    (kvp.Key, kvp.Value)


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

[<WebSharper.Core.Attributes.Name "Operators">]
[<WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.Operators, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.OperatorsProxy

#nowarn "86"

open WebSharper.JavaScript

module M = WebSharper.Macro

[<JavaScript>]
[<Name "range">]
let ( .. ) (min: 'T) (max: 'T) : seq<'T> =
    let count = 1 + As max - As min
    if count <= 0 then Seq.empty
    else Seq.init count (fun x -> As (x + As min))

[<JavaScript>]
[<Name "step">]
let ( .. .. ) (min: 'T1) (step: 'T2) (max: 'T1) : seq<'T1> =
    let s = sign (As<int> step)
    Seq.initInfinite (fun k -> As<int> min + k * As<int> step)
    |> Seq.takeWhile (fun k -> s * (As<int> max - As<int> k) >= 0)
    |> As

[<Inline "$r[0]">]
let ( ! ) (r: ref<'T>) = X<'T>

[<Inline "$a % $b">]
let ( % ) (a: 'T1) (b: 'T2) = X<'T3>

[<Inline "$a & $b">]
let ( &&& ) (a: 'T1) (b: 'T1) = X<'T1>

[<Inline "$a * $b">]
let ( * ) (a: 'T1) (b: 'T2) = X<'T3>

[<Inline "Math.pow($a, $b)">]
let ( ** ) (a: 'T1) (b: 'T2) = X<'T1>

[<Inline "Math.pow($a, $p)">]
let PowInteger (a: 'T, p: int) = X<'T>

[<Macro(typeof<M.Add>)>]
let ( + ) (a: 'T1) (b: 'T2) = X<'T3>

[<Macro(typeof<M.Sub>)>]
let ( - ) (a: 'T1) (b: 'T2) = X<'T3>

[<Macro(typeof<M.Div>)>]
let ( / ) (x: 'T1) (y: 'T2) = X<'T3>

[<Inline "void ($a[0] = $b)">]
let ( := ) (a: ref<'T>) (b: 'T) = X<unit>

[<Inline "function (x) { return $f($g(x)); }">]
let ( << ) (f: 'T1 -> 'T2) (g: 'T3 -> 'T1) = X<'T3 -> 'T2>

[<Inline "$a << $b">]
let inline ( <<< ) (a: 'T) (b: int) = X<'T>

[<Inline "$f($x)">]
let ( <| ) (f: 'T -> 'TR) (x: 'T) = X<'TR>

[<Inline "$f($x)($y)">]
let ( <|| ) (f: 'T1 -> 'T2 -> 'TR) (x: 'T1, y: 'T2) = X<'TR>

[<Inline "$f($x)($y)($z)">]
let ( <||| ) (f: 'T1 -> 'T2 -> 'T3 -> 'TR)
             (x: 'T1, y: 'T2, z: 'T3) = X<'TR>

[<Macro(typeof<M.EQ>)>]
let ( = ) (a: 'T) (b: 'T) = X<bool>

[<Macro(typeof<M.NE>)>]
let ( <> ) (a: 'T) (b: 'T) =  X<bool>

[<Macro(typeof<M.LT>)>]
let ( < ) (a: 'T) (b: 'T) = X<bool>

[<Macro(typeof<M.GT>)>]
let ( > ) (a: 'T) (b: 'T) = X<bool>

[<Macro(typeof<M.LE>)>]
let ( <= ) (a: 'T) (b: 'T) = X<bool>

[<Macro(typeof<M.GE>)>]
let ( >= ) (a: 'T) (b: 'T) = X<bool>

[<Inline "function (x) {return $g($f(x))}">]
let ( >> ) (f: 'T1 -> 'T2) (g: 'T2 -> 'T3) = X<'T1->'T3>

[<Inline "$a >> $b">]
let inline ( >>> ) (a: 'T) (b: int) : 'T = a >>> b

[<Inline>]
[<JavaScript>]
let ( @ ) a b = List.append a b

[<Inline "$a + $b">]
let ( ^ ) (a: string) (b: string) = a + b

[<Inline "$a ^ $b">]
let ( ^^^ ) (a: 'T) (b: 'T) = X<'T>

[<Inline "$f($x)">]
[<Name "pipe">]
let ( |> ) (x: 'T1) (f: 'T1 -> 'T2) = X<'T2>

[<Inline "$f($x)($y)">]
[<Name "pipe2">]
let ( ||> ) (x: 'T1, y: 'T2) (f: 'T1 -> 'T2 -> 'TR) = X<'TR>

[<Inline "$a | $b">]
let ( ||| ) (a: 'T) (b: 'T) = X<'T>

[<Inline "$f($x)($y)($z)">]
let ( |||> ) (x: 'T1, y: 'T2, z: 'T3)
             (f: 'T1 -> 'T2 -> 'T3 -> 'TR) = X<'TR>

[<Inline "+ $x">]
let ( ~+ ) (x: 'T) = X<'T>

[<Inline "- $x">]
let ( ~- ) (x: 'T) = X<'T>

[<Inline "~ $x">]
let ( ~~~ ) (x: 'T) = X<'T>

[<Inline "Math.abs($x)">]
let Abs (x: 'T) = X<'T>

[<Inline "Math.acos($x)">]
let Acos (x: 'T) = X<'T>

[<Inline "Math.asin($x)">]
let Asin (x: 'T) = X<'T>

[<Inline "Math.atan($x)">]
let Atan (x: 'T) = X<'T>

[<Inline "Math.atan2($x, $y)">]
let Atan2 (x: 'T1) (y: 'T1) = X<'T2>

[<Inline "$x">]
let Box (x: 'T) = X<obj>

[<Inline "Math.ceil($x)">]
let Ceiling (x: 'T) = X<'T>

[<Macro(typeof<M.Char>)>]
let ToChar (x: 'T) = X<char>

[<JavaScript>]
let Compare<'T> (a: 'T) (b: 'T) = Unchecked.compare a b

[<Inline "Math.cos($x)">]
let Cos (x: 'T) = X<'T>

[<Inline "(Math.exp($x)+Math.exp(-$x))/2">]
let Cosh<'T> (x: 'T) = X<'T>

[<Direct "void ($x[0]--)">]
[<Name "WebSharper.Ref.decr">]
let Decrement (x: ref<int>) = ()

[<JavaScript>]
let DefaultArg x d =
    match x with
    | Some x -> x
    | None   -> d

[<Inline "$x">]
let Enum<'T when 'T : enum<int>> (x: 'T) = X<'T>

[<Inline "Number($x)">]
let ToDouble (x: 'T) = X<double>

[<Inline "Math.exp($x)">]
let inline Exp (x: 'T) = X<'T>

[<JavaScript>]
let FailWith (msg: string) : 'T = raise (exn msg)

[<Inline "Number($x)">]
let ToFloat (x: 'T) = X<'T>

[<Inline "Math.floor($x)">]
let Floor (x: 'T) = X<'T>

[<Inline "$x[0]">]
let Fst (x: System.Tuple<'T1,'T2>) = X<'T1>

[<Inline>]
[<JavaScript>]
let Hash<'T when 'T : equality> (x: 'T) = Unchecked.hash x

[<Inline "$x">]
let Identity (x: 'T) = X<'T>

[<Inline "void $x">]
let Ignore (x: 'T) = X<unit>

[<Direct "void ($x[0]++)">]
[<Name "WebSharper.Ref.incr">]
let Increment (x: ref<int>) = ()

[<Inline "Infinity">]
let Infinity = Unchecked.defaultof<double>

[<JavaScript>]
let InvalidOp (msg: string) : 'T = raise (System.InvalidOperationException(msg))

[<Inline "($x << 0)">]
let ToInt (x: 'T) = X<int>

[<Inline "Number($x)">]
let ToSingle (x: 'T) = X<'T>

[<Inline "($x << 0)">]
let ToInt32 (x: 'T) = X<int32>

[<Inline "$x">]
let ToEnum<'T> (x: 'T) = X<'T>

[<Inline "Math.floor($x)">]
let ToInt64 (x: 'T) = X<int64>

[<Inline "Math.log($x)">]
let Log (x: 'T) = X<'T>

[<Inline "Math.log($x)/Math.log(10)">]
let Log10 (x: 'T) = X<'T>

[<JavaScript>]
let Max<'T when 'T : comparison> (a: 'T) (b: 'T) =
    if a > b then a else b

[<JavaScript>]
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

[<JavaScript>]
let Pown<'T> (a: 'T) (n: int) =
    let a = box a :?> double
    let rec p n =
        match n with
        | 1 ->
            a
        | n when n % 2 = 0 ->
            let b = p (n / 2)
            b * b
        | n ->
            a * (p (n - 1))
    p n

[<Inline "throw $e">]
let Raise (e: exn) = X<'T>

[<Inline "[$x]">]
let Ref (x: 'T) = X<ref<'T>>

[<Inline "Math.round($x)">]
let Round (x: 'T) = X<'T>

[<Inline "$x">]
let CreateSequence (x: seq<'T>) = X<seq<'T>>

[<JavaScript>]
let Sign<'T> (x: 'T) =
    match As<int> x with
    | 0            -> 0
    | n when n < 0 -> -1
    | _            -> 1

[<Inline "Math.sin($x)">]
let Sin (x: 'T) = X<'T>

[<Inline "(Math.exp($x)-Math.exp(-$x))/2">]
let Sinh (x: 'T) = x

[<Inline "$x[1]">]
let Snd (x: System.Tuple<'T1,'T2>) = X<'T2>

[<Inline "Math.sqrt($x)">]
let Sqrt (x: 'T1) = X<'T2>

[<Macro(typeof<M.String>)>]
let ToString (x: 'T) = X<string>

[<Inline "Math.tan($x)">]
let inline Tan (x: 'T) = X<'T>

[<Inline "(Math.exp(2*$x)-1)/(Math.exp(2*$x)+1)">]
let Tanh (x: 'T) = X<'T>

[<JavaScript>]
let inline Truncate<'T> (x: 'T) =
    if x <. 0 then Ceiling x else Floor x

[<Inline "$x">]
let Unbox (x: obj) = X<'T>

[<Inline>]
[<JavaScript>]
let IsNull (x: obj) = x = null

[<JavaScript>]
let Using t f =
    try f t finally (t :> System.IDisposable).Dispose()

[<JavaScript>]
[<Name "KeyValue">]
let KeyValuePattern (kvp: System.Collections.Generic.KeyValuePair<_,_>) =
    (kvp.Key, kvp.Value)


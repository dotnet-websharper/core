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

[<IntelliFactory.WebSharper.Core.Attributes.Name "Operators">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.Operators, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.OperatorsProxy

#nowarn "86"

module M = IntelliFactory.WebSharper.Macro

[<JavaScript>]
[<Name "range">]
let ( .. ) (min: 'T) (max: 'T) : seq<'T> =
    Seq.init (1 + As max - As min) (fun x -> As (x + As min))

[<JavaScript>]
[<Name "step">]
let ( .. .. ) (min: 'T1) (step: 'T2) (max: 'T1) : seq<'T1> =
    let s = sign (As<int> step)
    Seq.initInfinite (fun k -> As<int> min + k * As<int> step)
    |> Seq.takeWhile (fun k -> s * (As<int> max - As<int> k) >= 0)
    |> As

[<Inline "$r.contents">]
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

[<Macro(typeof<M.Division>)>]
let ( / ) (x: 'T1) (y: 'T2) = X<'T3>

[<Inline "void ($a.contents = $b)">]
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

[<JavaScript>]
let Decrement (x: ref<int>) = x := !x - 1

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

[<JavaScript>]
let Increment (x: ref<int>) = x := !x + 1

[<Inline "Infinity">]
let Infinity = Unchecked.defaultof<double>

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

[<Direct "throw $e">]
let Raise (e: exn) = X<'T>

[<Inline "{contents: $x}">]
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

[<JavaScript>]
let Using t f =
    try f t finally (t :> System.IDisposable).Dispose()

[<JavaScript>]
[<Name "KeyValue">]
let KeyValuePattern (kvp: System.Collections.Generic.KeyValuePair<_,_>) =
    (kvp.Key, kvp.Value)

[<IntelliFactory.WebSharper.Core.Attributes.Name "OperatorIntrinsics">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Core.Operators+OperatorIntrinsics, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module OperatorIntrinsicsProxy =
    [<Direct "$s.slice($st,$e)">]
    let Slice (s: 'T) (st: int) (e: int) = X<'T>

    [<Direct "$s.slice($st)">]
    let SliceStart (s: 'T) (st: int) = X<'T>

    [<JavaScript>]
    let GetStringSlice (source: string) (start: int option) (finish: int option) =
        match start, finish with
        | Some s, Some f -> Slice source s (f + 1)
        | Some s, None -> SliceStart source s
        | None, Some f -> Slice source 0 (f + 1)
        | _ -> ""

    [<JavaScript>]
    let GetArraySlice<'T> (source: 'T[]) (start: int option) (finish: int option) =
        match start, finish with
        | Some s, Some f -> Slice source s (f + 1)
        | Some s, None -> SliceStart source s
        | None, Some f -> Slice source 0 (f + 1)
        | _ -> [||]

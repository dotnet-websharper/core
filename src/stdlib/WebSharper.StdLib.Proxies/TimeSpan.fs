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

namespace WebSharper

open WebSharper.JavaScript

[<Proxy(typeof<TS>)>]
[<Type "number">]
type private TimeSpanProxy =

    [<Inline "0">]
    new () = {}

    [<Inline "Number($ticks) / 1E4">]
    new (ticks: int64) = {}

    [<Inline "(($h * 60 + $m) * 60 + $s) * 1E3">]
    new (h: int, m: int, s: int) = {}

    [<Inline "(((24 * $d + $h) * 60 + $m) * 60 + $s) * 1E3">]
    new (d: int, h: int, m: int, s: int) = {}

    [<Inline "(((24 * $d + $h) * 60 + $m) * 60 + $s) * 1E3 + $ms">]
    new (d: int, h: int, m: int, s: int, ms: int) = {}

    [<Inline "$this + $t">]
    member this.Add(t: TS) = X<TS>

    [<Inline "$this - $t">]
    member this.Subtract(t: TS) = X<TS>

    [<Inline>]
    member this.CompareTo(t: TS) =
        Unchecked.compare (this :> obj) (t :> obj)

    member this.Days
        with [<Inline "Math.trunc($this / 864E5)">] get () = X<int>

    member this.Hours
        with [<Inline "Math.trunc($this / 36E5) % 24">] get () = X<int>

    member this.Minutes
        with [<Inline "Math.trunc($this / 6E4) % 60">] get () = X<int>

    member this.Seconds
        with [<Inline "Math.trunc($this / 1E3) % 60">] get () = X<int>

    member this.Milliseconds
        with [<Inline "Math.trunc($this) % 1E3">] get () = X<int>

    member this.TotalDays
        with [<Inline "$this / 864E5">] get () = X<float>

    member this.TotalHours
        with [<Inline "$this / 36E5">] get () = X<float>

    member this.TotalMinutes
        with [<Inline "$this / 6E4">] get () = X<float>

    member this.TotalSeconds
        with [<Inline "$this / 1E3">] get () = X<float>

    member this.TotalMilliseconds
        with [<Inline "$this">] get () = X<float>

    member this.Ticks
        with [<Inline "BigInt(Math.trunc($this)) * BigInt(1E4) + BigInt(($this - Math.trunc($this)) * 1E4)">] get() = X<int64>

    [<Inline "-$this">]
    member this.Negate() = X<TS>

    [<Inline "Math.abs($this)">]
    member this.Duration() = X<TS>

    [<Inline>]
    static member Compare(t1: TS, t2: TS) =
        Unchecked.compare t1 t2

    [<Inline "$t1 == $t2">]
    static member Equals(t1: TS, t2: TS) = X<bool>

    [<Inline "$days * 864E5">]
    static member FromDays(days: float) = X<TS>

    [<Inline "$days * 864E5">]
    static member FromDays(days: int) = X<TS>

    [<Inline "$hours * 36E5">]
    static member FromHours(hours: float) = X<TS>

    [<Inline "$hours * 36E5">]
    static member FromHours(hours: int) = X<TS>

    [<Inline "$min * 6E4">]
    static member FromMinutes(min: float) = X<TS>

    [<Inline "$min * 6E4">]
    static member FromMinutes(min: int) = X<TS>

    [<Inline "$sec * 1E3">]
    static member FromSeconds(sec: float) = X<TS>

    [<Inline "$sec * 1E3">]
    static member FromSeconds(sec: int) = X<TS>

    [<Inline "$msec">]
    static member FromMilliseconds(msec: float) = X<TS>

    [<Inline "$msec">]
    static member FromMilliseconds(msec: int) = X<TS>

    [<Inline "Number($ticks) / 1E4">]
    static member FromTicks(ticks: int64) = X<TS>

    static member Zero
        with [<Inline "0">] get () = X<TS>

    static member MaxValue
        with [<Inline "Number.MAX_VALUE">] get () = X<TS>

    static member MinValue
        with [<Inline "Number.MIN_VALUE">] get () = X<TS>

    [<Inline "$a">]
    static member (~+) (a: TS) = X<TS>

    [<Inline "-$a">]
    static member (~-) (a: TS) = X<TS>

    [<Inline "$a + $b">]
    static member (+) (a: TS, b: TS) = X<TS>

    [<Inline "$a - $b">]
    static member (-) (a: TS, b: TS) = X<TS>

    [<Inline "$a == $b">]
    static member op_Equality (a: TS, b: TS) = X<bool>

    [<Inline "$a != $b">]
    static member op_Inequality (a: TS, b: TS) = X<bool>

    [<Inline "$a > $b">]
    static member op_GreaterThan (a: TS, b: TS) = X<bool>

    [<Inline "$a < $b">]
    static member op_LessThan (a: TS, b: TS) = X<bool>

    [<Inline "$a >= $b">]
    static member op_GreaterThanOrEqual (a: TS, b: TS) = X<bool>

    [<Inline "$a <= $b">]
    static member op_LessThanOrEqual (a: TS, b: TS) = X<bool>

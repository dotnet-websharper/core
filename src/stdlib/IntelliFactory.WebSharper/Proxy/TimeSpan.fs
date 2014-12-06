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

namespace IntelliFactory.WebSharper

open IntelliFactory.WebSharper.JavaScript

[<Proxy(typeof<System.TimeSpan>)>]
type private TimeSpanProxy =

    [<Inline "0">]
    new () = {}

    [<Inline "(($h * 60 + $m) * 60 + $s) * 1E3">]
    new (h: int, m: int, s: int) = {}

    [<Inline "(((24 * $d + $h) * 60 + $m) * 60 + $s) * 1E3">]
    new (d: int, h: int, m: int, s: int) = {}

    [<Inline "(((24 * $d + $h) * 60 + $m) * 60 + $s) * 1E3 + $ms">]
    new (d: int, h: int, m: int, s: int, ms: int) = {}

    [<Inline "$this + $t">]
    member this.Add(t: System.TimeSpan) = X<System.TimeSpan>

    [<Inline "$this - $t">]
    member this.Subtract(t: System.TimeSpan) = X<System.TimeSpan>

    [<Inline>]
    [<JavaScript>]
    member this.CompareTo(t: System.TimeSpan) =
        Unchecked.compare (this :> obj) (t :> obj)

    member this.Days
        with [<Inline "Math.floor($this / 864E5)">] get () = X<int>

    member this.Hours
        with [<Inline "Math.floor($this / 36E5) % 24">] get () = X<int>

    member this.Minutes
        with [<Inline "Math.floor($this / 6E4) % 60">] get () = X<int>

    member this.Seconds
        with [<Inline "Math.floor($this / 1E3) % 60">] get () = X<int>

    member this.Milliseconds
        with [<Inline "$this % 1E3">] get () = X<int>

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
        with [<Inline "$this * 1E4">] get() = X<int64>

    [<Inline "-$this">]
    member this.Negate() = X<System.TimeSpan>

    [<Inline "Math.abs($this)">]
    member this.Duration() = X<System.TimeSpan>

    [<Inline>]
    [<JavaScript>]
    static member Compare(t1: System.TimeSpan, t2: System.TimeSpan) =
        Unchecked.compare t1 t2

    [<Inline "$t1 == $t2">]
    static member Equals(t1: System.TimeSpan, t2: System.TimeSpan) = X<bool>

    [<Inline "$days * 864E5">]
    static member FromDays(days: float) = X<System.TimeSpan>

    [<Inline "$hours * 36E5">]
    static member FromHours(hours: float) = X<System.TimeSpan>

    [<Inline "$min * 6E4">]
    static member FromMinutes(min: float) = X<System.TimeSpan>

    [<Inline "$sec * 1E3">]
    static member FromSeconds(sec: float) = X<System.TimeSpan>

    [<Inline "$msec">]
    static member FromMilliseconds(msec: float) = X<System.TimeSpan>

    [<Inline "$ticks / 1E4">]
    static member FromTicks(ticks: int64) = X<System.TimeSpan>

    static member Zero
        with [<Inline "0">] get () = X<System.TimeSpan>

    static member MaxValue
        with [<Inline "Number.MAX_VALUE">] get () = X<System.TimeSpan>

    static member MinValue
        with [<Inline "Number.MIN_VALUE">] get () = X<System.TimeSpan>


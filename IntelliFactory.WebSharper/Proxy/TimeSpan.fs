// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

namespace IntelliFactory.WebSharper

[<Proxy(typeof<System.TimeSpan>)>]
type private TimeSpanProxy =

    [<Inline "$ticks / 10000">]
    new (ticks: int64) = {}

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

    [<Inline "-$this">]
    member this.Negate() = X<System.TimeSpan>

    member this.Ticks
        with [<Inline "10000 * $this">] get () = X<int64>

    [<Inline "Math.abs($this)">]
    member this.Duration() = X<System.TimeSpan>

    [<Inline>]
    [<JavaScript>]
    static member Compare(t1: System.TimeSpan, t2: System.TimeSpan) =
        Unchecked.compare t1 t2

    [<Inline "$t1 == $t2">]
    static member Equals(t1: System.TimeSpan, t2: System.TimeSpan) = X<bool>

    [<Inline "$ticks / 10000">]
    static member FromTicks(ticks: int64) = X<System.TimeSpan>

    [<Inline "$days * 24 * 60 * 60 * 1000">]
    static member FromDays(days: float) = X<System.TimeSpan>

    [<Inline "$hours * 60 * 60 * 1000">]
    static member FromHours(hours: float) = X<System.TimeSpan>

    [<Inline "$min * 60 * 1000">]
    static member FromMinutes(min: float) = X<System.TimeSpan>

    [<Inline "$sec * 1000">]
    static member FromSeconds(sec: float) = X<System.TimeSpan>

    [<Inline "$msec">]
    static member FromMilliseconds(msec: float) = X<System.TimeSpan>

    static member Zero
        with [<Inline "0">] get () = X<System.TimeSpan>

    static member MaxValue
        with [<Inline "Number.MAX_VALUE">] get () = X<System.TimeSpan>

    static member MinValue
        with [<Inline "Number.MIN_VALUE">] get () = X<System.TimeSpan>


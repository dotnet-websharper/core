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

namespace IntelliFactory.WebSharper

type private D = System.DateTime
type private K = System.DateTimeKind
type private TS = System.TimeSpan

[<Proxy(typeof<System.DateTime>)>]
type private DateTimeProxy =
    [<Inline "0">]
    new () = {}

    [<Inline "new Date($y,$mo-1,$d).getTime()">]
    new (y: int, mo: int, d: int) = {}

    [<Inline "new Date($y,$mo-1,$d,$h,$m,$s).getTime()">]
    new (y: int, mo: int, d: int, h: int, m: int, s: int) = {}

    [<Inline "new Date($y,$mo-1,$d,$h,$m,$s,$ms).getTime()">]
    new (y: int, mo: int, d: int, h: int, m: int, s: int, ms: int) = {}

    static member Now
        with [<Inline "new Date().getTime()">] get() = X<D>
    
    static member UtcNow
        with [<Inline "new Date().getTime()">] get() = X<D>
    
    [<Inline "1">]
    member this.Kind = X<System.DateTimeKind>

//    [<Inline "new Date()">]
//    static member EcmaNow() = X<obj>

//    [<Inline "new Date($this)">]
//    member this.ToEcma() = X<obj>

    static member Today
        with [<Direct "var d = new Date(); return new Date(d.getFullYear(),d.getMonth(),d.getDate())">] get() =
            X<D>

    member this.Date 
        with [<Direct "var d = new Date($this); return new Date(d.getFullYear(),d.getMonth(),d.getDate())">] get() =
            X<D>

    member this.TimeOfDay 
        with [<Direct "var d = new Date($this); return new Date(0,0,0,d.getHours(),d.getMinutes(),d.getSeconds(),d.getMilliseconds())">] get() =
            X<D>

    member this.Year
        with [<Inline "new Date($this).getFullYear()">] get() = X<int>

    member this.Month 
        with [<Inline "new Date($this).getMonth()+1">] get() = X<int>

    member this.Day 
        with [<Inline "new Date($this).getDate()">] get() = X<int>

    member this.Hour 
        with [<Inline "new Date($this).getHours()">] get() = X<int>
                                                  
    member this.Minute 
        with [<Inline "new Date($this).getMinutes()">] get() = X<int>
    
    member this.Second 
        with [<Inline "new Date($this).getSeconds()">] get() = X<int>

    member this.Millisecond 
        with [<Inline "new Date($this).getMilliseconds()">] get() = X<int>
    
    member this.DayOfWeek 
        with [<Inline "new Date($this).getDay()">] get() = X<System.DayOfWeek>

    [<Inline "$this + $t">]
    member this.Add(t: System.TimeSpan) = X<D>

    [<Inline "$this - $t">]
    member this.Subtract(t: System.TimeSpan) = X<D>

    [<Inline; JavaScript>]
    member this.AddYears(years: int) : D =
        let d = this.ToEcma()
        As (DateTimeProxy(d?getFullYear() + years,
                          d?getMonth()+1,
                          d?getDate(),
                          d?getHours(),
                          d?getMinutes(),
                          d?getSeconds(),
                          d?getMilliseconds()))

    [<Inline; JavaScript>]
    member this.AddMonths(months: int) : D =
        let d = this.ToEcma()
        As (DateTimeProxy(d?getFullYear(),
                          d?getMonth()+1 + months,
                          d?getDate(),
                          d?getHours(),
                          d?getMinutes(),
                          d?getSeconds(),
                          d?getMilliseconds()))

    [<Inline; JavaScript>]
    member this.AddDays(days: float) : D =
        this.Add(TS.FromDays days)

    [<Inline; JavaScript>]
    member this.AddHours(hours: float) : D =
        this.Add(TS.FromHours hours)

    [<Inline; JavaScript>]
    member this.AddMinutes(minutes: float) : D =
        this.Add (TS.FromMinutes minutes)

    [<Inline; JavaScript>]
    member this.AddSeconds(seconds: float) : D =
        this.Add (TS.FromSeconds seconds)

    [<Inline; JavaScript>]
    member this.AddMilliseconds(msec: float) : D =
        this.Add (TS.FromMilliseconds msec)
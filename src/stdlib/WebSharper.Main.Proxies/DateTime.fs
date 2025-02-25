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
open System.Runtime.InteropServices
open System

type private D = System.DateTime
type private K = System.DateTimeKind
type internal TS = System.TimeSpan
type private DO = System.DateTimeOffset

[<JavaScript>]
[<Name "DateUtil">]
module private DateTimeHelpers =
    let DatePortion (d: int) =
        let e = Date(d)
        Date(       
            e.GetFullYear(),
            e.GetMonth(),
            e.GetDate()
        ).GetTime()
        |> As<D>        

    let TimePortion (d: int) =
        let e = Date(d)
        TS(
            0,
            e.GetHours(), 
            e.GetMinutes(), 
            e.GetSeconds(), 
            e.GetMilliseconds()
        )        
        |> As<TS>        

    let AddYears(d: int, years) =
        let e = Date(d)
        Date(   
            e.GetFullYear() + years,
            e.GetMonth(),
            e.GetDate(),
            e.GetHours(),
            e.GetMinutes(),
            e.GetSeconds(),
            e.GetMilliseconds()
        ).GetTime()
        |> As<D>        

    let AddMonths(d: int, months: int) =
        let e = Date(d)
        Date(   
            e.GetFullYear(),
            e.GetMonth() + months,
            e.GetDate(),
            e.GetHours(),
            e.GetMinutes(),
            e.GetSeconds(),
            e.GetMilliseconds()
        ).GetTime()    
        |> As<D>        

    let TryParse (s: string) =
        let d = Date.Parse(s)   
        if JS.IsNaN(d) then
            None
        else Some d

    let Parse (s: string) =
        match TryParse s with
        | Some d -> d
        | _ ->
            failwith "Failed to parse date string."

    [<Direct "(new Date($d)).toLocaleDateString({}, {year: 'numeric', month: 'long', day: 'numeric', weekday: 'long'})">]
    let LongDate (d: obj) = X<string>
     
    [<Direct "(new Date($d)).toLocaleTimeString({}, {hour: '2-digit', minute: '2-digit', hour12: false})">]
    let ShortTime (d: obj) = X<string>

    [<Direct "(new Date($d)).toLocaleTimeString({}, {hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false})">]
    let LongTime (d: obj) = X<string>

    [<Direct "var d = new Date($y, $mo - 1, $d, $h, $m, $s, $ms); if ($y < 99) d.setFullYear($y); return d.getTime()">]
    let Create(y: int, mo: int, d: int, h: int, m: int, s: int, ms: int) = X<DateTime>
                
// DateTime is represented as an UTC epoch for remoting purposes.
// Properties for getting sub-dates/times like Day or Hour convert it to local time on the client for easier display purposes.
// This is inconsistent, but covers most common uses.
// If you need UTC time details, use .JS and its UTC methods.
[<Proxy(typeof<System.DateTime>)>]
type private DateTimeProxy =
    [<Inline "0">]
    new () = {}

    [<Inline; Pure>]
    static member CtorProxy (y: int, mo: int, d: int) = 
        DateTimeHelpers.Create(y, mo, d, 0, 0, 0, 0)

    [<Inline; Pure>]
    static member CtorProxy (y: int, mo: int, d: int, h: int, m: int, s: int) = 
        DateTimeHelpers.Create(y, mo, d, h, m, s, 0)

    [<Inline; Pure>]
    static member CtorProxy (y: int, mo: int, d: int, h: int, m: int, s: int, ms: int) = 
        DateTimeHelpers.Create(y, mo, d, h, m, s, ms)
    
    static member Now
        with [<Inline "Date.now()">] get() = X<D>

    static member UtcNow
        with [<Inline "Date.now()">] get() = X<D>
    
    [<Inline "1">]
    member this.Kind = X<System.DateTimeKind>

    [<Macro(typeof<Core.Macros.DateString>)>]
    member this.ToString(format: string) = Date(As<int> this).ToString()

    member this.Date 
        with [<Inline; JavaScript>] get() : D = DateTimeHelpers.DatePortion(As this)

    static member Today
        with [<Inline; JavaScript>] get() = DateTimeProxy.Now.Date  

    member this.TimeOfDay 
        with [<Inline; JavaScript>] get() = DateTimeHelpers.TimePortion(As this)

    member this.Year
        with [<Inline; JavaScript>] get() = Date(As<int> this).GetFullYear()

    member this.Month 
        with [<Inline; JavaScript>] get() = Date(As<int> this).GetMonth() + 1

    member this.Day 
        with [<Inline; JavaScript>] get() = Date(As<int> this).GetDate()

    member this.Hour 
        with [<Inline; JavaScript>] get() = Date(As<int> this).GetHours()
                                                  
    member this.Minute 
        with [<Inline; JavaScript>] get() = Date(As<int> this).GetMinutes()
    
    member this.Second 
        with [<Inline; JavaScript>] get() = Date(As<int> this).GetSeconds()

    member this.Millisecond 
        with [<Inline; JavaScript>] get() = Date(As<int> this).GetMilliseconds()
    
    member this.DayOfWeek 
        with [<Inline; JavaScript>] get() = As<System.DayOfWeek>(Date(As<int> this).GetDay())

    member this.Ticks
        with [<Inline "BigInt(Math.trunc($this)) * BigInt(1E4) + BigInt(($this - Math.trunc($this)) * 1E4)">] get() = X<int64>

    [<Inline "$this + $t">]
    member this.Add(t: TS) = X<D>
                            
    [<Inline "$this - $t">]
    member this.Subtract(t: TS) = X<D>

    [<Inline "$this - $d">]
    member this.Subtract(d: D) = X<TS>

    [<Inline; JavaScript>]
    member this.AddYears(years: int) : D = DateTimeHelpers.AddYears(As this, years)

    [<Inline; JavaScript>]
    member this.AddMonths(months: int) : D = DateTimeHelpers.AddMonths(As this, months)

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

    [<Inline; JavaScript>]
    member this.AddTicks(ticks: int64) : D =
        this.Add (TS.FromTicks ticks)

    [<Inline "new Date($this).toLocaleDateString()">]
    member this.ToShortDateString() = X<string>
    
    [<Inline; JavaScript>]
    member this.ToLongDateString() = DateTimeHelpers.LongDate(this)
    
    [<Inline; JavaScript>]
    member this.ToShortTimeString() = DateTimeHelpers.ShortTime(this)

    [<Inline; JavaScript>]
    member this.ToLongTimeString() = DateTimeHelpers.LongTime(this)

    [<Inline; JavaScript>]
    static member Parse(s) = As<D>(DateTimeHelpers.Parse(s))

    [<Inline; JavaScript>]
    static member TryParse(s, [<Out>] res: byref<D>) =
        match DateTimeHelpers.TryParse s with
        | Some d ->
            res <- As<D> d   
            true
        | _ -> false    

    static member MaxValue
        with [<Inline "8640000000000000">] get () = X<int>

    static member MinValue
        with [<Inline "-8640000000000000">] get () = X<int>

    [<Inline "$a + $b">]
    static member (+) (a: D, b: TS) = X<D>

    [<Inline "$a - $b">]
    static member (-) (a: D, b: TS) = X<D>

    [<Inline "$a - $b">]
    static member (-) (a: D, b: D) = X<TS>

    [<Inline "$a == $b">]
    static member op_Equality (a: D, b: D) = X<bool>

    [<Inline "$a != $b">]
    static member op_Inequality (a: D, b: D) = X<bool>

    [<Inline "$a > $b">]
    static member op_GreaterThan (a: D, b: D) = X<bool>

    [<Inline "$a < $b">]
    static member op_LessThan (a: D, b: D) = X<bool>

    [<Inline "$a >= $b">]
    static member op_GreaterThanOrEqual (a: D, b: D) = X<bool>

    [<Inline "$a <= $b">]
    static member op_LessThanOrEqual (a: D, b: D) = X<bool>

    [<Inline "new Date($y, $mo, 0).getDate()">]
    static member DaysInMonth (y: int, mo: int) = X<int>

    [<Inline "new Date($y, 1, 29).getDate() == 29">]
    static member IsLeapYear (y: int) = X<bool>

[<Proxy(typeof<System.DateTimeOffset>)>]
[<Prototype false>]
[<Name "DateTimeOffset">]
// "d" contains UTC epoch time
// "o" contains time zone offset in minutes
type private DateTimeOffsetProxy [<Inline "{d: $d, o: $o}">] (d: D, o: int) =

    [<Inline>]
    new (d: D, o: TS) = DateTimeOffsetProxy(d, int o.TotalMinutes) 

    [<Inline>]
    new (d: D) = DateTimeOffsetProxy(d, 0) 

    member this.DateTime = d

    [<Inline "$this.o * 60000">]
    member this.Offset = X<TS>

    [<Direct "var d = new Date(); return { d: d.getTime(), o: -d.getTimezoneOffset() } ">]
    static member Now = X<DO>

    [<Inline "{ d: Date.now(), o: 0 }">]
    static member UtcNow = X<DO>
        
    [<Inline>]
    member this.ToLocalTime() =
        DO(d, As<TS>(Date().GetTimezoneOffset()))
        
    [<Inline>]
    member this.ToUniversalTime() =
        DO(d, TS.Zero)

    [<Inline>]
    member this.UtcDateTime = d

    [<Inline>]
    member this.TimeOfDay = d.TimeOfDay

    [<Inline>]
    member this.Year = d.Year

    [<Inline>]
    member this.Month = d.Month

    [<Inline>]
    member this.Day = d.Day

    [<Inline>]
    member this.Hour = d.Hour
                                                  
    [<Inline>]
    member this.Minute = d.Minute
    
    [<Inline>]
    member this.Second = d.Second

    [<Inline>]
    member this.Millisecond = d.Millisecond
    
    [<Inline>]
    member this.DayOfWeek = d.DayOfWeek

    [<Inline>]
    member this.Ticks = d.Ticks

    [<Inline>]
    member this.Add(t: TS) = DateTimeOffsetProxy(d.Add(t), o)
                            
    [<Inline>]
    member this.Subtract(t: TS) = DateTimeOffsetProxy(d.Subtract(t), o)

    [<Inline>]
    member this.Subtract(o: DO) = d.Subtract(o?d: D)

    [<Inline>]
    member this.AddYears(years: int) = DateTimeOffsetProxy(d.AddYears(years), o)

    [<Inline>]
    member this.AddMonths(months: int) = DateTimeOffsetProxy(d.AddMonths(months), o)

    [<Inline>]
    member this.AddDays(days: float) = DateTimeOffsetProxy(d.AddDays(days), o)

    [<Inline>]
    member this.AddHours(hours: float) = DateTimeOffsetProxy(d.AddHours(hours), o)

    [<Inline>]
    member this.AddMinutes(minutes: float) = DateTimeOffsetProxy(d.AddMinutes(minutes), o)

    [<Inline>]
    member this.AddSeconds(seconds: float) = DateTimeOffsetProxy(d.AddSeconds(seconds), o)

    [<Inline>]
    member this.AddMilliseconds(msec: float) = DateTimeOffsetProxy(d.AddMilliseconds(msec), o)

    [<Inline>]
    member this.AddTicks(ticks: int64) = DateTimeOffsetProxy(d.AddTicks(ticks), o)

    [<Inline>]
    static member (+) (a: DO, b: TS) = a.Add(b)

    [<Inline>]
    static member (-) (a: DO, b: TS) = a.Subtract(b)

    [<Inline>]
    static member (-) (a: DO, b: DO) = a.Subtract(b)

    [<Inline "$a.d == $b.d">]
    static member op_Equality (a: DO, b: DO) = X<bool>

    [<Inline "$a.d != $b.d">]
    static member op_Inequality (a: DO, b: DO) = X<bool>

    [<Inline "$a.d > $b.d">]
    static member op_GreaterThan (a: DO, b: DO) = X<bool>

    [<Inline "$a.d < $b.d">]
    static member op_LessThan (a: DO, b: DO) = X<bool>

    [<Inline "$a.d >= $b.d">]
    static member op_GreaterThanOrEqual (a: DO, b: DO) = X<bool>

    [<Inline "$a.d <= $b.d">]
    static member op_LessThanOrEqual (a: DO, b: DO) = X<bool>

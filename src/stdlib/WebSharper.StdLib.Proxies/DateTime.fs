// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
        // Feb 29 + 1 year = Feb 28, like .NET
        let daysInTarget = Date(e.GetFullYear() + years, e.GetMonth() + 1, 0).GetDate()
        Date(   
            e.GetFullYear() + years,
            e.GetMonth(),
            min (e.GetDate()) daysInTarget,
            e.GetHours(),
            e.GetMinutes(),
            e.GetSeconds(),
            e.GetMilliseconds()
        ).GetTime()
        |> As<D>        

    let AddMonths(d: int, months: int) =
        let e = Date(d)
        // like .NET, the day is clamped to the length of the target month (Jan 31 + 1 month = Feb 28/29)
        let daysInTarget = Date(e.GetFullYear(), e.GetMonth() + months + 1, 0).GetDate()
        Date(   
            e.GetFullYear(),
            e.GetMonth() + months,
            min (e.GetDate()) daysInTarget,
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
    let CreateUnchecked(y: int, mo: int, d: int, h: int, m: int, s: int, ms: int) = X<DateTime>

    /// The constructor throws for components outside the calendar (2023-02-29, month 13, hour 24), as in .NET,
    /// instead of rolling over into the next month or day.
    let Validate(y: int, mo: int, d: int, h: int, m: int, s: int, ms: int) =
        let leap = y % 4 = 0 && (y % 100 <> 0 || y % 400 = 0)
        let days =
            match mo with
            | 2 -> if leap then 29 else 28
            | 4 | 6 | 9 | 11 -> 30
            | _ -> 31
        if y < 1 || y > 9999 || mo < 1 || mo > 12 || d < 1 || d > days then
            raise (System.ArgumentOutOfRangeException(null, "Year, Month, and Day parameters describe an un-representable DateTime."))
        if h < 0 || h > 23 || m < 0 || m > 59 || s < 0 || s > 59 then
            raise (System.ArgumentOutOfRangeException(null, "Hour, Minute, and Second parameters describe an un-representable DateTime."))
        if ms < 0 || ms > 999 then
            raise (System.ArgumentOutOfRangeException("millisecond", "Valid values are between 0 and 999, inclusive."))

    let Create(y: int, mo: int, d: int, h: int, m: int, s: int, ms: int) =
        Validate(y, mo, d, h, m, s, ms)
        CreateUnchecked(y, mo, d, h, m, s, ms)

    /// .NET ticks count 100 ns intervals of the wall-clock time from 0001-01-01; the value here is a UTC epoch of
    /// the local time, so shift by the local offset first.
    [<Direct "var t = $d - new Date($d).getTimezoneOffset() * 60000 + 62135596800000; return BigInt(Math.trunc(t)) * 10000n + BigInt(Math.round((t - Math.trunc(t)) * 1E4))">]
    let Ticks (d: DateTime) = X<int64>
                
// DateTime is represented as an UTC epoch for remoting purposes.
// Properties for getting sub-dates/times like Day or Hour convert it to local time on the client for easier display purposes.
// This is inconsistent, but covers most common uses.
// If you need UTC time details, use .JS and its UTC methods.
[<Proxy(typeof<System.DateTime>)>]
[<Type "number">]
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

    /// DateTimeKind.Utc: the components are UTC; otherwise local, like the constructor without a kind
    [<Inline; Pure>]
    static member CtorProxy (y: int, mo: int, d: int, h: int, m: int, s: int, kind: System.DateTimeKind) = 
        if kind = System.DateTimeKind.Utc then
            DateTimeHelpers.Validate(y, mo, d, h, m, s, 0)
            As<D> (JS.Inline("(function (d) { if ($0 < 100) d.setUTCFullYear($0); return d.getTime(); })(new Date(Date.UTC($0, $1 - 1, $2, $3, $4, $5)))", y, mo, d, h, m, s))
        else DateTimeHelpers.Create(y, mo, d, h, m, s, 0)
    
    static member Now
        with [<Inline "Date.now()">] get() = X<D>

    static member UtcNow
        with [<Inline "Date.now()">] get() = X<D>
    
    [<Inline "1">]
    member this.Kind = X<System.DateTimeKind>

    [<Inline>]
    member this.ToString(format: string) = DateTime.DateFormatter (As this) format

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

    member this.DayOfYear
        with [<Inline "(d => Math.round((Date.UTC(d.getFullYear(), d.getMonth(), d.getDate()) - Date.UTC(d.getFullYear(), 0, 1)) / 864e5) + 1)(new Date($this))">] get() = X<int>

    member this.Ticks
        with [<Inline; JavaScript>] get() = DateTimeHelpers.Ticks(As<DateTime> this)

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

    /// 9999-12-31 23:59:59.999 local time (the server sends DateTime.MaxValue as that local time too)
    static member MaxValue
        with [<Inline; JavaScript>] get () = DateTimeHelpers.CreateUnchecked(9999, 12, 31, 23, 59, 59, 999)

    /// 0001-01-01 00:00 local time, as the server sends DateTime.MinValue
    static member MinValue
        with [<Inline; JavaScript>] get () = DateTimeHelpers.CreateUnchecked(1, 1, 1, 0, 0, 0, 0)

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
[<Type "{d: number, o: number}">]
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

/// System.DateOnly: like a DateTime, the milliseconds of local midnight of that day
[<Proxy(typeof<System.DateOnly>)>]
[<Type "number">]
type private DateOnlyProxy =

    [<Inline "new Date($y, $m - 1, $d).getTime()">]
    new (y: int, m: int, d: int) = {}

    [<Inline>]
    static member FromDateTime(dt: D) : System.DateOnly = As (DateTimeHelpers.DatePortion(As dt))

    member this.Year with [<Inline; JavaScript>] get() = (As<D> this).Year
    member this.Month with [<Inline; JavaScript>] get() = (As<D> this).Month
    member this.Day with [<Inline; JavaScript>] get() = (As<D> this).Day
    member this.DayOfWeek with [<Inline; JavaScript>] get() = (As<D> this).DayOfWeek
    member this.DayOfYear with [<Inline; JavaScript>] get() = (As<D> this).DayOfYear
    // by calendar day, so a DST change doesn't move the time of day
    [<Inline "(d => new Date(d.getFullYear(), d.getMonth(), d.getDate() + $days).getTime())(new Date($this))">]
    member this.AddDays(days: int) = X<System.DateOnly>

    [<Inline>]
    member this.AddMonths(months: int) : System.DateOnly = As ((As<D> this).AddMonths months)

    [<Inline>]
    member this.AddYears(years: int) : System.DateOnly = As ((As<D> this).AddYears years)

    [<Inline>]
    override this.ToString() : string = WebSharper.Formatting.FormatDateOnly (As this)

    [<Inline>]
    member this.ToString(format: string) : string = (As<D> this).ToString(format)

    [<Inline "$a == $b">]
    static member op_Equality (a: System.DateOnly, b: System.DateOnly) = X<bool>
    [<Inline "$a != $b">]
    static member op_Inequality (a: System.DateOnly, b: System.DateOnly) = X<bool>
    [<Inline "$a < $b">]
    static member op_LessThan (a: System.DateOnly, b: System.DateOnly) = X<bool>
    [<Inline "$a > $b">]
    static member op_GreaterThan (a: System.DateOnly, b: System.DateOnly) = X<bool>
    [<Inline "$a <= $b">]
    static member op_LessThanOrEqual (a: System.DateOnly, b: System.DateOnly) = X<bool>
    [<Inline "$a >= $b">]
    static member op_GreaterThanOrEqual (a: System.DateOnly, b: System.DateOnly) = X<bool>

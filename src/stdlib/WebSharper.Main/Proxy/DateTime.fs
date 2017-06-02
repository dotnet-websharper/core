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
open System.Runtime.InteropServices

type private D = System.DateTime
type private K = System.DateTimeKind
type private TS = System.TimeSpan
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
        with [<Inline "Date.now()">] get() = X<D>

    static member UtcNow
        with [<Inline "Date.now()">] get() = X<D>
    
    [<Inline "1">]
    member this.Kind = X<System.DateTimeKind>

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
        with [<Inline "$this * 1E4">] get() = X<int64>

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

[<Proxy(typeof<System.DateTimeOffset>)>]
[<Name "DateTimeOffset">]
type private DateTimeOffsetProxy(d: int, o: int) =

    [<Inline>]
    new (d: D) = DateTimeOffsetProxy(As<int> d, 0) 

    [<Inline>]
    member this.DateTime = d |> As<D>

//    static member Now = X<D>

//    static member UtcNow = DateTimeOffsetProxy(DateTime, o: int)
        
    member this.ToLocalTime() =
        DateTimeOffsetProxy(d, As<int>(Date().GetTimezoneOffset())) |> As<DO>   
        
    member this.ToUniversalTime() =
        DateTimeOffsetProxy(d, 0) |> As<DO>

    [<Inline>]
    member this.UtcDateTime = this.ToUniversalTime().DateTime

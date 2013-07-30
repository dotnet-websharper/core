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
module J = IntelliFactory.WebSharper.JavaScript

module private DateTimeHelpers =
    [<Inline "new Date($y,$mo-1,$d,$h,$m,$s,$ms).getTime()">]
    let dateToEpoch (y: int) (mo: int) (d: int) (h: int) (m: int) (s: int) (ms: int) = X<float>

    [<Inline "new Date().getTime()">]
    let nowEpoch() = X<float> 
       
[<Name "DateTime">]
[<Proxy(typeof<System.DateTime>)>]
type private DateTimeProxy = //[<JavaScript>] (epoch: float) =
    [<Inline "0">]
    new () = {}

    [<Inline "new Date($y,$mo-1,$d).getTime()">]
    new (y: int, mo: int, d: int) = {}

    [<Inline "new Date($y,$mo-1,$d,$h,$m,$s).getTime()">]
    new (y: int, mo: int, d: int, h: int, m: int, s: int) = {}

    [<Inline "new Date($y,$mo-1,$d,$h,$m,$s,$ms).getTime()">]
    new (y: int, mo: int, d: int, h: int, m: int, s: int, ms: int) = {}

//    [<Name "toString">]
//    [<Inline "new Date($0.epoch).toString()">]
//    member this.Display() = X<string>

    [<Inline "new Date().getTime()">]
    static member Now = X<D>

    [<Inline "new Date().getTime()">]
    static member UtcNow = X<D>

    [<JavaScript>]
    static member Today : D = D.Now.Date

    [<Inline "1">]
    member this.Kind = X<System.DateTimeKind>

    [<JavaScript>]
    member this.Date : D =
        As (DateTimeProxy(this.Year, this.Month, this.Day, 0, 0, 0))

    [<JavaScript>]
    member this.TimeOfDay =
        TS(0, this.Hour, this.Minute, this.Second, this.Millisecond)

    [<Inline "Date($0.epoch).getFullYear()">]
    member this.Year = X<int>

    [<Inline "Date($0.epoch).getMonth()+1">]
    member this.Month = X<int>

    [<Inline "Date($0.epoch).getDate()">]
    member this.Day = X<int>

    [<Inline "Date($0.epoch).getHours()">]
    member this.Hour = X<int>

    [<Inline "Date($0.epoch).getMinutes()">]
    member this.Minute = X<int>

    [<Inline "Date($0.epoch).getSeconds()">]
    member this.Second = X<int>

    [<Inline "Date($0.epoch).getMilliseconds()">]
    member this.Millisecond = X<int>

    [<Inline "Date($0.epoch).getDay()">]
    member this.DayOfWeek = X<System.DayOfWeek>

    [<Inline "$this + $t">]
    member this.Add(t: System.TimeSpan) = X<D>

    [<Inline "$this - $t">]
    member this.Subtract(t: System.TimeSpan) = X<D>

    [<JavaScript>]
    member this.AddYears(years: int) : D =
        As (DateTimeProxy(this.Year + years,
                          this.Month,
                          this.Day,
                          this.Hour,
                          this.Minute,
                          this.Second,
                          this.Millisecond))

    [<JavaScript>]
    member this.AddMonths(months: int) : D =
        As (DateTimeProxy(this.Year,
                          this.Month + months,
                          this.Day,
                          this.Hour,
                          this.Minute,
                          this.Second,
                          this.Millisecond))

    [<JavaScript>]
    member this.AddDays(days: float) : D =
        this.Add(TS.FromDays days)

    [<JavaScript>]
    member this.AddHours(hours: float) : D =
        this.Add(TS.FromHours hours)

    [<JavaScript>]
    member this.AddMinutes(minutes: float) : D =
        this.Add (TS.FromMinutes minutes)

    [<JavaScript>]
    member this.AddSeconds(seconds: float) : D =
        this.Add (TS.FromSeconds seconds)

    [<JavaScript>]
    member this.AddMilliseconds(msec: float) : D =
        this.Add (TS.FromMilliseconds msec)
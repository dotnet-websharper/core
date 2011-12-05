// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

type private D  = System.DateTime
type private K  = System.DateTimeKind
type private TS = System.TimeSpan
module J        = IntelliFactory.WebSharper.JavaScript

[<Name "DateTime">]
[<Proxy(typeof<System.DateTime>)>]
type private DateTimeProxy [<JavaScript>] (epoch: int, kind: K) =

    [<JavaScript>]
    new () = DateTimeProxy(1, 1, 1)

    [<JavaScript>]
    new (ticks: int64) = DateTimeProxy(ticks, K.Unspecified)

    [<JavaScript>]
    new (ticks: int64, k: K) =
        DateTimeProxy(As<int> (ticks / TS.TicksPerMillisecond), k)

    [<JavaScript>]
    new (y: int, mo: int, d: int) =
        DateTimeProxy(y, mo, d, 0, 0, 0, 0, K.Unspecified)

    [<JavaScript>]
    new (y: int, mo: int, d: int, h: int, m: int, s: int) =
        DateTimeProxy(y, mo, d, h, m, s, 0, K.Unspecified)

    [<JavaScript>]
    new (y: int, mo: int, d: int, h: int, m: int, s: int, k: K) =
        DateTimeProxy(y, mo, d, h, m, s, 0, k)

    [<JavaScript>]
    new (y: int, mo: int, d: int, h: int, m: int, s: int, ms: int) =
        DateTimeProxy(y, mo, d, h, m, s, ms, K.Unspecified)

    [<JavaScript>]
    new (y: int, mo: int, d: int, h: int, m: int, s: int, ms: int, k: K) =
        DateTimeProxy((DateTimeProxy.TimeAt y mo d h m s ms : int), k)

    [<Inline "new Date($y,$mo-1,$d,$h,$m,$s,$ms).getTime()">]
    static member TimeAt (y: int) (mo: int) (d: int)
                         (h: int) (m: int) (s: int) (ms: int) = X<int>

    [<Inline "new Date().getTime()">]
    static member TimeNow() = X<int>

    [<JavaScript>]
    static member Now : D =
        As (DateTimeProxy(DateTimeProxy.TimeNow(), K.Local))

    [<JavaScript>]
    static member UtcNow : D =
        As (DateTimeProxy(DateTimeProxy.TimeNow(), K.Utc))

    [<JavaScript>]
    static member Today : D =
        System.DateTime.Now.Date

    [<JavaScript>]
    member this.Kind : K = kind

    [<JavaScript>]
    member this.Date
        with get () : D =
            As (DateTimeProxy(this.Year, this.Month, this.Day, 0, 0, 0))

    [<JavaScript>]
    member this.TimeOfDay
        with get () =
            TS(0, this.Hour, this.Minute,
                  this.Second, this.Millisecond)

    [<Inline "new Date($epoch)">]
    static member DateByEpoch(epoch: int) = X<obj>

    [<JavaScript>]
    static member GetItem (this: DateTimeProxy) (key: string) : int =
        let kind    = this?kind
        let pfx     = "get" + if kind = K.Utc then "UTC" else ""
        let date    = DateTimeProxy.DateByEpoch(this?epoch)
        J.Apply<int> date (pfx + key) [||]

    [<JavaScript>]
    member this.Year = DateTimeProxy.GetItem this "FullYear"

    [<JavaScript>]
    member this.Month = 1 + DateTimeProxy.GetItem this "Month"

    [<JavaScript>]
    member this.Day = DateTimeProxy.GetItem this "Date"

    [<JavaScript>]
    member this.Hour = DateTimeProxy.GetItem this "Hours"

    [<JavaScript>]
    member this.Minute = DateTimeProxy.GetItem this "Minutes"

    [<JavaScript>]
    member this.Second = DateTimeProxy.GetItem this "Seconds"

    [<JavaScript>]
    member this.Millisecond = DateTimeProxy.GetItem this "Milliseconds"

    [<JavaScript>]
    member this.Ticks = As<int64> (this.Millisecond * 10000)

    [<JavaScript>]
    member this.DayOfWeek : System.DayOfWeek =
        As (DateTimeProxy.GetItem this "Day")

    [<JavaScript>]
    member this.Add(t: System.TimeSpan) : D =
        As (DateTimeProxy(epoch + As t.TotalMilliseconds, kind))

    [<JavaScript>]
    member this.Subtract(t: System.TimeSpan) : D =
        As (DateTimeProxy(epoch - As t.TotalMilliseconds, kind))

    [<JavaScript>]
    member this.AddYears(years: int) : D =
        As (DateTimeProxy(this.Year + years,
                          this.Month,
                          this.Day,
                          this.Hour,
                          this.Minute,
                          this.Second,
                          this.Millisecond,
                          this.Kind))

    [<JavaScript>]
    member this.AddMonths(months: int) : D =
        As (DateTimeProxy(this.Year,
                          this.Month + months,
                          this.Day,
                          this.Hour,
                          this.Minute,
                          this.Second,
                          this.Millisecond,
                          this.Kind))

    [<JavaScript>]
    static member SpecifyKind(d: D, k: K) : D =
        As (DateTimeProxy((d?epoch: int), k))

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
        this.Add(TS.FromMilliseconds msec)

    [<JavaScript>]
    member this.AddTicks(ticks: int64) : D =
        this.Add (TS.FromTicks ticks)

    [<JavaScript>]
    member this.ToUniversalTime() : D =
        D.SpecifyKind(As this, K.Utc)

    [<JavaScript>]
    member this.ToLocalTime() : D =
        D.SpecifyKind(As this, K.Local)


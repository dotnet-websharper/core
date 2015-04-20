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

module WebSharper.Tests.DateTime

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

type private DayOfWeek    = System.DayOfWeek
type private DateTime     = System.DateTime
type private DateTimeKind = System.DateTimeKind
type private TimeSpan     = System.TimeSpan

[<JavaScript>]
let Tests =

    Section "DateTime"

    let d = DateTime(2010, 4, 8, 15, 5, 39)

    Test "Year" {
        Equal d.Year 2010
    }

    Test "Month" {
        Equal d.Month 4
    }

    Test "Day" {
        Equal d.Day 8
    }

    Test "Hour" {
        Equal d.Hour 15
    }

    Test "Minute" {
        Equal d.Minute 5
    }

    Test "Second" {
        Equal d.Second 39
    }

    Test "Millisecond" {
        Equal d.Millisecond 0
    }

    Test "DayOfWeek" {
        Equal d.DayOfWeek DayOfWeek.Thursday
    }

    Test "Add" {
        Equal (d.Add(TimeSpan(3, 4, 15)))
            (DateTime(2010, 4, 8, 18, 9, 54))
        Equal (d + TimeSpan(3, 4, 15))
            (DateTime(2010, 4, 8, 18, 9, 54))
    }

    Test "Subtract" {
        Equal (d.Subtract(TimeSpan(3, 4, 15)))
            (DateTime(2010, 4, 8, 12, 1, 24))
        Equal (d.Subtract(DateTime(2010, 4, 8, 12, 1, 24)))
            (TimeSpan(3, 4, 15))
        Equal (d - TimeSpan(3, 4, 15))
            (DateTime(2010, 4, 8, 12, 1, 24))
        Equal (d - DateTime(2010, 4, 8, 12, 1, 24))
            (TimeSpan(3, 4, 15))
    }

    Test "AddYears" {
        Equal (d.AddYears 24) (DateTime(2034, 4, 8, 15, 5, 39))
    }

    Test "AddMonths" {
        Equal (d.AddMonths 9) (DateTime(2011, 1, 8, 15, 5, 39))
    }

    Test "AddDays" {
        Equal (d.AddDays 2.3) (DateTime(2010, 4, 10, 22, 17, 39))
    }

    Test "AddHours" {
        Equal (d.AddHours 1.05) (DateTime(2010, 4, 8, 16, 08, 39))
    }

    Test "AddMinutes" {
        Equal (d.AddMinutes 141.23) (DateTime(2010, 4, 8, 17, 26, 52, 800))
    }

    Test "AddSeconds" {
        Equal (d.AddSeconds 234.15) (DateTime(2010, 4, 8, 15, 09, 33, 150))
    }

    Test "AddMilliseconds" {
        Equal (d.AddMilliseconds 123.) (DateTime(2010, 4, 8, 15, 05, 39, 123))
    }

//    Test "AddTicks" {
//        d.AddTicks 1230000L =? DateTime(2010, 4, 8, 15, 05, 39, 123)
//    }

    Test "Date" {
        ApproxEqual (As<float> d.Date) (As<float> (DateTime(2010, 4, 8)))
    }

    Test "TimeOfDay" {
        ApproxEqual (As<float> d.TimeOfDay) (As<float> (TimeSpan(15, 5, 39)))
    }

//    Test "Kind" {
//        DateTime.Now.Kind =? DateTimeKind.Local
//        DateTime.UtcNow.Kind =? DateTimeKind.Utc
//        d.Kind =? DateTimeKind.Unspecified
//    }

//    Test "SpecifyKind" {
//        DateTime.SpecifyKind(d, DateTimeKind.Utc).Kind =? DateTimeKind.Utc
//        DateTime.SpecifyKind(d, DateTimeKind.Local).Kind =? DateTimeKind.Local
//    }

//    Test "ToLocalTime" {
//        DateTime.UtcNow.ToLocalTime() =? DateTime.Now
//    }
//
//    Test "ToUtcTime" {
//        DateTime.Now.ToUniversalTime() =? DateTime.UtcNow
//    }

    Test "Comparison" {
        let d1 = DateTime(2010, 4, 8, 15, 5, 39)
        let d2 = DateTime(2010, 4, 8, 15, 5, 40)
        Equal (compare d d1) 0
        Equal (compare d d2) -1
        Equal (compare d2 d) 1
        True (d1 < d2)
        True (d <= d1)
    }

    Test "Equality" {
        let d1 = DateTime(2010, 4, 8, 15, 5, 39)
        let d2 = DateTime(2010, 4, 8, 15, 5, 42)
        Equal d d1
        NotEqual d d2
        Equal (d.GetHashCode()) (d1.GetHashCode())
        NotEqual (d.GetHashCode()) (d2.GetHashCode())
    }

//    Test "Now" {
//        DateTime.Now.Kind =? DateTimeKind.Local
//    }
//
//    Test "UtcNow" {
//        DateTime.UtcNow.Kind =? DateTimeKind.Utc
//    }

    Test "Today" {
        Equal DateTime.Today DateTime.Now.Date
    }

    Section "Native Dates"

    Test "Turnaround" {
        ApproxEqual (As<float> d.JS.Self) (As<float> d)
    }

    Test "Equality" {
        let a = DateTime.Now.JS
        let b = DateTime.Now.JS
        Equal a b
        let c = DateTime.Now.AddDays(1.).JS
        NotEqual a c
    }

    Test "Comparison" {
        let a = DateTime.Now.JS
        let b = DateTime.Now.AddDays(1.).JS
        True (Unchecked.compare a b < 0)
        True (Unchecked.compare b a > 0)
    }

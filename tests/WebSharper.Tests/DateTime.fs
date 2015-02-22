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

    let ( =?~ ) (a: obj) (b: obj) = abs (As<float> a - As<float> b) < 0.0001 =? true

    let d = DateTime(2010, 4, 8, 15, 5, 39)

    Test "Year" {
        d.Year =? 2010
    }

    Test "Month" {
        d.Month =? 4
    }

    Test "Day" {
        d.Day =? 8
    }

    Test "Hour" {
        d.Hour =? 15
    }

    Test "Minute" {
        d.Minute =? 5
    }

    Test "Second" {
        d.Second =? 39
    }

    Test "Millisecond" {
        d.Millisecond =? 0
    }

    Test "DayOfWeek" {
        d.DayOfWeek =? DayOfWeek.Thursday
    }

    Test "Add" {
        d.Add(TimeSpan(3, 4, 15)) =?
            DateTime(2010, 4, 8, 18, 9, 54)
        d + TimeSpan(3, 4, 15) =?
            DateTime(2010, 4, 8, 18, 9, 54)
    }

    Test "Subtract" {
        d.Subtract(TimeSpan(3, 4, 15)) =?
            DateTime(2010, 4, 8, 12, 1, 24)
        d.Subtract(DateTime(2010, 4, 8, 12, 1, 24)) =?
            TimeSpan(3, 4, 15)
        d - TimeSpan(3, 4, 15) =?
            DateTime(2010, 4, 8, 12, 1, 24)
        d - DateTime(2010, 4, 8, 12, 1, 24) =?
            TimeSpan(3, 4, 15)
    }

    Test "AddYears" {
        d.AddYears 24 =? DateTime(2034, 4, 8, 15, 5, 39)
    }

    Test "AddMonths" {
        d.AddMonths 9 =? DateTime(2011, 1, 8, 15, 5, 39)
    }

    Test "AddDays" {
        d.AddDays 2.3 =? DateTime(2010, 4, 10, 22, 17, 39)
    }

    Test "AddHours" {
        d.AddHours 1.05 =? DateTime(2010, 4, 8, 16, 08, 39)
    }

    Test "AddMinutes" {
        d.AddMinutes 141.23 =? DateTime(2010, 4, 8, 17, 26, 52, 800)
    }

    Test "AddSeconds" {
        d.AddSeconds 234.15 =? DateTime(2010, 4, 8, 15, 09, 33, 150)
    }

    Test "AddMilliseconds" {
        d.AddMilliseconds 123. =? DateTime(2010, 4, 8, 15, 05, 39, 123)
    }

//    Test "AddTicks" {
//        d.AddTicks 1230000L =? DateTime(2010, 4, 8, 15, 05, 39, 123)
//    }

    Test "Date" {
        d.Date =?~ DateTime(2010, 4, 8)
    }

    Test "TimeOfDay" {
        d.TimeOfDay =?~ TimeSpan(15, 5, 39)
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
        compare d d1 =? 0
        compare d d2 =? -1
        compare d2 d =? 1
        d1 < d2 =? true
        d <= d1 =? true
    }

    Test "Equality" {
        let d1 = DateTime(2010, 4, 8, 15, 5, 39)
        let d2 = DateTime(2010, 4, 8, 15, 5, 42)
        d =? d1
        d <>? d2
        d.GetHashCode() =? d1.GetHashCode()
        d.GetHashCode() <>? d2.GetHashCode()
    }

//    Test "Now" {
//        DateTime.Now.Kind =? DateTimeKind.Local
//    }
//
//    Test "UtcNow" {
//        DateTime.UtcNow.Kind =? DateTimeKind.Utc
//    }

    Test "Today" {
        DateTime.Today =? DateTime.Now.Date
    }

    Section "Native Dates"

    Test "Turnaround" {
        d.ToJS().Self =?~ d
    }

    Test "Equality" {
        let a = DateTime.Now.ToJS()
        let b = DateTime.Now.ToJS()
        a =? b
        let c = DateTime.Now.AddDays(1.).ToJS()
        a <>? c
    }

    Test "Comparison" {
        let a = DateTime.Now.ToJS()
        let b = DateTime.Now.AddDays(1.).ToJS()
        Unchecked.compare a b < 0 =? true
        Unchecked.compare b a > 0 =? true
    }

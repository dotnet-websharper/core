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

module IntelliFactory.WebSharper.Tests.DateTime

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

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

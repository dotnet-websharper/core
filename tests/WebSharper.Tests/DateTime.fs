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

module WebSharper.Tests.DateTime

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

type private DayOfWeek    = System.DayOfWeek
type private DateTime     = System.DateTime
type private DateTimeKind = System.DateTimeKind
type private TimeSpan     = System.TimeSpan

[<JavaScript>]
let private d = DateTime(2010, 4, 8, 15, 5, 39)

[<JavaScript>]
let timeZoneOffsetSimple =
    let offsetMins = Date().GetTimezoneOffset()
    let offsetHours = -(int offsetMins) / 60;
    let sign = if offsetHours >= 0 then "+" else "-"
    sign + string (abs offsetHours)

[<JavaScript>]
let timeZoneOffsetPadded =
    let offsetMins = Date().GetTimezoneOffset()
    let offsetHours = -(int offsetMins) / 60;
    let sign = if offsetHours >= 0 then "+" else "-"
    sign + (string (abs offsetHours)).PadLeft(2, '0')

[<JavaScript>]
let timeZoneOffsetWithMinutes =
    let offsetMins = Date().GetTimezoneOffset()
    let totalMins = abs (int offsetMins)
    let hours = (string (totalMins / 60)).PadLeft(2, '0')
    let minutes = (string (totalMins % 60)).PadLeft(2, '0')
    let sign = if offsetMins <= 0 then "+" else "-"
    sign + hours + ":" + minutes

[<JavaScript>]
let Tests =

    TestCategory "DateTime" {

        Test "Year" {
            equal d.Year 2010
        }

        Test "Month" {
            equal d.Month 4
        }

        Test "Day" {
            equal d.Day 8
        }

        Test "Hour" {
            equal d.Hour 15
        }

        Test "Minute" {
            equal d.Minute 5
        }

        Test "Second" {
            equal d.Second 39
        }

        Test "Millisecond" {
            equal d.Millisecond 0
        }

        Test "DayOfWeek" {
            equal d.DayOfWeek DayOfWeek.Thursday
        }

        Test "Add" {
            equal (d.Add(TimeSpan(3, 4, 15)))
                (DateTime(2010, 4, 8, 18, 9, 54))
            equal (d + TimeSpan(3, 4, 15))
                (DateTime(2010, 4, 8, 18, 9, 54))
        }

        Test "Subtract" {
            equal (d.Subtract(TimeSpan(3, 4, 15)))
                (DateTime(2010, 4, 8, 12, 1, 24))
            equal (d.Subtract(DateTime(2010, 4, 8, 12, 1, 24)))
                (TimeSpan(3, 4, 15))
            equal (d - TimeSpan(3, 4, 15))
                (DateTime(2010, 4, 8, 12, 1, 24))
            equal (d - DateTime(2010, 4, 8, 12, 1, 24))
                (TimeSpan(3, 4, 15))
        }

        Test "AddYears" {
            equal (d.AddYears 24) (DateTime(2034, 4, 8, 15, 5, 39))
        }

        Test "AddMonths" {
            equal (d.AddMonths 9) (DateTime(2011, 1, 8, 15, 5, 39))
        }

        Test "AddDays" {
            equal (d.AddDays 2.3) (DateTime(2010, 4, 10, 22, 17, 39))
        }

        Test "AddHours" {
            equal (d.AddHours 1.05) (DateTime(2010, 4, 8, 16, 08, 39))
        }

        Test "AddMinutes" {
            equal (d.AddMinutes 141.23) (DateTime(2010, 4, 8, 17, 26, 52, 800))
        }

        Test "AddSeconds" {
            equal (d.AddSeconds 234.15) (DateTime(2010, 4, 8, 15, 09, 33, 150))
        }

        Test "AddMilliseconds" {
            equal (d.AddMilliseconds 123.) (DateTime(2010, 4, 8, 15, 05, 39, 123))
        }

        Test "AddTicks" {
            equal (d.AddTicks 1230000L) (DateTime(2010, 4, 8, 15, 05, 39, 123))
        }

        Test "Date" {
            approxEqual (As<float> d.Date) (As<float> (DateTime(2010, 4, 8)))
        }

        Test "TimeOfDay" {
            approxEqual (As<float> d.TimeOfDay) (As<float> (TimeSpan(15, 5, 39)))
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
            equal (compare d d1) 0
            equal (compare d d2) -1
            equal (compare d2 d) 1
            isTrue (d1 < d2)
            isTrue (d <= d1)
        }

        Test "Equality" {
            let d1 = DateTime(2010, 4, 8, 15, 5, 39)
            let d2 = DateTime(2010, 4, 8, 15, 5, 42)
            equal d d1
            notEqual d d2
            equal (d.GetHashCode()) (d1.GetHashCode())
            notEqual (d.GetHashCode()) (d2.GetHashCode())
        }

    //    Test "Now" {
    //        DateTime.Now.Kind =? DateTimeKind.Local
    //    }
    //
    //    Test "UtcNow" {
    //        DateTime.UtcNow.Kind =? DateTimeKind.Utc
    //    }

        Test "Today" {
            equal DateTime.Today DateTime.Now.Date
        }

        Test "String" {
            let d = DateTime(2010, 4, 8, 15, 5, 39)
            Console.Log("DateTime.ToShortDateString", d.ToShortDateString())
            Console.Log("DateTime.ToLongDateString", d.ToLongDateString())
            equal (d.ToShortTimeString()) "15:05"
            equal (d.ToLongTimeString()) "15:05:39"
        }

        Test "Parse" {
            let d = DateTime(2010, 4, 8, 15, 5, 39)
            raises (DateTime.Parse("not a date"))
            equal (DateTime.Parse("Thu, 08 Apr 2010 15:05:39")) d
            equal (Date.Parse("Thu, 08 Apr 2010 15:05:39")) (As<int> d)
        }

        Test "TryParse" {
            let d = DateTime(2010, 4, 8, 15, 5, 39)
            equal (DateTime.TryParse("not a date") |> fst) false
            equal (DateTime.TryParse("Thu, 08 Apr 2010 15:05:39")) (true, d)
        }

        Test "DaysInMonth" {
            equal (DateTime.DaysInMonth(2018, 6)) 30
            equal (DateTime.DaysInMonth(2018, 2)) 28
            equal (DateTime.DaysInMonth(2020, 2)) 29
        }

        Test "IsLeapYear" {
            isFalse (DateTime.IsLeapYear 2018)
            isTrue (DateTime.IsLeapYear 2020)
            isFalse (DateTime.IsLeapYear 2100)
        }

        // ToString with custom format test is split in different parts
        // otherwise the compilation hangs for ever (seems to be a bug)

        Test "ToString with custom format works (Part #1)" {
            equal (DateTime(2014, 7, 1, 16, 37, 1, 2).ToString("r d")) "r 1"
            equal (DateTime(2014, 7, 13, 16, 37, 1, 2).ToString("r d")) "r 13"

            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r dd")) "r 01"
            equal (DateTime(2014, 7, 21, 16, 37, 0).ToString("r dd")) "r 21"

            equal (DateTime(2014, 7, 7, 16, 37, 0).ToString("r ddd")) "r Mon"
            equal (DateTime(2014, 7, 8, 16, 37, 0).ToString("r ddd")) "r Tue"
            equal (DateTime(2014, 7, 9, 16, 37, 0).ToString("r ddd")) "r Wed"
            equal (DateTime(2014, 7, 10, 16, 37, 0).ToString("r ddd")) "r Thu"
            equal (DateTime(2014, 7, 11, 16, 37, 0).ToString("r ddd")) "r Fri"
            equal (DateTime(2014, 7, 12, 16, 37, 0).ToString("r ddd")) "r Sat"
            equal (DateTime(2014, 7, 13, 16, 37, 0).ToString("r ddd")) "r Sun"

            equal (DateTime(2014, 7, 7, 16, 37, 0).ToString("r dddd")) "r Monday"
            equal (DateTime(2014, 7, 8, 16, 37, 0).ToString("r dddd")) "r Tuesday"
            equal (DateTime(2014, 7, 9, 16, 37, 0).ToString("r dddd")) "r Wednesday"
            equal (DateTime(2014, 7, 10, 16, 37, 0).ToString("r dddd")) "r Thursday"
            equal (DateTime(2014, 7, 11, 16, 37, 0).ToString("r dddd")) "r Friday"
            equal (DateTime(2014, 7, 12, 16, 37, 0).ToString("r dddd")) "r Saturday"
            equal (DateTime(2014, 7, 13, 16, 37, 0).ToString("r dddd")) "r Sunday"

            equal (DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r f")) "r 6"
            equal (DateTime.Parse("2009-06-15T13:45:30.05").ToString("r f")) "r 0"
            equal (DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r ff")) "r 61"
            equal (DateTime.Parse("2009-06-15T13:45:30.0050000").ToString("r ff")) "r 00"
            equal (DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r fff")) "r 617"
            equal (DateTime.Parse("2009-06-15T13:45:30.0005000").ToString("r fff")) "r 000"
            // JavaScript Date only support precision to the millisecond so we fill with 0
            equal (DateTime.Parse("2009-06-15T13:45:30.617").ToString("r ffff")) "r 6170"
            equal (DateTime.Parse("2009-06-15T13:45:30.000").ToString("r ffff")) "r 0000"
            equal (DateTime.Parse("2009-06-15T13:45:30.617").ToString("r fffff")) "r 61700"
            equal (DateTime.Parse("2009-06-15T13:45:30.000").ToString("r fffff")) "r 00000"
            equal (DateTime.Parse("2009-06-15T13:45:30.617").ToString("r ffffff")) "r 617000"
            equal (DateTime.Parse("2009-06-15T13:45:30.000").ToString("r ffffff")) "r 000000"
            equal (DateTime.Parse("2009-06-15T13:45:30.617").ToString("r fffffff")) "r 6170000"
        }
        
        Test "ToString with custom format works (Part #2)" {
            equal (DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r F")) "r 6"
            equal (DateTime.Parse("2009-06-15T13:45:30.05").ToString("r F")) "r "
            equal (DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r FF")) "r 61"
            equal (DateTime.Parse("2009-06-15T13:45:30.0050000").ToString("r FF")) "r "
            equal (DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r FFF")) "r 617"
            equal (DateTime.Parse("2009-06-15T13:45:30.0005000").ToString("r FFF")) "r "
            // JavaScript Date only support precision to the millisecond so we fill with 0
            equal (DateTime.Parse("2009-06-15T13:45:30.617").ToString("r FFFF")) "r 617"
            equal (DateTime.Parse("2009-06-15T13:45:30.000").ToString("r FFFF")) "r "
            equal (DateTime.Parse("2009-06-15T13:45:30.617").ToString("r FFFFF")) "r 617"
            equal (DateTime.Parse("2009-06-15T13:45:30.000").ToString("r FFFFF")) "r "
            equal (DateTime.Parse("2009-06-15T13:45:30.061").ToString("r FFFFFF")) "r 061"
            equal (DateTime.Parse("2009-06-15T13:45:30.000").ToString("r FFFFFF")) "r "
            equal (DateTime.Parse("2009-06-15T13:45:30.617").ToString("r FFFFFFF")) "r 617"
            equal (DateTime.Parse("2009-06-15T13:45:30.000").ToString("r FFFFFFF")) "r "

            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r g")) "r A.D."

            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r gg")) "r A.D."

            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r gg ggggg")) "r A.D. A.D."

            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r h")) "r 4"
            equal (DateTime(2014, 7, 1, 4, 37, 0).ToString("r h")) "r 4"
            // Test edge case where hour is 12
            equal (DateTime(2014, 7, 1, 0, 0, 0).ToString("r h")) "r 12"
            equal (DateTime(2014, 7, 1, 12, 0, 0).ToString("r h")) "r 12"

            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r hh")) "r 04"
            equal (DateTime(2014, 7, 1, 4, 37, 0).ToString("r hh")) "r 04"
            // Test edge case where hour is 12
            equal (DateTime(2014, 7, 1, 0, 0, 0).ToString("r hh")) "r 12"
            equal (DateTime(2014, 7, 1, 12, 0, 0).ToString("r hh")) "r 12"

            equal (DateTime(2014, 7, 1, 4, 37, 0).ToString("r H")) "r 4"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r H")) "r 16"

            equal (DateTime(2014, 7, 1, 4, 37, 0).ToString("r HH")) "r 04"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r HH")) "r 16"
                
            // WebSharper dates are always in local time
            // equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r K")) "r "
            // equal (DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r K")) "r Z"

            // Timezone dependent (test is configured for Europe/Paris timezone)
            // Commented below it the equivalent test for .NET
            // equal (DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r K")) "r +02:00"
            // equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r K")) "r +02:00"

            // WebSharper dates are always in local time
            // equal (DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Unspecified).ToString("r K")) "r "

            equal (DateTime(2014, 7, 1, 16, 3, 0).ToString("r m")) "r 3"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r m")) "r 37"

            equal (DateTime(2014, 7, 1, 16, 3, 0).ToString("r mm")) "r 03"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r mm")) "r 37"

            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r M")) "r 7"
            equal (DateTime(2014, 11, 1, 16, 37, 0).ToString("r M")) "r 11"

            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r MM")) "r 07"
            equal (DateTime(2014, 11, 1, 16, 37, 0).ToString("r MM")) "r 11"
        }

        Test "ToString with custom format works (Part #3)" {
            equal (DateTime(2014, 1, 1, 16, 37, 0).ToString("r MMM")) "r Jan"
            equal (DateTime(2014, 2, 1, 16, 37, 0).ToString("r MMM")) "r Feb"
            equal (DateTime(2014, 3, 1, 16, 37, 0).ToString("r MMM")) "r Mar"
            equal (DateTime(2014, 4, 1, 16, 37, 0).ToString("r MMM")) "r Apr"
            equal (DateTime(2014, 5, 1, 16, 37, 0).ToString("r MMM")) "r May"
            equal (DateTime(2014, 6, 1, 16, 37, 0).ToString("r MMM")) "r Jun"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r MMM")) "r Jul"
            equal (DateTime(2014, 8, 1, 16, 37, 0).ToString("r MMM")) "r Aug"
            equal (DateTime(2014, 9, 1, 16, 37, 0).ToString("r MMM")) "r Sep"
            equal (DateTime(2014, 10, 1, 16, 37, 0).ToString("r MMM")) "r Oct"
            equal (DateTime(2014, 11, 1, 16, 37, 0).ToString("r MMM")) "r Nov"
            equal (DateTime(2014, 12, 1, 16, 37, 0).ToString("r MMM")) "r Dec"

            equal (DateTime(2014, 1, 1, 16, 37, 0).ToString("r MMMM")) "r January"
            equal (DateTime(2014, 2, 1, 16, 37, 0).ToString("r MMMM")) "r February"
            equal (DateTime(2014, 3, 1, 16, 37, 0).ToString("r MMMM")) "r March"
            equal (DateTime(2014, 4, 1, 16, 37, 0).ToString("r MMMM")) "r April"
            equal (DateTime(2014, 5, 1, 16, 37, 0).ToString("r MMMM")) "r May"
            equal (DateTime(2014, 6, 1, 16, 37, 0).ToString("r MMMM")) "r June"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r MMMM")) "r July"
            equal (DateTime(2014, 8, 1, 16, 37, 0).ToString("r MMMM")) "r August"
            equal (DateTime(2014, 9, 1, 16, 37, 0).ToString("r MMMM")) "r September"
            equal (DateTime(2014, 10, 1, 16, 37, 0).ToString("r MMMM")) "r October"
            equal (DateTime(2014, 11, 1, 16, 37, 0).ToString("r MMMM")) "r November"
            equal (DateTime(2014, 12, 1, 16, 37, 0).ToString("r MMMM")) "r December"
        }

        Test "ToString with custom format works (Part #4)" {
            equal (DateTime(2014, 7, 1, 16, 37, 3).ToString("r s")) "r 3"
            equal (DateTime(2014, 7, 1, 16, 37, 31).ToString("r s")) "r 31"

            equal (DateTime(2014, 7, 1, 16, 37, 3).ToString("r ss")) "r 03"
            equal (DateTime(2014, 7, 1, 16, 37, 31).ToString("r ss")) "r 31"

            equal (DateTime(2014, 7, 1, 1, 37, 0).ToString("r t")) "r A"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r t")) "r P"
            equal (DateTime(2014, 7, 1, 1, 37, 0).ToString("r tt")) "r AM"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r tt")) "r PM"

            equal (DateTime(1,1,1).ToString("r y")) "r 1"
            equal (DateTime(0900,1,1).ToString("r y")) "r 0"
            equal (DateTime(1900,1,1).ToString("r y")) "r 0"
            equal (DateTime(2009,1,1).ToString("r y")) "r 9"
            equal (DateTime(2019,1,1).ToString("r y")) "r 19"

            equal (DateTime(1,1,1).ToString("r yy")) "r 01"
            equal (DateTime(0900,1,1).ToString("r yy")) "r 00"
            equal (DateTime(1900,1,1).ToString("r yy")) "r 00"
            equal (DateTime(2009,1,1).ToString("r yy")) "r 09"
            equal (DateTime(2019,1,1).ToString("r yy")) "r 19"

            equal (DateTime(1,1,1).ToString("r yyy")) "r 001"
            equal (DateTime(0900,1,1).ToString("r yyy")) "r 900"
            equal (DateTime(1900,1,1).ToString("r yyy")) "r 1900"
            equal (DateTime(2009,1,1).ToString("r yyy")) "r 2009"
            equal (DateTime(2019,1,1).ToString("r yyy")) "r 2019"

            equal (DateTime(1,1,1).ToString("r yyyy")) "r 0001"
            equal (DateTime(0900,1,1).ToString("r yyyy")) "r 0900"
            equal (DateTime(1900,1,1).ToString("r yyyy")) "r 1900"
            equal (DateTime(2009,1,1).ToString("r yyyy")) "r 2009"
            equal (DateTime(2019,1,1).ToString("r yyyy")) "r 2019"

            equal (DateTime(1,1,1).ToString("r yyyyy")) "r 00001"
            equal (DateTime(0900,1,1).ToString("r yyyyy")) "r 00900"
            equal (DateTime(1900,1,1).ToString("r yyyyy")) "r 01900"
            equal (DateTime(2009,1,1).ToString("r yyyyy")) "r 02009"
            equal (DateTime(2019,1,1).ToString("r yyyyy")) "r 02019"
        }

        Test "ToString with custom format works (Part #5)" {
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r z")) $"r {timeZoneOffsetSimple}"
            // equal (DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r z")) "r +2"
 
            // equal (DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r zz")) "r +00"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r zz")) $"r {timeZoneOffsetPadded}"
            // equal (DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r zz")) "r +02"

            // equal (DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r zzz")) "r +00:00"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r zzz")) $"r {timeZoneOffsetWithMinutes}"
            // equal (DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r zzz")) "r +02:00"

            // Time separator
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r :")) "r :"

            // Date separator
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r /")) "r /"

            // String quotation
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r \"hh\" h")) "r hh 4"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r 'hh' h")) "r hh 4"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r \'hh\'")) "r hh"

            // Format character
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r %h")) "r 4"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r %hh")) "r 44"

            // Escape character
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r \zz")) ($"r z{timeZoneOffsetSimple}")
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r \\zz")) ($"r z{timeZoneOffsetSimple}")
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r \\\zz")) ($"r \{timeZoneOffsetPadded}")
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r \\z\\z")) "r zz"

            // Escape character with verbatim string
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("""r \zz""")) ($"r z{timeZoneOffsetSimple}")
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("""r \\zz""")) ($"r \{timeZoneOffsetPadded}")
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("""r \\\zz""")) ($"r \z{timeZoneOffsetSimple}")
        }

        Test "ToString with custom format works (Part #6)" {
            // The tests below are for testing the behaviour when going outside
            // of the standard token ranges.
            // For example, the known tokens range for 'H' are "H", "HH", so
            // we want to test what happens when we do "HHH" or "HHHH" or "HHHHH"
            // In general, the tests below check what happens when right outside of the known
            // token ranges and in another exagerated case
            
            equal (DateTime(2014, 7, 13, 16, 37, 0).ToString("r dddd dddddd")) "r Sunday Sunday"

            // Can't test ffffffffffffff because there don't seems to be a helpers
            // for testing exception in WebSharper.Testing and using try ... with
            // don't seems to b worth it

            // Can't test FFFFFFFFFFFFFF because there don't seems to be a helpers
            // for testing exception in WebSharper.Testing and using try ... with
            // don't seems to b worth it

            equal (DateTime(2014, 7, 1, 12, 0, 0).ToString("r hhh hhhhh")) "r 12 12"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r HHH HHHHHHHH")) "r 16 16"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r KK KKK")) $"r {timeZoneOffsetWithMinutes}{timeZoneOffsetWithMinutes} {timeZoneOffsetWithMinutes}{timeZoneOffsetWithMinutes}{timeZoneOffsetWithMinutes}"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r mmm mmmm")) "r 37 37"
            equal (DateTime(2014, 12, 1, 16, 37, 0).ToString("r MMMMM MMMMMMMMM")) "r December December"
            equal (DateTime(2014, 7, 1, 16, 37, 31).ToString("r sss ssssss")) "r 31 31"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r ttt ttttttt")) "r PM PM"
            equal (DateTime(2019,1,1).ToString("r yyyyyy yyyyyyyyyy")) "r 002019 0000002019"
            equal (DateTime(2014, 7, 1, 16, 37, 0).ToString("r zzzz zzzzzz")) $"r {timeZoneOffsetWithMinutes} {timeZoneOffsetWithMinutes}"
        }

        Test "DateTime.ToString without separator works" {
            equal (DateTime(2017, 9, 5).ToString("yyyyMM")) "201709"
        }

        Test "DateTime.ToString('O') works" {
            // On .NET the result is "2014-09-11T16:37:02.0000000+02:00"
            // but because of JavaScript date precission we remove some trailing zero
            equal (DateTime(2014, 9, 11, 16, 37, 2).ToString("O")) ($"2014-09-11T16:37:02.000{timeZoneOffsetWithMinutes}")
            equal (DateTime(2014, 9, 11, 16, 37, 2).ToString("o")) ($"2014-09-11T16:37:02.000{timeZoneOffsetWithMinutes}")
        }

        Test "DateTime.ToString('D') works" {
            equal (DateTime(2014, 9, 1, 16, 37, 2).ToString("D")) "Monday, 01 September 2014"
        }

        Test "DateTime.ToString('d') works" {
            equal (DateTime(2014, 9, 1, 16, 37, 2).ToString("d")) "09/01/2014"
        }

        Test "DateTime.ToString('T') works" {
            equal (DateTime(2014, 9, 1, 6, 7, 2).ToString("T")) "06:07:02"
        }

        Test "DateTime.ToString('t') works" {
            equal (DateTime(2014, 9, 1, 6, 7, 2).ToString("t")) "06:07"
        }

        Test "#1433 DateTime when year < 100" {
            equal (DateTime(1, 1, 1).ToString("D")) "Monday, 01 January 1"
        }
    }

[<JavaScript>]
let NativeTests =

    TestCategory "Native Dates" {

        Test "Turnaround" {
            approxEqual (As<float> d.JS.Self) (As<float> d)
        }

        Test "Equality" {
            let a = DateTime.Now.JS
            let b = DateTime.Now.JS
            equal a b
            let c = DateTime.Now.AddDays(1.).JS
            notEqual a c
        }

        Test "Comparison" {
            let a = DateTime.Now.JS
            let b = DateTime.Now.AddDays(1.).JS
            isTrue (Unchecked.compare a b < 0)
            isTrue (Unchecked.compare b a > 0)
        }

    }

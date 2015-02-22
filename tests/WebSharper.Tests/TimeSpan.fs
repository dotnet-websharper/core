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

module WebSharper.Tests.TimeSpan

open WebSharper
open WebSharper.Testing

type TimeSpan = System.TimeSpan

[<JavaScript>]
let Tests =
    Section "TimeSpan"


    let s = TimeSpan(733869, 14, 12, 12, 545)
    let ss = TimeSpan(0, 14, 12, 15, 545)

    let ( =?~ ) (a: float) (b: float) =
        abs (a - b) < 0.0001 =? true

    Test "Construction" {
//        let s = TimeSpan(123L)
//        s.Ticks =? 123L
        let s = TimeSpan(10, 11, 12)
        s.Hours =? 10
        s.Minutes =? 11
        s.Seconds =? 12
        let s = TimeSpan(100, 10, 11, 12)
        s.Days =? 100
        s.Hours =? 10
        s.Minutes =? 11
        s.Seconds =? 12
        let s = TimeSpan(100, 10, 11, 12, 512)
        s.Days =? 100
        s.Hours =? 10
        s.Minutes =? 11
        s.Seconds =? 12
        s.Milliseconds =? 512
    }

    Test "Add" {
        let s1 = TimeSpan.FromMilliseconds(1.2)
        let s2 = TimeSpan.FromMilliseconds(8.)
        s1.Add(s2).TotalMilliseconds =? 9.2
    }

    Test "Subtract" {
        let s1 = TimeSpan.FromMilliseconds(40.)
        let s2 = TimeSpan.FromMilliseconds(32.)
        s1.Subtract(s2).TotalMilliseconds =? 8.
    }

    Test "Negate" {
        let s1 = TimeSpan.FromMilliseconds(1.)
        s1.Negate().TotalMilliseconds =? -1.
    }

    Test "Duration" {
        s.Negate().Duration().TotalMilliseconds =? s.TotalMilliseconds
    }

    Test "Hours" {
        s.Hours =? 14
    }

    Test "Minutes" {
        s.Minutes =? 12
    }

    Test "Seconds" {
        s.Seconds =? 12
    }

    Test "Milliseconds" {
        s.Milliseconds =? 545
    }

//    Test "Ticks" {
//        s.Ticks =? 634063327325450000L
//    }

    Test "TotalDays" {
        s.TotalDays =?~ 733869.5918
    }

    Test "TotalHours" {
        floor s.TotalHours =? 17612870.
    }

    Test "TotalMinutes" {
        ss.TotalMinutes =?~ 852.2590833
    }

    Test "TotalSeconds" {
        ss.TotalSeconds =?~ 51135.545
    }

    Test "TotalMilliseconds" {
        ss.TotalMilliseconds =?~ 51135545.
    }

    Test "Equality" {
        let s1 = TimeSpan(40, 10, 15)
        let s2 = TimeSpan(0, 40, 10, 15, 400)
        let s3 = TimeSpan(40, 10, 15)
        s1 <>? s2
        s1 =? s3
        s1.GetHashCode() <>? s2.GetHashCode()
        s1.GetHashCode() =? s3.GetHashCode()
    }

    Test "Comparison" {
        let s1 = TimeSpan(40, 10, 15)
        let s2 = TimeSpan(0, 40, 10, 15, 400)
        let s3 = TimeSpan(40, 10, 15)
        compare s1 s2 =? -1
        compare s2 s1 =? 1
        compare s1 s3 =? 0
    }

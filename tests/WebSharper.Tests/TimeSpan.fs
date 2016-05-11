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

module WebSharper.Tests.TimeSpan

open WebSharper
open WebSharper.Testing

type TimeSpan = System.TimeSpan

[<JavaScript>]
let Tests =
    TestCategory "TimeSpan" {
    
        let s = TimeSpan(733869, 14, 12, 12, 545)
        let ss = TimeSpan(0, 14, 12, 15, 545)

        Test "Construction" {
            let s = TimeSpan(123L)
            equal s.Ticks 123L
            let s = TimeSpan(10, 11, 12)
            equal (s.Hours) 10
            equal (s.Minutes) 11
            equal (s.Seconds) 12
            let s = TimeSpan(100, 10, 11, 12)
            equal (s.Days) 100
            equal (s.Hours) 10
            equal (s.Minutes) 11
            equal (s.Seconds) 12
            let s = TimeSpan(100, 10, 11, 12, 512)
            equal (s.Days) 100
            equal (s.Hours) 10
            equal (s.Minutes) 11
            equal (s.Seconds) 12
            equal (s.Milliseconds) 512
        }

        Test "Add" {
            let s1 = TimeSpan.FromMilliseconds(1.2)
            let s2 = TimeSpan.FromMilliseconds(8.)
            equal (s1.Add(s2).TotalMilliseconds) 9.2
        }

        Test "Subtract" {
            let s1 = TimeSpan.FromMilliseconds(40.)
            let s2 = TimeSpan.FromMilliseconds(32.)
            equal (s1.Subtract(s2).TotalMilliseconds) 8.
        }

        Test "Negate" {
            let s1 = TimeSpan.FromMilliseconds(1.)
            equal (s1.Negate().TotalMilliseconds) -1.
        }

        Test "Duration" {
            equal (s.Negate().Duration().TotalMilliseconds) s.TotalMilliseconds
        }

        Test "Hours" {
            equal (s.Hours) 14
        }

        Test "Minutes" {
            equal (s.Minutes) 12
        }

        Test "Seconds" {
            equal (s.Seconds) 12
        }

        Test "Milliseconds" {
            equal (s.Milliseconds) 545
        }

        Test "Ticks" {
            equal (s.Ticks) 634063327325450000L
        }

        Test "TotalDays" {
            approxEqual (s.TotalDays) 733869.5918
        }

        Test "TotalHours" {
            equal (floor s.TotalHours) 17612870.
        }

        Test "TotalMinutes" {
            approxEqual (ss.TotalMinutes) 852.2590833
        }

        Test "TotalSeconds" {
            approxEqual (ss.TotalSeconds) 51135.545
        }

        Test "TotalMilliseconds" {
            approxEqual (ss.TotalMilliseconds) 51135545.
        }

        Test "equality" {
            let s1 = TimeSpan(40, 10, 15)
            let s2 = TimeSpan(0, 40, 10, 15, 400)
            let s3 = TimeSpan(40, 10, 15)
            notEqual s1 s2
            equal (s1) s3
            notEqual (s1.GetHashCode()) (s2.GetHashCode())
            equal (s1.GetHashCode()) (s3.GetHashCode())
        }

        Test "Comparison" {
            let s1 = TimeSpan(40, 10, 15)
            let s2 = TimeSpan(0, 40, 10, 15, 400)
            let s3 = TimeSpan(40, 10, 15)
            equal (compare s1 s2) -1
            equal (compare s2 s1) 1
            equal (compare s1 s3) 0
        }

    }

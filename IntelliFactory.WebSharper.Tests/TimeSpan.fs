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

module IntelliFactory.WebSharper.Tests.TimeSpan

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

type TimeSpan = System.TimeSpan

[<JavaScript>]
let Tests =
    Section "TimeSpan"


    let s = TimeSpan(733869, 14, 12, 12, 545)
    let ss = TimeSpan(0, 14, 12, 15, 545)

    let ( =?~ ) (a: float) (b: float) =
        abs (a - b) < 0.0001 =? true

    Test "Construction" {
        let s = TimeSpan(123L)
        s.Ticks =? 123L
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
        let s1 = TimeSpan(120000L)
        let s2 = TimeSpan(800000L)
        s1.Add(s2).Ticks =? 920000L
    }

    Test "Subtract" {
        let s1 = TimeSpan(400000L)
        let s2 = TimeSpan(320000L)
        s1.Subtract(s2).Ticks =? 80000L
    }

    Test "Negate" {
        let s1 = TimeSpan(1000L)
        s1.Negate().Ticks =? -1000L
    }

    Test "Duration" {
        s.Negate().Duration().Ticks =? s.Ticks
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

    Test "Ticks" {
        s.Ticks =? 634063327325450000L
    }

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

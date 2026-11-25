// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
namespace Conformance

open System
open WebSharper
open Conformance.H

[<JavaScript>]
module Dates =
    let private d () = DateTime(2024, 2, 29, 13, 45, 30)

    let Cases () = [
        C "datetime.parts" (fun () -> let x = d () in string x.Year + "," + string x.Month + "," + string x.Day + "," + string x.Hour + "," + string x.Minute + "," + string x.Second)
        C "datetime.dayofweek" (fun () -> string (d ()).DayOfWeek + "," + string (d ()).DayOfYear)
        C "datetime.adddays" (fun () -> (d ()).AddDays(1.0).ToString("yyyy-MM-dd"))
        C "datetime.addmonths" (fun () -> (d ()).AddMonths(12).ToString("yyyy-MM-dd"))
        C "datetime.addyears" (fun () -> (d ()).AddYears(1).ToString("yyyy-MM-dd"))
        C "datetime.format.custom" (fun () -> (d ()).ToString("yyyy-MM-dd HH:mm:ss"))
        C "datetime.format.iso" (fun () -> (d ()).ToString("o"))
        C "datetime.tostring" (fun () -> string (d ()))
        C "datetime.subtract" (fun () -> string (DateTime(2024, 3, 1) - DateTime(2024, 2, 1)).TotalDays)
        C "datetime.compare" (fun () -> B (d () < (d ()).AddSeconds 1.0) + "," + B (d () = DateTime(2024, 2, 29, 13, 45, 30)))
        C "datetime.daysinmonth" (fun () -> string (DateTime.DaysInMonth(2024, 2)) + "," + B (DateTime.IsLeapYear 1900))
        C "datetime.parse" (fun () -> (DateTime.Parse "2024-01-15T10:00:00").ToString("yyyy-MM-dd HH:mm"))
        C "datetime.date" (fun () -> (d ()).Date.ToString("HH:mm"))
        C "datetime.invalid" (fun () -> string (DateTime(2023, 2, 29)).Day)
        C "datetime.invalid.time" (fun () -> try string (DateTime(2024, 1, 1, 24, 0, 0)).Hour with :? ArgumentOutOfRangeException -> "range")
        C "datetime.minvalue" (fun () -> string DateTime.MinValue.Year + "," + string DateTime.MaxValue.Year)
        C "datetime.ticks" (fun () -> string (DateTime(2000, 1, 1)).Ticks)
#if !JS
        C "datetimeoffset" (fun () -> let x = DateTimeOffset(2024, 1, 1, 0, 0, 0, TimeSpan.Zero) in string (x.ToUnixTimeSeconds()))
#else
        C "datetimeoffset" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "timespan.parts" (fun () -> let t = TimeSpan(1, 2, 3, 4) in string t.Days + "," + string t.Hours + "," + string t.TotalMinutes)
        C "timespan.tostring" (fun () -> string (TimeSpan(1, 2, 3, 4)) + "|" + string (TimeSpan.FromMinutes 90.5))
        C "timespan.negative" (fun () -> string (TimeSpan.FromSeconds -61.0))
        C "timespan.arith" (fun () -> string (TimeSpan.FromHours 1.0 + TimeSpan.FromMinutes 30.0).TotalHours)
        C "timespan.compare" (fun () -> string (compare (TimeSpan.FromSeconds 1.0) (TimeSpan.FromSeconds 2.0)))
        C "guid.parse" (fun () -> string (Guid.Parse "6F9619FF-8B86-D011-B42D-00CF4FC964FF"))
        C "guid.empty" (fun () -> string Guid.Empty)
        C "guid.equality" (fun () -> B (Guid.Parse "6f9619ff-8b86-d011-b42d-00cf4fc964ff" = Guid.Parse "6F9619FF-8B86-D011-B42D-00CF4FC964FF"))
        C "guid.newguid.unique" (fun () -> B (Guid.NewGuid() <> Guid.NewGuid()))
        C "char.parse" (fun () -> string (Char.Parse "x") + "," + string (Char.GetNumericValue '7'))
        C "char.tostring.int" (fun () -> string (int (Char.MaxValue)))
        C "bool.parse" (fun () -> string (Boolean.Parse "True") + "," + string (Boolean.Parse "false"))
        C "nullable" (fun () -> let x = Nullable 3 in let y = Nullable<int>() in string x.Value + "," + B y.HasValue + "," + string (y.GetValueOrDefault 9))
        C "math.functions" (fun () -> string (Math.Max(3, 7)) + "," + string (Math.Abs -2.5) + "," + string (Math.Floor -1.5) + "," + string (Math.Sign -3))
        C "math.constants" (fun () -> string (Math.Round(Math.PI, 4)) + "," + string (Math.Round(Math.E, 4)))
        C "math.clamp" (fun () -> string (Math.Clamp(15, 0, 10)))
        C "math.divrem" (fun () -> let q, r = Math.DivRem(17, 5) in string q + string r)
        C "math.truncate.log" (fun () -> string (Math.Truncate -2.9) + "," + string (Math.Log10 1000.0) + "," + string (Math.Log(8.0, 2.0)))
        C "convert.tobase" (fun () -> Convert.ToString(10, 2) + "," + string (Convert.ToInt32("ff", 16)))
        C "convert.toint32.rounding" (fun () -> string (Convert.ToInt32 2.5) + "," + string (Convert.ToInt32 3.5))
        C "convert.base64" (fun () -> Convert.ToBase64String [| 1uy; 2uy; 255uy |])
        C "convert.frombase64" (fun () -> Ji (Convert.FromBase64String "AQL/" |> Array.map int))
        C "convert.todouble" (fun () -> string (Convert.ToDouble 2.5) + "," + string (Convert.ToSingle 1.5))
        C "convert.toint32.base" (fun () -> string (Convert.ToInt32("-12", 10)) + "," + string (Convert.ToInt32("ffffffff", 16)) + "," + Convert.ToString(-1, 16))
#if !JS
        C "bitconverter" (fun () -> string (BitConverter.GetBytes(1).Length))
#else
        C "bitconverter" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "random.range" (fun () -> let r = Random() in let x = r.Next(5, 10) in B (x >= 5 && x < 10))
#if !JS
        C "uri" (fun () -> let u = Uri "https://example.com:8080/a/b?q=1" in u.Host + "," + string u.Port + "," + u.AbsolutePath)
#else
        C "uri" (fun () -> "WS-COMPILE-ERROR")
#endif
#if !JS
        C "typeof.int.name" (fun () -> typeof<string>.FullName)
#else
        C "typeof.int.name" (fun () -> "WS-COMPILE-ERROR")
#endif
#if !JS
        C "gettype.name" (fun () -> (box 1).GetType().Name)
#else
        C "gettype.name" (fun () -> "WS-COMPILE-ERROR")
#endif
    ]

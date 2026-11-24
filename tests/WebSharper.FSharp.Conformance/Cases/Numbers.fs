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
module Numbers =
    let private big () = Int32.MaxValue
    let private bigL () = Int64.MaxValue
    let private zero () = 0
    let private zeroF () = 0.0
    let private n (x: int) = x

    let Cases () = [
        C "int.overflow.wraps" (fun () -> string (big () + 1))
        C "int.multiply.wraps" (fun () -> string (big () * 2))
        C "int.negate.min" (fun () -> string (-(Int32.MinValue + n 0)))
        C "int.division.truncates" (fun () -> string (-7 / n 2) + "," + string (7 / n -2))
        C "int.remainder.sign" (fun () -> string (-7 % n 3) + "," + string (7 % n -3))
        C "int.divide.zero" (fun () -> string (1 / zero ()))
        C "int.modulo.zero" (fun () -> string (1 % zero ()))
        C "int64.overflow.wraps" (fun () -> string (bigL () + 1L))
        C "int64.large" (fun () -> string (9007199254740993L + int64 (n 0)))
        C "int64.multiply" (fun () -> string (123456789L * int64 (n 1000000)))
        C "int64.division" (fun () -> string (-9000000000000000007L / int64 (n 2)))
        C "uint32.wraps" (fun () -> string (0u - uint32 (n 1)))
        C "byte.wraps" (fun () -> string (255uy + byte (n 1)))
        C "sbyte.wraps" (fun () -> string (127y + sbyte (n 1)))
        C "int16.wraps" (fun () -> string (32767s + int16 (n 1)))
        C "uint64.max" (fun () -> string (UInt64.MaxValue - uint64 (n 0)))
#if !JS
        C "checked.overflow" (fun () -> string (Checked.(+) (big ()) 1))
#else
        C "checked.overflow" (fun () -> "WS-COMPILE-ERROR")
#endif
#if !JS
        C "checked.int.conversion" (fun () -> string (Checked.int 3000000000.0))
#else
        C "checked.int.conversion" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "conversion.int.of.float" (fun () -> string (int 3.99) + "," + string (int -3.99))
        C "conversion.byte.of.int" (fun () -> string (byte (n 300)))
        C "conversion.int.of.string" (fun () -> string (int "42"))
        C "conversion.int.of.bad.string" (fun () -> string (int "4x2"))
        C "conversion.float.of.string" (fun () -> string (float "2.5"))
        C "conversion.int.of.char" (fun () -> string (int 'A'))
        C "conversion.char.of.int" (fun () -> string (char (n 66)))
        C "conversion.int64.of.int" (fun () -> string (int64 (n -5)))
        C "conversion.uint32.of.negative" (fun () -> string (uint32 (n -1)))
        C "int.parse" (fun () -> string (Int32.Parse "-17"))
        C "int.parse.bad" (fun () -> string (Int32.Parse "abc"))
        C "int.parse.overflow" (fun () -> string (Int32.Parse "3000000000"))
        C "int.tryparse" (fun () -> match Int32.TryParse "12" with | true, v -> string v | _ -> "no")
        C "int.tryparse.bad" (fun () -> match Int32.TryParse "1.5" with | true, v -> string v | _ -> "no")
        C "float.tostring" (fun () -> string 1.5 + "," + string 2.0 + "," + string 0.1)
        C "float.tostring.large" (fun () -> string 1e21 + "," + string 123456789012.0)
        C "float.tostring.small" (fun () -> string 1e-7)
        C "float.nan" (fun () -> string nan + "," + string infinity + "," + string -infinity)
        C "float.divide.zero" (fun () -> string (1.0 / zeroF ()))
        C "float.nan.equality" (fun () -> B (nan = nan) + "," + B (nan <> nan))
        C "float.compare.nan" (fun () -> string (compare nan 1.0))
        C "float.round.banker" (fun () -> string (round 2.5) + "," + string (round 3.5) + "," + string (round -2.5))
        C "float.math.round" (fun () -> string (Math.Round 0.5) + "," + string (Math.Round 1.5))
        C "float.math.round.digits" (fun () -> string (Math.Round(2.345, 2)))
        C "float.truncate" (fun () -> string (truncate -2.7) + "," + string (floor -2.7) + "," + string (ceil -2.2))
        C "float32.precision" (fun () -> string (float32 0.1))
        C "float32.arith" (fun () -> string (0.1f + 0.2f))
        C "float.sum" (fun () -> string (0.1 + 0.2))
        C "float.parse" (fun () -> string (Double.Parse "1e3"))
        C "float.tostring.format" (fun () -> (3.14159).ToString("F2"))
        C "float.tostring.n" (fun () -> (1234567.891).ToString("N2"))
        C "int.tostring.d" (fun () -> (42).ToString("D5"))
        C "int.tostring.x" (fun () -> (255).ToString("X") + "," + (255).ToString("x4"))
#if !JS
        C "decimal.add" (fun () -> string (0.1M + 0.2M))
#else
        C "decimal.add" (fun () -> "WS-COMPILE-ERROR")
#endif
#if !JS
        C "decimal.divide" (fun () -> string (1M / 3M))
#else
        C "decimal.divide" (fun () -> "WS-COMPILE-ERROR")
#endif
        C "bigint.pow" (fun () -> string (bigint 2 ** 100))
        C "bigint.divide" (fun () -> string (bigint.Parse "1000000000000000000000" / bigint 7))
        C "pown" (fun () -> string (pown 2 10) + "," + string (pown 2.0 -2))
        C "abs.sign" (fun () -> string (abs (n -5)) + "," + string (sign (n -5)) + "," + string (sign -0.0))
        C "abs.min.int" (fun () -> string (abs Int32.MinValue))
        C "min.max" (fun () -> string (min 3 (n 5)) + "," + string (max 3.0 nan))
        C "sqrt.negative" (fun () -> string (sqrt -1.0))
        C "math.pow" (fun () -> string (2.0 ** 0.5))
        C "bitwise.ops" (fun () -> string (n 12 &&& 10) + "," + string (n 12 ||| 3) + "," + string (n 12 ^^^ 5) + "," + string (~~~ (n 0)))
        C "bitwise.shift" (fun () -> string (n 1 <<< 31) + "," + string (n -16 >>> 2) + "," + string (n 1 <<< 33))
        C "bitwise.shift.uint" (fun () -> string (0x80000000u >>> 31))
        C "bitwise.int64" (fun () -> string (1L <<< 40))
        C "hex.literal" (fun () -> string 0xFF + "," + string 0b1010 + "," + string 0o17)
        C "int.compare" (fun () -> string (compare 1 (n 2)) + "," + string (compare "b" "a"))
        C "int.max.value" (fun () -> string Int32.MaxValue + "," + string Int64.MinValue)
        C "char.arith" (fun () -> string (char (int 'a' + n 1)))
        C "range.step" (fun () -> Ji [ 10 .. -3 .. 0 ])
        C "range.float" (fun () -> J (List.map string [ 0.0 .. 0.5 .. 1.5 ]))
        C "incr.decr" (fun () -> let r = ref 0 in incr r; incr r; decr r; string r.Value)
    ]

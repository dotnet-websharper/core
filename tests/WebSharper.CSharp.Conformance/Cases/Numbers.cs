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
using System;
using System.Collections.Generic;
using System.Globalization;
using WebSharper;
using static Conformance.H;

namespace Conformance
{
    [JavaScript]
    public static class Numbers
    {
        static int Big() => int.MaxValue;
        static long BigL() => long.MaxValue;
        static int Zero() => 0;

        public static List<(string, Func<string>)> Cases() => new List<(string, Func<string>)>
        {
            C("int.overflow.wraps", () => $"{Big() + 1}"),
            C("int.multiply.wraps", () => $"{Big() * 2}"),
            C("int.division.truncates", () => $"{-7 / 2},{7 / -2}"),
            C("int.remainder.sign", () => $"{-7 % 3},{7 % -3}"),
            C("int.divide.by.zero", () => $"{1 / Zero()}"),
            C("int.checked.overflow", () => $"{checked(Big() + 1)}"),
            C("uint.wraps", () => { uint u = 0; u--; return $"{u}"; }),
            C("byte.wraps", () => { byte b = 255; b++; return $"{b}"; }),
            C("short.cast.truncates", () => $"{unchecked((short)70000)}"),
            C("long.max", () => $"{BigL()}"),
            C("long.overflow.wraps", () => $"{BigL() + 1}"),
            C("long.multiply", () => $"{3_000_000_000L * 3}"),
            C("double.tostring", () => $"{0.1 + 0.2}|{1.0}|{1e21}|{1.5e-7}|{double.NaN}|{1.0 / 0}"),
            C("double.to.int.truncates", () => $"{(int)2.9},{(int)-2.9}"),
            C("double.to.int.nan", () => { double n = double.NaN; return $"{(int)n}"; }),
            C("math.round.bankers", () => $"{Math.Round(2.5)},{Math.Round(3.5)},{Math.Round(-2.5)}"),
#if !JS
            C("math.round.digits", () => $"{Math.Round(1.005, 2)},{Math.Round(2.345, 2)}"),
#else
            C("math.round.digits", () => "WS-COMPILE-ERROR"),
#endif
            C("math.round.awayfromzero", () => $"{Math.Round(2.5, MidpointRounding.AwayFromZero)}"),
            C("math.floor.ceiling", () => $"{Math.Floor(-1.5)},{Math.Ceiling(-1.5)},{Math.Truncate(-1.5)}"),
#if !JS
            C("math.clamp.sign", () => $"{Math.Clamp(15, 0, 10)},{Math.Sign(-3.2)},{Math.Abs(-4)}"),
#else
            C("math.clamp.sign", () => "WS-COMPILE-ERROR"),
#endif
            C("math.max.min.int", () => $"{Math.Max(3, 7)},{Math.Min(3L, 7L)}"),
            C("format.N2", () => (1234567.891).ToString("N2")),
            C("format.F3", () => (3.14159).ToString("F3")),
            C("format.P1", () => (0.256).ToString("P1")),
            C("format.D5", () => 42.ToString("D5")),
            C("format.X", () => 255.ToString("X") + "," + 255.ToString("x4")),
            C("format.E2", () => 12345.678.ToString("E2")),
            C("format.interp.align", () => $"[{42,5}][{42,-5}]"),
            C("format.interp.spec", () => $"{3.14159:F2}|{0.5:P0}|{1234:N0}"),
            C("format.custom", () => (1234.5).ToString("0,0.00") + "|" + (0.5).ToString("#.##")),
            C("string.format", () => string.Format("{0}-{1:F1}-{0}", 1, 2.25)),
            C("parse.int", () => $"{int.Parse("42")},{int.Parse(" -7 ")}"),
            C("parse.int.invalid", () => $"{int.Parse("4x2")}"),
            C("parse.int.overflow", () => $"{int.Parse("99999999999")}"),
            C("tryparse.int", () => { var ok = int.TryParse("12a", out var v); return $"{B(ok)},{v}"; }),
#if !JS
            C("parse.double", () => $"{double.Parse("1.5e3", CultureInfo.InvariantCulture)}"),
#else
            C("parse.double", () => "WS-COMPILE-ERROR"),
#endif
            C("tryparse.double", () => { var ok = double.TryParse("abc", out var v); return $"{B(ok)},{v}"; }),
            C("convert.toint32.rounds", () => $"{Convert.ToInt32(2.5)},{Convert.ToInt32(3.5)}"),
            C("int.tostring.negative.zero", () => $"{-0.0}"),
#if !JS
            C("char.arith", () => { char c = 'a'; c++; return $"{c},{(int)'A'},{(char)66}"; }),
#else
            C("char.arith", () => "WS-COMPILE-ERROR"),
#endif
            C("char.isdigit.letter", () => $"{B(char.IsDigit('7'))},{B(char.IsLetter('7'))},{B(char.IsWhiteSpace('\t'))},{B(char.IsUpper('Q'))}"),
#if !JS
            C("bitwise.shift", () => $"{1 << 33},{-16 >> 2},{-16 >>> 2},{1L << 40}"),
#else
            C("bitwise.shift", () => "WS-COMPILE-ERROR"),
#endif
            C("bitwise.ops", () => $"{6 & 3},{6 | 3},{6 ^ 3},{~5}"),
            C("compound.assign.ops", () => { int x = 10; x += 5; x *= 2; x -= 3; x /= 4; x %= 4; x <<= 2; return $"{x}"; }),
            C("nullable.arith", () => { int? a = null; int? b = 3; return $"{a + b}|{(a ?? 0) + b}|{B((a + b).HasValue)}"; }),
            C("nullable.compare", () => { int? a = null; return $"{B(a < 3)},{B(a == null)},{B(a >= 3)}"; }),
            C("int.minvalue.negate", () => { int m = int.MinValue; return $"{-m}"; }),
            C("double.equality.nan", () => $"{B(double.NaN == double.NaN)},{B(double.NaN.Equals(double.NaN))}"),
            C("float.precision", () => { float f = 0.1f; return $"{f}|{(double)f}"; }),
            C("int.compareto", () => $"{3.CompareTo(5)},{5.CompareTo(5)}," + "b".CompareTo("a")),
        };
    }
}

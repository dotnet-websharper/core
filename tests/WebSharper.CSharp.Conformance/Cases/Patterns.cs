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
using WebSharper;
using static Conformance.H;

namespace Conformance
{
    [JavaScript] public abstract class Shape { }
    [JavaScript] public class Circle : Shape { public double R; public Circle(double r) { R = r; } }
    [JavaScript] public class Rect : Shape { public double W, H; public Rect(double w, double h) { W = w; H = h; } }
    public enum Color { Red, Green = 5, Blue }
    [Flags] public enum Perm { None = 0, Read = 1, Write = 2, Exec = 4 }

    [JavaScript]
    public static class Patterns
    {
        static string Classify(object o) => o switch
        {
            null => "null",
            int i when i < 0 => "negative int",
            int i => $"int {i}",
            string { Length: 0 } => "empty string",
            string s => $"string {s}",
            Point { X: 0, Y: 0 } => "origin",
            Point(var x, var y) when x == y => $"diagonal {x}",
            Point p => $"point {p.X}",
            int[] { Length: > 2 } a => $"long array {a[0]}",
            _ => "other",
        };

        static double Area(Shape s) => s switch
        {
            Circle c => Math.Floor(Math.PI * c.R * c.R * 100) / 100,
            Rect { W: var w, H: var h } => w * h,
            _ => throw new ArgumentException("shape"),
        };

        static string Grade(int score) => score switch
        {
            >= 90 => "A",
            >= 80 and < 90 => "B",
            < 0 or > 100 => "invalid",
            _ => "C",
        };

        static string ListPat(int[] xs) => xs switch
        {
            [] => "empty",
            [var a] => $"one {a}",
            [var a, var b] => $"two {a},{b}",
            [var first, .., var last] => $"many {first}..{last}",
        };

        static string SliceLen(int[] xs) => xs is [_, .. var mid, _] ? $"{mid.Length}" : "short";

        static string Tuple(int a, bool b) => (a, b) switch
        {
            (0, _) => "zero",
            (_, true) => "flag",
            var (x, _) when x > 10 => "big",
            _ => "rest",
        };

        public static List<(string, Func<string>)> Cases() => new List<(string, Func<string>)>
        {
            C("switch.type.patterns", () => string.Join("|", new object[] { null, -1, 3, "", "x", new Point(0, 0), new Point(2, 2), new Point(1, 3), new[] { 7, 8, 9 }, 1.5 }.Select2(Classify))),
            C("switch.property.positional", () => $"{Area(new Circle(1))},{Area(new Rect(2, 3))}"),
            C("switch.throw.arm", () => $"{Area(null)}"),
            C("switch.relational", () => string.Join(",", new[] { 95, 85, 50, -1, 101 }.Select2(Grade))),
            C("switch.list.patterns", () => string.Join("|", new[] { new int[0], new[] { 1 }, new[] { 1, 2 }, new[] { 1, 2, 3, 4 } }.Select2(ListPat))),
            C("switch.list.slice", () => SliceLen(new[] { 1, 2, 3, 4, 5 }) + "," + SliceLen(new[] { 1 })),
            C("switch.tuple", () => $"{Tuple(0, true)},{Tuple(5, true)},{Tuple(20, false)},{Tuple(3, false)}"),
            C("switch.nomatch", () => { int x = 7; var r = x switch { 1 => "one", 2 => "two" }; return r; }),
            C("is.not.null", () => { object o = "s"; return B(o is not null) + "," + B(o is string { Length: 1 }); }),
            C("is.type.var", () => { object o = 42; if (o is int n && n > 40) return $"{n + 1}"; return "no"; }),
            C("as.cast", () => { object o = "x"; var s = o as string; var p = o as Point; return $"{s}|{(p == null ? "null" : "p")}"; }),
            C("cast.invalid", () => { object o = "x"; var p = (Point)o; return p.ToString(); }),
            C("cast.valid", () => { object o = new Dog(); Animal a = (Animal)o; object n = null; var d = (Dog)n; return a.Sound() + "," + B(d == null); }),
            C("unbox.invalid", () => { object o = "a"; return $"{(int)o}"; }),
            C("unbox.valid", () => { object o = 5, b = true; return $"{(int)o},{(bool)b}"; }),
            C("reference.equality.class", () => { var a = new Dog(); Animal b = a; object c = new Dog(); return B(a == b) + "," + B(a != c) + "," + B((object)null == null); }),
            C("switch.statement.goto", () => { var r = ""; for (int i = 0; i < 4; i++) { switch (i) { case 0: case 1: r += "a"; break; case 2: r += "b"; break; default: r += "c"; break; } } return r; }),
            C("switch.string", () => { string s = "two"; switch (s) { case "one": return "1"; case "two": return "2"; default: return "?"; } }),
            C("enum.tostring", () => $"{Color.Green}|{(Color)6}|{(int)Color.Blue}|{(Color)42}"),
            C("enum.flags.tostring", () => $"{Perm.Read | Perm.Write}|{Perm.None}"),
            C("enum.hasflag", () => B((Perm.Read | Perm.Exec).HasFlag(Perm.Exec)) + "," + B((Perm.Read).HasFlag(Perm.Write))),
            C("enum.parse", () => $"{Enum.Parse<Color>("Blue")}|{(int)Enum.Parse<Color>("Green")}"),
            C("enum.tryparse", () => { var ok = Enum.TryParse<Color>("Purple", out var c); return $"{B(ok)},{c}"; }),
            C("enum.getvalues", () => J(Enum.GetValues<Color>())),
            C("enum.switch", () => Color.Blue switch { Color.Red => "r", Color.Blue => "b", _ => "?" }),
        };
    }

    [JavaScript]
    public static class SeqExt
    {
        public static IEnumerable<R> Select2<T, R>(this IEnumerable<T> xs, Func<T, R> f) { foreach (var x in xs) yield return f(x); }
    }
}

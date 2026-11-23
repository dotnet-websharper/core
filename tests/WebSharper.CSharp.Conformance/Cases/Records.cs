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
using System.Linq;
using WebSharper;
using static Conformance.H;

namespace Conformance
{
    [JavaScript] public record Point(int X, int Y);
    [JavaScript] public record Point3(int X, int Y, int Z) : Point(X, Y);
    [JavaScript] public record Person(string Name, int Age) { public string Greeting => $"Hi {Name}"; }
    [JavaScript] public record Tagged(string Name, int[] Tags);
    [JavaScript] public record struct Vec(double X, double Y);
    [JavaScript] public readonly record struct Money(string Currency, long Cents);
    [JavaScript] public record WithBody { public string A { get; init; } public int B { get; init; } }
    [JavaScript] public record Nested(Point P, string Label);

    [JavaScript]
    public static class Records
    {
        public static List<(string, Func<string>)> Cases() => new List<(string, Func<string>)>
        {
            C("record.tostring", () => new Point(1, 2).ToString()),
            C("record.tostring.nested", () => new Nested(new Point(1, 2), "a").ToString()),
            C("record.tostring.derived", () => new Point3(1, 2, 3).ToString()),
            C("record.tostring.body", () => new WithBody { A = "x", B = 2 }.ToString()),
            C("record.equality", () => B(new Point(1, 2) == new Point(1, 2)) + "," + B(new Point(1, 2).Equals(new Point(1, 2))) + "," + B(new Point(1, 2) != new Point(2, 1))),
            C("record.equality.object", () => B(((object)new Point(1, 2)).Equals(new Point(1, 2)))),
            C("record.equality.derived", () => B(new Point(1, 2) == new Point3(1, 2, 0))),
            C("record.equality.nested", () => B(new Nested(new Point(1, 2), "a") == new Nested(new Point(1, 2), "a"))),
            C("record.equality.array.member", () => { var t = new[] { 1 }; return B(new Tagged("a", t) == new Tagged("a", t)) + "," + B(new Tagged("a", new[] { 1 }) == new Tagged("a", new[] { 1 })); }),
            C("record.hash.equal", () => B(new Point(1, 2).GetHashCode() == new Point(1, 2).GetHashCode())),
            C("record.with", () => (new Person("Ann", 30) with { Age = 31 }).ToString()),
            C("record.with.derived", () => { Point p = new Point3(1, 2, 3); var q = p with { X = 9 }; return q.ToString(); }),
            C("record.with.unchanged.original", () => { var a = new Person("Ann", 30); var b = a with { Name = "Bo" }; return a.Name + b.Name; }),
            C("record.deconstruct", () => { var (x, y) = new Point(3, 4); return $"{x},{y}"; }),
            C("record.computed.member", () => new Person("Ann", 30).Greeting),
            C("record.in.hashset", () => { var s = new HashSet<Point> { new Point(1, 2), new Point(1, 2), new Point(2, 1) }; return $"{s.Count}"; }),
            C("record.as.dict.key", () => { var d = new Dictionary<Point, string> { [new Point(1, 2)] = "a" }; return d[new Point(1, 2)]; }),
            C("record.distinct", () => $"{new[] { new Point(1, 2), new Point(1, 2) }.Distinct().Count()}"),
            C("record.groupby", () => $"{new[] { new Point(1, 2), new Point(1, 2), new Point(0, 0) }.GroupBy(p => p).Count()}"),
            C("record.struct.tostring", () => new Vec(1.5, 2).ToString()),
            C("record.struct.equality", () => B(new Vec(1, 2) == new Vec(1, 2)) + "," + B(new Money("EUR", 5).Equals(new Money("EUR", 5)))),
            C("record.struct.copy", () => { var a = new Vec(1, 2); var b = a; b.X = 9; return $"{a.X},{b.X}"; }),
            C("record.struct.with", () => (new Money("EUR", 5) with { Cents = 7 }).ToString()),
            C("record.struct.default", () => default(Vec).ToString()),
            C("record.equals.null", () => { Point p = null; return B(p == null) + "," + B(new Point(1, 2).Equals(null)); }),
            C("record.reference.equals", () => B(ReferenceEquals(new Point(1, 2), new Point(1, 2)))),
            C("record.init.props", () => { var w = new WithBody { A = "a" }; return $"{w.A},{w.B}"; }),
            C("record.list.contains", () => B(new List<Point> { new Point(1, 2) }.Contains(new Point(1, 2)))),
            C("record.indexof", () => $"{Array.IndexOf(new[] { new Point(0, 0), new Point(1, 2) }, new Point(1, 2))}"),
        };
    }
}

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
    [JavaScript] public interface IGreeter { string Name { get; } string Greet() => $"Hello {Name}"; }
    [JavaScript] public class Plain : IGreeter { public string Name => "plain"; }
    [JavaScript] public class Custom : IGreeter { public string Name => "custom"; public string Greet() => "Yo"; }

    [JavaScript] public abstract class Animal { public virtual string Sound() => "..."; public string Describe() => $"{GetName()} says {Sound()}"; protected abstract string GetName(); }
    [JavaScript] public class Dog : Animal { public override string Sound() => "woof"; protected override string GetName() => "dog"; }
    [JavaScript] public class Puppy : Dog { public override string Sound() => base.Sound() + "!"; protected override string GetName() => "puppy"; }
    [JavaScript] public class Hider : Dog { public new string Sound() => "hidden"; }

    [JavaScript]
    public class Box<T> where T : IComparable<T>
    {
        public T Value;
        public Box(T v) { Value = v; }
        public bool Bigger(T other) => Value.CompareTo(other) > 0;
#if !JS
        public static T Default() => default;
#else
        public static T Default() => (T)(object)null;
#endif
    }

    [JavaScript]
    public class Ctor
    {
        public static List<string> Log = new List<string>();
        public string A = Init("field");
        public Ctor() : this(1) { Log.Add("ctor()"); }
        public Ctor(int x) { Log.Add($"ctor({x})"); }
        static string Init(string s) { Log.Add(s); return s; }
    }

    [JavaScript]
    public class Required { public required string Name { get; init; } public int Age { get; init; } = 18; }

    [JavaScript]
    public class PrimaryCtor(string name, int age)
    {
        public string Describe() => $"{name}/{age}";
        public void Birthday() => age++;
    }

    [JavaScript]
    public class Eq
    {
        public int Id;
        public override bool Equals(object o) => o is Eq e && e.Id == Id;
        public override int GetHashCode() => Id;
    }

    [JavaScript]
    public class WithToString { public override string ToString() => "custom!"; }

    [JavaScript]
    public static class Types
    {
        static string Generic<T>(T x) => x?.ToString() ?? "null";
        static T Max<T>(T a, T b) where T : IComparable<T> => a.CompareTo(b) >= 0 ? a : b;

        public static List<(string, Func<string>)> Cases() => new List<(string, Func<string>)>
        {
            C("interface.default.method", () => { IGreeter a = new Plain(), b = new Custom(); return a.Greet() + "|" + b.Greet(); }),
            C("virtual.dispatch", () => new Dog().Describe() + "|" + new Puppy().Describe()),
            C("new.hiding", () => { var h = new Hider(); Dog d = h; return h.Sound() + "|" + d.Sound(); }),
            C("generic.constraint", () => B(new Box<int>(5).Bigger(3)) + "," + B(new Box<string>("a").Bigger("b"))),
            C("generic.default", () => $"{Box<int>.Default()}|{Box<string>.Default() ?? "null"}"),
            C("generic.method.max", () => $"{Max(3, 9)},{Max("b", "a")}"),
            C("generic.tostring", () => $"{Generic(1)},{Generic<string>(null)},{Generic(1.5)}"),
            C("ctor.chaining.order", () => { Ctor.Log.Clear(); new Ctor(); return J(Ctor.Log); }),
            C("required.init", () => { var r = new Required { Name = "n" }; return $"{r.Name},{r.Age}"; }),
            C("primary.ctor", () => { var p = new PrimaryCtor("x", 1); p.Birthday(); return p.Describe(); }),
            C("override.equals", () => B(new Eq { Id = 1 }.Equals(new Eq { Id = 1 })) + "," + B(new Eq { Id = 1 } == new Eq { Id = 1 }) + "," + $"{new[] { new Eq { Id = 1 }, new Eq { Id = 1 } }.Distinct().Count()}"),
            C("override.equals.dictionary", () => { var d = new Dictionary<Eq, int> { [new Eq { Id = 3 }] = 7 }; return $"{d[new Eq { Id = 3 }]}"; }),
            C("override.tostring", () => $"{new WithToString()}|{string.Concat(new WithToString(), "")}"),
            C("object.tostring.default", () => new Plain().ToString()),
#if !JS
            C("gettype.name", () => new Dog().GetType().Name),
            C("typeof.name", () => typeof(Dog).Name + "|" + typeof(List<int>).Name),
            C("typeof.fullname", () => typeof(Dog).FullName + "|" + typeof(Dog).Namespace + "|" + typeof(string).FullName + "|" + typeof(Dictionary<string, int>).Name),
#else
            C("gettype.name", () => "WS-COMPILE-ERROR"),
            C("typeof.name", () => "WS-COMPILE-ERROR"),
            C("typeof.fullname", () => "WS-COMPILE-ERROR"),
#endif
            C("is.base.interface", () => { object d = new Puppy(); return B(d is Animal) + B(d is IGreeter) + B(d is Dog); }),
            C("boxing.equality", () => { object a = 1, b = 1; return B(a == b) + "," + B(a.Equals(b)) + "," + B(Equals(a, b)); }),
            C("boxing.int.long", () => { object a = 1, b = 1L; return B(a.Equals(b)); }),
            C("anonymous.type", () => { var a = new { X = 1, Y = "y" }; var b = new { X = 1, Y = "y" }; return a.ToString() + "|" + B(a.Equals(b)); }),
            C("datetime.basic", () => { var d = new DateTime(2024, 2, 29, 13, 5, 9); return $"{d.Year}-{d.Month}-{d.Day} {d.Hour}:{d.Minute}:{d.Second} {d.DayOfWeek} {d.DayOfYear}"; }),
            C("datetime.arith", () => { var d = new DateTime(2024, 1, 31); return $"{d.AddMonths(1):yyyy-MM-dd}|{d.AddDays(1):yyyy-MM-dd}|{(new DateTime(2024, 3, 1) - d).TotalDays}"; }),
            C("datetime.format", () => new DateTime(2024, 7, 4, 9, 3, 0).ToString("yyyy-MM-dd HH:mm:ss") + "|" + new DateTime(2024, 7, 4).ToString("ddd, dd MMM yyyy")),
            C("datetime.iso", () => new DateTime(2024, 7, 4, 9, 3, 0, DateTimeKind.Utc).ToString("o")),
            C("datetime.parse", () => DateTime.Parse("2024-07-04T09:03:00").ToString("yyyy/MM/dd HH:mm")),
            C("timespan", () => { var t = TimeSpan.FromMinutes(90) + TimeSpan.FromSeconds(30); return $"{t}|{t.TotalHours}|{t.Hours}|{t.Minutes}"; }),
            C("timespan.format", () => TimeSpan.FromMilliseconds(3723004).ToString(@"hh\:mm\:ss\.fff")),
            C("timespan.format.standard", () => { var t = new TimeSpan(1, 2, 3, 4, 50); return t.ToString("c") + "|" + t.ToString("g") + "|" + t.ToString("G") + "|" + TimeSpan.FromMinutes(5).ToString("g"); }),
            C("timespan.format.custom", () => { var t = new TimeSpan(3, 4, 5, 6, 70); return t.ToString(@"d\.hh\:mm") + "|" + t.ToString("%d") + "|" + t.ToString(@"dd' days' h' h'") + "|" + t.ToString(@"ss\.FFF"); }),
            C("anonymous.type.nested", () => { var a = new { Name = (string)null, Flag = true, Inner = new { V = 1.5 } }; return $"{a}|{new { }}"; }),
            C("dateonly", () => new DateOnly(2024, 2, 28).AddDays(2).ToString("yyyy-MM-dd")),
            C("dateonly.ops", () => { var d = new DateOnly(2024, 1, 31); var e = d.AddMonths(1); return $"{e.Year}-{e.Month}-{e.Day},{e.DayOfWeek},{B(d < e)},{B(new DateOnly(2024, 3, 1) == d.AddDays(30))},{d.AddYears(1):yyyy/MM/dd}"; }),
            C("dateonly.tostring", () => $"{new DateOnly(2024, 7, 4)}|" + new DateOnly(2024, 7, 4).ToString()),
            C("guid.parse.format", () => { var g = Guid.Parse("0f8fad5b-d9cb-469f-a165-70867728950e"); return g.ToString() + "|" + g.ToString("N") + "|" + B(g == new Guid("0F8FAD5B-D9CB-469F-A165-70867728950E")); }),
            C("guid.empty", () => Guid.Empty.ToString()),
            C("random.seeded.range", () => { var r = new Random(1); var x = r.Next(10); return B(x >= 0 && x < 10); }),
            C("nullable.struct.getvalueordefault", () => { int? a = null; return $"{a.GetValueOrDefault()},{a.GetValueOrDefault(4)}"; }),
            C("nullable.value.null", () => { int? a = null; return $"{a.Value}"; }),
            C("convert.tostring.bool", () => true.ToString() + "|" + bool.Parse("TRUE")),
            C("string.gethashcode.stable", () => B("abc".GetHashCode() == "abc".GetHashCode())),
            C("valuetuple.compare", () => $"{Math.Sign((1, 2).CompareTo((1, 3)))}"),
            C("comparer.default", () => $"{Math.Sign(Comparer<string>.Default.Compare("a", "B"))},{Math.Sign(Comparer<int>.Default.Compare(2, 1))}"),
            C("equalitycomparer.default", () => B(EqualityComparer<Point>.Default.Equals(new Point(1, 1), new Point(1, 1)))),
        };
    }
}

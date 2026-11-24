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
    [JavaScript]
    public class Counter
    {
        public static int Instances;
        public int Value { get; private set; }
        public Counter() { Instances++; }
        public event Action<int> Changed;
        public void Inc() { Value++; Changed?.Invoke(Value); }
        public int this[int i] => Value * i;
        public static Counter operator +(Counter c, int n) { for (var i = 0; i < n; i++) c.Inc(); return c; }
        public static implicit operator int(Counter c) => c.Value;
    }

    [JavaScript]
    public class CustomException : Exception
    {
        public int Code { get; }
        public CustomException(string message, int code) : base(message) { Code = code; }
    }

    [JavaScript]
    public struct MutPoint { public int X; public void Move() { X++; } }

    [JavaScript]
    public static class Language
    {
        static string Opt(int a, int b = 2, string c = "c") => $"{a},{b},{c}";
        static int Sum(params int[] xs) => xs.Sum();
        static void Out(out int a, ref int b) { a = 5; b *= 2; }
        static string Names(string first = "f", string last = "l") => first + last;
        static IEnumerable<int> Fib() { int a = 0, b = 1; while (true) { yield return a; (a, b) = (b, a + b); } }
        static IEnumerable<string> WithFinally(List<string> log) { try { yield return "a"; yield return "b"; } finally { log.Add("finally"); } }
        static IEnumerable<int> YieldBreak(int n) { for (int i = 0; ; i++) { if (i >= n) yield break; yield return i; } }

        public static List<(string, Func<string>)> Cases() => new List<(string, Func<string>)>
        {
            C("closure.loop.capture.for", () => { var fs = new List<Func<int>>(); for (int i = 0; i < 3; i++) fs.Add(() => i); return J(fs.Select(f => f())); }),
            C("closure.loop.capture.foreach", () => { var fs = new List<Func<int>>(); foreach (var i in new[] { 0, 1, 2 }) fs.Add(() => i); return J(fs.Select(f => f())); }),
            C("closure.mutation", () => { int x = 1; Action inc = () => x++; inc(); inc(); return $"{x}"; }),
            C("local.function.recursive", () => { int Fact(int n) => n <= 1 ? 1 : n * Fact(n - 1); return $"{Fact(10)}"; }),
            C("static.local.function", () => { static int Sq(int n) => n * n; return $"{Sq(7)}"; }),
            C("optional.named.args", () => Opt(1) + "|" + Opt(1, c: "z") + "|" + Names(last: "L")),
            C("params", () => $"{Sum()},{Sum(1, 2, 3)},{Sum(new[] { 4, 5 })}"),
            C("out.ref", () => { int b = 3; Out(out var a, ref b); return $"{a},{b}"; }),
            C("ref.local", () => { var arr = new[] { 1, 2 }; ref int r = ref arr[1]; r = 9; return J(arr); }),
            C("iterator.lazy.infinite", () => J(Fib().Skip(10).Take(3))),
            C("iterator.finally.on.dispose", () => { var log = new List<string>(); foreach (var s in WithFinally(log)) { log.Add(s); break; } return J(log); }),
            C("iterator.yield.break", () => J(YieldBreak(3))),
            C("iterator.reenumerate", () => { var e = YieldBreak(2); return J(e) + J(e); }),
            C("exception.custom", () => { try { throw new CustomException("boom", 42); } catch (CustomException e) { return $"{e.Message},{e.Code}"; } }),
            C("exception.message", () => { try { throw new InvalidOperationException("bad op"); } catch (Exception e) { return e.Message; } }),
            C("exception.filter", () => { try { throw new CustomException("x", 2); } catch (CustomException e) when (e.Code == 1) { return "one"; } catch (CustomException e) when (e.Code == 2) { return "two"; } }),
            C("exception.finally.order", () => { var log = new List<string>(); try { try { log.Add("try"); throw new Exception("e"); } finally { log.Add("finally"); } } catch { log.Add("catch"); } return J(log); }),
            C("exception.rethrow", () => { try { try { throw new ArgumentException("inner"); } catch (Exception) { throw; } } catch (ArgumentException e) { return e.Message; } }),
            C("exception.inner", () => { var e = new Exception("outer", new Exception("inner")); return e.InnerException.Message; }),
            C("exception.null.deref", () => { string s = null; return $"{s.Length}"; }),
            C("exception.tostring.prefix", () => new InvalidOperationException("m").Message),
            C("using.dispose.order", () => { var log = new List<string>(); using (var a = new Disp(log, "a")) using (var b = new Disp(log, "b")) { log.Add("body"); } return J(log); }),
            C("using.declaration", () => { var log = new List<string>(); void Run() { using var a = new Disp(log, "a"); log.Add("body"); } Run(); return J(log); }),
            C("events", () => { var c = new Counter(); var log = new List<int>(); Action<int> h = v => log.Add(v); c.Changed += h; c.Inc(); c.Changed -= h; c.Inc(); return J(log) + c.Value; }),
            C("operator.overload.implicit", () => { var c = new Counter(); c += 3; int v = c; return $"{v},{c[2]}"; }),
            C("static.field", () => { Counter.Instances = 0; new Counter(); new Counter(); return $"{Counter.Instances}"; }),
            C("delegate.multicast", () => { var log = new List<string>(); Action a = () => log.Add("1"); a += () => log.Add("2"); a(); return J(log); }),
            C("delegate.multicast.return", () => { Func<int> f = () => 1; f += () => 2; return $"{f()}"; }),
            C("func.compose", () => { Func<int, int> inc = x => x + 1; Func<int, int> dbl = x => x * 2; Func<int, int> both = x => dbl(inc(x)); return $"{both(3)}"; }),
            C("struct.copy.semantics", () => { var a = new MutPoint(); var b = a; b.Move(); var arr = new[] { a }; arr[0].Move(); var list = new List<MutPoint> { a }; var l0 = list[0]; l0.Move(); return $"{a.X},{b.X},{arr[0].X},{list[0].X}"; }),
            C("struct.default.equality", () => B(new MutPoint().Equals(new MutPoint())) + "," + B(new MutPoint { X = 1 }.Equals(new MutPoint()))),
            C("null.conditional", () => { Counter c = null; int? v = c?.Value; string s = null; return $"{v?.ToString() ?? "none"},{s?.Length ?? -1}"; }),
            C("null.coalescing.assign", () => { List<int> l = null; l ??= new List<int>(); l.Add(1); string s = null; s ??= "d"; return $"{l.Count},{s}"; }),
            C("conditional.chain", () => { int x = 5; return x > 3 ? x > 4 ? "big" : "mid" : "small"; }),
            C("goto.labels", () => { int i = 0; again: i++; if (i < 3) goto again; return $"{i}"; }),
            C("while.dowhile.continue", () => { var r = ""; int i = 0; do { i++; if (i == 2) continue; r += i; } while (i < 4); while (i > 0) { i--; if (i == 1) break; } return r + i; }),
            C("lazy", () => { var n = 0; var l = new Lazy<int>(() => ++n); var a = l.Value; var b = l.Value; return $"{a},{b},{n},{B(l.IsValueCreated)}"; }),
            C("default.literal", () => { int i = default; string s = default; return $"{i},{s ?? "null"}"; }),
            C("nameof", () => nameof(Counter.Value) + nameof(Language)),
            C("checked.unchecked.block", () => { int m = int.MaxValue; int r = unchecked(m + 1); return $"{r}"; }),
            C("discard.assign", () => { _ = int.TryParse("5", out var v); return $"{v}"; }),
            C("lambda.static.default.params", () => { var f = (int a, int b = 10) => a + b; return $"{f(1)},{f(1, 2)}"; }),
        };
    }

    [JavaScript]
    public class Disp : IDisposable
    {
        readonly List<string> log; readonly string name;
        public Disp(List<string> log, string name) { this.log = log; this.name = name; }
        public void Dispose() => log.Add("dispose " + name);
    }
}

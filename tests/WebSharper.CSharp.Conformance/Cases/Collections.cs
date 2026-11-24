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
    public static class Collections
    {
        public static List<(string, Func<string>)> Cases() => new List<(string, Func<string>)>
        {
            C("list.ops", () => { var l = new List<int> { 3, 1, 2 }; l.Add(5); l.Insert(0, 9); l.Remove(1); l.RemoveAt(1); return J(l) + l.Count; }),
            C("list.sort.default", () => { var l = new List<int> { 10, 9, 1, 100 }; l.Sort(); return J(l); }),
            C("list.sort.strings", () => { var l = new List<string> { "b", "B", "a", "A" }; l.Sort(StringComparer.Ordinal); return J(l); }),
            C("list.sort.comparison", () => { var l = new List<int> { 1, 3, 2 }; l.Sort((a, b) => b.CompareTo(a)); return J(l); }),
            C("list.removeall.findindex", () => { var l = new List<int> { 1, 2, 3, 4, 5 }; var n = l.RemoveAll(x => x % 2 == 0); return $"{n},{J(l)},{l.FindIndex(x => x > 1)}"; }),
            C("list.indexer.outofrange", () => { var l = new List<int> { 1 }; return $"{l[5]}"; }),
            C("list.getrange.reverse", () => { var l = new List<int> { 1, 2, 3, 4 }; l.Reverse(); return J(l.GetRange(1, 2)); }),
            C("list.binarysearch", () => $"{new List<int> { 1, 3, 5, 7 }.BinarySearch(5)},{new List<int> { 1, 3, 5, 7 }.BinarySearch(4)}"),
            C("list.modify.during.foreach", () => { var l = new List<int> { 1, 2 }; foreach (var x in l) l.Add(x); return "no exn"; }),
            C("array.outofrange", () => { var a = new int[2]; return $"{a[2]}"; }),
            C("array.default.values", () => J(new int[3]) + "|" + J(new string[2].Select(s => s ?? "null")) + "|" + J(new bool[2].Select(B))),
            C("array.sort.reverse", () => { var a = new[] { 3, 1, 2 }; Array.Sort(a); Array.Reverse(a); return J(a); }),
            C("array.resize.copy", () => { var a = new[] { 1, 2 }; Array.Resize(ref a, 4); var b = new int[4]; Array.Copy(a, b, 2); return J(a) + J(b); }),
#if !JS
            C("array.2d", () => { var m = new int[2, 3]; m[1, 2] = 5; return $"{m.GetLength(0)},{m.GetLength(1)},{m[1, 2]},{m.Length}"; }),
#else
            C("array.2d", () => "WS-COMPILE-ERROR"),
#endif
            C("array.jagged", () => { var j = new int[2][]; j[0] = new[] { 1 }; j[1] = new[] { 2, 3 }; return $"{j[1][1]},{j.Sum(r => r.Length)}"; }),
            C("array.equality.ref", () => B(new[] { 1 }.Equals(new[] { 1 })) + "," + B(new[] { 1 }.SequenceEqual(new[] { 1 }))),
            C("array.fill.indexof", () => { var a = new int[3]; Array.Fill(a, 7); return $"{J(a)},{Array.IndexOf(a, 7)},{Array.IndexOf(a, 8)}"; }),
            C("array.range.slice", () => J(new[] { 1, 2, 3, 4, 5 }[1..^1]) + J(new[] { 1, 2, 3 }[^2..])),
            C("array.covariance", () => { object[] o = new string[] { "a" }; return $"{o[0]}"; }),
            C("dict.insertion.order", () => { var d = new Dictionary<string, int> { ["b"] = 1, ["a"] = 2, ["c"] = 3 }; d.Remove("a"); d["d"] = 4; return J(d.Keys); }),
            C("dict.int.keys.order", () => { var d = new Dictionary<int, string> { [10] = "a", [2] = "b", [1] = "c" }; return J(d.Keys); }),
            C("dict.negative.keys.order", () => { var d = new Dictionary<int, int> { [5] = 0, [-3] = 0, [0] = 0, [-1] = 0 }; return J(d.Keys) + J(new HashSet<int> { 9, -2, 4 }); }),
            C("dict.trygetvalue.defaults", () => { var d = new Dictionary<string, bool>(); var e = new Dictionary<string, double>(); d.TryGetValue("x", out var b); e.TryGetValue("x", out var x); return $"{B(b)},{x}"; }),
            C("dict.comparer.ordinal", () => { var d = new Dictionary<string, int>(StringComparer.OrdinalIgnoreCase) { ["a"] = 1 }; d["A"] = 2; var h = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { "x", "X", "y" }; return $"{d.Count},{d["a"]},{h.Count},{StringComparer.Ordinal.Compare("a", "B")},{StringComparer.OrdinalIgnoreCase.Equals("ab", "AB")}"; }),
            C("sorteddictionary.ops", () => { var d = new SortedDictionary<int, string>(Comparer<int>.Create((a, b) => b.CompareTo(a))) { [1] = "a", [3] = "c", [2] = "b" }; d.Remove(3); return J(d.Keys) + J(d.Values) + $"{d.Count},{B(d.ContainsKey(2))},{B(d.TryGetValue(9, out var v))}"; }),
            C("sortedset.ops", () => { var s = new SortedSet<string> { "pear", "apple", "fig" }; s.Remove("fig"); return J(s) + $"{s.Min},{s.Max},{B(s.Add("apple"))},{s.Count}"; }),
            C("dict.missing.key", () => new Dictionary<string, int>()["x"].ToString()),
            C("dict.add.duplicate", () => { var d = new Dictionary<string, int>(); d.Add("a", 1); d.Add("a", 2); return "no exn"; }),
            C("dict.trygetvalue", () => { var d = new Dictionary<string, int> { ["a"] = 1 }; var ok = d.TryGetValue("b", out var v); return $"{B(ok)},{v},{B(d.ContainsKey("a"))}"; }),
            C("dict.kvp.tostring", () => new Dictionary<string, int> { ["a"] = 1 }.First().ToString()),
            C("dict.tryadd.getordefault", () => { var d = new Dictionary<string, int>(); d.TryAdd("a", 1); var b = d.TryAdd("a", 2); return $"{B(b)},{d["a"]},{d.GetValueOrDefault("z", 9)}"; }),
            C("dict.comparer.ignorecase", () => { var d = new Dictionary<string, int>(StringComparer.OrdinalIgnoreCase) { ["Key"] = 1 }; return B(d.ContainsKey("KEY")); }),
            C("dict.tuple.key", () => { var d = new Dictionary<(int, string), int> { [(1, "a")] = 5 }; return $"{d[(1, "a")]}"; }),
            C("hashset.ops", () => { var s = new HashSet<int> { 1, 2, 3 }; s.IntersectWith(new[] { 2, 3, 4 }); s.UnionWith(new[] { 9 }); return J(s.OrderBy(x => x)) + B(s.Add(2)); }),
            C("hashset.string.order", () => J(new HashSet<string> { "z", "a", "m" })),
            C("stack.queue", () => { var s = new Stack<int>(); s.Push(1); s.Push(2); var q = new Queue<int>(); q.Enqueue(1); q.Enqueue(2); return $"{s.Pop()},{s.Peek()},{q.Dequeue()},{q.Count},{J(new Stack<int>(new[] { 1, 2, 3 }))}"; }),
            C("stack.pop.empty", () => $"{new Stack<int>().Pop()}"),
            C("sorteddictionary", () => { var d = new SortedDictionary<string, int> { ["b"] = 1, ["a"] = 2 }; return J(d.Keys); }),
            C("sortedset", () => J(new SortedSet<int> { 5, 1, 3, 1 })),
            C("linkedlist", () => { var l = new LinkedList<int>(); l.AddLast(2); l.AddFirst(1); l.AddLast(3); return J(l) + l.First.Value; }),
            C("collection.expression", () => { int[] a = [1, 2, 3]; List<int> l = [.. a, 4]; return J(l); }),
#if !JS
            C("span.sum", () => { Span<int> s = stackalloc int[] { 1, 2, 3 }; var t = 0; foreach (var x in s) t += x; return $"{t}"; }),
#else
            C("span.sum", () => "WS-COMPILE-ERROR"),
#endif
            C("readonly.wrapper", () => { var l = new List<int> { 1 }; var r = l.AsReadOnly(); l.Add(2); return $"{r.Count}"; }),
            C("list.capacity.tostring", () => new List<int>().ToString()),
            C("tuple.tostring", () => (1, "a", true).ToString() + "|" + Tuple.Create(1, 2).ToString()),
            C("tuple.equality", () => B((1, "a") == (1, "a")) + "," + B((1, "a").Equals((1, "b")))),
            C("tuple.named", () => { var t = (Name: "x", Age: 3); var (n, a) = t; return $"{t.Name},{t.Item2},{n},{a}"; }),
            C("tuple.swap", () => { int a = 1, b = 2; (a, b) = (b, a); return $"{a},{b}"; }),
        };
    }
}

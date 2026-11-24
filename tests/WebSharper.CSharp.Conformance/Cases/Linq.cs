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
    public static class LinqCases
    {
        static IEnumerable<int> Counted(List<string> log) { for (int i = 1; i <= 5; i++) { log.Add($"y{i}"); yield return i; } }

        public static List<(string, Func<string>)> Cases() => new List<(string, Func<string>)>
        {
            C("linq.where.select", () => J(Enumerable.Range(1, 10).Where(x => x % 3 == 0).Select(x => x * x))),
            C("linq.select.index", () => J(new[] { "a", "b" }.Select((s, i) => $"{i},{s}"))),
            C("linq.orderby.stable", () => J(new[] { "bb", "a", "cc", "d", "ee" }.OrderBy(s => s.Length))),
            C("linq.orderby.thenby", () => J(new[] { (2, "b"), (1, "z"), (2, "a"), (1, "a") }.OrderBy(t => t.Item1).ThenByDescending(t => t.Item2).Select(t => $"{t.Item1},{t.Item2}"))),
            C("linq.orderby.strings.culture", () => J(new[] { "b", "B", "a", "A", "_" }.OrderBy(s => s))),
            C("linq.order", () => J(new[] { 3, 1, 2 }.Order())),
            C("linq.groupby", () => J(new[] { 1, 2, 3, 4, 5 }.GroupBy(x => x % 2).Select(g => $"{g.Key}:{J(g)}"))),
            C("linq.groupby.resultselector", () => J(new[] { "ab", "c", "de" }.GroupBy(s => s.Length, (k, g) => $"{k}={g.Count()}"))),
            C("linq.tolookup", () => { var l = new[] { "ab", "c", "de" }.ToLookup(s => s.Length); return $"{l[2].Count()},{l[9].Count()},{l.Count}"; }),
            C("linq.tolookup.order", () => { var l = new[] { "bb", "a", "cc", "d" }.ToLookup(s => s.Length, s => s.ToUpper()); return string.Join("|", l.Select(g => $"{g.Key}:{J(g)}")) + "," + B(l.Contains(2)); }),
            C("linq.take.range.fromend", () => J(Enumerable.Range(1, 5).Take(^2..)) + J(new[] { 1, 2 }.Take(1..9)) + J(new[] { 1, 2 }.Take(3..1))),
            C("linq.todictionary.dup", () => $"{new[] { 1, 1 }.ToDictionary(x => x).Count}"),
            C("linq.first.empty", () => $"{new int[0].First()}"),
            C("linq.firstordefault", () => $"{new int[0].FirstOrDefault()},{new string[0].FirstOrDefault() ?? "null"},{new[] { 1, 2 }.FirstOrDefault(x => x > 5, -1)}"),
            C("linq.single.many", () => $"{new[] { 1, 2 }.Single()}"),
            C("linq.last.elementat", () => $"{new[] { 1, 2, 3 }.Last()},{new[] { 1, 2, 3 }.ElementAt(1)},{new[] { 1 }.ElementAtOrDefault(5)}"),
            C("linq.aggregate", () => $"{Enumerable.Range(1, 5).Aggregate((a, b) => a * b)},{new[] { "a", "b" }.Aggregate("x", (acc, s) => acc + s, r => r.ToUpper())}"),
            C("linq.sum.average", () => $"{new[] { 1, 2, 3, 4 }.Average()},{new[] { 1.5, 2.5 }.Sum()},{new[] { 2, 4 }.Average(x => x * 1.0)}"),
            C("linq.average.empty", () => $"{new int[0].Average()}"),
            C("linq.sum.overflow", () => $"{new[] { int.MaxValue, 1 }.Sum()}"),
            C("linq.min.max", () => $"{new[] { 3, 1, 2 }.Min()},{new[] { 3, 1, 2 }.Max()},{new[] { "bb", "a" }.MaxBy(s => s.Length)},{new[] { "bb", "a" }.MinBy(s => s.Length)}"),
            C("linq.min.empty", () => $"{new int[0].Min()}"),
            C("linq.distinctby.unionby", () => J(new[] { "a", "A", "b" }.DistinctBy(s => s.ToLower())) + J(new[] { 1, 2 }.Union(new[] { 2, 3 }))),
            C("linq.except.intersect", () => J(new[] { 1, 2, 3, 2 }.Except(new[] { 2 })) + J(new[] { 1, 2, 3 }.Intersect(new[] { 3, 2 }))),
            C("linq.zip", () => J(new[] { 1, 2, 3 }.Zip(new[] { "a", "b" }, (n, s) => $"{n},{s}")) + J(new[] { 1, 2 }.Zip(new[] { 'x', 'y' }).Select(t => $"{t.First},{t.Second}"))),
            C("linq.skip.take", () => J(Enumerable.Range(1, 10).Skip(2).Take(3)) + J(Enumerable.Range(1, 5).SkipWhile(x => x < 3).TakeWhile(x => x < 5)) + J(Enumerable.Range(1, 5).TakeLast(2)) + J(Enumerable.Range(1, 5).SkipLast(3))),
            C("linq.take.range", () => J(Enumerable.Range(1, 10).Take(2..4))),
            C("linq.chunk", () => J(Enumerable.Range(1, 5).Chunk(2).Select(c => J(c)))),
            C("linq.selectmany", () => J(new[] { "ab", "cd" }.SelectMany(s => s.ToCharArray()))),
            C("linq.join", () => J(new[] { (1, "a"), (2, "b") }.Join(new[] { (2, "x"), (1, "y"), (1, "z") }, l => l.Item1, r => r.Item1, (l, r) => l.Item2 + r.Item2))),
            C("linq.groupjoin", () => J(new[] { 1, 2 }.GroupJoin(new[] { 1, 1, 3 }, a => a, b => b, (a, bs) => $"{a}:{bs.Count()}"))),
            C("linq.any.all.contains", () => $"{B(new int[0].All(x => x > 0))},{B(new[] { 1 }.Any())},{B(new[] { 1, 2 }.Contains(2))}"),
            C("linq.count.predicate", () => $"{Enumerable.Range(1, 10).Count(x => x > 7)},{new[] { 1 }.LongCount()}"),
            C("linq.repeat.empty.concat", () => J(Enumerable.Repeat("x", 3).Concat(Enumerable.Empty<string>()).Append("y").Prepend("w"))),
            C("linq.reverse.defaultifempty", () => J(new[] { 1, 2 }.Reverse()) + J(new int[0].DefaultIfEmpty(7))),
            C("linq.sequenceequal", () => B(new[] { 1, 2 }.SequenceEqual(new List<int> { 1, 2 }))),
            C("linq.cast.oftype", () => J(new object[] { 1, "a", 2, null }.OfType<int>()) + J(new object[] { "a", "b" }.Cast<string>())),
            C("linq.cast.invalid", () => J(new object[] { 1, "a" }.Cast<int>())),
            C("linq.cast.valid", () => J(new object[] { "a", null, "b" }.Cast<string>().Select(s => s ?? "null")) + J(new object[] { 1, 2 }.Cast<int>())),
            C("linq.sum.selector.overflow", () => $"{new[] { 1, 2 }.Sum(x => int.MaxValue / 2 + x)}"),
            C("linq.sum.long", () => $"{new[] { long.MaxValue - 1, 1L }.Sum()}"),
            C("linq.deferred", () => { var log = new List<string>(); var q = Counted(log).Where(x => { log.Add($"w{x}"); return x % 2 == 0; }).Take(1); log.Add("built"); var r = q.ToList(); return J(log) + J(r); }),
            C("linq.query.syntax", () => J(from x in Enumerable.Range(1, 6) let sq = x * x where sq > 5 orderby x descending select $"{x}:{sq}")),
            C("linq.query.group", () => J(from w in new[] { "apple", "avocado", "banana" } group w by w[0] into g select $"{g.Key},{g.Count()}")),
            C("linq.toarray.tolist.tohashset", () => $"{new[] { 1, 1, 2 }.ToHashSet().Count},{new[] { 1 }.ToList().Count}"),
            C("linq.range.negative", () => J(Enumerable.Range(1, -1))),
            C("linq.index", () => J(new[] { "a", "b" }.Index().Select(t => $"{t.Index},{t.Item}"))),
            C("linq.countby.aggregateby", () => J(new[] { "a", "b", "a" }.CountBy(s => s).Select(kv => $"{kv.Key},{kv.Value}"))),
        };
    }
}

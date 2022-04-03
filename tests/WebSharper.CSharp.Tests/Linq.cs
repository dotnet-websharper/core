// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;
using static WebSharper.JavaScript.Pervasives;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("Linq IEnumerable extensions")]
    class Linq : TestCategory
    {
        // TODO: replace uses of As<_> with casts.

        [Test]
        public void Aggregate()
        {
            Equal(arr.Aggregate(Add), 21, "Reduce (no init value)");
            Raises(() => (new int[] { }).Aggregate(Add), "Reduce fails on empty");
            Equal(arr.Aggregate(5, Add), 26, "Fold (init value)");
            Equal(arr.Aggregate(5, Add, Add5), 31, "Fold (init value) + Map");
        }

        [Test]
        public void All()
        {
            IsTrue(arr.All(x => x > 0 && x < 7));
            IsTrue(empty.All(_ => false), "empty.All(f) always true");
        }

        [Test]
        public void Any()
        {
            IsTrue(arr.Any(), "nonempty.Any() = true");
            IsFalse(empty.Any(), "empty.Any() = false");
            IsTrue(arr.Any(x => x == 3), "Any(f) true");
            IsFalse(arr.Any(x => x == 7), "Any(f) false");
            IsFalse(empty.Any(x => true), "empty.Any(f) always false");
        }

        [Test]
        public void Average()
        {
            Equal(arr.Average(), 3.5, "int[].Average()");
            Equal(narr.Average(), 3.5, "int?[].Average()");
            Equal(nnarr.Average(), null, "int?[].Average() = null when all null");
            Equal(arr.Average(x => x * 3), 10.5, "int[].Average(f)");
            Equal(arr.Average(x => x % 2 == 0 ? null : As<int?>(x)), 3, "int[].Average(f?)");
            Raises(() => empty.Average(), "empty.Average() throws");
        }

        [Test]
        public void Concat()
        {
            var x = new int[] { 1, 2 };
            var y = new int[] { 3, 4, 5, 6 };
            Equal(x.Concat(y).ToArray(), arr);
        }

        [Test]
        public void Contains()
        {
            var x = new int[] { 1, 2, 3, 10 };
            var odds = new int[] { 1, 5, 11 };
            IsTrue(x.Contains(1));
            IsFalse(x.Contains(4));
            IsTrue(odds.Contains(3, mod2));
            IsFalse(odds.Contains(4, mod2));
        }

        [Test]
        public void Count()
        {
            Equal(arr.Count(), 6);
            Equal(narr.Count(x => x == null), 3);
        }

        [Test]
        public void DefaultIfEmpty()
        {
            Equal(arr.DefaultIfEmpty(), arr, "No default value, not empty");
            Equal(empty.DefaultIfEmpty(), new int[] { 0 }, "No default value, empty");
            Equal(arr.DefaultIfEmpty(1), arr, "With default value, not empty");
            Equal(empty.DefaultIfEmpty(1), new int[] { 1 }, "With default value, empty");
        }

        [Test]
        public void Distinct()
        {
            Equal((new int[] { 1, 2, 3, 2, 1 }).Distinct().ToArray(), new int[] { 1, 2, 3 }, "Simple");
            Equal((new int[] { 1, 2, 3, 2, 1 }).Distinct(mod2).ToArray(), new int[] { 1, 2 }, "With EqualityComparer");
        }

        [Test]
        public void ElementAt()
        {
            Equal(arr.ElementAt(2), 3, "Within bounds");
            Raises(() => arr.ElementAt(6), "Out of bounds");
        }

        [Test]
        public void ElementAtOrDefault()
        {
            Equal(arr.ElementAtOrDefault(2), 3, "Within bounds");
            Equal(arr.ElementAtOrDefault(6), 0, "Out of bounds int");
            Equal(narr.ElementAtOrDefault(20), null, "Out of bounds nullable");
        }

        [Test]
        public void Except()
        {
            Equal(arr.Except(new int[] { 3, 5 }).ToArray(), new int[] { 1, 2, 4, 6 }, "Simple case");
            Equal(arr.Except(empty).ToArray(), arr, "Except empty = original");
            Equal(arr.Except(new int[] { 1 }, mod2).Count(), 1, "Non-empty with EqualityComparer");
        }

        [Test]
        public void First()
        {
            Equal(arr.First(), 1, "Non-empty");
            Raises(() => empty.First(), "Empty");
            Equal(arr.First(x => x % 2 == 0), 2, "Non-empty with predicate");
            Raises(() => arr.First(x => x > 10), "Non-empty with failing predicate");
        }

        [Test]
        public void FirstOrDefault()
        {
            Equal(arr.FirstOrDefault(), 1, "Non-empty");
            Equal(empty.FirstOrDefault(), 0, "Empty int");
            Equal(arr.FirstOrDefault(x => x % 2 == 0), 2, "Non-empty with predicate");
            Equal(arr.FirstOrDefault(x => x > 10), 0, "Non-empty with failing predicate");
        }

        [Test]
        public void GroupBy()
        {
            var g1 = arr.GroupBy(x => x % 2).ToArray();
            Equal(g1.ElementAt(0).Key, 1, "source, keySelector");
            Equal(g1.ElementAt(0).ToArray(), new int[] { 1, 3, 5 }, "source, keySelector");
            Equal(g1.ElementAt(1).Key, 0, "source, keySelector");
            Equal(g1.ElementAt(1).ToArray(), new int[] { 2, 4, 6 }, "source, keySelector");
            var g2 = arr.GroupBy((x => x - 1), mod2).ToArray();
            Equal(g2.ElementAt(0).Key, 0, "source, keySelector, comparer");
            Equal(g2.ElementAt(0).ToArray(), new int[] { 1, 3, 5 }, "source, keySelector, comparer");
            Equal(g2.ElementAt(1).Key, 1, "source, keySelector, comparer");
            Equal(g2.ElementAt(1).ToArray(), new int[] { 2, 4, 6 }, "source, keySelector, comparer");
            var g3 = arr.GroupBy((x => x % 2), (k, rs) => rs.Concat(new[] { k }).ToArray()).ToArray();
            Equal(g3.ElementAt(0), new[] { 1, 3, 5, 1 }, "source, keySelector, resultSelector");
            Equal(g3.ElementAt(1), new[] { 2, 4, 6, 0 }, "source, keySelector, resultSelector");
            var g4 = arr.GroupBy((x => x % 2), (x => x + 10)).ToArray();
            Equal(g4.ElementAt(0).Key, 1, "source, keySelector, elementSelector");
            Equal(g4.ElementAt(0).ToArray(), new int[] { 11, 13, 15 }, "source, keySelector, elementSelector");
            Equal(g4.ElementAt(1).Key, 0, "source, keySelector, elementSelector");
            Equal(g4.ElementAt(1).ToArray(), new int[] { 12, 14, 16 }, "source, keySelector, elementSelector");
            var g5 = arr.GroupBy((x => x - 1), (x => x + 10), mod2).ToArray();
            Equal(g5.ElementAt(0).Key, 0, "source, keySelector, elementSelector, comparer");
            Equal(g5.ElementAt(0).ToArray(), new int[] { 11, 13, 15 }, "source, keySelector, elementSelector, comparer");
            Equal(g5.ElementAt(1).Key, 1, "source, keySelector, elementSelector, comparer");
            Equal(g5.ElementAt(1).ToArray(), new int[] { 12, 14, 16 }, "source, keySelector, elementSelector, comparer");
            var g6 = arr.GroupBy((x => x - 1), ((k, rs) => rs.Concat(new[] { k }).ToArray()), mod2).ToArray();
            Equal(g6.ElementAt(0), new int[] { 1, 3, 5, 0 }, "source, keySelector, resultSelector, comparer");
            Equal(g6.ElementAt(1), new int[] { 2, 4, 6, 1 }, "source, keySelector, resultSelector, comparer");
            var g7 = arr.GroupBy((x => x % 2), (x => x + 10), ((k, rs) => rs.Concat(new[] { k }).ToArray())).ToArray();
            Equal(g7.ElementAt(0), new int[] { 11, 13, 15, 1 }, "source, keySelector, elementSelector, resultSelector");
            Equal(g7.ElementAt(1), new int[] { 12, 14, 16, 0 }, "source, keySelector, elementSelector, resultSelector");
            var g8 = arr.GroupBy((x => x - 1), (x => x + 10), ((k, rs) => rs.Concat(new[] { k }).ToArray()), mod2).ToArray();
            Equal(g8.ElementAt(0), new int[] { 11, 13, 15, 0 }, "source, keySelector, elementSelector, resultSelector, comparer");
            Equal(g8.ElementAt(1), new int[] { 12, 14, 16, 1 }, "source, keySelector, elementSelector, resultSelector, comparer");
        }

        [Test]
        public void GroupJoin()
        {
            var g1 = arr.GroupJoin(new[] { "aaa", "bb", "cc", "d" },
                outer => outer,
                inner => inner.Length,
                (outer, inners) => (
                    new[] { outer.ToString() })
                        .Concat(inners)
                        .ToArray());
            var expected1 = new[] {
                new[] { "1", "d" },
                new[] { "2", "bb", "cc" },
                new[] { "3", "aaa" },
                new[] { "4" },
                new[] { "5" },
                new[] { "6" } };
            Equal(g1.ToArray(), expected1, "Base case");
            var g2 = (new[] { 1, 2 }).GroupJoin(new[] { "aaa", "bb", "cc", "d" },
                outer => outer,
                inner => inner.Length,
                (outer, inners) => (
                    new[] { outer.ToString() })
                        .Concat(inners)
                        .ToArray(),
                mod2);
            var expected2 = new[] {
                new[] { "1", "aaa", "d" },
                new[] { "2", "bb", "cc" } };
            Equal(g2.ToArray(), expected2, "With equality comparer");
        }

        [Test]
        public void Intersect()
        {
            var a = new int[] { 2, 3, 8, 9 };
            Equal(arr.Intersect(a).ToArray(), new int[] { 2, 3 }, "Non-empty");
            Equal(a.Intersect(arr).ToArray(), new int[] { 2, 3 }, "Non-empty");
            Equal(arr.Intersect(empty).ToArray(), empty, ".Intersect(empty)");
            Equal(empty.Intersect(arr).ToArray(), empty, "empty.Intersect()");
            Equal(arr.Intersect(new int[] { 1 }, mod2).ToArray(), new int[] { 1 }, "Non-empty with EqualityComparer");
        }

        [Test]
        public void Join()
        {
            var g1 = arr.Join(new[] { "aaa", "bb", "cc", "d" },
                outer => outer,
                inner => inner.Length,
                (outer, inner) => outer.ToString() + inner);
            var expected1 = new[] { "1d", "2bb", "2cc", "3aaa" };
            Equal(g1.ToArray(), expected1, "Base case");
            var g2 = arr.Join(new[] { "aaa", "bb", "cc", "d" },
                outer => outer,
                inner => inner.Length,
                (outer, inner) => outer.ToString() + inner,
                mod2);
            var expected2 = new[] { "1aaa", "1d", "2bb", "2cc" };
            Equal(g2.ToArray(), expected2, "With equality comparer");
        }

        [Test]
        public void Last()
        {
            Equal(arr.Last(), 6, "Non-empty");
            Raises(() => empty.Last(), "Empty");
            Equal(arr.Last(x => x < 5), 4, "Non-empty with predicate");
            Raises(() => arr.Last(x => x < 0), "Non-empty with failing predicate");
        }

        [Test]
        public void LastOrDefault()
        {
            Equal(arr.LastOrDefault(), 6, "Non-empty");
            Equal(empty.LastOrDefault(), 0, "Empty");
            Equal(arr.LastOrDefault(x => x < 5), 4, "Non-empty with predicate");
            Equal(arr.LastOrDefault(x => x < 0), 0, "Non-empty with failing predicate");
        }

        [Test]
        public void LongCount()
        {
            Equal(arr.LongCount(), 6L);
            Equal(narr.LongCount(x => x == null), 3L);
        }

        [Test]
        public void Max()
        {
            Equal(arr.Max(), 6, "Non-empty");
            Raises(() => empty.Max(), "Empty");
            Equal(narr.Max(), 6, "Non-empty nullable");
            Equal(nnarr.Max(), null, "Non-empty nullable all null");
            Equal(arr.Max(x => 10 - x), 9, "Non-empty with selector");
            Equal(arr.Max(x => (x % 2 == 1) ? As<int?>(null) : 10 - x), 8, "Non-empty with selector to nullable");
            Equal(arr.Max(x => As<int?>(null)), null, "Non-empty with selector to null");
        }

        [Test]
        public void Min()
        {
            Equal(arr.Min(), 1, "Non-empty");
            Raises(() => empty.Min(), "Empty");
            Equal(narr.Min(), 1, "Non-empty nullable");
            Equal(nnarr.Min(), null, "Non-empty nullable all null");
            Equal(arr.Min(x => 20 - x), 14, "Non-empty with selector");
            Equal(arr.Min(x => (x % 2 == 0) ? As<int?>(null) : 20 - x), 15, "Non-empty with selector to nullable");
            Equal(arr.Min(x => As<int?>(null)), null, "Non-empty with selector to null");
        }

        [Test]
        public void OrderBy()
        {
            var arr1 = new int[] { 3, 1, 4, 2, 6, 5 };
            var arr_rev = new int[] { 6, 5, 4, 3, 2, 1 };
            Equal(arr1.OrderBy(x => x).ToArray(), arr, "Id");
            Equal(arr1.OrderBy(x => x, inv).ToArray(), arr_rev, "Id with comparer");
            Equal(arr1.OrderBy(x => -x).ToArray(), arr_rev, "Neg");
            Equal(arr1.OrderBy(x => -x, inv).ToArray(), arr, "Neg with comparer");
            Equal(arr1.OrderByDescending(x => x).ToArray(), arr_rev, "Desc id");
            Equal(arr1.OrderByDescending(x => x, inv).ToArray(), arr, "Desc id with comparer");
            Equal(arr1.OrderByDescending(x => -x).ToArray(), arr, "Desc neg");
            Equal(arr1.OrderByDescending(x => -x, inv).ToArray(), arr_rev, "Desc neg with comparer");
        }

        [Test]
        public void ThenBy()
        {
            var arr1 = new int[] { 3, 1, 4, 2, 6, 5 };
            var arr = new int[] { 2, 4, 6, 1, 3, 5 };
            var arr_rev = new int[] { 6, 4, 2, 5, 3, 1 };
            Equal(arr1.OrderBy(x => x % 2).ThenBy(x => x).ToArray(), arr, "Id");
            Equal(arr1.OrderBy(x => x % 2).ThenBy(x => x, inv).ToArray(), arr_rev, "Id with comparer");
            Equal(arr1.OrderBy(x => x % 2).ThenBy(x => -x).ToArray(), arr_rev, "Neg");
            Equal(arr1.OrderBy(x => x % 2).ThenBy(x => -x, inv).ToArray(), arr, "Neg with comparer");
            Equal(arr1.OrderBy(x => x % 2).ThenByDescending(x => x).ToArray(), arr_rev, "Desc id");
            Equal(arr1.OrderBy(x => x % 2).ThenByDescending(x => x, inv).ToArray(), arr, "Desc id with comparer");
            Equal(arr1.OrderBy(x => x % 2).ThenByDescending(x => -x).ToArray(), arr, "Desc neg");
            Equal(arr1.OrderBy(x => x % 2).ThenByDescending(x => -x, inv).ToArray(), arr_rev, "Desc neg with comparer");
        }

        [Test]
        public void Range()
        {
            Equal(Enumerable.Range(1, 6).ToArray(), arr, "Non-empty");
            Equal(System.Linq.Enumerable.Range(1, 0).ToArray(), empty, "Empty");
            Raises((() => Enumerable.Range(1, -1)), "Negative count raises");
        }

        [Test]
        public void Repeat()
        {
            Equal(Enumerable.Repeat("a", 5).ToArray(), new[] { "a", "a", "a", "a", "a" }, "Non-empty");
            Equal(Enumerable.Repeat("a", 0).ToArray(), new string[] { }, "Empty");
            Raises((() => Enumerable.Repeat("a", -1)), "Negative count raises");
        }

        [Test]
        public void Reverse()
        {
            Equal(arr.Reverse().ToArray(), new[] { 6, 5, 4, 3, 2, 1 }, "Non-empty");
            Equal(empty.Reverse().ToArray(), empty, "Empty");
        }

        [Test]
        public void Select()
        {
            Equal(arr.Select(x => x * 2).ToArray(), new[] { 2, 4, 6, 8, 10, 12 }, "Non-empty");
            Equal(empty.Select(x => 42).ToArray(), empty, "Empty");
            Equal(arr.Select((x, i) => x * i).ToArray(), new[] { 0, 2, 6, 12, 20, 30 }, "Non-empty with index");
            Equal(empty.Select((x, i) => 42).ToArray(), empty, "Empty with index");
        }

        [Test]
        public void SelectMany()
        {
            Equal(arr.SelectMany(x => new[] { x, x * 2 }).ToArray(),
                new[] { 1, 2, 2, 4, 3, 6, 4, 8, 5, 10, 6, 12 }, "Non-empty");
            Equal(empty.SelectMany(x => new[] { x, x * 2 }).ToArray(), empty, "Empty");
            Equal(arr.SelectMany((x, i) => new[] { x, i }).ToArray(),
                new[] { 1, 0, 2, 1, 3, 2, 4, 3, 5, 4, 6, 5 }, "Non-empty with index");
            Equal(empty.SelectMany((x, i) => new[] { x, i }).ToArray(), empty, "Empty with index");
            Equal(arr2.SelectMany(x => arr2, (x, y) => x + y).ToArray(), new[] { 2, 3, 3, 4 }, "With result selector");
        }

        [Test]
        public void SequenceEqual()
        {
            IsTrue(arr.SequenceEqual(arr), "Self array");
            //IsTrue(seq().SequenceEqual(new[] { 1, 2, 3, 4, 5, 6 }), "Sequence");
            IsTrue(arr.SequenceEqual(new[] { 1, 2, 1, 2, 1, 2 }), "With equality comparer");
        }

        [Test]
        public void Single()
        {
            Equal((new[] { 174 }).Single(), 174, "Non-empty");
            Raises((() => (new[] { 1, 2 }.Single())), "Too long raises");
            Raises((() => empty.Single()), "Empty raises");
            Equal(arr.Single(x => x == 5), 5, "With predicate");
            Raises((() => arr.Single(x => x % 2 == 0)), "With predicate, too many matches raises");
            Raises((() => arr.Single(x => x == 42)), "With predicate, no matches raises");
        }

        [Test]
        public void SingleOrDefault()
        {
            Equal((new[] { 174 }).SingleOrDefault(), 174, "Non-empty");
            Raises((() => (new[] { 1, 2 }.SingleOrDefault())), "Too long raises");
            Equal(empty.SingleOrDefault(), 0, "Empty returns default");
            Equal(arr.SingleOrDefault(x => x == 5), 5, "With predicate");
            Raises((() => arr.SingleOrDefault(x => x % 2 == 0)), "With predicate, too many matches raises");
            Equal(arr.SingleOrDefault(x => x == 42), 0, "With predicate, no matches returns default");
        }

        [Test]
        public void Skip()
        {
            Equal(arr.Skip(2).ToArray(), new[] { 3, 4, 5, 6 }, "Non-empty");
            Equal(arr.Skip(0).ToArray(), arr, "Skip 0");
            Equal(arr.Skip(9).ToArray(), empty, "Skip > length");
        }

        [Test]
        public void SkipWhile()
        {
            Equal(arr.SkipWhile(x => x < 4).ToArray(), new[] { 4, 5, 6 }, "Non-empty");
            Equal(arr.SkipWhile(x => x < 6).ToArray(), new[] { 6 }, "Non-empty, skip to exactly last");
            Equal(arr.SkipWhile(x => true).ToArray(), empty, "True predicate");
            Equal(arr.SkipWhile(x => false).ToArray(), arr, "False predicate");
            Equal(arr.SkipWhile((x, i) => i < 4).ToArray(), new[] { 5, 6 }, "Non-empty with index");
            Equal(arr.SkipWhile((x, i) => i < 5).ToArray(), new[] { 6 }, "Non-empty with index, skip to exactly last");
        }

        [Test]
        public void Sum()
        {
            Equal(arr.Sum(), 21, "Non-empty");
            Equal(empty.Sum(), 0, "Empty");
            Equal(narr.Sum(), 21, "Non-empty nullable");
            Equal(nnarr.Sum(), null, "Non-empty nullable all null");
            Equal(arr.Sum(x => 10 - x), 39, "Non-empty with selector");
            Equal(arr.Sum(x => (x % 2 == 1) ? As<int?>(null) : 10 - x), 18, "Non-empty with selector to nullable");
            Equal(arr.Sum(x => As<int?>(null)), null, "Non-empty with selector to null");
        }

        [Test]
        public void Take()
        {
            Equal(arr.Take(2).ToArray(), new[] { 1, 2 }, "Non-empty");
            Equal(arr.Take(0).ToArray(), empty, "Take 0");
            Equal(arr.Take(9).ToArray(), arr, "Take > length");
        }

        [Test]
        public void TakeWhile()
        {
            Equal(arr.TakeWhile(x => x < 4).ToArray(), new[] { 1, 2, 3 }, "Non-empty");
            Equal(arr.TakeWhile(x => true).ToArray(), arr, "True predicate");
            Equal(arr.TakeWhile(x => false).ToArray(), empty, "False predicate");
            Equal(arr.TakeWhile((x, i) => i < 4).ToArray(), new[] { 1, 2, 3, 4 }, "Non-empty with index");
        }

        [Test]
        public void ToArray()
        {
            Equal(seq().ToArray(), arr, "Non-empty");
            Equal(emptySeq().ToArray(), empty, "Empty");
        }

        [Test]
        public void ToDictionary()
        {
            var d = arr.ToDictionary(x => x - 1);
            foreach (var x in d)
            {
                Equal(x.Key, x.Value - 1, "Key selector");
            }
            var d2 = arr.ToDictionary((x => x - 1), (x => x + 1));
            foreach (var x in d2)
            {
                Equal(x.Key, x.Value - 2, "Key selector, element selector");
            }
            var d3 = (new[] { 1, 2 }).ToDictionary((x => x - 1), mod2);
            foreach (var x in d3)
            {
                Equal(x.Key, x.Value - 1, "Key selector, equality comparer");
            }
            var d4 = (new[] { 1, 2 }).ToDictionary((x => x - 1), (x => x + 1), mod2);
            foreach (var x in d4)
            {
                Equal(x.Key, x.Value - 2, "Key selector, element selector, equality comparer");
            }
            Raises((() => arr.ToDictionary(x => x % 2)), "Identical keys raises");
            Raises((() => arr.ToDictionary((x => x), mod2)), "Identical keys with equality comparer raises");
        }

        [Test]
        public void ToList()
        {
            var l = seq().ToList();
            Equal(l.Count, 6);
            for (var x = 0; x < 6; ++x)
            {
                Equal(l[x], x + 1);
            }
        }

        [Test]
        public void Union()
        {
            SameValues(arr.Union(empty).ToArray(), arr, "Empty as second arg");
            SameValues(empty.Union(arr).ToArray(), arr, "Empty as first arg");
            SameValues((new[] { 1, 3, 5 }).Union(new[] { 2, 4, 6 }), arr, "Non-overlapping");
            SameValues((new[] { 1, 2, 3, 5 }).Union(new[] { 2, 4, 6 }), arr, "Overlapping");
            SameValues((new[] { 1 }).Union(new[] { 2 }, mod2), new[] { 2, 1 }, "Non-overlapping with equality comparer");
            var a = (new[] { 1, 2, 3 }).Union(new[] { 2, 4 }, mod2).ToArray();
            IsTrue(a.Count() == 2 && (a[0] % 2) + (a[1] % 2) == 1, "Overlapping with equality comparer");
        }

        [Test]
        public void Where()
        {
            Equal(arr.Where(x => x % 2 == 0).ToArray(), new[] { 2, 4, 6 }, "Non-empty");
            Equal(arr.Where(x => true).ToArray(), arr, "True predicate");
            Equal(arr.Where(x => false).ToArray(), empty, "False predicate");
            Equal(arr.Where((x, i) => i % 2 == 0).ToArray(), new[] { 1, 3, 5 }, "With index");
        }

        [Test]
        public void Zip()
        {
            Equal(arr.Zip(new[] { 7, 3, -1 }, (x, y) => x + y).ToArray(), new[] { 8, 5, 2 }, "Both non-empty");
            Equal(empty.Zip(arr, (x, y) => x + y).ToArray(), empty, "Second empty");
            Equal(arr.Zip(empty, (x, y) => x + y).ToArray(), empty, "Second empty");
        }

        [Test]
        public void QuerySelect()
        {
            var q1 =
                from x in arr
                select x;
            Equal(q1.ToArray(), arr, "Query with trivial select");
            NotEqual(q1, arr, "Trivial query hides source");
            var q2 =
                from x in arr
                select x * 2;
            Equal(q2.ToArray(), new[] { 2, 4, 6, 8, 10, 12 }, "Query with select");
            var q3 =
                from x in arr2
                from y in arr2
                select x + y;
            Equal(q3.ToArray(), new[] { 2, 3, 3, 4 }, "Query with two sources");
        }

        [Test]
        public void QueryLet()
        {
            var q1 =
                from x in arr
                let y = x * 2
                select y;
            Equal(q1.ToArray(), new[] { 2, 4, 6, 8, 10, 12 }, "Query with let and trivial selector");
            var q2 =
                from x in arr
                let y = x * 2
                select y + 1;
            Equal(q2.ToArray(), new[] { 3, 5, 7, 9, 11, 13 }, "Query with let");
        }

        [Test]
        public void QueryWhere()
        {
            var q1 =
                from x in arr
                where x % 2 == 0
                select x;
            Equal(q1.ToArray(), new[] { 2, 4, 6 }, "Query with where and identity selector");
            var q2 =
                from x in arr
                where x % 2 == 0
                select x + 1;
            Equal(q2.ToArray(), new[] { 3, 5, 7 }, "Query with where");
            var q3 =
                from x in arr
                from y in arr
                where x < 3
                where y < 3
                select x + y;
            Equal(q3.ToArray(), new[] { 2, 3, 3, 4 }, "Query with multiple sources and where");
        }

        [Test]
        public void QueryOrderBy()
        {
            var arr1 = new int[] { 3, 1, 4, 2, 6, 5 };
            var arr_rev = new int[] { 6, 4, 2, 5, 3, 1 };
            var q1 =
                from x in arr1
                orderby x 
                select x;
            Equal(q1.ToArray(), arr, "Single ordering");
            var q2 =
                from x in arr1
                orderby x % 2, -x
                select x;
            Equal(q2.ToArray(), arr_rev, "Multiple orderings");
            var q3 =
                from x in arr1
                orderby x % 2, x descending
                select x;
            Equal(q3.ToArray(), arr_rev, "Multiple orderings with descending");
        }

        [Test]
        public void QueryGroupBy()
        {
            var q1 =
                from x in arr
                group x by x % 2;
            var g1 = q1.ToArray();
            Equal(g1.ElementAt(0).Key, 1, "source, keySelector");
            Equal(g1.ElementAt(0).ToArray(), new int[] { 1, 3, 5 }, "source, keySelector");
            Equal(g1.ElementAt(1).Key, 0, "source, keySelector");
            Equal(g1.ElementAt(1).ToArray(), new int[] { 2, 4, 6 }, "source, keySelector");
            var q2 =
                from x in arr
                group x + 10 by x % 2;
            var g2 = q2.ToArray();
            Equal(g2.ElementAt(0).Key, 1, "source, keySelector, elementSelector");
            Equal(g2.ElementAt(0).ToArray(), new int[] { 11, 13, 15 }, "source, keySelector, elementSelector");
            Equal(g2.ElementAt(1).Key, 0, "source, keySelector, elementSelector");
            Equal(g2.ElementAt(1).ToArray(), new int[] { 12, 14, 16 }, "source, keySelector, elementSelector");
        }

        [Test]
        public void QueryJoin()
        {
            var inner = new[] { "aaa", "bb", "cc", "d" };
            var q1 =
                from x in arr
                join i in inner on x equals i.Length
                select x.ToString() + i;
            var expected1 = new[] { "1d", "2bb", "2cc", "3aaa" };
            Equal(q1.ToArray(), expected1, "Base case");
        }

        [Test]
        public void QueryJoinInto()
        {
            var inner = new[] { "aaa", "bb", "cc", "d" };
            var q1 =
                from x in arr
                join i in inner on x equals i.Length into inners
                select (new[] { x.ToString() }).Concat(inners).ToArray();
            var expected1 = new[] {
                new[] { "1", "d" },
                new[] { "2", "bb", "cc" },
                new[] { "3", "aaa" },
                new[] { "4" },
                new[] { "5" },
                new[] { "6" } };
            Equal(q1.ToArray(), expected1, "Base case");
        }

        [Test]
        public void QueryContinuation()
        {
            var q1 =
                from x in arr
                select x * 2 into y
                select y + 1;
            Equal(q1.ToArray(), new[] { 3, 5, 7, 9, 11, 13 }, "Query continuation");
        }

        public int ExprAsValue(System.Linq.Expressions.Expression<Func<int>> act)
        {
            return (As<Func<int>>(act)).Invoke();
        }

        [Test]
        public void LinqExpressionTest()
        {
            Equal(ExprAsValue(() => 2), 2);
        }

        #region Auxiliary values

        private void SameValues(IEnumerable<int> s1, IEnumerable<int> s2, string msg)
        {
            var a1 = s1.ToList();
            var a2 = s2.ToList();
            Equal(a1.Count, a2.Count, msg);
            for (var i = 0; i < a1.Count; ++i)
            {
                var j = 0;
                for (j = 0; j < a2.Count && a1[i] != a2[j]; ++j)
                {
                }
                if (j == a2.Count)
                {
                    IsTrue(false, msg);
                    return;
                }
                else
                {
                    a2.RemoveAt(j);
                }
            }
            IsTrue(true, msg);
        }

        class Mod2Eq : EqualityComparer<int>
        {
            public override bool Equals(int x, int y) => x % 2 == y % 2;

            public override int GetHashCode(int x) => x % 2;
        }

        class InvCmp : Comparer<int>
        {
            public override int Compare(int x, int y) => y - x;
        }

        private IEnumerable<int> seq()
        {
            yield return 1;
            yield return 2;
            yield return 3;
            yield return 4;
            yield return 5;
            yield return 6;
        }

        private IEnumerable<int> emptySeq()
        {
            yield break;
        }

        private int[] arr = new int[] { 1, 2, 3, 4, 5, 6 };
        private int[] arr2 = new int[] { 1, 2 };
        private int?[] narr = new int?[] { 1, null, 2, 3, null, 4, null, 5, 6 };
        private int?[] nnarr = new int?[] { null, null };
        private int[] empty = new int[] { };
        private IEqualityComparer<int> mod2 = new Mod2Eq();
        private IComparer<int> inv = new InvCmp();

        private int Add(int x, int y) => x + y;

        private int Add5(int x) => x + 5; 
        #endregion
    }
}
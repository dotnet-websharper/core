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
using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using WebSharper.Testing;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("C# Collections")]
    public class Collections : TestCategory
    {
        [Test]
        public void DictionaryTryGetValue()
        {
            var d = new Dictionary<int, string>() { { 1, "one" }, { 2, "two" } };
            string res = null;
            IsTrue(d.TryGetValue(2, out res));
            Equal(res, "two");
            IsFalse(d.TryGetValue(3, out res));
        }

        [Test]
        public void DictionaryIterate()
        {
            var d = new Dictionary<int, string>() { { 1, "one" }, { 2, "two" } };
            var l = new List<KeyValuePair<int, string>>();
            foreach (var i in d) l.Add(i);
            Equal(l.ToArray(), new[] { new KeyValuePair<int, string>(1, "one"), new KeyValuePair<int, string>(2, "two") });
        }

        [Test]
        public void HashSetIterate()
        {
            var s = new HashSet<int>() { 1, 5 };
            var l = new List<int>();
            foreach (var i in s) l.Add(i);
            Equal(l.ToArray(), new[] { 1, 5 });
        }

        [Test]
        public void ListPattern()
        {
            Expect(3);
            List<int> l = new() { 1, 2, 3 };
            IsTrue(l is [1, 2, 3]);
            if (l is [1, var two, 3])
            {
                Equal(two, 2);
            }
            IsFalse(l is [1, _, 4]);
        }

        [Test]
        public void CollectionExpressions()
        {
            List<int> l = [1, 2, 3];
            IsTrue(l is [1, 2, 3]);

            List<int> l2 = [4, 5];
            List<int> l3 = [.. l, .. l2, 6];
            IsTrue(l3 is [1, 2, 3, 4, 5, 6]);

            // with arrays
            int[] a = [1, 2, 3];
            IsTrue(a is [1, 2, 3]);

            int[] a3 = [.. a, .. l2, 6];
            IsTrue(a3 is [1, 2, 3, 4, 5, 6]);

            // with enumerable
            IEnumerable<int> e = [1, 2, 3];
            IsTrue(e.ToArray() is [1, 2, 3]);

            IEnumerable<int> e3 = [.. e, .. l2, 6];
            IsTrue(e3.ToArray() is [1, 2, 3, 4, 5, 6]);
        }

        [Test]
        public void ListPatternWithSlice()
        {
            List<int> l = new() { 1, 2, 3 };
            IsTrue(l is [.., 3]);
            IsTrue(l is [1, 2, 3, ..]);

            // with arrays
            int[] a = new[] { 1, 2, 3 };
            IsTrue(a is [.., 3]);
            IsTrue(a is [1, 2, 3, ..]);
        }

        public class TimerRemaining
        {
            public int[] buffer { get; set; } = new int[10];
        }

        [Test("Collection construction with implicit from the end indexing")]
        public void ImplicitFromTheEnd()
        {
            var countdown = new TimerRemaining()
            {
                buffer =
                    {
                        [^1] = 0,
                        [^2] = 1,
                        [^3] = 2,
                        [^4] = 3,
                        [^5] = 4,
                        [^6] = 5,
                        [^7] = 6,
                        [^8] = 7,
                        [^9] = 8,
                        [^10] = 9
                    }
            };

            Equal(countdown.buffer[0], 9);
            Equal(countdown.buffer[9], 0);
        }

        [Test("Array creation with a size")]
        public void ArrayCreationWithSize()
        {
            var a = new int[3];
            Equal(a, new[] { 0, 0, 0 });
            var n = 2;
            var s = new string[n];
            Equal(s.Length, 2);
            IsTrue(s[0] == null);
            var j = new int[2][];
            Equal(j.Length, 2);
            IsTrue(j[1] == null);
        }

        static string Kind(Action f)
        {
            try { f(); return "none"; }
            catch (ArgumentOutOfRangeException) { return "ArgumentOutOfRange"; }
            catch (InvalidOperationException) { return "InvalidOperation"; }
            catch (Exception) { return "Exception"; }
        }

        [Test("Collection and LINQ semantics from the C# conformance sweep (#1650)")]
        public void ConformanceCollections()
        {
            Equal(string.Join(",", new object[] { 1, "a", 2, null }.OfType<int>().Select(x => x.ToString())), "1,2");
            Equal(string.Join(",", new[] { 1, 2, 3 }.Intersect(new[] { 3, 2 }).Select(x => x.ToString())), "2,3");
            Equal(Kind(() => { var l = new List<int> { 1, 2 }; foreach (var x in l) l.Add(x); }), "InvalidOperation");
            Equal(Kind(() => { var l = new List<int> { 1 }; var _ = l[5]; }), "ArgumentOutOfRange");
            Equal(Kind(() => new int[0].First()), "InvalidOperation");
            Equal(Kind(() => new int[0].Last()), "InvalidOperation");
            Equal(Kind(() => new[] { 1, 2 }.Single()), "InvalidOperation");
            Equal(Kind(() => new int[0].Min()), "InvalidOperation");
            Equal(Kind(() => new int[0].Average()), "InvalidOperation");
            Equal(Kind(() => Enumerable.Range(1, -1).ToList()), "ArgumentOutOfRange");
            Equal(Kind(() => new Stack<int>().Pop()), "InvalidOperation");
            Equal(Kind(() => new Queue<int>().Dequeue()), "InvalidOperation");
            Equal(Kind(() => { int? n = null; var _ = n.Value; }), "InvalidOperation");
            Equal(Kind(() => "hello".Substring(3, 5)), "ArgumentOutOfRange");
            Equal(Kind(() => { var r = 7 switch { 1 => "one" }; }), "InvalidOperation");
            Equal(string.Join("|", " a , b ,c ".Split(',', StringSplitOptions.TrimEntries)), "a|b|c");
            IsFalse(int.TryParse("12a", out var v));
            Equal(v, 0);
            var s = new Stack<int>(); s.Push(1); s.Push(2);
            Equal(s.Peek(), 2);
            Equal(s.Pop(), 2);
        }
    }
}

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
    }
}

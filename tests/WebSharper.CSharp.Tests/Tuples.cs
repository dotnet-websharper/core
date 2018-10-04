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
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;
using System.Collections;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("C# Tuples")]
    public class Tuples : TestCategory
    {
        [Test]
        public void Construction()
        {
            var t = (0, "hello");
            Equal(t.Item1, 0);
            Equal(t.Item2, "hello");
            var (x, y) = t;
            Equal(x, 0);
            Equal(y, "hello");
            var (x2, _) = t;
            Equal(x2, 0);

            var ct = new Tuple<int, int>(2, 3);
            var cvt = new ValueTuple<int, int>(2, 3);
            Equal(ct, (2, 3).ToTuple());
            Equal(cvt, (2, 3));
        }

        [Test]
        public void Comparison()
        {
            IsTrue((0, "hello") == (0, "hello"));
            IsTrue((0, "hello") != (0, "hi"));
        }

        [Test]
        public void Named()
        {
            var t = (a: 0, b: "hello");
            Equal(t.a, 0);
            Equal(t.b, "hello");
            var (x, y) = t;
            Equal(x, 0);
            Equal(y, "hello");
        }

        [Test]
        public void Foreach()
        {
            var arr = new[] { (0, (0, 0)), (1, (1, 1)) };
            var r = new List<int>();
            foreach ((int x, (int y, int z)) in (arr))
                r.Add(x + y + z);
            foreach (var (x, (y, z)) in (arr))
                r.Add(x + y + z);
            Equal(r.ToArray(), new[] { 0, 3, 0, 3 });
        }

        [Test]
        public void Conversions()
        {
            var t = (0, "hello").ToTuple();
            Equal(t.Item1, 0);
            Equal(t.Item2, "hello");
            var (x, y) = t; // deconstructing a System.Tuple
            Equal(x, 0);
            Equal(y, "hello");
            var (x2, _) = t;
            Equal(x2, 0);
            var vt = t.ToValueTuple();
            Equal(vt.Item1, 0);
            Equal(vt.Item2, "hello");
        }

        [Test]
        public void Methods()
        {
            IsTrue((1, 2).Equals((1, 2)));
            IsTrue((1, 2).ToTuple().Equals((1, 2).ToTuple()));
            Equal((1, 2).CompareTo((1, 3)), -1);
            NotEqual((1, 2).GetHashCode(), (1, 3).GetHashCode());
            NotEqual((1, 2).ToTuple().GetHashCode(), (1, 3).ToTuple().GetHashCode());
            Equal((1, 2).ToString(), "1,2"); // not using custom ToString yet
            Equal((1, 2).ToTuple().ToString(), "1,2");
        }

        [Test]
        public void Deconstruction()
        {
            var t = (1, "hello");
            var st = (2, "hi").ToTuple();
            int a; string b;
            (a, b) = t; // deconstruct to existing vars
            Equal(a, 1);
            Equal(b, "hello");
            (a, b) = st;
            Equal(a, 2);
            Equal(b, "hi");
            (a, _) = t;
            _ = (3, "");
            Equal(a, 1);
            Equal(b, "hi");
        }

        // Mutable structs are not yet supported

        //[Test]
        //public void Mutability()
        //{
        //    void NotIncrFst((int, string) tup)
        //    {
        //        tup.Item1++;
        //    }
        //    void Incr(ref int i)
        //    {
        //        i++;
        //    }
        //    void IncrFst(ref (int, string) tup)
        //    {
        //        tup.Item1++;
        //    }
        //    void IncrFst2(ref (int a, string b) tup)
        //    {
        //        tup.a++;
        //    }

        //    var t = (0, "hello");
        //    t.Item1 = 1;
        //    t.Item1++;
        //    Equal(t, (2, "hello"));
        //    t.Item1 = 2;
        //    NotIncrFst(t); // does not increment because of struct copying
        //    Equal(t.Item1, 2);
        //    t.Item1 = 2;
        //    Incr(ref t.Item1);
        //    Equal(t.Item1, 3);
        //    t.Item1 = 3;
        //    IncrFst(ref t);
        //    Equal(t.Item1, 4);
        //    t.Item1 = 4;
        //    IncrFst2(ref t);
        //    Equal(t.Item1, 5);
        //}
    }
}

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
    [JavaScript, Test("C# Pattern matching")]
    public class PatternMatching : TestCategory
    {
        class FakeTuple
        {
            int _a;
            string _b;

            public FakeTuple(int a, string b)
            {
                _a = a;
                _b = b;
            }

            public void Deconstruct(out int a, out string b)
            {
                a = _a;
                b = _b;
            }
        }

        [Test]
        public void Deconstruction()
        {
            var t = new FakeTuple(0, "hello");
            var (a, b) = t;
            Equal(a, 0);
            Equal(b, "hello");
        }

        [Test]
        public void Switch()
        {
            object o = "hello";
            int res = 0;
            switch (o)
            {
                case null:
                    res = 1;
                    break;
                case string s when s[0] == 'h':
                    res = 2;
                    break;
                default:
                    res = 4;
                    break;
            }
            Equal(res, 2);
        }

        [Test]
        public void Is()
        {
            object o = "hello";
            int res = 0;
            if (o is string s && s[0] == 'h')
                res = 2;
            Equal(res, 2);
        }

        [Test("C# property pattern matching", TestKind.Skip)]
        public void Property()
        {
            var o = new { X = 1 };
            var res = 0;
            if (o is { X: var x })
                res = x;
            Equal(res, 1);
        }

        [Test("C# recursive pattern matching", TestKind.Skip)]
        public void Recursive()
        {
            var o = new { X = (1, 3) };
            var res = 0;
            if (o is { X: var (x, y) })
                res = x + y;
            Equal(res, 4);
            var o2 = new { X = (1, 3), Y = 2 };
            if (o2 is { X: (var x2, var y2), Y: var z })
                res = x2 + y2 + z;
            Equal(res, 6);
        }

        [Test]
        public void Relational()
        {
            var myValue = 5;
            IsTrue(myValue is > 0 and <= 10);
            IsFalse(myValue is <= 0 or > 10);
            var ok = false;
            switch (myValue)
            {
                case <= 0:
                    break;
                case > 0 and <= 10:
                    ok = true;
                    break;         
            }
            IsTrue(ok);
        }

        [Test]
        public void Combinators()
        {
            var myValue = 1;
            var res = 0;
            if (myValue is var a and < 10)
            {
                res = a;
            }
            Equal(res, 1);
            myValue = 2;
            if (myValue is var b and > 10)
            {
                res = b;
            }
            Equal(res, 1);
            if (myValue is not (var c and > 10))
            {
                res = 3;
            }
            Equal(res, 3);
        }

        [Test]
        public void Discard()
        {
            var (a, _, _) = (1, 2, 3);
            Equal(a, 1);
        }

        [Test]
        public void Type()
        {
            object x = 1;
            IsTrue(x is int);
            var t = (1, "hi");
            IsTrue(t is (int, string));
        }
    }
}

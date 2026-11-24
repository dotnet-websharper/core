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
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("String operations")]
    class Strings : TestCategory
    {
        class A
        {
            public override string ToString()
            {
                return "a object";
            }
        }

        class B
        {
            public override string ToString() => "b object";
        }

        [Test]
        public void Format()
        {
            Equal(String.Format("xyz"), "xyz");
            Equal(String.Format("{0}", (object)"xyz"), "xyz");
            var xyz = "xyz";
            Equal(String.Format(xyz), "xyz");
            var pricePerOunce = 17.36;
            Equal(String.Format("The current price is {0} per ounce.", pricePerOunce),
                "The current price is 17.36 per ounce.");
            Equal(String.Format("{0,6} {1,15}", "foo", 12), "   foo              12");
            Equal(String.Format("{0,-6} {1,-15}", "foo", 12), "foo    12             ");
            Equal(String.Format("{0} {1} {2}", "foo", 12, new A()), "foo 12 a object");
            Equal(String.Format("{0} {1} {2} {3}", "foo", 12, 12.35, (long)500), "foo 12 12.35 500");
            Equal(String.Format("{{{0}}}", 5), "{5}");
            Equal(String.Format("{0:F1}|{1:N2}|{2:E2}", 2.25, -1234.567, 12345.678), "2.2|-1,234.57|1.23E+004");
            Equal(String.Format("{0}|{1}", true, 1e21), "True|1E+21");
            Equal((1234567.891).ToString("N2"), "1,234,567.89");
            Equal((0.256).ToString("P1"), "25.6 %");
            Equal(255.ToString("x"), "ff");

            var fmt = "{0} {1}";
            Equal(String.Format(fmt, 1, 2), "1 2");
            fmt = "The current price is {0} per ounce.";
            Equal(String.Format(fmt, pricePerOunce),
                "The current price is 17.36 per ounce.");
            fmt = "{0,6} {1,15}";
            Equal(String.Format(fmt, "foo", 12), "   foo              12");
            fmt = "{0,-6} {1,-15}";
            Equal(String.Format(fmt, "foo", 12), "foo    12             ");
            fmt = "{0} {1} {2}";
            Equal(String.Format(fmt, "foo", 12, new A()), "foo 12 a object");
            fmt = "{0} {1} {2} {3}";
            Equal(String.Format(fmt, "foo", 12, 12.35, (long)500), "foo 12 12.35 500");
        }

        [Test]
        public void Comparisons()
        {
            IsTrue("a" == "a");
            IsTrue("a" != "b");
            IsTrue("a" == (object)"a");

            IsTrue("a".Equals("a"));
            IsFalse("a".Equals("b"));
            IsTrue("a".Equals((object)"a"));
            Equal("a".CompareTo("b"), -1);
            Equal("a".CompareTo((object)"b"), -1);

            IsTrue('a' == 'a');
            IsTrue('a' != 'b');
            IsTrue('a' < 'b');

            IsTrue('a'.Equals('a'));
            IsFalse('a'.Equals('b'));
            IsTrue('a'.Equals((object)'a'));
            Equal('a'.CompareTo('b'), -1);
            Equal('a'.CompareTo((object)'b'), -1);
        }

        [Test]
        public void Construction()
        {
            Equal(new string(new[] { 'a', 'b' }), "ab");
            Equal(new string('a', 3), "aaa");
            Equal(new string(new[] { 'a', 'b', 'c', 'd', 'e' }, 2, 2), "cd");
        }

        [Test]
        public void Basics()
        {
            var hs = "a".GetHashCode();
            var hc = 'a'.GetHashCode();
            Equal("a".ToString(),"a");
            Equal('a'.ToString(), "a");
        }

        [Test]
        public void Concat()
        {
            Equal(String.Concat("a", "b"), "ab");
            Equal(String.Concat("a", "b", "c", "d", "e"), "abcde");
            Equal(String.Concat(new A()), "a object");
            Equal(String.Concat(new A(), "!"), "a object!");
            Equal(String.Concat(new[] { "a", "b" }), "ab");
            Equal(String.Concat(new List<string> { "a", "b" }), "ab");
            Equal(String.Concat(new A(), new B(), new A(), new B(), new A()), "a objectb objecta objectb objecta object");
            Equal(String.Concat(new List<A> { new A(), new A() }), "a objecta object");
        }

        [Test("Interpolated string holes convert like .NET (#1643)")]
        public void InterpolatedStringConversions()
        {
            int a = 1, b = 2;
            Equal($"{a}{b}", "12");
            Equal($"{a}{b}x", "12x");
            IsTrue((object)$"{7}" is string);
            string s = null;
            object o = null;
            int? n = null;
            Equal($"[{s}][{o}][{n}]", "[][][]");
            Equal($"{true}|{false}", "True|False");
            Equal($"{3.14159:F2}|{0.5:P0}|{1234:N0}|{255:X4}|{42:D5}", "3.14|50 %|1,234|00FF|00042");
            Equal($"{1234.5:0,0.00}|{0.5:#.##}", "1,234.50|.5");
            Equal($"[{42,5}][{42,-5}][{3.5,6:F2}]", "[   42][42   ][  3.50]");
        }
    }
}
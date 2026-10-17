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
using System.Threading.Tasks;
using WebSharper.Testing;
using static WebSharper.JavaScript.Pervasives;
using static WebSharper.JavaScript.Interop;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("C# regression tests")]
    public class Regression : TestCategory
    {
        // just compiling is enough to pass
        public static class CctorOptimizerBug
        {
            static int x = 1;

            [Inline("parseInt($s)")]
            public static int ParseInt(string s) => 0;

            public static int parse() => ParseInt("12345");
        }

        public class BaseClass
        {
            public string Message { get; protected set; }

            protected virtual void InitMessage() =>
                this.Message = "Hi from base class!";

            public BaseClass() => InitMessage();
        }

        public class SubClass : BaseClass
        {
            protected override void InitMessage() =>
                this.Message = "Hi from subclass!";

            public SubClass() { }
        }

        public class SubClass2 : BaseClass
        {
            protected override void InitMessage() =>
                this.Message = "Hi from subclass!";
        }

        [Test]
        public void ImplicitBaseCall()
        {
            Equal(new SubClass().Message, "Hi from subclass!");
            Equal(new SubClass2().Message, "Hi from subclass!");
        }

        private class ReverseIntComparer : IComparer<int>
        {
            public int Compare(int x, int y) => y.CompareTo(x);
        }

        [Test]
        public void MemoryExtensionsMinMax()
        {
            int[] values = [3, 1, 4, 2];
            Equal(MemoryExtensions.Max<int>(values), 4);
            Equal(MemoryExtensions.Min<int>(values), 1);
            Equal(MemoryExtensions.Max<int>(values, new ReverseIntComparer()), 1);
            Equal(MemoryExtensions.Min<int>(values, new ReverseIntComparer()), 4);
            Equal(MemoryExtensions.Max<int>(values, null), 4);
            Equal(MemoryExtensions.Min<int>(values, null), 1);
            Raises(() => MemoryExtensions.Max<int>(Array.Empty<int>()));
            Raises(() => MemoryExtensions.Min<int>(Array.Empty<int>()));

            int?[] nullable = [null, 3, null, 1];
            Equal(MemoryExtensions.Max<int?>(nullable), 3);
            Equal(MemoryExtensions.Min<int?>(nullable), 1);
            Equal(MemoryExtensions.Max<int?>(Array.Empty<int?>()), null);
            Equal(MemoryExtensions.Min<int?>(Array.Empty<int?>()), null);
            Equal(MemoryExtensions.Max<int?>(new int?[] { null, null }), null);
            Equal(MemoryExtensions.Min<int?>(new int?[] { null, null }), null);

            string[] strings = [null, "a", "c", "b"];
            Equal(MemoryExtensions.Max<string>(strings), "c");
            Equal(MemoryExtensions.Min<string>(strings), "a");
            Equal(MemoryExtensions.Max<string>(Array.Empty<string>()), null);
            Equal(MemoryExtensions.Min<string>(Array.Empty<string>()), null);

            double[] doubles = [double.NaN, 3, 1];
            Equal(MemoryExtensions.Max<double>(doubles), 3);
            IsTrue(double.IsNaN(MemoryExtensions.Min<double>(doubles)));
        }

        // look in compiled code that `f` is on top level
        public void LocalFunctionStrictMode()
        {
            if (1 < 2)
            {
                void f() { }
                f();
            }
        }
    }
}

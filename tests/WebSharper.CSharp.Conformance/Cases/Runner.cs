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

namespace Conformance
{
    [JavaScript]
    public static class H
    {
        public static (string, Func<string>) C(string name, Func<string> f) => (name, f);
        public static string J<T>(IEnumerable<T> xs) => "[" + string.Join(",", xs.Select(x => $"{x}")) + "]";
        public static string B(bool b) => b ? "true" : "false";
    }

    [JavaScript]
    public static class Runner
    {
        static string Kind(Exception e)
        {
            if (e is ArgumentOutOfRangeException) return "ArgumentOutOfRange";
            if (e is ArgumentNullException) return "ArgumentNull";
            if (e is ArgumentException) return "Argument";
            if (e is KeyNotFoundException) return "KeyNotFound";
            if (e is IndexOutOfRangeException) return "IndexOutOfRange";
            if (e is InvalidOperationException) return "InvalidOperation";
#if !JS
            if (e is DivideByZeroException) return "DivideByZero";
#endif
            if (e is OverflowException) return "Overflow";
            if (e is FormatException) return "Format";
#if !JS
            if (e is NullReferenceException) return "NullReference";
#endif
            if (e is InvalidCastException) return "InvalidCast";
#if !JS
            if (e is NotSupportedException) return "NotSupported";
#endif
#if !JS
            if (e is NotImplementedException) return "NotImplemented";
#endif
            return "Exception";
        }

        public static string[] Run()
        {
            var all = new List<(string, Func<string>)>();
            all.AddRange(Numbers.Cases());
            all.AddRange(Strings.Cases());
            all.AddRange(Records.Cases());
            all.AddRange(Patterns.Cases());
            all.AddRange(Collections.Cases());
            all.AddRange(LinqCases.Cases());
            all.AddRange(Language.Cases());
            all.AddRange(Types.Cases());
            var res = new List<string>();
            foreach (var (name, f) in all)
            {
                string v;
                try { v = f(); }
                catch (Exception e)
                {
                    v = "EXN " + Kind(e);
#if JS
                    v += " :: " + e.Message;
#endif
                }
                res.Add(name + " = " + v);
            }
            return res.ToArray();
        }
    }
}

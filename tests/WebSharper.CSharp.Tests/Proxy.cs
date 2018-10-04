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
    [Proxy(typeof(StringBuilder))]
    internal class StringBuilderProxy
    {
        private List<string> b = new List<string>();

        [Name("append")]
        public StringBuilderProxy Append(string s)
        {
            b.Add(s);
            return this;
        }

        public override string ToString()
        {
            var s = String.Concat(b);
            b.Clear();
            b.Add(s);
            return s;
        }
    }
}

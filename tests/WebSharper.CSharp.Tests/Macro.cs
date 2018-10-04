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
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using WebSharper.Core;
using WebSharper.Core.AST;
using WebSharper.Testing;

namespace WebSharper.CSharp.Tests
{
    class AddMacro : Macro
    {
        public override MacroResult TranslateCall(MacroCall call)
        {
            var args = call.Arguments.Select(ASTHelpers.IgnoreExprSourcePos).ToArray();
            if (args.All(a => a.IsValue))
            {
                var sum = call.Arguments.Sum(a => ((Literal.Int)((Expression.Value)a).Value).Value);
                return MacroResult.NewMacroOk(Expression.NewValue(Literal.NewInt(sum)));
            }
            else return MacroResult.MacroFallback;
        }
    }

    [JavaScript, Test("C# macro")]
    public class MacroTest : TestCategory
    {
        [Macro(typeof(AddMacro)), JavaScript]
        public static int Add(int a, int b) => a + b;

        [Test]
        public void AddMacro()
        {
            Equal(MacroTest.Add(1, 2), 3, "transformed");
            var x = 1;
            Equal(MacroTest.Add(x, 2), 3, "fallback");
        }
    }
}

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

    [JavaScript]
    public class MacroTest : TestCategory
    {
        //[Macro(typeof(AddMacro))]
        //public static int Add(int a, int b) => a + b;

        //[Test]
        //public void AddMacro()
        //{
        //    Equal(MacroTest.Add(1, 2), 3, "transformed");
        //    var x = 1;
        //    Equal(MacroTest.Add(x, 2), 3, "fallback");
        //}
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Microsoft.FSharp.Core;
using A = WebSharper.Core.Attributes;

namespace WebSharper.CSharp.Tests
{
    //[A.JavaScript]
    public class Interop
    {
        //[A.JavaScript]
        public class FSharpTypeTests
        {
            public static bool[] OptionTests
            {
                get
                {
                    return new[] {
                        FSharpOption<int>.get_IsNone(FSharpOption<int>.None),
                        //FSharpOption<int>.None == null,
                        //FSharpOption<int>.GetTag(FSharpOption<int>.None) == 0,
                        OptionModule.IsNone(FSharpOption<int>.None),
                        FSharpOption<int>.Some(3).Value == 3
                    };
                }
            }

            public static bool[] ChoiceTests
            {
                get
                {
                    return new[] {
                        WebSharper.Tests.Interop.Union.NewA(1).IsA,
                        FSharpChoice<int, string>.NewChoice1Of2(1).IsChoice1Of2,
                        (FSharpChoice<int, string>.NewChoice1Of2(1) as FSharpChoice<int, string>.Choice1Of2).Item == 1
                    };
                }
            }
        }
    }
}

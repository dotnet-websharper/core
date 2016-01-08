using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Microsoft.FSharp.Core;
using A = WebSharper.Core.Attributes;
using I = WebSharper.Collections.Tests.Interop;

namespace WebSharper.CSharp.Tests
{
    //[A.JavaScript]
    //public delegate void MyAction();

    [A.JavaScript]
    public class Interop
    {
        [A.JavaScript]
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
                        I.Union.NewA(1).IsA,
                        FSharpChoice<int, string>.NewChoice1Of2(1).IsChoice1Of2,
                        (FSharpChoice<int, string>.NewChoice1Of2(1) as FSharpChoice<int, string>.Choice1Of2).Item == 1
                    };
                }
            }

            //public static bool[] DelegateTests
            //{ 
            //    get
            //    {
            //        var act = new System.Action(() => { });
            //        MyAction mact = new MyAction(act);

            //        var res = new[] { 0 };
            //        var a1 = new System.Action<int>(x => res[0] += x);
            //        var a2 = new System.Action<int>(x => res[0] += 1);
            //        var comb = a1 + a2;
            //        comb.Invoke(2);
            //        return new[] { res[0] == 3 };
            //    }
            //}
        }
    }
}

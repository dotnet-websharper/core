using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Microsoft.FSharp.Core;
using A = WebSharper.Core.Attributes;


namespace WebSharper.CSharp.Tests
{
    [A.JavaScript]
    public class Class1
    {
        public string HelloWorld() { return "Hello " + "world!"; }

        //public string GetValue(FSharpOption<string> opt) { return opt.Value; }

        //public FSharpOption<string> GetSomeA() { return FSharpOption<string>.Some("A"); }
    }
}

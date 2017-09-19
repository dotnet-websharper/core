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

        [Test]
        public void ImplicitBaseCall()
        {
            Equal(new SubClass().Message, "Hi from subclass!");
        }
    }
}

using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;
using static WebSharper.JavaScript.Pervasives;
using static WebSharper.JavaScript.Interop;
using SiteletTest = WebSharper.CSharp.Sitelets.Tests.SiteletTest;

namespace WebSharper.CSharp.Tests
{
    [Test("C# Basic tests")]
    public class Tests : TestCategory
    {
        [Generated(typeof(TestGenerator))]
        public static IControlBody RunTests() { return null; }

        public static IControlBody RunTests(bool runServerTests)
        {
            Remoting.ShouldRun = runServerTests;
            return RunTests();
        }

        public string GetHelloWorld() { return "Hello " + "world!"; }
        public string HelloWorldProp => "Hello " + "world!";

        [Test("Hello World")]
        public void HelloWorld()
        {
            Equal(GetHelloWorld(), "Hello world!");
            Equal(HelloWorldProp, "Hello world!");
        }

        [Test("WebSharper.Testing Assertions")]
        public void Assertions()
        {
            Expect(6);
            Equal(1, 1, "F# (=) equality");
            NotEqual(As<object>("1"), As<object>(1), "F# (<>) inequality");
            JsEqual(As<object>("1"), As<object>(1), "JS (==) equality");
            ApproxEqual(Math.Sqrt(2), 1.41421, "ApproxEqual");
            IsTrue(1 == 1, "IsTrue");
            Raises(() => { throw new Exception(); }, "Raises");
        }

        public IEnumerable<int> Fibonacci()
        {
            yield return 1;
            yield return 1;
            var a = 1;
            var b = 1;
            while (true)
            {
                var c = a + b;
                a = b;
                b = c;
                yield return b;
            }
        }

        public IEnumerable<int> IteratorBreak()
        {
            for (var i = 0; i < 10; i++)
            {
                yield return i;
                if (i == 5) yield break;
            }
        }

        [Test]
        public void Generator()
        {
            Equal(
                Fibonacci().Take(5).ToArray(),
                new int[] { 1, 1, 2, 3, 5 },
                "Fibonacci"
            );
            Equal(
                 IteratorBreak().ToArray(),
                 new int[] { 0, 1, 2, 3, 4, 5 },
                 "Yield break"
            );
        }

        private bool SeqDisposed = false;

        public IEnumerable<int> DisposingSeq()
        {
            try
            {
                while (true)
                {
                    yield return 1;
                }
            }
            finally
            {
                SeqDisposed = true;
            }
        }

        [Test("try/finally in iterators, known issue: https://github.com/intellifactory/websharper/issues/542", TestKind.Skip)]
        public void DisposingGenerator()
        {
            foreach (var a in DisposingSeq().Take(2))
            {
                Equal(a, 1);
                IsFalse(SeqDisposed);
            }
            IsTrue(SeqDisposed);
        }

        public string First { get; set; }
        public string Second { get; set; } = "Hello";

        public int FirstField;
        public int SecondField;
        public int FirstProp { get; set; }
        public int SecondProp { get; set; }

        [Test("Auto properties")]
        public void AutoProperty()
        {
            First = "Hi";
            Equal(this.First, "Hi");
            Equal(Second, "Hello");
        }

        [Test("Chained setters")]
        public void ChainedSetters()
        {
            SecondField = FirstField = 1;
            Equal(FirstField, 1);
            Equal(SecondField, 1);

            SecondField = FirstField++;
            Equal(FirstField, 2);
            Equal(SecondField, 1);

            SecondField = --FirstField;
            Equal(FirstField, 1);
            Equal(SecondField, 1);

            SecondField = FirstField += 4;
            Equal(FirstField, 5);
            Equal(SecondField, 5);

            SecondProp = FirstProp = 1;
            Equal(FirstProp, 1);
            Equal(SecondProp, 1);

            SecondProp = FirstProp++;
            Equal(FirstProp, 2);
            Equal(SecondProp, 1);

            SecondProp = --FirstProp;
            Equal(FirstProp, 1);
            Equal(SecondProp, 1);

            SecondProp = FirstProp += 4;
            Equal(FirstProp, 5);
            Equal(SecondProp, 5);
        }

        [Test]
        public void PropertyTest(int x)
        {
            Equal(x, x, "Self-equal: " + As<string>(x));
        }

        [Test]
        public void RandomPropertyTest()
        {
            foreach (var awrr in RandomValues.ArrayOf(RandomValues.Int))
                Equal(awrr, awrr);
            foreach (var arr in new RandomValues.Sample<int[]>(RandomValues.ArrayOf(RandomValues.Int)))
                Equal(arr, arr);
            foreach (var arr in new RandomValues.Sample<int[]>())
                Equal(arr, arr);
        }

        [Inline("$a + $b")]
        public static object InlinedAdd(object a, object b) => X<object>();

        [Inline]
        public static int InlineWithReturn(bool b)
        {
            if (b)
                return 1;
            else
                return 2;
        }

        [Test]
        public void InlineTest()
        {
            Equal(InlinedAdd(1, 2), 3);
            Equal(InlinedAdd("1", 2), "12");
            Equal(InlineWithReturn(true), 1);
            Equal(InlineWithReturn(false), 2);
            Equal(WebSharper.JavaScript.JS.Inline<int>("1 + 1"), 2);
            Equal(WebSharper.JavaScript.JS.Inline<int>("1 + $0", 2), 3);
        }

        //[Test]
        //public async Task VariableScopingAsync()
        //{
        //    //var res = 0;
        //var adders = new List<System.Action>();
        //for (var i = 0; i <= 5; i++)
        //{
        //    var b = i + await GetOneAsync();
        //    adders.Add(() => res += b);
        //}
        //foreach (var adder in adders) adder();
        //Equal(res, 21, "variable inside loop");
        //}

        [Test]
        public void VariableScoping()
        {
            var res = 0;
            var adders = new List<System.Action>();
            var a = 0;
            for (var i = 0; i <= 5; i++)
            {
                a = i;
                adders.Add(() => res += a);
            }
            foreach (var adder in adders) adder();
            Equal(res, 30, "variable outside loop");

            res = 0;
            adders.Clear();
            for (var i = 0; i <= 5; i++)
            {
                var b = i;
                adders.Add(() => res += b);
            }
            foreach (var adder in adders) adder();
            Equal(res, 15, "variable inside loop");
        }

        [Test]
        public void Lazy()
        {
            var l2 = new Lazy<int>(() => 1 + 1);
            Equal(l2.IsValueCreated, false);
            Equal(l2.Value, 2);
        }

        [Test]
        public void EmptyGuid()
        {
            Equal(Guid.Empty.ToString(), "00000000-0000-0000-0000-000000000000");
        }

        [Test("goto and variable scoping, known issue: https://github.com/intellifactory/websharper/issues/542", TestKind.Skip)]
        public void VariableScopingGoto()
        {
            var res = 0;
            var adders = new List<System.Action>();
            for (var i = 0; i <= 10; i++)
            {
                if (i == 6) goto outOfLoop;
                var b = i;
                adders.Add(() => res += b);
            }
            outOfLoop: foreach (var adder in adders) adder();
            Equal(res, 15, "goto from inside loop");
        }

        [Test("break and variable scoping, known issue: https://github.com/intellifactory/websharper/issues/542", TestKind.Skip)]
        public void VariableScopingBreak()
        {
            var res = 0;
            var adders = new List<System.Action>();
            for (var i = 0; i <= 10; i++)
            {
                if (i == 6) break;
                var b = i;
                adders.Add(() => res += b);
            }
            foreach (var adder in adders) adder();
            Equal(res, 15, "break inside loop");
        }

        [Remote]
        public static Task<string[]> SiteletTestLinks()
        {
            var sitelet = SiteletTest.Main;
            return Task.FromResult(new[] {
                sitelet.Router.Link("/").Value.OriginalString,
                sitelet.Router.Link(SiteletTest.JohnDoe).Value.OriginalString,
            }); 
        }

        [Test("SiteletBuilder correctness")]
        public async Task SiteletBuilderTest()
        {
            if (Remoting.ShouldRun)
            {
                var siteletTestLinks = await SiteletTestLinks();
                Equal(siteletTestLinks[0], "/");
                Equal(siteletTestLinks[1], "/person/John/Doe/30");
            }
            else
            {
                Expect(0);
            }
        }
    }
}

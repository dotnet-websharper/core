using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.FSharp.Core;
using WebSharper.Testing;
using I = WebSharper.Collections.Tests.Interop;
using Microsoft.FSharp.Control;
using WebSharper.JavaScript;

namespace WebSharper.CSharp.Tests
{
    [JavaScript]
    public static class Extensions
    {
        public static FSharpFunc<T, U> ToFSharpFunc<T, U>(this Func<T, U> f)
            => FSharpFunc<T, U>.FromConverter(WebSharper.JavaScript.Pervasives.As<Converter<T, U>>(f));
    }

    [JavaScript, Test("C#/F# interop tests")]
    public class Interop : TestCategory
    {
        [Test]
        public void Option()
        {
            IsTrue(FSharpOption<int>.get_IsNone(FSharpOption<int>.None));
            Equal(FSharpOption<int>.None, null);
            Equal(FSharpOption<int>.GetTag(FSharpOption<int>.None), 0);
            Equal(FSharpOption<int>.GetTag(FSharpOption<int>.Some(2)), 1);
            Equal(FSharpOption<int>.Some(3).Value, 3);
            Equal(FSharpOption<int>.Some(1), FSharpConvert.Some(1));
            Equal(FSharpConvert.Option((int?)1), FSharpOption<int>.Some(1));
        }

        [Test]
        public void OptionConditionalAccess()
        {
            var o = FSharpOption<int>.Some(3);
            Equal(o?.Value, 3);
            var o2 = FSharpOption<FSharpOption<int>>.Some(FSharpOption<int>.Some(4));
            Equal(o2?.Value.Value, 4);
        }

        [Test]
        public void Union()
        {
            IsTrue(I.Union.NewA(1).IsA);
            var x = I.Union.NewA(1);
            var res = 0;
            switch (x.Tag)
            {
                case I.Union.Tags.A:
                    res = ((I.Union.A)x).Item;
                    break;
            }
            Equal(res, 1);
            Equal(I.Union.NewA(1).X(), 1);
            Equal(((I.Union.B)I.Union.NewB("XY")).name, "XY");
            IsTrue(I.Union.C.IsC);
            Equal(I.Union.C.Tag, I.Union.Tags.C);
        }

        [Test]
        public void ErasedUnion()
        {
            IsTrue(I.Module.ErasedUnion1.IsUnion1Of2);
            IsTrue(I.Module.ErasedUnion2.IsUnion2Of2);

            var x = I.Module.ErasedUnion1;
            var res = 0;
            switch (x.Tag)
            {
                case Union<int, string>.Tags.Union1Of2:
                    res = ((Union<int, string>.Union1Of2)x).Item;
                    break;
            }
            Equal(res, 42);

            x = Union<int, string>.NewUnion1Of2(43);
            switch (x.Tag)
            {
                case Union<int, string>.Tags.Union1Of2:
                    res = ((Union<int, string>.Union1Of2)x).Item;
                    break;
            }
            Equal(res, 43);

            IsTrue(x is Union<int, string>.Union1Of2);
            IsFalse(x is Union<int, string>.Union2Of2);

            res = 0;
            switch (x)
            {
                case Union<int, string>.Union2Of2 u:
                    break;
                case Union<int, string>.Union1Of2 u:
                    res = u.Item;
                    break;
            }
            Equal(res, 43);

            var res2 = "";
            x = I.Module.ErasedUnion2;
            switch (x.Tag)
            {
                case Union<int, string>.Tags.Union2Of2:
                    res2 = ((Union<int, string>.Union2Of2)x).Item;
                    break;
            }
            Equal(res2, "hi");

            x = Union<int, string>.NewUnion2Of2("hi!");
            switch (x.Tag)
            {
                case Union<int, string>.Tags.Union2Of2:
                    res2 = ((Union<int, string>.Union2Of2)x).Item;
                    break;
            }
            Equal(res2, "hi!");

            // implicit conversions
            x = "hi";
            string s = x;
            Equal(s, "hi");
        }

        [Test]
        public void ErasedOption()
        {
            IsTrue(I.Module.UndefVal.IsUndefined);
            IsTrue(I.Module.DefVal.IsDefined);

            var x = Optional<int>.Undefined;
            var res = 0;
            switch (x.Tag)
            {
                case Optional<int>.Tags.Undefined:
                    res = -1;
                    break;
            }
            Equal(res, -1);
            JsEqual(x.Value, JS.Undefined);

            x = Optional<int>.NewDefined(4);
            switch (x.Tag)
            {
                case Optional<int>.Tags.Defined:
                    res = ((Optional<int>.Defined)x).Item;
                    break;
            }
            Equal(res, 4);
            JsEqual(x.Value, 4);

            // implicit conversions
            x = 5;
            int i = x;
            Equal(i, 5);
        }

        [Test]
        public void Record()
        {
            var x = new I.Record(1, null);
            Equal(x.A, 1);
            Equal(x.B, null);
            x.A = 2;
            Equal(x.A, 2);
            Equal(x.X(), 3);
        }

        [Test]
        public void Choice()
        {
            IsTrue(FSharpChoice<int, string>.NewChoice1Of2(1).IsChoice1Of2);
            Equal((FSharpChoice<int, string>.NewChoice1Of2(1) as FSharpChoice<int, string>.Choice1Of2).Item, 1);
        }

        [Test]
        public void BigTuple()
        {
            var t = I.Module.BigTuple();
            Equal(t.Item1, 1);
            Equal(t.Item4, 4);
            Equal(t.Item7, 7);
            Equal(t.Rest, Tuple.Create(8, 9, 10));
            Equal((object)t, Tuple.Create(1, 2, 3, 4, 5, 6, 7, Tuple.Create(8, 9, 10)));
        }

        [Test]
        public void Params()
        {
            var c = new I.Class();
            Equal(c.ParamsSum(), 0);
            Equal(c.ParamsSum(3), 3);
            Equal(c.ParamsSum(3, 4), 7);
            Equal(c.ParamsSum(new[] { 3, 4 }), 7);
        }

        [Test]
        public void Optionals()
        {
            var c = new I.Class();
            Equal(c.Optionals(5), 7);
            Equal(c.Optionals(5, 5), 10);
            Equal(c.Optionals(y: 5), 6);
            Equal(c.Optionals(5, 5, 5), 15);
        }

        [Test]
        public void FSharpOptionals()
        {
            var c = new I.Class();
            Equal(c.FSharpOptionals(), 3);
            Equal(c.FSharpOptionals(5), 7);
            Equal(c.FSharpOptionals(y: 5), 6);
            Equal(c.FSharpOptionals(5, 5), 10);
        }

        [Test]
        public void InlinedOptionals()
        {
            var c = new I.Class();
            Equal(c.OptionalsInl(5), 7);
            Equal(c.OptionalsInl(5, 5), 10);
            Equal(c.OptionalsInl(y: 5), 6);
            Equal(c.OptionalsInl(5, 5, 5), 15);
        }

        [Test]
        public void ParamsLet()
        {
            Equal(I.Module.ParamsLet(), 0);
            Equal(I.Module.ParamsLet(3), 3);
            Equal(I.Module.ParamsLet(3, 4), 7);
            Equal(I.Module.ParamsLet(new[] { 3, 4 }), 7);
        }

        [Test]
        public void Generics()
        {
            Equal(new I.GenericClass<int>().GenericMethod<string>("test"), "test");
        }

        int TestInt = 0;

        public void AddToTestInt(object sender, int x)
        {
            TestInt += x;
        }

        [Test]
        public void Event()
        {
            var c = new I.Class();
            c.Event += AddToTestInt;
            c.TriggerEvent(1);
            Equal(TestInt, 1, "adding handler then triggering");
            c.Event += AddToTestInt;
            c.TriggerEvent(2);
            Equal(TestInt, 5, "adding another handler then triggering");
            c.Event -= AddToTestInt;
            c.TriggerEvent(3);
            Equal(TestInt, 8, "removing one handler then triggering");
        }

        public void AddToTestInt(int x)
        {
            TestInt += x;
        }

        [Test]
        public void DelegateEvent()
        {
            var c = new I.Class();
            c.DelEvent += AddToTestInt;
            c.TriggerDelEvent(1);
            Equal(TestInt, 1, "adding handler then triggering");
            c.DelEvent += AddToTestInt;
            c.TriggerDelEvent(2);
            Equal(TestInt, 5, "adding another handler then triggering");
            c.DelEvent -= AddToTestInt;
            c.TriggerDelEvent(3);
            Equal(TestInt, 8, "removing one handler then triggering");
        }

        [Test]
        public void FSharpFunc()
        {
            Equal(I.Module.ReturnsFunc().Invoke(3), 3);
            Equal(I.Module.ReturnsFunc2().Invoke(1).Invoke(2), 3);
            Func<int, int> f = I.Module.ReturnsFunc().Invoke;
            Equal(f(3), 3);
            Equal(I.Module.InvokeFunc(FSharpConvert.Fun((int x) => x + 1), 1), 2);
            Equal(I.Module.InvokeFunc2(FSharpConvert.Fun((int x, int y) => x + y), 1, 2), 3);
        }

        [Test]
        public void FSharpRef()
        {
            var x = FSharpConvert.Ref<int>();
            Equal(x.contents, 0);
            Equal(x.Value, 0);
            x.Value++;
            Equal(x.Value, 1);
            var y = FSharpConvert.Ref<int>(5);
            Equal(y.Value, 5);
        }

        public void Increment(ref int x)
        {
            x++;
        }

        [Test]
        public void ModuleLet()
        {
            Equal(I.Module.ValueA, 1);
            Equal(I.Module.ValueB, 2);
            I.Module.ValueB = 3;
            Equal(I.Module.ValueB, 3);
        }

        [Test]
        public void JSInterop()
        {
            var a = new { x = new { y = 2 } };
            Equal(a.GetJS<int>("x", "y"), 2);
            var i = new[] { "x", "y" };
            Equal(a.GetJS<int>(i), 2);

            a.GetJS("x").SetJS("y", 3);
            Equal(a.GetJS<int>("x", "y"), 3);

            a.SetJS("x", 4);
            Equal(a.GetJS<int>("x"), 4);
        }

        [Test]
        public void ImplicitConversions()
        {
            FSharpOption<int> o = 3;
            Equal(o, FSharpConvert.Some(3));
        }

        [Test]
        public void JSObjectTest()
        {
            var o = new JSObject() { { "background-color", "#666" } };
            Equal(o["background-color"], "#666");
            o["background-color"] = "#777";
            Equal(o["background-color"], "#777");
        }
    }
}
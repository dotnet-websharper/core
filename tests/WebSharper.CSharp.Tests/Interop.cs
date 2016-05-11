using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.FSharp.Core;
using WebSharper.Testing;
using I = WebSharper.Collections.Tests.Interop;
using Microsoft.FSharp.Control;

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
        }
    
        [Test]
        public void OptionConditionalAccess()
        {
            var o = FSharpOption<int>.Some(3);
            Equal(o?.Value, 3);
            Equal(o?.ToString(), "3");
            var o2 = FSharpOption<FSharpOption<int>>.Some(FSharpOption<int>.Some(4));
            Equal(o2?.Value.Value, 4);
        }

        [Test]
        public void Union()
        {
            IsTrue(I.Union.NewA(1).IsA);
        }

        [Test]
        public void Record()
        {
            var x = new I.Record(1, null);
            Equal(x.A, 1);
            Equal(x.B, null);
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

        public void AddToTestInt (object sender, int x)
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

        [Test]
        public void FSharpFunc()
        {
            Equal(I.Module.ReturnsFunc().Invoke(3), 3);
            Equal(I.Module.ReturnsFunc2().Invoke(1).Invoke(2), 3);
            Func<int, int> f = I.Module.ReturnsFunc().Invoke;
            Equal(f(3), 3);
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
    }
}
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
using Microsoft.FSharp.Control;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;
using static WebSharper.JavaScript.Pervasives;

namespace WebSharper.CSharp.Tests
{
    delegate int FuncWithOptional(int a = 5);

    [JavaScript, Test("Delegate tests")]
    class DelegateTests : TestCategory
    {
        [Test]
        public void Delegates()
        {
            Expect(8);

            A0(() => IsTrue(true, "Action lambda invocation"));
            A1(x => Equal(x, 122, "Action<T> lambda invocation"));
            Equal(F1(() => 41), 41, "Func<T> lambda invocation");
            Equal(F2(x => x + 41), 42, "Func<T,U> lambda invocation");

            A0(a0);
            A1(a1);
            Equal(F1(f1), 32, "Func<T> method invocation");
            Equal(F2(f2), 5, "Func<T,U> method invocation");
        }

        [Test]
        public void StaticDelegates()
        {
            Equal(F1(static () => 41), 41, "Func<T> lambda invocation");
            Equal(F2(static x => x + 41), 42, "Func<T,U> lambda invocation");
        }

        [Test]
        public async void AsyncLambda()
        {
            var x = 0;
            Equal(x, 0);
            var t = new Task(() => { x = 1; });
            A0(async () => { await t; });
            await FSharpAsync.StartAsTask(FSharpAsync.Sleep(200), null, null);
            // await Task.Delay(200);
            Equal(x, 1);
        }

        private int F1(Func<int> f)
        {
            return f();
        }

        private int F2(Func<int, int> f)
        {
            return f(1);
        }

        private void A0(Action f)
        {
            f();
        }

        private void A1(Action<int> f)
        {
            f(122);
        }

        private int f1() => 32;
        private int f1m() => 42;

        private int f2(int x) => x + 4;

        private void a0() => IsTrue(true, "Action method invocation");

        private void a1(int x) => Equal(x, 122, "Action<T> method invocation");

        [Test]
        public void StaticDelegate()
        {
            var d = new Action(DoNothing);
            d();
            Equal(ForTesting, 2);
        }

        //public static int ForTesting = 0; 

        public static int ForTesting { get; set; } = 1;

        public static void DoNothing() { ForTesting++; }

        [Test]
        public void Target()
        {
            var d = new Func<int>(f1);
            Equal(d.Target, this);
        }

        [Test]
        public void Equality()
        {
            var d1 = new Func<int>(f1);
            var d2 = new Func<int>(f1);
            var d3 = new Func<int>(f1m);
            IsTrue((object)d1 == (object)d2);
            IsTrue(d1 == d2);
            IsTrue(d1.Equals(d2));
            IsTrue(d1.Equals((object)d2));
            IsTrue(d1 != null);
            IsFalse(d1 == d3);
            IsFalse((object)d1 == (object)d3);
            IsTrue(d1 != d3);
        }

        [Test]
        public void DelegateCombine()
        {
            var act = new System.Action(() => { });
            var res = new[] { 0 };
            var a1 = new System.Action<int>(x => res[0] += x);
            var a2 = new System.Action<int>(x => res[0] += 1);
            var comb = a1 + a2;
            comb.Invoke(2);
            Equal(res[0], 3);
            Equal(comb.GetInvocationList().Length, 2);
        }

        [Test("Delegate with default value, known issue: https://github.com/intellifactory/websharper/issues/788", TestKind.Skip)]
        public void DelegateWithOptional()
        {
            var f = new FuncWithOptional(x => x);
            Equal(f(3), 3);
            Equal(f(), 5);
        }

        [Test]
        public void AnonymousMethod()
        {
            Func<int, int, int> sum = delegate (int a, int b) { return a + b; };
            Equal(sum(3, 4), 7);

            Func<int, int, int> sumStatic = static delegate (int a, int b) { return a + b; };
            Equal(sumStatic(3, 4), 7);
        }


        public event Action MyEvent;

        public int EventTester = 0;

        public void IncrEventTester()
        {
            EventTester++;
        }

        public void IncrEventTesterByTwo()
        {
            EventTester += 2;
        }

        public event Action MyExplicitEvent
        {
            add { EventTester++; MyEvent += value; }
            remove { EventTester--; MyEvent -= value; }
        }

        [Test]
        public void Event()
        {
            MyEvent += IncrEventTester;
            this.MyEvent();
            Equal(EventTester, 1, "added IncrEventTester, triggered");

            MyEvent += IncrEventTesterByTwo;
            MyEvent();
            Equal(EventTester, 4, "added IncrEventTesterByTwo, triggered");

            MyEvent -= IncrEventTester;
            MyEvent();
            Equal(EventTester, 6, "removed IncrEventTester, triggered");

            MyEvent += IncrEventTesterByTwo;
            MyEvent();
            Equal(EventTester, 10, "added another IncrEventTesterByTwo, triggered");

            MyEvent -= IncrEventTesterByTwo;
            MyEvent();
            Equal(EventTester, 12, "removed one IncrEventTesterByTwo, triggered");

            MyEvent = null;

            MyExplicitEvent += IncrEventTester;
            Equal(EventTester, 13, "explicit add handler");
            MyEvent();
            Equal(EventTester, 14, "triggered");

            MyExplicitEvent -= IncrEventTester;
            Equal(EventTester, 13, "explicit remove handler");
            Equal(MyEvent, null, "empty event is null");
        }
    }
}

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
using Microsoft.FSharp.Core;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;
using static WebSharper.Core.JavaScript.Parser;
using static WebSharper.Sitelets.Http.Method;

[assembly: WebSharper.JavaScript("Tests.cs")] // test for JavaScript("FileName")

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("C# syntax")]
    class Syntax : TestCategory
    {
        [Test]
        public void While()
        {
            var x = 0;
            while (x < 10) x++;
            Equal(x, 10);
        }

        [Test]
        public void DoWhile()
        {
            var x = 0;
            do x++;
            while (x < 10);
            Equal(x, 10);
        }

        [Test]
        public void For()
        {
            var x = 0;
            for (int i = 0; i < 10; i++)
                x += i;
            Equal(x, 45);
        }

        [Test]
        public void Continue()
        {
            var x = 0;
            for (int i = 0; i < 10; i++)
            {
                if (i % 2 == 0) continue;
                x += i;
            }
            Equal(x, 25);
        }

        [Test]
        public void Break()
        {
            var a = 0;
            for (var i = 0; i < 10; i++)
            {
                a = i;
                if (i == 5) break;
            }
            Equal(a, 5);
        }

        [Test]
        public void Goto()
        {
            var a = 0;
            goto x;
            a += 1;
x: Equal(a, 0);
        }

        [Test]
        public void GotoFinally()
        {
            var a = 0;
            try
            {
                goto x;
            }
            finally
            {
                a = 1;
            }
x: Equal(a, 1);
        }

        [Test]
        public void GotoFinally2()
        {
            var a = 0;
            var b = 0;
            try
            {
                goto y;
                b++;
y: goto x;
            }
            finally
            {
                a = 1;
            }
            b++;
x: Equal(a, 1);
            Equal(b, 0);
        }

        public IEnumerable<int> SmallSeq()
        {
            yield return 1;
            yield return 2;
            yield return 3;
        }

        [Test]
        public void Yield()
        {
            Equal(SmallSeq().Take(5).ToArray(), new[] { 1, 2, 3 });
        }

        [Test]
        public void ForEach()
        {
            var arr = new[] { 1, 2, 3 };
            var x = 0;
            foreach (var e in arr)
                x += e;
            Equal(x, 6);
        }

        [Test]
        public void ForEachExtension()
        {
            var coll = new TestExtensionGetEnumerator() { Values = new() { 1, 2, 3 } };
            var x = 0;
            foreach (var e in coll)
                x += e;
            Equal(x, 6);
        }

        [Test("C# local functions")]
        public void LocalFunctions()
        {
            var one = 1;
            int addOneLocal(int x) => x + one;
            Equal(addOneLocal(1), 2);

            int addOneThis(int x) => x + this.GetOne();
            Equal(addOneThis(1), 2);

            static int add(int x, int y) { return x + y; }
            Equal(add(1, 2), 3);

            [Inline] int addOneInline(int x) => x + one;
            Equal(addOneInline(1), 2);

        }

        class Disposing : IDisposable
        {
            public Action OnDispose = null;

            public Disposing(Action onDispose)
            {
                OnDispose = onDispose;
            }

            public void Dispose()
            {
                OnDispose();
            }
        }

        [Test]
        public void Using()
        {
            var d = 1;
            using (var x = new Disposing(() => d = 2)) { }
            Equal(d, 2);
        }

        [Test]
        public void If()
        {
            var arr = new List<int>();
            for (int i = 0; i < 5; i++)
                if (i % 2 == 0)
                    arr.Add(i);
                else arr.Add(i * 2);
            Equal(arr.ToArray(), new[] { 0, 2, 2, 6, 4 });
        }

        public int GetOne() => 1;

        [Test]
        public void Switch()
        {
            var arr = new List<int>();
            for (int i = 0; i < 4; i++)
                switch (i)
                {
                    case 0:
                        arr.Add(1);
                        break;
                    case 1:
                    case 2:
                        arr.Add(2);
                        break;
                    default:
                        arr.Add(3);
                        break;
                }
            Equal(arr.ToArray(), new[] { 1, 2, 2, 3 });
            Equal(arr[1], 2);
        }

        [Test("C# switch expression")]
        public void SwitchExpression()
        {
            var arr = new List<int>();
            for (int i = 0; i < 4; i++)
                arr.Add(
                    i switch
                    {
                        0 => 1,
                        _ when (i == 1 || i == 2) => 2,
                        _ => 3
                    });
            Equal(arr.ToArray(), new[] { 1, 2, 2, 3 });
            Equal(arr[1], 2);
        }

        [Test]
        public void ArrayIndexer()
        {
            var arr = new[] { 1, 2 };
            Equal(arr[1], 2);
            arr[1] = 3;
            Equal(arr[1], 3);
        }

        [Test]
        public void Types()
        {
            IsTrue(2 is int);
            Equal((object)"d", "d");
        }

        [Test]
        public void Try()
        {
            var x = 0;
            try { throw new Exception("Test"); }
            catch (Exception e) { if (e.Message == "Test") x = 1; }
            Equal(x, 1);
        }

        public void Increment(ref int x)
        {
            x++;
        }

        public void OutOne(out int x)
        {
            x = 1;
        }

        public ref readonly int InRefOut(in int x)
        {
            return ref x;
        }

        public int InRefReadonly(ref readonly int x)
        {
            return x;
        }

        public int f = 0;

        [Test]
        public void RefTest()
        {
            int x = 0;
            Increment(ref x);
            Equal(x, 1);
            int xr = InRefOut(x);
            Equal(xr, 1);
            int irr = InRefReadonly(ref x);
            Equal(irr, 1);
            OutOne(out var y);
            Equal(y, 1);
            OutOne(out int z);
            Equal(z, 1);
            var a = new[] { 2 };
            Increment(ref a[0]);
            Equal(a[0], 3);
            Increment(ref f);
            Equal(f, 1);
        }

        [Test]
        public void RefLocals()
        {
            int x = 0;
            int y = 0;
            ref var r = ref x;
            r++;
            Equal(x, 1);
            r = ref y;
            r--;
            Equal(y, -1);
            x = r = 2;
            r += 2;
            Equal(x, 2);
            Equal(y, 4);
        }

        class Cat
        {
            public int Age { get; set; }
            public string Name { get; set; }
        }

        class MyCollection : IEnumerable<int>
        {
            public List<int> Values = new List<int>();

            public void Add(int x) { Values.Add(x); }

            public void Add(string x) { Values.Add(int.Parse(x)); }

            public IEnumerator<int> GetEnumerator()
            {
                return ((IEnumerable<int>)Values).GetEnumerator();
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return ((IEnumerable<int>)Values).GetEnumerator();
            }
        }

        [Test]
        public void Initializers()
        {
            List<Cat> cats = new List<Cat>
            {
                new Cat(){ Name = "Sylvester", Age = 8 },
                new Cat(){ Name = "Whiskers", Age = 2 },
                new Cat(){ Name = "Sasha", Age = 14 }
            };

            Equal(cats.Count, 3);
            Equal(cats[1].Age, 2);

            var numbers = new Dictionary<int, string>
            {
                [7] = "seven",
                [9] = "nine",
                [13] = "thirteen"
            };

            Equal(numbers.Count, 3);
            Equal(numbers[13], "thirteen");

            var numbers2 = new Dictionary<int, string>
            {
                { 7, "seven" },
                { 9, "nine" },
                { 13, "thirteen" }
            };

            Equal(numbers2.Count, 3);
            Equal(numbers2[13], "thirteen");

            var coll = new MyCollection() { 1, "2", 3 };
            Equal(coll.ToArray(), new[] { 1, 2, 3 });
        }

        public int field = 4;
        public int fieldDefVal;
        public int Prop { get; set; } = 5;
        public int PropDupl { get => Prop; set => Prop = value; }
        public int PropDefVal { get; set; }
        public int RenamedProp { [Name("RnProp")] get; [Name("setRnProp")] set; } = 6;
        [Name("RnProp2")]
        public int RenamedProp2 { get; set; } = 8;

        [Test]
        public void Initialization()
        {
            Equal(field, 4);
            Equal(fieldDefVal, 0);
            Equal(Prop, 5);
            Equal(PropDupl, 5);
            Prop += 10;
            Equal(PropDupl, 15);
            PropDupl += 15;
            Equal(PropDupl, 30);
            Equal(PropDefVal, 0);
            Equal(RenamedProp, 6);
            RenamedProp = 7;
            Equal(RenamedProp, 7);
            Equal(RenamedProp2, 8);
            RenamedProp2 = 9;
            Equal(RenamedProp2, 9);
        }

        [Test]
        public void AnonymousObject()
        {
            var local = 3;
            var a = new { x = 1, y = "hello", local, field, Prop };
            Equal(a.x, 1);
            Equal(a.y, "hello");
            Equal(a.local, 3);
            Equal(a.field, 4);
            Equal(a.Prop, 5);
        }

        [Test]
        public void InterpolatedString()
        {
            string firstName = "James", lastName = "Bond";
            Equal($"My name is {lastName}. {firstName} {lastName}.", "My name is Bond. James Bond.");

            Equal($"align:{"x",2 + 3}", "align:    x");
        }

        [Test]
        public void RawInterpolatedString()
        {
            string firstName = "James", lastName = "Bond";
            Equal($"""
                My name is {lastName}.
                {firstName} {lastName}.
            """,
                """
                    My name is Bond.
                    James Bond.
                """);
        }

        const string _firstName = "James";
        const string _lastName = "Bond";

        const string _introduction = $"My name is {_lastName}. {_firstName} {_lastName}.";

        [Test]
        public void ConstantInterpolatedString()
        {
            Equal(_introduction, "My name is Bond. James Bond.");
        }

        [Test("Date formatting, known issue: https://github.com/intellifactory/websharper/issues/787", TestKind.Skip)]
        public void DateFormatting()
        {
            var d = new DateTime(2010, 4, 8, 15, 5, 39);
            Equal($"format:{d:hh}", "format:03");
        }

        public int OptionalTest(int x, int y = 2, int z = 0, int w = 0) => 100 * x + 10 * y + z - w;

        public int DefaultOptionalTest([Optional] int x) => x;

        public string OptionalStringTest(string x = "None") => x;

        [Test]
        public void OptionalArgument()
        {
            Equal(OptionalTest(1), 120);
            Equal(OptionalTest(1, 0), 100);
            Equal(OptionalTest(1, 5), 150);
            Equal(OptionalTest(1, 5, 2), 152);
            Equal(DefaultOptionalTest(1), 1);
            Equal(DefaultOptionalTest(), 0);
            Equal(OptionalStringTest(), "None");
            Equal(OptionalStringTest(null), null);
            Equal(OptionalStringTest("Some"), "Some");
        }

        [Test]
        public void NamedArgument()
        {
            Equal(OptionalTest(x: 1), 120);
            Equal(OptionalTest(y: 5, x: 1), 150);
            Equal(OptionalTest(z: 3, x: 1), 123);
            Equal(OptionalTest(x: 1, 3), 130);
            Equal(OptionalTest(x: 1, 3, w: 1), 129);
        }

        public int ParamArrayTest(int x = 0, params int[] xs) => x + xs.Sum();

        [Test]
        public void ParamArray()
        {
            Equal(ParamArrayTest(), 0);
            Equal(ParamArrayTest(1), 1);
            Equal(ParamArrayTest(1, 2), 3);
            Equal(ParamArrayTest(1, 2, 3), 6);
            Equal(ParamArrayTest(xs: 1), 1);
            Equal(ParamArrayTest(xs: new[] { 1, 2 }), 3);
        }

        [Test]
        public void FuncInterop()
        {
            Equal(
                Microsoft.FSharp.Collections.ArrayModule.Map<int, string>(FSharpConvert.Fun((int i) => i.ToString()), new[] { 1, 2 }),
                new[] { "1", "2" }
            );
        }

        [Test]
        public void Conversions()
        {
            int intFromChar = 'a';
            StrictEqual(intFromChar, 97, "char to int implicit");
            StrictEqual((int)'a', 97, "char to int explicit");

            StrictEqual((char)97, 'a', "int to char explicit");

            double intFromFloat = 3.2;
            StrictEqual((int)intFromFloat, 3, "float to int explicit");

            MyDouble custom = 12 + 1;
            StrictEqual(custom.Value, 13.0, "Custom implicit conversion in");
            double val = custom;
            StrictEqual(val, 13.0, "Custom implicit conversion out");
            StrictEqual((double)custom, 13.0, "Custom explicit conversion out");
            MyDouble addTest = custom + 1;
            StrictEqual(addTest.Value, 14.0, "Two way conversion");
            addTest += + 1;
            StrictEqual(addTest.Value, 15.0, "Conversion with compound operator");
            addTest++;
            StrictEqual(addTest.Value, 16.0, "Conversion with increment operator");

            unchecked
            {
                StrictEqual((byte)-55, 201, "byte truncates, unchecked C#");
                StrictEqual((sbyte)1000, -24, "sbyte truncates, unchecked C#");
                StrictEqual((Int16)100000, -31072, "Int16 truncates, unchecked C#");
                StrictEqual((UInt16)100000, 34464, "UInt16 truncates, unchecked C#");
                StrictEqual((int)1000000000000, -727379968, "int truncates, unchecked C#");
                StrictEqual((uint)1000000000000, 3567587328, "uint truncates, unchecked C#");
            }
        }

        [Test]
        public void OpearatorOverloading()
        {
            MyNumber decrTest = new(16);

            decrTest -= 2; // note: overloaded to decrease with twice the amount
            StrictEqual(decrTest.Value, 12.0, "Operator overloading, compound operator overload");
            decrTest--; // note: overloaded to decrement twice
            StrictEqual(decrTest.Value, 10.0, "Operator overloading, decrement operator overload");
        }

        [Test]
        public void Dynamic()
        {
            dynamic a = 1;
            Equal(a + 1, 2);
            Equal(a.toExponential(), "1e+0");
            Equal(a.toFixed(2), "1.00");
            a = "1";
            Equal(a + "1", "11");
            a = new { x = 1 };
            Equal(a.x, 1);
            a = new[] { 2 };
            Equal(a[0], 2);
        }

        [Test]
        public void Default()
        {
            Equal(default(int), 0);
            Equal(default(string), null);
        }

        private class NullTest
        {
            int x;

            public NullTest(int x)
            {
                this.x = x;
            }

            public int? X() => this.x;

            public int? Increment()
            {
                this.x++;
                return this.x;
            }
        }

        [Test]
        public void NullConditionalAccess()
        {
            NullTest x = null;
            Equal(x?.X(), null, "object null case");
            x = new NullTest(42);
            Equal(x?.X(), 42, "object non null case");
            Equal(x?.Increment(), 43, "check side effect 1");
            Equal(x?.Increment(), 44, "check side effect 2");

            int? y = null;
            Equal(y?.ToString(), null, "Nullable null case");
            y = 23;
            Equal(y?.ToString(), "23", "Nullable non-null case");
        }

        [Test]
        public void NullCoalesce()
        {
#nullable enable
            NullTest? x = null;
            Equal((x ?? new NullTest(42)).X(), 42, "object null case");
            x = new NullTest(53);
            Equal((x ?? new NullTest(42)).X(), 53, "object non-null case");
            Equal(x.Increment() ?? 12, 54, "check side effect 1");
            Equal(x.Increment() ?? 35, 55, "check side effect 2");
            NullTest xx = null!;
            Equal(xx, null!);
#nullable disable

            int? y = null;
            Equal(y ?? 67, 67, "Nullable null case");
            y = 23;
            Equal(y ?? 67, 23, "Nullable non-null case");
            bool? b = null;
            Equal(b ?? true, true, "Nullable bool null");
            b = false;
            Equal(b ?? true, false, "Nullable bool false");

        }

        public int this[int i] => i * i;

        public string this[string x] { get { return x + "!"; } }

        [Test]
        public void Indexers()
        {
            Equal(this[4], 16);
            Equal(this["hi"], "hi!");
        }

        enum Days { Sat, Sun, Mon, Tue, Wed, Thu, Fri };

        [Test]
        public void Enums()
        {
            var d1 = Days.Sat;
            var d2 = Days.Tue;
            IsTrue(d1.Equals(d1));
            IsTrue(d1.Equals((object)d1));
            IsTrue(d1 == d1);
            IsTrue(d1 != d2);
            IsTrue(d1 < d2);
            Equal(d1.CompareTo(d2), -1);
            IsTrue(d1.HasFlag(Days.Sat));
            IsTrue(d1.GetHashCode() == d1.GetHashCode());
            IsTrue(d1.GetHashCode() != d2.GetHashCode());
        }

        public class ExprVarCtorB
        {
            public ExprVarCtorB(int i, out int j)
            {
                j = i;
            }
        }

        public class ExprVarCtorD : ExprVarCtorB
        {
            public ExprVarCtorD(int i, out int k) : base(i, out var j)
            {
                k = j;
            }
        }

        public int ExprVarField = int.TryParse("42", out var i) ? i : 0;

        public int ExprVarProp { get; set; } = int.TryParse("42", out var i) ? i : 0;

        [Test]
        public void ExpressionVariables()
        {
            var d = new ExprVarCtorD(5, out var x);
            Equal(x, 5, "expression variable in constructor initializer");

            var strings = new string[] { "5" };

            Equal(ExprVarField, 42, "expression variable in field initializer");
            Equal(ExprVarProp, 42, "expression variable in property initializer");

            var r = from s in strings
                    select int.TryParse(s, out var i) ? i : 0;

            Equal(r.ToArray(), new[] { 5 }, "expression variable in query");
        }

        [Test("C# target-typed new")]
        public void TargetTypedNew()
        {
            string[] strings = { "5" };
            IsTrue(Enumerable.SequenceEqual(strings, new[] { "5" }));

            List<int> list = new();
            Equal(list.Count, 0);
        }

        [Test]
        public void TargetTypedConditional()
        {
            int? res = 1 < 2 ? 3 : null;
            Equal(res, 3);
        }

        [Test("C# Index", TestKind.Skip)]
        public void IndexTest()
        {
            string[] strings = { "0", "1", "2", "3", "4" };

            Index i = 2;
            Index j = ^2;

            Equal(strings[i], "2");
            Equal(strings[j], "3");
        }

        [Test("C# Range", TestKind.Skip)]
        public void RangeTest()
        {
            int[] someArray = new int[5] { 1, 2, 3, 4, 5 };
            Equal(someArray[0..2], new[] { 1, 2 });
            Equal(someArray[1..^0], new[] { 2, 3, 4, 5 });
        }

        public static void Validate(bool condition, [CallerArgumentExpression("condition")] string message = null)
        {
            if (!condition)
            {
                throw new InvalidOperationException($"Argument failed validation: <{message}>");
            }
        }

        [Test("CallerArgumentExpression attrribute", TestKind.Skip)]
        public void CallerArgumentExpressionTest()
        {
            try
            {
                Validate(1 == 2);
            }
            catch (Exception e)
            {
                Equal(e.Message, "Argument failed validation: <1 == 2>");
                throw;
            }
        }

        public delegate int LambdaWithDefault(int i = 1);

        public int DefaultLambdaParameter(LambdaWithDefault f)
        {
            return f();
        }

        [Test("lambda with default parameter", TestKind.Skip)]
        public void TestDefaultLambdaParameter()
        {
            Equal(DefaultLambdaParameter(x => x + 2), 3);
        }

        [Test("Extension syntax")]
        public void ExtensionSyntax()
        {
            var a = new MyNumber(1);
            IsFalse(a.IsEven);
            var b = new MyNumber(2);
            IsTrue(b.IsEven);
            Equal(a.AddOne().Value, 2);
            Equal(MyNumber.Add(a, b).Value, 3);
            Equal(MyNumber.Subtract(a, b).Value, -1);
            Equal(MyNumber.Zero.Value, 0);
            Equal((new MyNumber(5) % new MyNumber(2)).Value, 1);
            a.MutableVal = 3;
            Equal(a.MutableVal, 3);
            a += 1; // overloaded +=
            Equal(a.Value, 5);
        }

        [Test("Null-coalescing assignment")]
        public void NullCoalescingAssignment()
        {
            string x = null;
            x ??= "hello";
            Equal(x, "hello");

            x = null;
            ref var rx = ref x;
            rx ??= "hi";
            Equal(x, "hi");
        }

        [Test("Null-conditional assignment")]
        public void NullConditionalAssignment()
        {
            FSharpRef<string> x = null;
            x?.Value ??= "hello";
            Equal(x?.Value, null);
            x = new("");
            Equal(x?.Value, "");
            x?.Value = "hello";
            Equal(x.Value, "hello");

            int[] a = null;
            a?[0] = 2;
            Equal(a?[0], null);
            a = [1];
            Equal(a?[0], 1);
            a?[0] = 2;
            Equal(a?[0], 2);

            FSharpRef<FSharpRef<int>> rr = new(new(0));
            rr?.Value?.Value = 3;
            Equal(rr.Value.Value, 3);

            x.Value = null;
            ref var rx = ref x;
            rx?.Value ??= "hi";
            Equal(x.Value, "hi");
        }

        [Test()]
        public void FieldKeywordTest()
        {
            var x = new FieldKeyword { Message = "hello" };
            Equal(x.Message, "hello");
            Raises(() => x.Message = null);
        }
    }

    [JavaScript]
    class TestExtensionGetEnumerator
    {
        public List<int> Values = new List<int>();
    }

    [JavaScript]
    static class TestExtensionGetEnumeratorExtension
    {
        public static IEnumerator<int> GetEnumerator(this TestExtensionGetEnumerator coll)
        {
            return coll.Values.GetEnumerator();
        }
    }

    [JavaScript]
    public class MyDouble
    {
        public double val;
        public MyDouble(double d) { val = d; }

        public double Value => val;

        public static implicit operator double(MyDouble d)
        {
            return d.val;
        }

        public static implicit operator MyDouble(double d)
        {
            return new MyDouble(d);
        }

        public override string ToString()
        {
            return val.ToString();
        }
    }

    [JavaScript]
    public class MyNumber
    {
        public int val;
        public MyNumber(int d) { val = d; }

        public int Value => val;

        public static MyNumber operator +(MyNumber a, MyNumber b)
        {
            return new(a.Value + b.Value);
        }

        // instance-level -- overload;
        public void operator --()
        {
            val--;
            val--;
        }

        // instance-level -= overload;
        public void operator -=(int d)
        {
            val -= 2 * d;
        }
    }

    [JavaScript]
    public static class MyNumberExtension
    {
        extension(MyNumber a)
        {
            // Extension property shorthand:
            public bool IsEven => a.Value % 2 == 0;

            // Extension property:
            public int MutableVal
            {
                get { return a.val; }
                set { a.val = value; }
            }

            // Extension method:
            public MyNumber AddOne() => new(a.Value + 1);

            // Static extension method within:
            public static MyNumber Subtract(MyNumber x, MyNumber y) => new(x.Value - y.Value);

            // instance-level += overload;
            public void operator +=(int d)
            {
                a.val += 2 * d;
            }
        }

        extension(MyNumber)
        {
            // Static extension property:
            public static MyNumber Zero => new MyNumber(0);

            // Static extension method:
            public static MyNumber Add(MyNumber a, MyNumber b) => a + b;

            // Static extension operator:
            public static MyNumber operator %(MyNumber a, MyNumber b)
            {
                return new(a.Value % b.Value);
            }
        }
    }

    [JavaScript]
    public class FieldKeyword {
        public string Message
        {
            get;
            set => field = value ?? throw new ArgumentNullException(nameof(value));
        }
    }

    [JavaScript]
    public class NameChangeEventArgs : EventArgs
    {
        public string OldName { get; set; }
        public string NewName { get; set; }
    }

    // this already has JavaScript attribute
    public partial class PartialClass
    {
        public int Field2 = 2;

        private string _name;
        public partial string Name
        {
            get => _name;
            set
            {
                var oldName = _name;
                _name = value;
                _nameChanged(this, new() { OldName = oldName, NewName = _name });
            }
        }

        partial void PartialMethod() { Value = 3; }

        private partial int PartialMethodRelaxed() { return Value; }

        public void SetValueTo4()
        {
            Value = 4;
        }

        public partial PartialClass(string name) { Name = name; }

        private EventHandler<NameChangeEventArgs> _nameChanged;
        
        public partial event EventHandler<NameChangeEventArgs> OnNameChanged
        {
            add { _nameChanged += value; }
            remove { _nameChanged -= value; }
        }

    }
}

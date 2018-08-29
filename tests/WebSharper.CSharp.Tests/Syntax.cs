using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;
using System.Collections;

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

        public int f = 0;

        [Test]
        public void RefTest()
        {
            int x = 0;
            Increment(ref x);
            Equal(x, 1);
            int xr = InRefOut(x);
            Equal(xr, 1);
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

            Equal($"align:{"x", 2 + 3}", "align:    x");
        }

        [Test("Date formatting, known issue: https://github.com/intellifactory/websharper/issues/787", TestKind.Skip)]
        public void DateFormatting()
        {
            var d = new DateTime(2010, 4, 8, 15, 5, 39);
            Equal($"format:{d:hh}", "format:03");
        }

        public int OptionalTest(int x, int y = 2, int z = 0, int w = 0) => 100*x + 10*y + z - w;

        public int DefaultOptionalTest([Optional] int x) => x;

        [Test]
        public void OptionalArgument()
        {
            Equal(OptionalTest(1), 120);
            Equal(OptionalTest(1, 0), 100);
            Equal(OptionalTest(1, 5), 150);
            Equal(OptionalTest(1, 5, 2), 152);
            Equal(DefaultOptionalTest(1), 1);
            Equal(DefaultOptionalTest(), 0);
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

            MyNumber custom = 12 + 1;
            StrictEqual(custom.Value, 13.0, "Custom implicit conversion in");
            double val = custom;
            StrictEqual(val, 13.0, "Custom implicit conversion out");
            StrictEqual((double)custom, 13.0, "Custom explicit conversion out");
            MyNumber addTest = custom + 1;
            StrictEqual(addTest.Value, 14.0, "Operator overloading");

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

            public int? Increment() {
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
            NullTest x = null;
            Equal((x ?? new NullTest(42)).X(), 42, "object null case");
            x = new NullTest(53);
            Equal((x ?? new NullTest(42)).X(), 53, "object non-null case");
            Equal(x.Increment() ?? 12, 54, "check side effect 1");
            Equal(x.Increment() ?? 35, 55, "check side effect 2");

            int? y = null;
            Equal(y ?? 67, 67, "Nullable null case");
            y = 23;
            Equal(y ?? 67, 23, "Nullable non-null case");
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
    }

    [JavaScript]
    public class MyNumber
    {
        public double val;
        public MyNumber(double d) { val = d; }

        public double Value => val;

        public static implicit operator double(MyNumber d)
        {
            return d.val;
        }
        public static implicit operator MyNumber(double d)
        {
            return new MyNumber(d);
        }
        public static MyNumber operator +(MyNumber a, MyNumber b)
        {
            return a.Value + b.Value;
        }
        public override string ToString()
        {
            return val.ToString();
        }
    }

    // this already has JavaScript attribute
    public partial class PartialClass
    {
        public int Field2 = 2;

        partial void PartialMethod() { Value = 3; }

        public void SetValueTo4()
        {
            Value = 4;
        }
    }
}

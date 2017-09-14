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

        public int f = 0;

        [Test]
        public void RefTest()
        {
            int x = 0;
            Increment(ref x);
            Equal(x, 1);
#if CSHARP7
            OutOne(out var y);
            Equal(y, 1);
            OutOne(out int z);
            Equal(z, 1);
#endif
            var a = new[] { 2 };
            Increment(ref a[0]);
            Equal(a[0], 3);
            Increment(ref f);
            Equal(f, 1);
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
                new Cat(){ Name = "Sylvester", Age=8 },
                new Cat(){ Name = "Whiskers", Age=2 },
                new Cat(){ Name = "Sasha", Age=14 }
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
#if CSHARP7
        public int PropDupl { get => Prop; set => Prop = value; }
#else
        public int PropDupl {
            get { return Prop; }
            set { Prop = value; }
        }
#endif
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

            var d = new DateTime(2010, 4, 8, 15, 5, 39);
            Equal($"format:{d:hh}", "format:03");
        }

        public int OptionalTest(int x, int y = 2, int z = 0) => x + y + z;

        public int DefaultOptionalTest([Optional] int x) => x;

        [Test]
        public void OptionalArgument()
        {
            Equal(OptionalTest(1), 3);
            Equal(OptionalTest(1, 0), 1);
            Equal(OptionalTest(1, 5), 6);
            Equal(OptionalTest(1, 5, 2), 8);
            Equal(DefaultOptionalTest(1), 1);
            Equal(DefaultOptionalTest(), 0);
        }

        [Test]
        public void NamedArgument()
        {
            Equal(OptionalTest(x: 1), 3);
            Equal(OptionalTest(y: 5, x: 1), 6);
            Equal(OptionalTest(z: 2, x: 2), 6);
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

        class MyNumber
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
        }

        [Test]
        public void Conversions()
        {
            Console.WriteLine("Running Conversions test..");

            int intFromChar = 'a';
            Equal(intFromChar, 97, "char to int implicit");
            Equal((int)'a', 97, "char to int explicit");

            Equal((char)97, 'a', "int to char explicit");

            double intFromFloat = 3.2;
            Equal((int)intFromFloat, 3, "float to int explicit");

            MyNumber custom = 12 + 1;
            Equal(custom.Value, 13.0, "Custom implicit conversion in");
            double val = custom;
            Equal(val, 13.0, "Custom implicit conversion out");
            Equal((double)custom, 13.0, "Custom explicit conversion out");
            MyNumber addTest = custom + 1;
            Equal(addTest.Value, 14.0, "Operator overloading");

            JavaScript.Console.Log("Conversions test", "finished");
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

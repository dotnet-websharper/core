using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("C# classes")]
    class ObjectTests : TestCategory
    {
        public static bool StaticConstructorRan = false;

        class CctorTest
        {
            public static int TestStatic = 0;

            static CctorTest()
            {
                TestStatic = 1;
                StaticConstructorRan = true;
            }

            public static void RunCctor() { }
        }

        [Test]
        public void StaticConstructor()
        {
            CctorTest.RunCctor();
            IsTrue(StaticConstructorRan);
            Equal(CctorTest.TestStatic, 1);
        }

        class CtorTest
        {
            public int OriginalValue;
            public int Value = 1;

            public CtorTest()
            {
                OriginalValue = Value;
                Value = 2;
            }

            public CtorTest(int value) : this()
            {
                Value = value;
            }

            public CtorTest(bool bvalue) : this(bvalue ? 1 : 0)
            {
            }
        }

        [Test]
        public void Constructor()
        {
            var o1 = new CtorTest();
            Equal(o1.OriginalValue, 1);
            Equal(o1.Value, 2);
            var o2 = new CtorTest(3);
            Equal(o2.OriginalValue, 1);
            Equal(o2.Value, 3);
            var o3 = new CtorTest() { Value = 4 };
            Equal(o3.OriginalValue, 1);
            Equal(o3.Value, 4);
        }

        class GenericClass<T>
        {
            public U GenericMethod<U>(U x)
            {
                return x;
            }
        }

        [Test]
        public void Generics()
        {
            Equal(new GenericClass<int>().GenericMethod<string>("test"), "test");
        }

        class SubClass : BaseClass
        {
            public SubClass() : base(2) { }

            public override int VirtualMethod() { return 2; }

            public int BaseCall() { return base.VirtualMethod(); }

            public SubClass(int v) : this() { Value += v; }
        }

        class BaseClass : AbstractBaseClass
        {
            public virtual int VirtualMethod() { return 1; }

            public override int AbstractMethod() { return 2; }
        
            public BaseClass() { }

            public BaseClass(int v) { Value = v; }

            public int Value = 1;
        }

        abstract class AbstractBaseClass
        {
            public abstract int AbstractMethod();
        }

        [Test]
        public void Inheritance()
        {
            Equal(new BaseClass().Value, 1, "Field initialization");
            Equal(new BaseClass().VirtualMethod(), 1, "Virtual method");
            Equal(new BaseClass().AbstractMethod(), 2, "Abstract method");
            Equal((new BaseClass() as AbstractBaseClass).AbstractMethod(), 2, "Abstract method called on base class");
            Equal(new BaseClass(3).Value, 3, "Overloaded constructor");
            Equal(new SubClass().Value, 2, "Base call on constructor");
            Equal(new SubClass().VirtualMethod(), 2, "Overridden method");
            Equal(new SubClass().BaseCall(), 1, "Base method call");
            Equal(new SubClass(3).Value, 5, "Chained constructor");
        }

        interface ISomething
        {
            int Foo();

            string Bar { get; }
        }

        class Something : ISomething
        {
            public string Bar => "Bar";

            public int Foo() => 42;
        }

        [Test]
        public void InterfaceImplementations()
        {
            var o = new Something();
            Equal(o.Bar, "Bar");
            Equal(((ISomething)o).Bar, "Bar");
            Equal(o.Foo(), 42);
            Equal(((ISomething)o).Foo(), 42);
        }

        struct StructTest
        {
            public readonly int X;
            public readonly string Y;

            public StructTest(int x, string y)
            {
                X = x;
                Y = y;
            }

            public int X2 => X + 1;
        }

        StructTest DefStruct;

        [Test]
        public void Struct()
        {
            Equal(new StructTest().X, 0);
            Equal(new StructTest().Y, null);
            Equal(DefStruct.X, 0);
            Equal(DefStruct.Y, null);
            Equal(new StructTest(1, "").X, 1);
            Equal(new StructTest(1, "").X2, 2);
        }

        [Test]
        public void PartialClasses()
        {
            var o = new PartialClass();
            Equal(o.Field1, 1);
            Equal(o.Field2, 2);
            Equal(o.Value, 0);
            o.SetValueTo3ByPartialMethod();
            Equal(o.Value, 3);
            o.SetValueTo4();
            Equal(o.Value, 4);
        }

        class Renamings
        {
            [Name("x")]
            public int y = 1;

            [Name("X")]
            public int RNValue { get { return y; } set { y = value; } }

            [Name("GetX")]
            public int RNMethod() => y;

            [Name("xx")]
            public int yy { get; set; } = 2;
        }

        [Test]
        public void Renaming()
        {
            var r = new Renamings();
            Equal(r.y, 1);
            r.y = 2;
            Equal(r.y, 2);
            Equal(r.RNValue, 2);
            r.RNValue = 3;
            Equal(r.RNValue, 3);
            Equal(r.RNMethod(), 3);

            Equal(r.yy, 2);
            r.yy = 3;
            Equal(r.yy, 3);

            dynamic o = r;
            Equal(o.x, 3);
            Equal(o.X(), 3);
            Equal(o.GetX(), 3);
            o.set_X(4);
            Equal(o.x, 4);

            Equal(o.xx(), 3);
            o.set_xx(4);
            Equal(o.xx(), 4);
        }

        [JavaScript]
        public struct MyStruct
        {
            public readonly int X;
            public readonly int Y;

            public MyStruct(int x, int y)
            {
                this.X = x;
                this.Y = y;
            }

            public int Sum => X + Y;
        }

        [Test]
        public void Structs()
        {
            var s1 = new MyStruct(1, 2);
            var s2 = new MyStruct(1, 2);
            var s3 = new MyStruct(1, 3);
            IsTrue(s1.Equals(s2));
            IsTrue((object)s1 == (object)s2);
            IsTrue((System.ValueType)s1 == (System.ValueType)s2);
            IsFalse(s1.Equals(s3));
            IsTrue(s1.GetHashCode() != -1);
            Equal(s1.Sum, 3);
        }

        public class MyException : Exception
        {
            public bool IsThisMyException => true;
        }

        [Test]
        public void Exception()
        {
            var res = "";
            try
            {
                throw new MyException();
            }
            catch (MyException e)
            {
                if (e.IsThisMyException)
                    res = "ok";
                else
                    res = "wrong method value on exception";
            }
            catch (ArgumentException)
            {
                res = "wrong type of exception";
            }
            catch
            {
                res = "wrong type of exception";
            }
            Equal(res, "ok");
        }
    }

    [JavaScript]
    public partial class PartialClass
    {
        public int Field1 = 1;

        public int Value { get; set; }

        partial void PartialMethod();

        public void SetValueTo3ByPartialMethod()
        {
            PartialMethod();
        }
    }
}

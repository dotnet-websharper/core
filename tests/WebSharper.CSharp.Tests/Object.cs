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
        class CctorTest
        {
            public static int TestStatic = 0;

            static CctorTest()
            {
                TestStatic = 1;
            }
        }

        [Test]
        public void StaticConstructor()
        {
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

        class BaseClass
        {
            public virtual int VirtualMethod() { return 1; }

            public BaseClass() { }

            public BaseClass(int v) { Value = v; }

            public int Value = 1;
        }

        [Test]
        public void Inheritance()
        {
            Equal(new BaseClass().Value, 1, "Field initialization");
            Equal(new BaseClass().VirtualMethod(), 1, "Virtual method");
            Equal(new BaseClass(3).Value, 3, "Overloaded constructor");
            Equal(new SubClass().Value, 2, "Base call on constructor");
            Equal(new SubClass().VirtualMethod(), 2, "Overridden method");
            Equal(new SubClass().BaseCall(), 1, "Base method call");
            Equal(new SubClass(3).Value, 5, "Chained constructor");
        }
    }
}

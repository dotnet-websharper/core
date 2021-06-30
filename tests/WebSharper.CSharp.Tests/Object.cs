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

            public override SubClass CovariantVirtualMethod() { return this; }

            public int BaseCall() { return base.VirtualMethod(); }

            public SubClass(int v) : this() { Value += v; }

            public int ValueAlias => this.Value;
        }

        class BaseClass : AbstractBaseClass
        {
            public virtual int VirtualMethod() { return 1; }

            public override int AbstractMethod() { return 2; }

            public virtual BaseClass CovariantVirtualMethod() { return this; }

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

        [Test("C# covariant inheritance", TestKind.Skip)]
        public void CovariantInheritance()
        {
            Equal(new BaseClass().CovariantVirtualMethod().Value, 1, "Covariant virtual method");
            Equal(new SubClass().CovariantVirtualMethod().ValueAlias, 2, "Covariant override method");
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

        interface ITestDefaultImpl
        {
            int Foo() => 42;
        }

        class TestDefaultImpl : ITestDefaultImpl
        {
        }

        class TestDefaultImpl2 : ITestDefaultImpl
        {
            int Foo() => 2;
        }

        [Test("C# interface default implementations", TestKind.Skip)]
        public void InterfaceDefaultImplementations()
        {
            var o = new TestDefaultImpl();
            Equal(((ITestDefaultImpl)o).Foo(), 42);
            var o2 = new TestDefaultImpl2();
            Equal(((ITestDefaultImpl)o).Foo(), 2);
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
            Equal(o.GetValueByPartialMethod(), 3);
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

            public MyException() : base("This is my exception") { }
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
                {
                    if (e.Message == "This is my exception")
                        res = "ok";
                    else
                        res = "wrong message on exception";
                }
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

        partial void PartialMethodNotImpl();

        private partial int PartialMethodRelaxed();

        public void SetValueTo3ByPartialMethod()
        {
            PartialMethod();
            PartialMethodNotImpl();
        }

        public int GetValueByPartialMethod()
        {
            return PartialMethodRelaxed();
        }
    }
}

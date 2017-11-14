using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;
using static WebSharper.JavaScript.Pervasives;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("C# Arithmetic")]
    class Arithmetic : TestCategory
    {
#if CSHARP7
        [Test]
        public void LiteralWithUnderscore()
        {
            Equal(1_024, 1024);
        }
#endif

        [Test]
        public void Int32()
        {
            int x = 22;
            Equal(+x, 22, "Unary plus");
            Equal(-x, -22, "Unary minus");
            Equal(x + 3, 25, "Addition");
            Equal(x - 4, 18, "Subtraction");
            Equal(x * 3, 66, "Multiplication");
            Equal(x / 5, 4, "Division");
            Equal(x % 5, 2, "Modulus");
            Equal(x | 13, 31, "Bitwise or");
            Equal(x & 13, 4, "Bitwise or");
            Equal(x ^ 13, 27, "Bitwise or");
            Equal(x >> 2, 5, "Shift right");
            Equal(x << 2, 88, "Shift left");
            IsTrue(x == 22, "Equality");
            IsFalse(x == 23, "Equality");
            IsTrue(x != 23, "Inequality");
            IsFalse(x != 22, "Inequality");
            IsTrue(x < 25, "Less than");
            IsFalse(x < 22, "Less than");
            IsTrue(x > 12, "Greater than");
            IsFalse(x > 22, "Greater than");
            IsTrue(x <= 26, "Less than or equal");
            IsTrue(x <= 22, "Less than or equal");
            IsTrue(x >= 14, "Greater than or equal");
            IsTrue(x >= 22, "Greater than or equal");
        }

        [Test]
        public void Long()
        {
            long x = 22;
            Equal(+x, 22, "Unary plus");
            Equal(-x, -22, "Unary minus");
            Equal(x + 3, 25, "Addition");
            Equal(x - 4, 18, "Subtraction");
            Equal(x * 3, 66, "Multiplication");
            Equal(x / 5, 4, "Division");
            Equal(x % 5, 2, "Modulus");
            Equal(x | 13, 31, "Bitwise or");
            Equal(x & 13, 4, "Bitwise or");
            Equal(x ^ 13, 27, "Bitwise or");
            Equal(x >> 2, 5, "Shift right");
            Equal(x << 2, 88, "Shift left");
            IsTrue(x == 22, "Equality");
            IsFalse(x == 23, "Equality");
            IsTrue(x != 23, "Inequality");
            IsFalse(x != 22, "Inequality");
            IsTrue(x < 25, "Less than");
            IsFalse(x < 22, "Less than");
            IsTrue(x > 12, "Greater than");
            IsFalse(x > 22, "Greater than");
            IsTrue(x <= 26, "Less than or equal");
            IsTrue(x <= 22, "Less than or equal");
            IsTrue(x >= 14, "Greater than or equal");
            IsTrue(x >= 22, "Greater than or equal");
        }

        [Test]
        public void Double()
        {
            double x = 22.3;
            Equal(+x, 22.3, "Unary plus");
            Equal(-x, -22.3, "Unary minus");
            ApproxEqual(x + 3.1, 25.4, "Addition");
            ApproxEqual(x - 4.2, 18.1, "Subtraction");
            ApproxEqual(x * 3.1, 69.13, "Multiplication");
            ApproxEqual(x / 5.1, 4.372549, "Division");
            ApproxEqual(x % 5.1, 1.9, "Modulus");
            IsTrue(x == 22.3, "Equality");
            IsFalse(x == 23, "Equality");
            IsTrue(x != 23, "Inequality");
            IsFalse(x != 22.3, "Inequality");
            IsTrue(x < 25, "Less than");
            IsFalse(x < 22.3, "Less than");
            IsTrue(x > 12, "Greater than");
            IsFalse(x > 22.3, "Greater than");
            IsTrue(x <= 26, "Less than or equal");
            IsTrue(x <= 22.3, "Less than or equal");
            IsTrue(x >= 14, "Greater than or equal");
            IsTrue(x >= 22.3, "Greater than or equal");
        }

        [Test]
        public void Float()
        {
            float x = 22.3f;
            Equal(+x, 22.3f, "Unary plus");
            Equal(-x, -22.3f, "Unary minus");
            ApproxEqual(x + 3.1f, 25.4f, "Addition");
            ApproxEqual(x - 4.2f, 18.1f, "Subtraction");
            ApproxEqual(x * 3.1f, 69.13f, "Multiplication");
            ApproxEqual(x / 5.1f, 4.372549f, "Division");
            ApproxEqual(x % 5.1f, 1.9f, "Modulus");
            IsTrue(x == 22.3f, "Equality");
            IsFalse(x == 23, "Equality");
            IsTrue(x != 23, "Inequality");
            IsFalse(x != 22.3f, "Inequality");
            IsTrue(x < 25, "Less than");
            IsFalse(x < 22.3f, "Less than");
            IsTrue(x > 12, "Greater than");
            IsFalse(x > 22.3f, "Greater than");
            IsTrue(x <= 26, "Less than or equal");
            IsTrue(x <= 22.3f, "Less than or equal");
            IsTrue(x >= 14, "Greater than or equal");
            IsTrue(x >= 22.3f, "Greater than or equal");
        }

        [Test]
        public void Char()
        {
            Equal('a' + 2, 'c', "Addition");
            Equal('c' - 2, 'a', "Subtraction");
            IsTrue('a' == 'a', "Equality");
            IsFalse('a' == 'c', "Equality");
            IsFalse('a' != 'a', "Inequality");
            IsTrue('a' != 'c', "Inequality");
            IsTrue('a' < 'c', "Less than");
            IsFalse('c' < 'c', "Less than");
            IsTrue('c' > 'a', "Greater than");
            IsFalse('c' > 'c', "Greater than");
            IsTrue('a' <= 'c', "Less than or equal");
            IsTrue('c' <= 'c', "Less than or equal");
            IsTrue('c' >= 'a', "Greater than or equal");
            IsTrue('c' >= 'c', "Greater than or equal");
        }

        [Test]
        public void String()
        {
            Equal("foo" + "bar", "foobar", "Concatenation");
            IsTrue("foo" == "foo", "Equality");
            IsTrue("foo" != "bar", "Inequality");
            Equal("foo" + 1, "foo1");
            Equal(1 + "foo", "1foo");
        }

        [Test]
        public void NullableAbsorbing()
        {
            int? x = 1;
            int? y = null;
            int? z = x + y;
            IsTrue(z == null);
        }

        [Test]
        public void CompareTo()
        {
            Equal((3).CompareTo(5), -1);
            Equal((4).CompareTo(4), 0);
            Equal((5).CompareTo(3), 1);
            Equal((3.0).CompareTo(5.0), -1);
        }

        [Test]
        public void NumEquals()
        {
            IsTrue((3).Equals(3));
            IsTrue((3).Equals((object)3));
            IsFalse((3).Equals("3"));
            IsFalse((3).Equals((object)"3"));
        }

        [Test]
        public void Bool()
        {
            IsTrue(true.Equals(true));
            IsTrue(true.Equals((object)true));
            IsTrue(true == true);
            IsTrue(true != false);
            IsTrue(true.GetHashCode() == true.GetHashCode());
            IsTrue(true.GetHashCode() != false.GetHashCode());
            Equal(true.CompareTo(false), 1);
            Equal(bool.TrueString, "true");
            Equal(bool.FalseString, "false");
            Equal(true.ToString(), "true");
            Equal(false.ToString(), "false");
            IsTrue(bool.Parse("true") && bool.Parse("True"));
            IsTrue(!bool.Parse("false") && !bool.Parse("False"));
        }

        [Test]
        public void TimeSpanTest()
        {
            var t = TimeSpan.FromHours(1);
            IsTrue(t == +t);
            Equal((-t).Hours, -1);
            Equal((t + t).Hours, 2);
            Equal(t - t, TimeSpan.Zero);
            IsTrue(t == +t);
            IsTrue(t != -t);
            IsTrue(t + t > t);
            IsTrue(t < t + t);
            IsTrue(t + t >= t);
            IsTrue(t <= t + t);
        }

        [Test]
        public void DateTimeTest()
        {
            var d = new System.DateTime(2017, 11, 14);
            var d2 = new System.DateTime(2017, 11, 15);
            var t = TimeSpan.FromHours(1);
            Equal((d2 - d).Days, 1);
            IsTrue(d == d + TimeSpan.Zero);
            IsTrue(d != d2);
            IsTrue(d + t > d);
            IsTrue(d - t < d);
            IsTrue(d + t >= d);
            IsTrue(d <= d + t);
        }
    }
}

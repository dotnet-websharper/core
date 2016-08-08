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
    }
}

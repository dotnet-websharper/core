using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("String operations")]
    class Strings : TestCategory
    {
        class A
        {
            public override string ToString()
            {
                return "a object";
            }
        }

        [Test]
        public void Format()
        {
            Equal(String.Format("xyz"), "xyz");
            Equal(String.Format("{0}", (object)"xyz"), "xyz");
            var xyz = "xyz";
            Equal(String.Format(xyz), "xyz");
            var pricePerOunce = 17.36;
            Equal(String.Format("The current price is {0} per ounce.", pricePerOunce),
                "The current price is 17.36 per ounce.");
            Equal(String.Format("{0,6} {1,15}", "foo", 12), "   foo              12");
            Equal(String.Format("{0,-6} {1,-15}", "foo", 12), "foo    12             ");
            Equal(String.Format("{0} {1} {2}", "foo", 12, new A()), "foo 12 a object");
            Equal(String.Format("{0} {1} {2} {3}", "foo", 12, 12.35, (long)500), "foo 12 12.35 500");

            var fmt = "{0} {1}";
            Equal(String.Format(fmt, 1, 2), "1 2");
            fmt = "The current price is {0} per ounce.";
            Equal(String.Format(fmt, pricePerOunce),
                "The current price is 17.36 per ounce.");
            fmt = "{0,6} {1,15}";
            Equal(String.Format(fmt, "foo", 12), "   foo              12");
            fmt = "{0,-6} {1,-15}";
            Equal(String.Format(fmt, "foo", 12), "foo    12             ");
            fmt = "{0} {1} {2}";
            Equal(String.Format(fmt, "foo", 12, new A()), "foo 12 a object");
            fmt = "{0} {1} {2} {3}";
            Equal(String.Format(fmt, "foo", 12, 12.35, (long)500), "foo 12 12.35 500");
        }
    }
}

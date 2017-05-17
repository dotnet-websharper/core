using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;
using System.Collections;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("C# Pattern matching")]
    public class PatternMatching : TestCategory
    {
        class FakeTuple
        {
            int _a;
            string _b;

            public FakeTuple(int a, string b)
            {
                _a = a;
                _b = b;
            }

            public void Deconstruct(out int a, out string b)
            {
                a = _a;
                b = _b;
            }
        }

        [Test]
        public void Deconstruction()
        {
            var t = new FakeTuple(0, "hello");
            var (a, b) = t;
            Equal(a, 0);
            Equal(b, "hello");
        }

        [Test]
        public void Switch()
        {
            object o = "hello";
            int res = 0;
            switch (o)
            {
                case null:
                    res = 1;
                    break;
                case string s when s[0] == 'h':
                    res = 2;
                    break;
                default:
                    res = 4;
                    break;
            }
            Equal(res, 2);
        }

        [Test]
        public void Is()
        {
            object o = "hello";
            int res = 0;
            if (o is string s && s[0] == 'h')
                res = 2;
            Equal(res, 2);
        }
    }
}

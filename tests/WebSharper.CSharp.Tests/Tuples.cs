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
    [JavaScript, Test("C# Tuples")]
    public class Tuples : TestCategory
    {
        [Test]
        public void Construction()
        {
            var t = (0, "hello");
            Equal(t.Item1, 0);
            Equal(t.Item2, "hello");
            var (x, y) = t;
            Equal(x, 0);
            Equal(y, "hello");
        }

        [Test]
        public void Named()
        {
            var t = (a: 0, b: "hello");
            Equal(t.a, 0);
            Equal(t.b, "hello");
            var (x, y) = t;
            Equal(x, 0);
            Equal(y, "hello");
        }

        [Test]
        public void Foreach()
        {
            var e = new[] { (0, (0, 0)), (1, (1, 1)) };
            var r = new List<int>();
            foreach ((int x, (int y, int z)) in e)
                r.Add(x + y);
            foreach (var (x, (y, z)) in e)
                r.Add(x + y);
            Equal(r.ToArray(), new[] { 0, 3, 0, 3 });
        }
    }
}

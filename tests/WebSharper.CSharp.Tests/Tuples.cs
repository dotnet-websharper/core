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
#if CSHARP7
        [Test]
        public void Construction()
        {
            var t = (0, "hello");
            Equal(t.Item1, 0);
            Equal(t.Item2, "hello");
            var (x, y) = t;
            Equal(x, 0);
            Equal(y, "hello");
            var (x2, _) = t;
            Equal(x2, 0);
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
            var arr = new[] { (0, (0, 0)), (1, (1, 1)) };
            var r = new List<int>();
            foreach ((int x, (int y, int z)) in (arr))
                r.Add(x + y + z);
            foreach (var (x, (y, z)) in (arr))
                r.Add(x + y + z);
            Equal(r.ToArray(), new[] { 0, 3, 0, 3 });
        }

        [Test]
        public void Conversions()
        {
            var t = (0, "hello").ToTuple();
            Equal(t.Item1, 0);
            Equal(t.Item2, "hello");
            var (x, y) = t; // deconstructing a System.Tuple
            Equal(x, 0);
            Equal(y, "hello");
            var (x2, _) = t;
            Equal(x2, 0);
            var vt = t.ToValueTuple();
            Equal(vt.Item1, 0);
            Equal(vt.Item2, "hello");
        }

        // Mutable structs are not yet supported

        //[Test]
        //public void Mutability()
        //{
        //    void NotIncrFst((int, string) tup)
        //    {
        //        tup.Item1++;
        //    }
        //    void Incr(ref int i)
        //    {
        //        i++;
        //    }
        //    void IncrFst(ref (int, string) tup)
        //    {
        //        tup.Item1++;
        //    }
        //    void IncrFst2(ref (int a, string b) tup)
        //    {
        //        tup.a++;
        //    }

        //    var t = (0, "hello");
        //    t.Item1 = 1;
        //    t.Item1++;
        //    Equal(t, (2, "hello"));
        //    t.Item1 = 2;
        //    NotIncrFst(t); // does not increment because of struct copying
        //    Equal(t.Item1, 2);
        //    t.Item1 = 2;
        //    Incr(ref t.Item1);
        //    Equal(t.Item1, 3);
        //    t.Item1 = 3;
        //    IncrFst(ref t);
        //    Equal(t.Item1, 4);
        //    t.Item1 = 4;
        //    IncrFst2(ref t);
        //    Equal(t.Item1, 5);
        //}
#endif
    }
}

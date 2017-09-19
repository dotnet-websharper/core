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
    [JavaScript, Test("Guid tests")]
    class GuidTests : TestCategory
    {
        [Test]
        public void Comparison()
        {
            var g = Guid.NewGuid();
            var g2 = g;
            IsTrue(g == g2);
            IsTrue(g != Guid.Empty);
            IsTrue(g != Guid.NewGuid());
            Equal(g.CompareTo(Guid.Empty), 1);
            Equal(g.CompareTo((object)Guid.Empty), 1);
        }

        public void GuidToString()
        {
            var g = Guid.Parse("cf34c174-82ec-4bb1-b0a5-79edc485128c");
            Equal(g.ToString("N"), "cf34c17482ec4bb1b0a579edc485128c");
            Equal(g.ToString("D"), "cf34c174-82ec-4bb1-b0a5-79edc485128c");
            Equal(g.ToString("B"), "{cf34c174-82ec-4bb1-b0a5-79edc485128c}");
            Equal(g.ToString("P"), "(cf34c174-82ec-4bb1-b0a5-79edc485128c)");
            Equal(g.ToString("X"), "{0xcf34c174,0x82ec,0x4bb1,{0xb0,0xa5,0x79,0xed,0xc4,0x85,0x12,0x8c}}");
        }

        public void Parse()
        {
            var n = "cf34c17482ec4bb1b0a579edc485128c";
            var d = "cf34c174-82ec-4bb1-b0a5-79edc485128c";
            var b = "{cf34c174-82ec-4bb1-b0a5-79edc485128c}";
            var p = "(cf34c174-82ec-4bb1-b0a5-79edc485128c)";
            var x = "{0xcf34c174,0x82ec,0x4bb1,{0xb0,0xa5,0x79,0xed,0xc4,0x85,0x12,0x8c}}";
            var g = Guid.Parse(n);
            Equal(Guid.Parse(d), g);
            Equal(Guid.Parse(b), g);
            Equal(Guid.Parse(p), g);
            Equal(Guid.Parse(x), g);
            Equal(Guid.ParseExact(n, "N"), g);
            Equal(Guid.ParseExact(d, "D"), g);
            Equal(Guid.ParseExact(b, "B"), g);
            Equal(Guid.ParseExact(p, "P"), g);
            Equal(Guid.ParseExact(x, "X"), g);
            IsFalse(Guid.TryParse("cf34c174-82ec-4bb1-b0a5+79edc485128c", out g));
        }

        public void TryParse()
        {
            IsTrue(Guid.TryParse("cf34c174-82ec-4bb1-b0a5-79edc485128c", out var g));
            Equal(g.ToString(), "cf34c174-82ec-4bb1-b0a5-79edc485128c");

            IsFalse(Guid.TryParse("notaguid", out g));
        }
    }
}

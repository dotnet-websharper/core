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
            IsTrue(g == g);
            IsTrue(g != Guid.Empty);
            IsTrue(g != Guid.NewGuid());
            Equal(g.CompareTo(Guid.Empty), 1);
            Equal(g.CompareTo((object)Guid.Empty), 1);
        }

        public void Parse()
        {
            var g = Guid.Parse("cf34c174-82ec-4bb1-b0a5-79edc485128c");
            Equal(g.ToString(), "cf34c174-82ec-4bb1-b0a5-79edc485128c");
        }

        public void TryParse()
        {
            IsTrue(Guid.TryParse("cf34c174-82ec-4bb1-b0a5-79edc485128c", out var g));
            Equal(g.ToString(), "cf34c174-82ec-4bb1-b0a5-79edc485128c");

            IsFalse(Guid.TryParse("notaguid", out g));
        }
    }
}

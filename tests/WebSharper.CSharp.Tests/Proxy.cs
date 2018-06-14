using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;
using static WebSharper.JavaScript.Pervasives;
using static WebSharper.JavaScript.Interop;

namespace WebSharper.CSharp.Tests
{
    [Proxy(typeof(StringBuilder))]
    internal class StringBuilderProxy
    {
        private List<string> b = new List<string>();

        public StringBuilderProxy Append(string s)
        {
            b.Add(s);
            return this;
        }

        public override string ToString()
        {
            var s = String.Concat(b);
            b.Clear();
            b.Add(s);
            return s;
        }
    }
}

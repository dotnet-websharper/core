using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Web;

namespace WebSharper.CSharp.Tests
{
    public class InlineControlTest
    {
        public static Web.Control RunTestsControl(bool runServerTests) =>
            new InlineControl(() => Tests.RunTests(runServerTests));
    }
}

#load "../CSharpTester.fsx"

let source = """
using WebSharper;

namespace WebSharper.CSharp.Tests
{
    [JavaScript]
    public static class Test
    {
        public static int Add1(int x)
        {
            return x + 1;
        }

        public static int TestValue = Add1(41);
    }
}
"""

CSharpTester.toJSFiles source

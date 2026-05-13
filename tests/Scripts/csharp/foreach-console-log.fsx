#load "../CSharpTester.fsx"

let source = """
using System;
using WebSharper;
using WebSharper.JavaScript;

namespace WebSharper.CSharp.Tests
{
    [JavaScript]
    public class TestDefaultImpl
    {
        [SPAEntryPoint]
        public static void Main() 
        {
            var arr = new[] { 1 };
            foreach (var a in arr) 
            {
                Console.Log(a);
            }
        }
    }
}
"""

CSharpTester.toJSFiles source

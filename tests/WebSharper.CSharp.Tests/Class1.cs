using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Microsoft.FSharp.Core;
using A = WebSharper.Core.Attributes;
using System.Threading.Tasks;

namespace WebSharper.CSharp.Tests
{
    public class Server
    {
        [A.Remote]
        public static Task<int> GetOneAsync()
        {
            return Task.FromResult(1);
        }
    }

    [A.JavaScript]
    public class Class1
    {
        public int GetSync() => 3;

        //public void GoToTest()
        //{
        //    try
        //    {
        //    A:
        //        Console.WriteLine(1);
        //        goto A;
        //    }
        //    catch
        //    {
        //        goto C;
        //    }
        //    finally
        //    {
        //    B:
        //        Console.WriteLine(1);
        //        goto B;
        //    }
        //C:
        //    return;
        //}

        //public async Task<int> GetIntAsync()
        //{
        //    var t = new Task<int>(GetSync);
        //    try
        //    {

        //    }
        //    finally
        //    {
        //        var x = await t;
        //    }
        //    return await t;
        //}

        public async Task<bool> RemotingTest()
        {
            var r = await Server.GetOneAsync();
            return r == 1;
        }

        public async Task<int> GetOneAsync()
        {
            var o = Task.FromResult(1);
            return await o;
        }

        public IEnumerable<int> Fibonacci()
        {
            yield return 1;
            yield return 1;
            var a = 1;
            var b = 1;
            while (true)
            {
                var c = a + b;
                a = b;
                b = c;
                yield return b;
            }
        }

        public string HelloWorld() { return "Hello " + "world!"; }

        //public async Task<string> AsyncHelloWorld() => await Task.Run(() => "Hello world!");

        //public string GetValue(FSharpOption<string> opt) { return opt.Value; }

        //public FSharpOption<string> GetSomeA() { return FSharpOption<string>.Some("A"); }
    }
}

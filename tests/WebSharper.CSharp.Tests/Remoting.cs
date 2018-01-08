using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.FSharp.Core;
using WebSharper.Testing;
using System.Threading.Tasks;
using System.Threading;
using WebSharper.Web;
using static WebSharper.JavaScript.Pervasives;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Serializable]
    public struct TestStruct
    {
        public int X;
        public int Y;

        public TestStruct (int x, int y)
        {
            this.X = x;
            this.Y = y;
        }
    }

    [JavaScript, Serializable]
    public class TestClass
    {
        public int X = 0;
        public int Y { get; set; }
    }

    public static class Server
    {
        [Remote]
        public static Task<int> GetOneAsync()
        {
            return Task.FromResult(1);
        }

        [Remote]
        public static Task<int> AddOneAsync(int x)
        {
            return Task.FromResult(x + 1);
        }

        private static Dictionary<int, int> values = new Dictionary<int, int>();

        [Remote]
        public static void Void() { }

        [Remote]
        public static void Set(int key, int value)
        {
            values[key] = value;
        }

        [Remote]
        public static Task SetAsync(int key, int value)
        {
            Set(key, value);
            return Task.FromResult<object>(null); // .NET 4.6 has Task.CompletedTask;
        }

        [Remote]
        public static Task<int> Extract(int key)
        {
            Thread.Sleep(1000); // to wait out a call to void Set();
            int value = -1;
            values.TryGetValue(key, out value);
            values.Remove(key);
            return Task.FromResult(value);
        }

        [Remote]
        public static async Task Login(string user)
        {
            var ctx = WebSharper.Web.Remoting.GetContext();
            await ctx.UserSession.LoginUserAsync(user);
        }

        [Remote]
        public static async Task<string> GetUser()
        {
            var ctx = WebSharper.Web.Remoting.GetContext();
            return await ctx.UserSession.GetLoggedInUserAsync();
        }

        [Remote]
        public static async Task Logout()
        {
            var ctx = WebSharper.Web.Remoting.GetContext();
            await ctx.UserSession.LogoutAsync();
        }

        [Remote]
        public static Task<TestClass> IncrementXY(TestClass o)
        {
            o.X++;
            o.Y++;
            return Task.FromResult(o);
        }

        [Remote]
        public static Task<TestStruct> IncrementXYStruct(TestStruct o)
        {
            return Task.FromResult(new TestStruct(o.X + 1, o.Y + 1));
        }

        public static int Zero()
        {
            return 0;
        }

        static Server()
        {
            // test IsClient
            if (WebSharper.Pervasives.IsClient)
            {
                throw (new Exception("This should not happen: IsClient is true on the server"));
            }
            else
            {
                if (!WebSharper.Pervasives.IsClient)
                {
                    WebSharper.Core.Remoting.AddHandler(typeof(Handler), new HandlerImpl());
                }
            }
        }
    }

    public abstract class Handler
    {
        [Remote]
        public abstract Task<int> Increment(int x);

        [Remote]
        public abstract Task Reset();
    }

    public class HandlerImpl : Handler
    {
        private static int counter = 0;

        public override Task<int> Increment(int x)
        {
            counter += x;
            return Task.FromResult(counter);
        }

        public override Task Reset()
        {
            counter = 0;
            return Task.FromResult(0);
        }
    }

    [JavaScript, Test("Task based remoting")]
    public class Remoting : TestCategory
    {
        public static bool ShouldRun { get; set; } = true;

        public int GetSync() => 3;

        [Test]
        public async Task SimpleAsync()
        {
            if (!ShouldRun) { Expect(0); return; }
            var r = await Server.GetOneAsync();
            Equal(r, 1);
        }

        [Test]
        public async Task SimpleAsyncWith1Arg()
        {
            if (!ShouldRun) { Expect(0); return; }
            var r = await Server.AddOneAsync(1783);
            Equal(r, 1784);
        }

        [Test]
        public void SimpleVoid()
        {
            if (!ShouldRun) { Expect(0); return; }
            Server.Void();
            IsTrue(true);
        }

        [Test]
        public async Task VoidAndCheckReturn()
        {
            if (!ShouldRun) { Expect(0); return; }
            var k = (int)(JavaScript.Math.Round(JavaScript.Math.Random() * 100));
            var v = (int)(JavaScript.Math.Round(JavaScript.Math.Random() * 100));
            Server.Set(k, v);
            var v2 = await Server.Extract(k);
            Equal(v, v2);
        }

        [Test]
        public async Task VoidTaskAndCheckReturn()
        {
            if (!ShouldRun) { Expect(0); return; }
            var k = (int)(JavaScript.Math.Round(JavaScript.Math.Random() * 100));
            var v = (int)(JavaScript.Math.Round(JavaScript.Math.Random() * 100));
            await Server.SetAsync(k, v);
            var v2 = await Server.Extract(k);
            Equal(v, v2);
        }

        [Test]
        public async Task UserSession()
        {
            if (!ShouldRun) { Expect(0); return; }
            Equal(await Server.GetUser(), null, "No logged in user");
            await Server.Login("testuser");
            Equal(await Server.GetUser(), "testuser", "Get logged in user");
            await Server.Logout();
            Equal(await Server.GetUser(), null, "Logout");
        }
         
        [Test]
        public async Task CustomHandler()
        {
            if (!ShouldRun) { Expect(0); return; }
            await Remote<Handler>().Reset();
            Equal(await Remote<Handler>().Increment(5), 5);
            Equal(await Remote<Handler>().Increment(5), 10);
        }

        [Test]
        public async Task CustomClass()
        {
            if (!ShouldRun) { Expect(0); return; }
            var o = new TestClass();
            o.X = 1;
            o.Y = 1;
            o = await Server.IncrementXY(o);
            Equal(o.X, 2);
            Equal(o.Y, 2);
        }

        [Test]
        public async Task CustomStruct()
        {
            if (!ShouldRun) { Expect(0); return; }
            var o = new TestStruct(1, 1);
            o = await Server.IncrementXYStruct(o);
            Equal(o.X, 2);
            Equal(o.Y, 2);
        }

        [Test]
        public void IsClientTest()
        {
            if (WebSharper.Pervasives.IsClient)
            {
                if (!WebSharper.Pervasives.IsClient)
                {
                    Server.Zero();
                }
                else
                {
                    Equal(1, 1, "IsClient in if statement");
                }
            }
            else
            {
                Server.Zero();
            }
            var ok =
                WebSharper.Pervasives.IsClient ? 
                    (!WebSharper.Pervasives.IsClient ? Server.Zero() : 1)
                    : Server.Zero();
            Equal(ok, 1, "IsClient in conditional expression");
        }
    }
}

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}
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
    [JavaScript]
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

    [JavaScript]
    public class TestClass
    {
        public int X = 0;
        public int Y { get; set; }
    }

    [JavaScript]
    public class TestClassSub : TestClass
    {
    }

    [JavaScript]
    public record TestRecordPos(int X, int Y);

    [JavaScript]
    public record TestRecord
    {
        public int X { get; }
        public int Y { get; }
        public TestRecord (int x, int y)
        {
            X = x;
            Y = y;
        }
    }

    public static class CSharpServer
    {
        [Remote]
        public static Task<int> GetOneAsync()
        {
            return Task.FromResult(1);
        }

        [Remote]
        public static Task<int> FailOnServer()
        {
            return Task.FromException<int>(new Exception("Deliberately failing for testing."));
        }

        [Remote]
        public static Task<int> AddOneAsync(int x)
        {
            return Task.FromResult(x + 1);
        }

        [Remote]
        public static Task<(int, int)> AddOnesAsync((int, int) t)
        {
            var (x, y) = t;
            return Task.FromResult((x + 1, y + 1));
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
            return Task.CompletedTask;
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
        public static Task<TestClassSub> IncrementXYSub(TestClassSub o)
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

        [Remote]
        public static Task<TestRecordPos> IncrementXYRecordPos(TestRecordPos o)
        {
            return Task.FromResult(new TestRecordPos(o.X + 1, o.Y + 1));
        }

        [Remote]
        public static Task<TestRecord> IncrementXYRecord(TestRecord o)
        {
            return Task.FromResult(new TestRecord(o.X + 1, o.Y + 1));
        }

        public static int Zero()
        {
            return 0;
        }

        static CSharpServer()
        {
            WebSharper.Core.Remoting.AddHandler(typeof(Handler), new HandlerImpl());
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
            var r = await CSharpServer.GetOneAsync();
            Equal(r, 1);
        }

        [Test]
        public async Task SimpleAsyncWith1Arg()
        {
            if (!ShouldRun) { Expect(0); return; }
            var r = await CSharpServer.AddOneAsync(1783);
            Equal(r, 1784);
        }

        [Test]
        public async Task WithValueTuple()
        {
            if (!ShouldRun) { Expect(0); return; }
            var r = await CSharpServer.AddOnesAsync((1783, 456));
            Equal(r, (1784, 457));
        }

        [Test]
        public void SimpleVoid()
        {
            if (!ShouldRun) { Expect(0); return; }
            CSharpServer.Void();
            IsTrue(true);
        }

        [Test]
        public async Task VoidAndCheckReturn()
        {
            if (!ShouldRun) { Expect(0); return; }
            var k = (int)(JavaScript.Math.Round(JavaScript.Math.Random() * 100));
            var v = (int)(JavaScript.Math.Round(JavaScript.Math.Random() * 100));
            CSharpServer.Set(k, v);
            var v2 = await CSharpServer.Extract(k);
            Equal(v, v2);
        }

        [Test]
        public async Task VoidTaskAndCheckReturn()
        {
            if (!ShouldRun) { Expect(0); return; }
            var k = (int)(JavaScript.Math.Round(JavaScript.Math.Random() * 100));
            var v = (int)(JavaScript.Math.Round(JavaScript.Math.Random() * 100));
            await CSharpServer.SetAsync(k, v);
            var v2 = await CSharpServer.Extract(k);
            Equal(v, v2);
        }

        [Test]
        public async Task UserSession()
        {
            if (!ShouldRun) { Expect(0); return; }
            Equal(await CSharpServer.GetUser(), null, "No logged in user");
            await CSharpServer.Login("testuser");
            Equal(await CSharpServer.GetUser(), "testuser", "Get logged in user");
            await CSharpServer.Logout();
            Equal(await CSharpServer.GetUser(), null, "Logout");
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
            o.Y = 2;
            o = await CSharpServer.IncrementXY(o);
            Equal(o.X, 2);
            Equal(o.Y, 3);
        }

        [Test]
        public async Task CustomSubClass()
        {
            if (!ShouldRun) { Expect(0); return; }
            var o = new TestClassSub();
            o.X = 1;
            o.Y = 1;
            o = await CSharpServer.IncrementXYSub(o);
            Equal(o.X, 2);
            Equal(o.Y, 2);
        }

        [Test]
        public async Task CustomStruct()
        {
            if (!ShouldRun) { Expect(0); return; }
            var o = new TestStruct(1, 2);
            o = await CSharpServer.IncrementXYStruct(o);
            Equal(o.X, 2);
            Equal(o.Y, 3);
        }

        [Test]
        public async Task RecordPos()
        {
            if (!ShouldRun) { Expect(0); return; }
            var o = new TestRecordPos(1, 2);
            o = await CSharpServer.IncrementXYRecordPos(o);
            IsTrue(o == new TestRecordPos(1, 2));
        }

        [Test]
        public async Task Record()
        {
            if (!ShouldRun) { Expect(0); return; }
            var o = new TestRecord(1, 2);
            o = await CSharpServer.IncrementXYRecord(o);
            IsTrue(o == new TestRecord(1, 2));
        }

        [Test]
        public void IsClientTest()
        {
            if (WebSharper.Pervasives.IsClient)
            {
                if (!WebSharper.Pervasives.IsClient)
                {
                    CSharpServer.Zero();
                }
                else
                {
                    Equal(1, 1, "IsClient in if statement");
                }
            }
            else
            {
                CSharpServer.Zero();
            }
            var ok =
                WebSharper.Pervasives.IsClient ? 
                    (!WebSharper.Pervasives.IsClient ? CSharpServer.Zero() : 1)
                    : CSharpServer.Zero();
            Equal(ok, 1, "IsClient in conditional expression");
        }

        static public async Task<int> AddOneAsyncSafe(int x)
        {
            try
            {
                return await CSharpServer.AddOneAsync(x);
            }
            catch (Exception)
            {
                Console.WriteLine("Unexpected server error");
                throw;
            }
        }

        static public async Task<int> TestServerFailure()
        {
            try
            {
                return await CSharpServer.FailOnServer();
            }
            catch (Exception)
            {
                Console.WriteLine("Successfully caught exception in C# async");
                throw;
            }
        }

        [Test]
        public async Task AsyncTryCatch()
        {
            if (!ShouldRun) { Expect(0); return; }
            var r = await AddOneAsyncSafe(1783);
            Equal(r, 1784);
            r = 0;
            try
            {
                await TestServerFailure();
            }
            catch (Exception)
            {
                r = 1;
            }
            Equal(r, 1);
        }
    }
}

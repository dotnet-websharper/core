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
using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using WebSharper.Testing;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("C# Task and async tests")]
    public class TaskAsync : TestCategory
    {
        public async Task<int> GetOneAsync()
        {
            var o = Task.FromResult(1);
            return await o;
        }

        [Test("Async/Await")]
        public async Task AsyncAwait()
        {
            var one = await GetOneAsync();
            Equal(one, 1);
        }

        [Test("loop inside Async")]
        public async Task AsyncLoop()
        {
            var i = 0;
            while (i < 10)
            {
                await Task.Delay(0);
                i++;
            }
            Equal(i, 10);
        }

        int Counter = 0;

        async Task IncrementWithCancel(CancellationToken ct)
        {
            for (var i = 0; i < 10; i++)
            {
                await Task.Delay(10);
                ct.ThrowIfCancellationRequested();
                Counter++;
            }
        }

        [Test("Yield")]
        public async Task Yield()
        {
            var one = await GetOneAsync();
            await Task.Yield();
            Equal(one, 1);
        }

        [Test("Task cancellation")]
        public async void TaskCancellation()
        {
            Expect(0);
            var cts = new CancellationTokenSource();
            var task = IncrementWithCancel(cts.Token);
            await Task.Delay(30);
            cts.Cancel();
            await Task.Delay(50);
            IsTrue(task.IsCanceled);
            IsTrue(Counter < 5);
        }

        [Test("Local async function")]
        public async void LocalAsync()
        {
            async Task<int> GetOneAsyncLocal()
            {
                var o = Task.FromResult(1);
                return await o;
            }

            var one = await GetOneAsyncLocal();
            Equal(one, 1);
        }

        [Test("Result")]
        public void Result()
        {
            Equal(Task.FromResult<int>(1).Result, 1);
            Raises(() => Task.FromException<int>(new Exception()).Result);
            Raises(() => new Task<int>(() => 2).Result);
        }

        [Test]
        public async Task WebWorker()
        {
            var worker = new JavaScript.Worker(self =>
            {
                self.Onmessage = e =>
                {
                    self.PostMessage(GlobalClass.GlobalFunction((string)e.Data));
                };
            });
            var t = new TaskCompletionSource<string>();
            worker.Onmessage = e =>
            {
                t.SetResult("The worker replied: " + (string)e.Data);
            };
            worker.PostMessage("Hello world!");
            var res = await t.Task;
            worker.Terminate();
            Equal(res, "The worker replied: [worker] Hello world!");
        }
    }

    // This needs to be a separate class, otherwise the web worker bundle
    // will include QUnit as a dependency.
    [JavaScript]
    public static class GlobalClass
    {
        public static string GlobalFunction(string s)
        {
            return "[worker] " + s;
        }

    }
}

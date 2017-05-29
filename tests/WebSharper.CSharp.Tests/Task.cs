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
    }
}

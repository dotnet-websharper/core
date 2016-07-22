# Task-based asynchronicity in C#

WebSharper supports [C# Task-based Asynchronous Pattern][asyncs] on the client,
implementing them with JavaScript callbacks.

As in C#, an object of type `Task` or `Task<T>` represents a waiting, ongoing
or finished operation. The limitations on the client are:

* All parallelism is cooperative as the JavaScript runtime is single-threaded. 
  You have to yield control inside a
  task to let other workflows execute.
  The current supported way is to use `await Task.Delay(0)` to yield control
  at periodically when you are defining a CPU-heavy computation that .
  `Task.Yield` is currently unavailable but planned.
  For example: 
  
	public async void LotsOfHelloWorld(int n)
	{
		for(int i = 0; i<n; i++)
		{
			Console.WriteLine("Hello world!"); // prints to JavaScript console
			if (i % 1000 == 0) await Task.Delay(0); // yield control regularly  
		}
	}

* Parallelization is possible with `WhenAny` and `WaitAll` methods.
	
* There is no way to use `Async.RunSynchronously`. Also, no `Task` static methods
  are available that are using a specific `TaskScheduler`.

* Currently `await` by the `IAsyncResult` interface is not supported, but planned.
  
[asyncs]: https://msdn.microsoft.com/en-us/library/mt674882.aspx

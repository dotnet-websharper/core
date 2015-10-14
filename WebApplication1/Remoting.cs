using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

using static WebSharper.Core.Attributes;
using Microsoft.FSharp.Control;
using static Microsoft.FSharp.Core.ExtraTopLevelOperators;

namespace WebApplication1
{
    public static class Server
    {
        [Remote]
        public static FSharpAsync<string> DoSomething(string input) =>
            DefaultAsyncBuilder.Return(new string(input.ToCharArray().Reverse().ToArray()));
    }
}
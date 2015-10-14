using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using WebSharper.Html.Client;
using static WebSharper.Html.Client.Tags;
using static WebSharper.Html.Client.EventsPervasives;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Control;
using static Microsoft.FSharp.Core.ExtraTopLevelOperators;
using static WebSharper.Core.Attributes;

namespace WebApplication1
{
    [JavaScript]
    public static class FuncHelper
    {
        public static FSharpFunc<A, B> Convert<A, B>(Func<A, B> f) =>
            FSharpFunc<A, B>.FromConverter(x => f(x));

        //public static FSharpFunc<A, Unit> Convert<A>(Action<A> f) =>
        //    FSharpFunc<A, Unit>.FromConverter(x => { f(x); return null; });

        //public static FSharpFunc<A, FSharpFunc<B, C>> Convert<A, B, C>(Func<A, B, C> f) =>
        //    FSharpFunc<A, FSharpFunc<B, C>>.FromConverter(x => FSharpFunc<B, C>.FromConverter(y => f(x, y)));

        public static FSharpFunc<A, FSharpFunc<B, Unit>> Convert<A, B>(Action<A, B> f) =>
            FSharpFunc<A, FSharpFunc<B, Unit>>.FromConverter(x =>
                FSharpFunc<B, Unit>.FromConverter(y => { f(x, y); return null; }));
    }

    [JavaScript]
    public static class Client
    {
        public static Element Main()
        {
            var input = Input(new[] { Attr.Value("") });
            var output = H1(new Element[] { });
            var button = Button(new[] { Text("Send") });
            var async = Microsoft.FSharp.Core.ExtraTopLevelOperators.DefaultAsyncBuilder;
            //var x = 1 + 1;
            OnClick(FuncHelper.Convert<Element, Events.MouseEvent>((a, b) =>
            {
                FSharpAsync.Start(
                    async.Bind<string, Unit>(Server.DoSomething(input.Value),
                        FuncHelper.Convert<string, FSharpAsync<Unit>>(data => { output.Text = data; return async.Zero(); })),
                    FSharpOption<System.Threading.CancellationToken>.None);
            })).Invoke(button);
            return Div(new[]
            {
                input,
                button,
                HR(new Element[] { }),
                H4(new[] { Attr.Class("text-muted"), Text("The server responded:") }),
                Div(new[] { Attr.Class("jumbotron"), output })
            });
        }
    }
}
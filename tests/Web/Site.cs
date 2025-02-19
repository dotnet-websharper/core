using System;
using System.Threading.Tasks;
using WebSharper;
using WebSharper.Sitelets;
using C = WebSharper.Sitelets.Tests.Client;

namespace Web
{
    public class ExtraBundleTest
    {
        [JavaScript]
        public static C.Node SayHello()
        {
            Console.WriteLine("Hello world from System.Console!");
            WebSharper.JavaScript.Console.Log("Hello world from WebSharper.JavaScript.Console!");
            return C.Elt("div", C.Text("Hello from an inline control!"));
        }

        public static Task<Content> MainPage(Context<object> ctx, object endpoint) =>
            Content.Page(
                new WebSharper.Web.InlineControl(() => SayHello()),
                Bundle: "extrabundle"
            );
    }
}
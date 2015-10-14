using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

using Microsoft.FSharp.Core;
using Microsoft.FSharp.Control;

using WebSharper.Html.Server;
using static WebSharper.Html.Server.Html;
using static WebSharper.Html.Server.Tags;
using WebSharper.Sitelets;
using static WebSharper.Application.SPA;
using Microsoft.FSharp.Collections;
using WebSharper;

[assembly: Website(typeof(WebApplication1.Site.Website))]

namespace WebApplication1
{
    [Serializable]
    public class MainControl : WebSharper.Web.Control
    {
        public override WebSharper.Html.Client.IControlBody Body
        {
            // TODO: property getters/setters should look for attributes defined on the property itself too
            [WebSharper.Core.Attributes.JavaScript]
            get
            { return Client.Main(); }
        }
    }

    public static class Templating
    {
        class Page
        {
            public string Title { get; set; }
            public Element[] Body { get; set; }
        }

        static Content.Template<Page> MainTemplate =
            new Content.Template<Page>("~/Main.html")
                .With("title", x => x.Title)
                .With("body", x => x.Body);

        public static FSharpAsync<Content<EndPoint>> Main(string title, Element[] body) =>
             Content.WithTemplate<EndPoint, Page>(MainTemplate, new Page() { Title = title, Body = body });
    }

    public static class Site
    {
        static FSharpAsync<Content<EndPoint>> HomePage(Context<EndPoint> ctx) =>
            Templating.Main("Home",
                new[] {
                    H1(new[] { Text("Say Hi to the server!") }),
                    Div(new[] { new MainControl() })
                });

        //[Website]
        static Sitelet<EndPoint> Main =>
            WebSharper.Application.SinglePage(
                FSharpFunc<Context<EndPoint>, FSharpAsync<Content<EndPoint>>>.FromConverter(HomePage));

        public class Website : IWebsite<EndPoint>
        {
            public FSharpList<EndPoint> Actions => FSharpList<EndPoint>.Empty;

            public Sitelet<EndPoint> Sitelet => Main;
        }
    }
}
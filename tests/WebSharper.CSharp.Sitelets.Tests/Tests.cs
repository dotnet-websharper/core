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
using WebSharper;
using WebSharper.Web;
using WebSharper.Sitelets;
using Elt = WebSharper.Sitelets.Tests.Server.Elt;
using Attr = WebSharper.Sitelets.Tests.Server.Attr;
using Text = WebSharper.Sitelets.Tests.Server.Text;
using C = WebSharper.Sitelets.Tests.Client;

namespace WebSharper.CSharp.Sitelets.Tests
{
    [Serializable]
    public class TestControl : Web.Control
    {
        [JavaScript]
        public override IControlBody Body =>
            C.Elt("div", C.Text("Hello from a web control class!"));
    }

    public class SiteletTest
    {
        [JavaScript]
        public static C.Node SayHello()
        {
            Console.WriteLine("Hello world from System.Console!");
            JavaScript.Console.Log("Hello world from WebSharper.JavaScript.Console!");
            return C.Elt("div", C.Text("Hello from an inline control!"));
        }

        [JavaScript]
        public static C.Node SayHello(string msg)
        {
            return C.Elt("div", C.Text($"Hello from an inline control with message: {msg}!"));
        }

        [JavaScript]
        public static C.Node Hello =>
            C.Elt("div", C.Text("Hello from an inline control calling a static property!"));

        public static Sitelet<object> Main =>
            new SiteletBuilder()
                .With("/", ctx =>
                    Content.Page(
                        Body:
                            Elt("div",
                                Elt("div",
                                    Elt("a", Attr("href", ctx.Link(JohnDoe)),
                                        Text("Go to John Doe's page"))),
                                Elt("div",
                                    Elt("a", Attr("href", ctx.Link(JaneDoe)),
                                        Text("Go to Jane Doe's page"))),
                               Elt("form",
                                    Attr("action", ctx.Link(EmptyQueryPerson)),
                                    Attr("method", "get"),
                                    Text("This form should work. "),
                                    Elt("input", Attr("name", "first"), Attr("value", "Jane")),
                                    Elt("input", Attr("name", "last"), Attr("value", "Smith")),
                                    Elt("input", Attr("name", "age"), Attr("type", "number"), Attr("value", "42")),
                                    Elt("input", Attr("type", "submit"))),
                                Elt("form",
                                    Attr("action", ctx.Link(EmptyQueryPerson)),
                                    Attr("method", "post"),
                                    Text("This form should lead to a 404 (method specified on the endpoint). "),
                                    Elt("input", Attr("name", "first"), Attr("value", "Jane")),
                                    Elt("input", Attr("name", "last"), Attr("value", "Smith")),
                                    Elt("input", Attr("name", "age"), Attr("type", "number"), Attr("value", "42")),
                                    Elt("input", Attr("type", "submit"))),
                                new TestControl(),
                                new WebSharper.Web.InlineControl(() => SayHello()),
                                new WebSharper.Web.InlineControl(() => SayHello("ok")),
                                new WebSharper.Web.InlineControl(() => Hello)
                                )))
                .With<Person>((ctx, person) =>
                    Content.Page(
                        Body:
                            Elt("div",
                                Text(String.Format("{0} {1} is {2} years old. ",
                                    person.name.first, person.name.last, person.age)),
                                Elt("a", Attr("href", ctx.Link("/")),
                                    Text("Go back to C# sitelets tests home")))))
                .With<PersonRec>((ctx, person) =>
                    Content.Page(
                        Body:
                            Elt("div",
                                Text(String.Format("{0} {1} is {2} years old. (from C# record)",
                                    person.name.first, person.name.last, person.age)),
                                Elt("a", Attr("href", ctx.Link("/")),
                                    Text("Go back to C# sitelets tests home")))))
                .With<QueryPerson>((ctx, person) =>
                    Content.Page(
                        Body:
                            Elt("div",
                                person.age.HasValue ?
                                    Text(String.Format("{0} {1} is {2} years old. ",
                                        person.name.first, person.name.last, person.age.Value)) :
                                    Text(String.Format("{0} {1} won't tell their age.",
                                        person.name.first, person.name.last)),
                                Elt("a", Attr("href", ctx.Link("/")),
                                    Text("Go back to C# sitelets tests home")))))
                .Install();

        [EndPoint("/person/{name}/{age}", "/person/{age}/{name}")]
        public class Person
        {
            public Name name;
            public int age;
        }

        [EndPoint("{first}/{last}")]
        public class Name
        {
            public string first;
            public string last;
        }

        [EndPoint("/personrec/{name}/{age}", "/personrec/{age}/{name}")]
        public record PersonRec(NameRec name, int age);

        [EndPoint("{first}/{last}")]
        public record NameRec(string first, string last);

        [Method("GET"), EndPoint("qperson/{name}")]
        public class QueryPerson
        {
            public QueryName name;

            [Query]
            public int? age;
        }

        public class QueryName
        {
            [Query]
            public string first;

            [Query]
            public string last;
        }

        private static Elt Elt(string name, params Web.INode[] children) => new Elt(name, children);
        private static Attr Attr(string name, string value) => new Attr(name, value);
        private static Text Text(string text) => new Text(text);
        public static Person JohnDoe => new Person { name = new Name { first = "John", last = "Doe" }, age = 30 };
        public static PersonRec JaneDoe => new (new ("Jane", "Doe"), 31);
        private static QueryPerson EmptyQueryPerson => new QueryPerson { name = new QueryName() };
    }
}

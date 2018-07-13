module WebSharper.Testing.Resources

open WebSharper

[<Require(typeof<JQuery.Resources.JQuery>)>]
[<Sealed>]
type QUnit() =
    inherit Resources.BaseResource("https://code.jquery.com/qunit/",
        "qunit-2.6.1.js", "qunit-2.6.1.css")

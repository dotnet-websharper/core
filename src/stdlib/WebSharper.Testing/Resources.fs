module WebSharper.Testing.Resources

open WebSharper

[<Require(typeof<JQuery.Resources.JQuery>)>]
[<Sealed>]
type QUnit() =
    inherit Resources.BaseResource("https://code.jquery.com/qunit/",
        "qunit-1.18.0.js", "qunit-1.18.0.css")

[<assembly: Require(typeof<QUnit>)>]
do ()
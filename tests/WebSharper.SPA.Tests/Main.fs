module WebSharper.SPA.Tests

open WebSharper
open WebSharper.JavaScript

[<SPAEntryPoint>]
let Main() =
    WebSharper.Tests.Main.RunTests()
        .ReplaceInDom(JS.Document.QuerySelector "#main")
    

[<assembly: JavaScript>]
do ()

open WebSharper
open WebSharper.Html.Server
let app =
    Warp.CreateSPA(fun _ ->
        [
            Div [
                Testing.Runner.Run [
                    typeof<WebSharper.Collections.Tests.Dictionary.Foo>.Assembly
                    typeof<WebSharper.Tests.AddMacro>.Assembly
                    typeof<WebSharper.Web.Tests.HelloWorld>.Assembly
                    typeof<WebSharper.Html5.Tests.Samples>.Assembly
                ]
            ]
        ])
    |> Warp.Run
printfn "Started."
do System.Diagnostics.Process.Start("http://localhost:9000") |> ignore
System.Console.ReadLine() |> ignore
app.Stop()
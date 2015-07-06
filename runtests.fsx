printf "Loading references..."; stdout.Flush()
#r "build/Release/WebSharper.Core.JavaScript.dll"
#r "build/Release/WebSharper.Core.dll"
#r "build/Release/WebSharper.JavaScript.dll"
#r "build/Release/WebSharper.Collections.dll"
#r "build/Release/WebSharper.Control.dll"
#r "build/Release/WebSharper.JQuery.dll"
#r "build/Release/WebSharper.Main.dll"
#r "build/Release/WebSharper.Html.Client.dll"
#r "build/Release/WebSharper.Html.Server.dll"
#r "build/Release/WebSharper.Web.dll"
#r "build/Release/WebSharper.Sitelets.dll"
#r "build/Release/WebSharper.Testing.dll"
#r "build/Release/WebSharper.InterfaceGenerator.Tests.dll"
#r "build/Release/Mono.Cecil.dll"
#r "build/Release/IntelliFactory.Core.dll"
#r "build/Release/WebSharper.Compiler.dll"
printfn "Done."

printf "Loading WebSharper.Tests..."; stdout.Flush()
#load "tests/WebSharper.Tests/Array.fs"
#load "tests/WebSharper.Tests/Array2D.fs"
#load "tests/WebSharper.Tests/Async.fs"
#load "tests/WebSharper.Tests/Basis.fs"
#load "tests/WebSharper.Tests/Char.fs"
#load "tests/WebSharper.Tests/DateTime.fs"
#load "tests/WebSharper.Tests/Double.fs"
#load "tests/WebSharper.Tests/Exception.fs"
#load "tests/WebSharper.Tests/Int32.fs"
#load "tests/WebSharper.Tests/KeyValuePair.fs"
#load "tests/WebSharper.Tests/Lazy.fs"
#load "tests/WebSharper.Tests/List.fs"
#load "tests/WebSharper.Tests/Macro.fs"
#load "tests/WebSharper.Tests/Math.fs"
#load "tests/WebSharper.Tests/Object.fs"
#load "tests/WebSharper.Tests/Operators.fs"
#load "tests/WebSharper.Tests/Option.fs"
#load "tests/WebSharper.Tests/Proxy.fs"
#load "tests/WebSharper.Tests/Queue.fs"
#load "tests/WebSharper.Tests/Random.fs"
#load "tests/WebSharper.Tests/Ref.fs"
#load "tests/WebSharper.Tests/Regression.fs"
#load "tests/WebSharper.Tests/Seq.fs"
#load "tests/WebSharper.Tests/Stack.fs"
#load "tests/WebSharper.Tests/String.fs"
#load "tests/WebSharper.Tests/TimeSpan.fs"
#load "tests/WebSharper.Tests/Printf.fs"
#load "tests/WebSharper.Tests/Tupled.fs"
#load "tests/WebSharper.Tests/WIG.fs"
printfn "Done."

printf "Loading WebSharper.Collections.Tests..."; stdout.Flush()
#load "tests/WebSharper.Collections.Tests/Dictionary.fs"
#load "tests/WebSharper.Collections.Tests/Set.fs"
#load "tests/WebSharper.Collections.Tests/Map.fs"
#load "tests/WebSharper.Collections.Tests/ResizeArray.fs"
#load "tests/WebSharper.Collections.Tests/LinkedList.fs"
#load "tests/WebSharper.Collections.Tests/HashSet.fs"
printfn "Done."

printf "Loading WebSharper.Html5.Tests..."; stdout.Flush()
#load "tests/WebSharper.Html5.Tests/Tests.fs"
printfn "Done."

printf "Loading WebSharper.Web.Tests..."; stdout.Flush()
#load "tests/WebSharper.Web.Tests/Remoting.fs"
#load "tests/WebSharper.Web.Tests/HelloWorld.fs"
printfn "Done."

open System.IO

printf "Downloading and running Warp..."; stdout.Flush()
let install pkg =
    if not (Directory.Exists(Path.Combine(__SOURCE_DIRECTORY__, "packages", pkg))) then
        try
            System.Diagnostics.Process
                .Start(
                    Path.Combine(__SOURCE_DIRECTORY__, "tools", "NuGet", "NuGet.exe"),
                    sprintf "install %s -outputdirectory %s -excludeversion" pkg
                        (Path.Combine(__SOURCE_DIRECTORY__, "packages")))
                .WaitForExit()
        with exn ->
            eprintfn "Warning: Failed to install nuget package: %s" pkg
let download (url: string) out =
    try
        let req = System.Net.WebRequest.CreateHttp(url)
        let resp = req.GetResponse()
        use r = new StreamReader(resp.GetResponseStream())
        let text = r.ReadToEnd()
        File.WriteAllText(out, text)
    with exn ->
        eprintfn "Warning: Failed to download %s" (Path.GetFileName out)
do  download "https://raw.githubusercontent.com/intellifactory/websharper.warp/master/WebSharper.Warp/Warp.fs"
        (Path.Combine(__SOURCE_DIRECTORY__, "build", "Warp.fs"))
    install "Owin"
    install "Microsoft.Owin"
    install "Microsoft.Owin.Host.HttpListener"
    install "Microsoft.Owin.Hosting"
    install "Microsoft.Owin.FileSystems"
    install "Microsoft.Owin.StaticFiles"
    install "WebSharper.Owin"
#r "packages/Owin/lib/net40/Owin.dll"
#r "packages/Microsoft.Owin/lib/net45/Microsoft.Owin.dll"
#r "packages/Microsoft.Owin.Host.HttpListener/lib/net45/Microsoft.Owin.Host.HttpListener.dll"
#r "packages/Microsoft.Owin.Hosting/lib/net45/Microsoft.Owin.Hosting.dll"
#r "packages/Microsoft.Owin.FileSystems/lib/net45/Microsoft.Owin.FileSystems.dll"
#r "packages/Microsoft.Owin.StaticFiles/lib/net45/Microsoft.Owin.StaticFiles.dll"
#r "packages/WebSharper.Owin/lib/net45/WebSharper.Owin.dll"
#load "build/Warp.fs"

// Run
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

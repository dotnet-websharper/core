#load "tools/includes.fsx"

open System
open System.Diagnostics
open System.IO
open IntelliFactory.Core
open IntelliFactory.Build

module Config =
    let PackageId = "WebSharper"
    let PackageVerion = "2.5-alpha"
    let Company = "IntelliFactory"
    let Description = "F#-to-JavaScript compiler and web application framework"
    let LicenseUrl = "http://websharper.com/licensing"
    let Tags = ["Web"; "JavaScript"; "F#"]
    let Website = "http://bitbucket.org/IntelliFactory/websharper"

[<AutoOpen>]
module Extensions =

    type IParametric<'T> with

        member x.DefaultRefs(r: ReferenceBuilder) =
            [
                r.NuGet("Mono.Cecil").Reference()
                r.NuGet("IntelliFactory.Core").Reference()
                r.NuGet("IntelliFactory.Xml").Reference()
                r.Assembly("System.Configuration")
                r.Assembly("System.Xml")
                r.Assembly("System.Xml.Linq")
                r.Assembly("System.Web")
            ]

        member x.WithDefaultRefs() =
            x.References x.DefaultRefs

        member x.ClearRefs() =
            let r = BuildTool().Reference
            FSharpConfig.References.Custom (x.DefaultRefs r) x

let bt =
    BuildTool()
        .PackageId(Config.PackageId, Config.PackageVerion)
        .WithCommandLineArgs()
        .Configure(fun bt ->
            let outDir = BuildConfig.OutputDir.Find bt
            bt
            |> BuildConfig.RootDir.Custom __SOURCE_DIRECTORY__
            |> WebSharperConfig.WebSharperHome.Custom (Some outDir)
            |> Logs.Config.Custom(Logs.Default.Verbose().ToConsole()))
        .WithDefaultRefs()

let beforeBuild () =
    let rs =
        bt.ResolveReferences bt.Framework.Net45 [
            bt.Reference.NuGet("YUICompressor.NET").Version("2.2.1.0").Reference()
        ]
    bt.FSharp.ExecuteScript("compress.fsx", rs)

beforeBuild ()

let ifJavaScript =
    bt.FSharp.Library("IntelliFactory.JavaScript")
        .SourcesFromProject()
        .Embed(["Runtime.js"; "Runtime.min.js"])

let wsCore =
    bt.FSharp.Library("IntelliFactory.WebSharper.Core")
        .References(fun r ->
            [
                r.Project(ifJavaScript)
            ])
        .SourcesFromProject()

let wsCompiler =
    bt.FSharp.Library("IntelliFactory.WebSharper.Compiler")
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(wsCore)
            ])
        .SourcesFromProject()

let ws =
    bt.FSharp.ConsoleExecutable("WebSharper")
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(wsCore)
                r.Project(wsCompiler)
            ])
        .SourcesFromProject()

let ifWS =
    bt.WebSharper.Library("IntelliFactory.WebSharper")
        .ClearRefs()
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(wsCore)
            ])
        .SourcesFromProject()
        .Embed(["Json.js"; "Json.min.js"])

let wsInterfaceGenerator =
    bt.FSharp.Library("IntelliFactory.WebSharper.InterfaceGenerator")
        .References(fun r ->
            [
                r.Project(wsCore)
                r.Project(ifWS)
            ])
        .SourcesFromProject()

let wsDom =
    bt.WebSharper.Extension("IntelliFactory.WebSharper.Dom").ClearRefs()
        .Modules(["Definition"])
        .References(fun r ->
            [
                r.Project(wsCore)
                r.Project(wsInterfaceGenerator)
            ])

let wsJQuery =
    bt.WebSharper.Extension("IntelliFactory.WebSharper.JQuery").ClearRefs()
        .Modules(["Definition"])
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsDom)
                r.Project(wsCore)
                r.Project(wsInterfaceGenerator)
            ])

let wsCollections =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Collections").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
            ])

let wsControl =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Control").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
            ])

let wsEcma =
    bt.WebSharper.Extension("IntelliFactory.WebSharper.Ecma").ClearRefs()
        .Modules(["Definition"])
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsInterfaceGenerator)
            ])

let wsTesting =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Testing").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
            ])

let wsTests =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Tests").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsCollections)
                r.Project(wsControl)
                r.Project(wsTesting)
            ])

let ifHtml =
    bt.WebSharper.Library("IntelliFactory.Html").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
           ])

let wsHtml =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Html").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsJQuery)
                r.Project(wsDom)
           ])

let wsWeb =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Web").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsHtml)
                r.Project(ifHtml)
            ])

let wsWebTests =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Web.Tests").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsWeb)
                r.Project(wsTesting)
                r.Project(wsHtml)
                r.Project(ifHtml)
                r.Project(wsDom)
            ])

let ifReactive =
    bt.WebSharper.Library("IntelliFactory.Reactive").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsHtml)
                r.Project(ifHtml)
                r.Project(wsCollections)
                r.Project(wsControl)
            ])

let ifFormlet =
    bt.WebSharper.Library("IntelliFactory.Formlet").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsHtml)
                r.Project(ifHtml)
                r.Project(wsCollections)
                r.Project(wsControl)
                r.Project(ifReactive)
            ])

let wsFormlet =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Formlet").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsHtml)
                r.Project(ifHtml)
                r.Project(wsCollections)
                r.Project(wsControl)
                r.Project(wsDom)
                r.Project(wsHtml)
                r.Project(wsJQuery)
                r.Project(ifFormlet)
                r.Project(ifReactive)
            ])

let wsFormletTests =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Formlet.Tests").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsHtml)
                r.Project(ifHtml)
                r.Project(wsCollections)
                r.Project(wsControl)
                r.Project(wsDom)
                r.Project(wsHtml)
                r.Project(wsJQuery)
                r.Project(ifReactive)
                r.Project(ifFormlet)
                r.Project(wsFormlet)
                r.Assembly("System.Web")
                r.Project(wsWeb)
            ])

let wsHtml5 =
    bt.WebSharper.Extension("IntelliFactory.WebSharper.Html5").ClearRefs()
        .Modules(["Definition"])
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsInterfaceGenerator)
                r.Project(wsEcma)
                r.Project(wsDom)
            ])

let wsHtml5Tests =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Html5.Tests").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsHtml)
                r.Project(wsHtml5)
                r.Project(ifHtml)
                r.Project(wsEcma)
                r.Project(wsDom)
                r.Assembly("System.Web")
                r.Project(wsWeb)
                r.Project(wsJQuery)
            ])

let wsSitelets =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Sitelets").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsHtml)
                r.Project(ifHtml)
                r.Project(wsWeb)
            ])

let wsSiteletsTests =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Sitelets.Tests").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsHtml)
                r.Project(ifHtml)
                r.Project(wsWeb)
                r.Project(wsSitelets)
                r.Project(wsWeb)
                r.Project(wsDom)
                r.Project(wsCollections)
                r.Project(wsControl)
            ])

let websiteConfigFile =
    let content =
        use w = new StringWriter()
        w.WriteLine("module Website.Config")
        let var (name: string) (value: string) : unit =
            fprintfn w @"let %s = @""%s""" name (value.Replace(@"""", @""""""))
        let version = PackageVersion.Current.Find bt
        let full = PackageVersion.Full.Find bt
        let tag =
            match Environment.GetEnvironmentVariable("REVISION") with
            | null | "" -> "unknown"
            | tag -> tag
        var "Tag" tag
        var "PackageId" (PackageId.Current.Find bt)
        var "Version" (string version)
        var "AssemblyVersion" (string (Version (version.Major, version.Minor)))
        var "AssemblyFileVersion" (string full)
        var "Description" Config.Description
        var "Website" Config.Website
        w.ToString()
        |> FileSystem.TextContent
    let p = Path.Combine(__SOURCE_DIRECTORY__, "build", "Config.fs")
    content.WriteFile p
    p

let website =
    bt.WebSharper.Library("Website").ClearRefs()
        .Sources([ websiteConfigFile ])
        .Modules([ "Dependencies"; "Actions"; "Client"; "Controls"; "Skin"; "Content"; "Main" ])
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(ifWS)
                r.Project(ifHtml)
                r.Project(wsHtml)
                r.Project(wsHtml5)
                r.Project(wsCore)
                r.Project(wsSitelets)
                r.Project(wsWeb)
                r.Project(wsDom)
                r.Project(wsCollections)
                r.Project(wsControl)
                r.Project(wsJQuery)
                r.Project(wsTesting)
            ])

bt.Solution [
    ifJavaScript
    wsCore
    wsCompiler
    ws
    ifWS
    wsInterfaceGenerator
    wsDom
    wsJQuery
    wsCollections
    wsControl
    wsEcma
    wsTesting
    wsTests
    ifHtml
    wsHtml
    wsWeb
    wsWebTests
    ifReactive
    ifFormlet
    wsFormlet
    wsFormletTests
    wsHtml5
    wsHtml5Tests
    wsSitelets
    wsSiteletsTests
    website

    bt.NuGet.CreatePackage()
        .Configure(fun x ->
            {
                x with
                    Description = Config.Description
                    ProjectUrl = Some Config.Website
                    LicenseUrl = Some Config.LicenseUrl
            })
        .Add(ifJavaScript)
        .Add(wsCore)
        .Add(wsCompiler)
        .Add(ws)
        .Add(ifWS)
        .Add(wsInterfaceGenerator)
        .Add(wsDom)
        .Add(wsJQuery)
        .Add(wsCollections)
        .Add(wsControl)
        .Add(wsEcma)
        .Add(wsTesting)
        .Add(ifHtml)
        .Add(wsHtml)
        .Add(wsWeb)
        .Add(ifReactive)
        .Add(ifFormlet)
        .Add(wsFormlet)
        .Add(wsHtml5)
        .Add(wsSitelets)

]
|> bt.Dispatch

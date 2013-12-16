#load "tools/includes.fsx"
#load "src/VisualStudio.fsx"
open System
open System.Diagnostics
open System.IO
open IntelliFactory.Core
open IntelliFactory.Build

module Config =
    let PackageId = "WebSharper"
    let NumericVersion = Version("2.5.0.0")
    let VersionSuffix = None
    let PackageVerion = "2.5"
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
                r.NuGet("Mono.Cecil.Security").Version("0.9.28-security").Reference()
                r.NuGet("IntelliFactory.Core").Reference()
                r.NuGet("IntelliFactory.Xml").Version("0.5.0").Reference()
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

let root = __SOURCE_DIRECTORY__
let compiler = Path.Combine(root, "build", "compiler")

let bt =
    BuildTool()
        .PackageId(Config.PackageId, Config.PackageVerion)
        .Configure(fun bt ->
            let outDir = BuildConfig.OutputDir.Find bt
            bt
            |> BuildConfig.RootDir.Custom root
            |> FSharpConfig.OtherFlags.Custom ["--optimize+"]
            |> WebSharperConfig.WebSharperHome.Custom (Some compiler)
            |> Logs.Config.Custom(Logs.Default.Verbose().ToConsole()))
        .WithDefaultRefs()


let cbt =
    bt
    |> BuildConfig.OutputDir.Custom compiler

let beforeBuild () =
    let rs =
        bt.ResolveReferences bt.Framework.Net45 [
            bt.Reference.NuGet("YUICompressor.NET").Version("2.2.1.0").Reference()
        ]
    bt.FSharp.ExecuteScript("compress.fsx", rs)

beforeBuild ()

let ifJavaScript =
    cbt.FSharp.Library("IntelliFactory.JavaScript")
        .SourcesFromProject()
        .Embed(["Runtime.js"; "Runtime.min.js"])

let wsCore =
    cbt.FSharp.Library("IntelliFactory.WebSharper.Core")
        .References(fun r ->
            [
                r.Project(ifJavaScript)
            ])
        .SourcesFromProject()

let wsCompiler =
    cbt.FSharp.Library("IntelliFactory.WebSharper.Compiler")
        .References(fun r ->
            [
                r.Project(ifJavaScript)
                r.Project(wsCore)
            ])
        .SourcesFromProject()

let wsInterfaceGenerator =
    cbt.FSharp.Library("IntelliFactory.WebSharper.InterfaceGenerator")
        .References(fun r ->
            [
                r.Project(wsCore)
            ])
        .SourcesFromProject()

let ws =
    cbt.FSharp.ConsoleExecutable("WebSharper")
        .References(fun r ->
            [
                r.Project ifJavaScript
                r.Project wsCore
                r.Project wsCompiler
                r.Project wsInterfaceGenerator
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

let wsDom =
    bt.WebSharper.Extension("IntelliFactory.WebSharper.Dom").ClearRefs()
        .Modules(["Definition"])
        .References(fun r ->
            [
                r.Project(ifWS)
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

let wsEcma =
    bt.WebSharper.Extension("IntelliFactory.WebSharper.Ecma").ClearRefs()
        .Modules(["Definition"])
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsInterfaceGenerator)
            ])

let wsControl =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Control").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)  
                r.Project(wsEcma)
            ])

let wsTesting =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Testing").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
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

let wsTests =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Tests").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsCollections)
                r.Project(wsEcma)
                r.Project(wsControl)
                r.Project(wsTesting)
                r.Project(ifReactive)
            ])

let wsCollectionsTests =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Collections.Tests").ClearRefs()
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(ifWS)
                r.Project(wsCore)
                r.Project(wsCollections)
                r.Project(wsEcma)
                r.Project(wsControl)
                r.Project(wsTesting)
                r.Project(ifReactive)
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
        .Embed(["styles/Formlet.css"; "images/ActionAdd.png"; "images/ActionCheck.png"; "images/ActionDelete.png"; "images/ErrorIcon.png"; "images/InfoIcon.png" ])

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
                r.Project(wsTests)
                r.Project(wsCollectionsTests)
            ])

let exports : list<INuGetExportingProject> =
    [
        ifWS
        wsDom
        wsJQuery
        wsCollections
        wsEcma
        wsControl
        wsTesting
        ifHtml
        wsHtml
        wsWeb
        ifReactive
        ifFormlet
        wsFormlet
        wsHtml5
        wsSitelets
    ]

module VSI = IntelliFactory.WebSharper.VisualStudioIntegration

let configureVSI (nuPkg: NuGetPackageBuilder) : VSI.Config =
    let root = __SOURCE_DIRECTORY__
    let nupkgPath = nuPkg.GetComputedFileName()
    let vsixPath = Path.ChangeExtension(nupkgPath, ".vsix")
    {
        NuPkgPath = nupkgPath
        RootPath = root
        VsixPath = vsixPath
    }

let nuPkg =
    let nuPkg =
        bt.NuGet.CreatePackage()
            .Configure(fun x ->
                {
                    x with
                        Description = Config.Description
                        ProjectUrl = Some Config.Website
                        LicenseUrl = Some Config.LicenseUrl
                })
            .AddNuGetExportingProject(ws)
    nuPkg.AddNuGetExportingProject {
        new INuGetExportingProject with
            member p.NuGetFiles =
                seq {
                    let cfg = configureVSI nuPkg
                    yield! VSI.BuildContents cfg
                    yield {
                        new INuGetFile with
                            member x.Read() = File.OpenRead(typedefof<list<_>>.Assembly.Location) :> _
                            member x.TargetPath = "/tools/net45/FSharp.Core.dll"
                    }
                    for p in exports do
                        for f in p.NuGetFiles do
                            yield {
                                new INuGetFile with
                                    member x.Read() = f.Read()
                                    member x.TargetPath = "/tools/net45/" + Path.GetFileName f.TargetPath
                            }
                }
    }

bt.Solution [
    ifJavaScript
    wsCore
    wsCompiler
    wsInterfaceGenerator
    ws
    ifWS
    wsDom
    wsJQuery
    wsCollections
    wsEcma
    wsControl
    wsTesting
    ifHtml
    wsHtml
    wsWeb
    wsWebTests
    ifReactive
    ifFormlet
    wsFormlet
    wsHtml5
    wsSitelets
    wsTests
    wsCollectionsTests
    wsFormletTests
    wsHtml5Tests
    wsSiteletsTests

    nuPkg
]
|> bt.Dispatch

let buildWebsiteFiles () =
    let rootDir = __SOURCE_DIRECTORY__
    let ver = nuPkg.GetComputedVersion()
    let content =
        use w = new StringWriter()
        w.WriteLine("module Website.Config")
        let var (name: string) (value: string) : unit =
            fprintfn w @"let %s = @""%s""" name (value.Replace(@"""", @""""""))
        IntelliFactory.Build.Mercurial.InferTag rootDir
        |> Option.iter (var "Tag")
        var "PackageId" Config.PackageId
        var "Version" ver
        var "Description" Config.Description
        var "Website" Config.Website
        w.ToString()
    File.WriteAllText(Path.Combine(rootDir, "build", "Config.fs"), content)
    let ( +/ ) a b = Path.Combine(a, b)
    let zipPackageFile =
        rootDir +/ "Web" +/ "downloads" +/ sprintf "%s-%O.zip" Config.PackageId ver
    use memory = new System.IO.MemoryStream()
    do
        use archive = new System.IO.Compression.ZipArchive(memory, System.IO.Compression.ZipArchiveMode.Create)
        let addFile path =
            let entry = archive.CreateEntry(Path.GetFileName(path))
            use s = entry.Open()
            use i = File.OpenRead(path)
            i.CopyTo(s)
        addFile (nuPkg.GetComputedFileName())
        addFile (rootDir +/ "LICENSE.md")
        addFile (configureVSI nuPkg).VsixPath
    let f = FileInfo(zipPackageFile)
    let d = f.Directory
    if not d.Exists then
        d.Create()
    File.WriteAllBytes(zipPackageFile, memory.ToArray())

let buildVsix () =
    configureVSI nuPkg
    |> VSI.BuildVsixFile

buildVsix ()
buildWebsiteFiles ()

bt.Solution [
    website

    bt.WebSharper.HostWebsite("Web")
        .ClearRefs()
        .References(fun r ->
            let projs : list<IReferenceProject> =
                [
                    ifJavaScript
                    wsCore
                    ifWS
                    wsDom
                    wsJQuery
                    wsCollections
                    wsEcma
                    wsControl
                    wsTesting
                    wsTests
                    wsCollectionsTests
                    ifHtml
                    wsHtml
                    wsWeb
                    // wsWebTests
                    ifReactive
                    ifFormlet
                    wsFormlet
                    // wsFormletTests
                    wsHtml5
                    wsHtml5Tests
                    wsSitelets
                    // wsSiteletsTests
                    website
                ]
            [
                for p in projs do
                    yield r.Project p
            ])
]
|> bt.Dispatch


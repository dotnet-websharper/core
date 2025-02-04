module Compiler.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO

[<SetUp>]
let Setup () =
    ()

#if DEBUG
let configuration = "Debug"
let isDebug = true
#else
let configuration = "Release"
let isDebug = false
#endif

let programFiles = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles)
let get6xfolder root =
    Directory.GetDirectories(root)
    |> Array.sort
    |> Array.find (fun d ->
        let n = Path.GetFileName d
        n.StartsWith("6.") && not (n.Contains("-"))
    )
    
let ancAppRefRoot =
    Path.Combine(
        get6xfolder (Path.Combine(programFiles, @"dotnet\packs\Microsoft.AspNetCore.App.Ref")),
        @"ref\net8.0"
    )
        
let ncAppRefRoot =
    Path.Combine(
        get6xfolder (Path.Combine(programFiles, @"dotnet\packs\Microsoft.NETCore.App.Ref")),
        @"ref\net8.0"
    )

[<Test>]
let ``Compilation test`` () =
    let mkProjectCommandLineArgs (dllName, fileNames) = 
        [| 

            let references =
                Seq.append
                    (Directory.GetFiles(ancAppRefRoot, "*.dll")) 
                    (Directory.GetFiles(ncAppRefRoot, "*.dll"))

            for r in references do
                  yield "-r:" + r
            for x in fileNames do 
                yield x

            let nugets =
                [ 
                    @"fsharp.core\6.0.0\lib\netstandard2.0\FSharp.Core.dll"
                    @"htmlagilitypack\1.11.0\lib\netstandard2.0\HtmlAgilityPack.dll"
                    @"system.xml.xpath.xmldocument\4.3.0\ref\netstandard1.3\System.Xml.XPath.XmlDocument.dll"
                    @"mono.cecil\0.11.4\lib\netstandard2.0\Mono.Cecil.dll"
                    @"mono.cecil\0.11.4\lib\netstandard2.0\Mono.Cecil.Mdb.dll"
                    @"mono.cecil\0.11.4\lib\netstandard2.0\Mono.Cecil.Pdb.dll"
                    @"mono.cecil\0.11.4\lib\netstandard2.0\Mono.Cecil.Rocks.dll"
                    @"system.codedom\4.4.0\ref\netstandard2.0\System.CodeDom.dll"
                    @"system.configuration.configurationmanager\4.4.0\ref\netstandard2.0\System.Configuration.ConfigurationManager.dll"
                    @"system.security.cryptography.protecteddata\4.4.0\ref\netstandard2.0\System.Security.Cryptography.ProtectedData.dll"
                ]

            for n in nugets do
                yield @"-r:" + System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile) + @"\.nuget\packages\" + n
            
            yield "-g"
            yield "--debug:portable"
            yield "--noframework"
            yield "--define:TRACE"
            if isDebug then
                yield "--define:DEBUG"
            yield "--define:NET"
            yield "--define:NET5_0"
            yield "--define:NETCOREAPP"
            yield "--define:NET5_0_OR_GREATER"
            yield "--define:NETCOREAPP1_0_OR_GREATER"
            yield "--define:NETCOREAPP1_1_OR_GREATER"
            yield "--define:NETCOREAPP2_0_OR_GREATER"
            yield "--define:NETCOREAPP2_1_OR_GREATER"
            yield "--define:NETCOREAPP2_2_OR_GREATER"
            yield "--define:NETCOREAPP3_0_OR_GREATER"
            yield "--define:NETCOREAPP3_1_OR_GREATER"
            yield "--optimize-"
            yield "--tailcalls-"
            yield "--target:exe"
            yield "--warn:3"
            yield "--warnaserror:3239"
            yield "--fullpaths"
            yield "--flaterrors"
            yield "--highentropyva+"
            yield "--targetprofile:netcore"
            yield "--nocopyfsharpcore"
            yield "--deterministic+"
            yield "--simpleresolution"
            yield "--nowarn:3186"
            yield "--jsmap"

            let projRefs =
                [
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.AspNetCore.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Collections.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Control.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Core.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Core.JavaScript.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.InterfaceGenerator.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.JavaScript.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Main.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.MathJS.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.MathJS.Extensions.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Sitelets.dll"
                    $@"..\..\build\{configuration}\Tests\net8.0\WebSharper.Sitelets.Tests.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Testing.dll"
                    $@"..\..\build\{configuration}\netstandard2.0\WebSharper.Web.dll"
                ]
            for projRef in projRefs do
                yield @"-r:" + Path.Combine(__SOURCE_DIRECTORY__, projRef)

            yield "--out:" + dllName
            yield "--project:" + Path.Combine(__SOURCE_DIRECTORY__, @"..\GeneratedProject\Preview.fsproj")
            yield "--wsconfig:" + Path.Combine(__SOURCE_DIRECTORY__, @"..\GeneratedProject\wsconfig.json")
        |]

    let fileNames =
        [
            Path.Combine(__SOURCE_DIRECTORY__, $@"obj\{configuration}\net8.0\.NETCoreApp,Version=v6.0.AssemblyAttributes.fs")
            Path.Combine(__SOURCE_DIRECTORY__, @"..\..\msbuild\AssemblyInfo.fs")
            Path.Combine(__SOURCE_DIRECTORY__, @"..\GeneratedProject\example1.abc.fs")
            Path.Combine(__SOURCE_DIRECTORY__, @"..\GeneratedProject\Site.fs")
            Path.Combine(__SOURCE_DIRECTORY__, @"..\GeneratedProject\Startup.fs")
        ]
    let args = mkProjectCommandLineArgs (@"bin\example1.abc.dll", fileNames)

    System.Environment.CurrentDirectory <- Path.Combine(__SOURCE_DIRECTORY__, @"..\GeneratedProject\")
    
    let stopWatch1 = System.Diagnostics.Stopwatch.StartNew()
    let res1 = WebSharper.FSharp.Program.main args
    stopWatch1.Stop()
    let time1 = stopWatch1.Elapsed 
    res1 |> should equal 0

    let stopWatch2 = System.Diagnostics.Stopwatch.StartNew()
    let res2 = WebSharper.FSharp.Program.main args
    stopWatch2.Stop()
    let time2 = stopWatch2.Elapsed 
    res2 |> should equal 0

    time2 |> should lessThan time1

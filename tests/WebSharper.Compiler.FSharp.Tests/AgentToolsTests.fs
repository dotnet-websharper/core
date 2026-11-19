module Compiler.AgentToolsTests

open System.IO
open System.Text.Json.Nodes
open NUnit.Framework
open FsUnit
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open WebSharper.FSharp.Service

// ---------------------------------------------------------------------------------------------
// MCP protocol

let private noTools () = JsonArray()
let private echoTool (name: string) (args: string) = if name = "fails" then Error "it failed" else Ok (name + " " + args)
let private handle line = Mcp.handle "1.0" "instructions" (fun () -> AgentTools.toolsJsonOf AgentTools.freeTools) echoTool line |> Option.map (fun r -> r.ToJsonString())

let private parse (r: string option) = JsonNode.Parse(Option.get r)

[<Test>]
let ``MCP initialize uses the requested protocol version when supported`` () =
    let r = handle """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}""" |> parse
    r.["id"].GetValue<int>() |> should equal 1
    r.["result"].["protocolVersion"].GetValue<string>() |> should equal "2025-06-18"
    r.["result"].["serverInfo"].["name"].GetValue<string>() |> should equal "websharper-booster"
    r.["result"].["capabilities"].["tools"] |> should not' (be Null)

[<Test>]
let ``MCP initialize offers the newest protocol version for an unknown one`` () =
    let r = handle """{"jsonrpc":"2.0","id":"a","method":"initialize","params":{"protocolVersion":"1999-01-01"}}""" |> parse
    r.["id"].GetValue<string>() |> should equal "a"
    r.["result"].["protocolVersion"].GetValue<string>() |> should equal (List.head Mcp.supportedProtocolVersions)

[<Test>]
let ``MCP notifications get no response`` () =
    handle """{"jsonrpc":"2.0","method":"notifications/initialized"}""" |> should equal None

[<Test>]
let ``MCP tools/list describes every tool with an object schema`` () =
    let r = handle """{"jsonrpc":"2.0","id":2,"method":"tools/list"}""" |> parse
    let tools = r.["result"].["tools"].AsArray()
    tools.Count |> should equal AgentTools.freeTools.Length
    for t in tools do
        t.["name"].GetValue<string>() |> should not' (be EmptyString)
        t.["description"].GetValue<string>() |> should not' (be EmptyString)
        t.["inputSchema"].["type"].GetValue<string>() |> should equal "object"
    tools |> Seq.map (fun t -> t.["name"].GetValue<string>()) |> should contain "get_signature"

[<Test>]
let ``MCP tools/call returns text content, tool errors as isError`` () =
    let ok = handle """{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"get_signature","arguments":{"name":"List.map"}}}""" |> parse
    ok.["result"].["isError"].GetValue<bool>() |> should equal false
    ok.["result"].["content"].[0].["type"].GetValue<string>() |> should equal "text"
    ok.["result"].["content"].[0].["text"].GetValue<string>() |> should equal """get_signature {"name":"List.map"}"""
    let err = handle """{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"fails"}}""" |> parse
    err.["result"].["isError"].GetValue<bool>() |> should equal true
    err.["result"].["content"].[0].["text"].GetValue<string>() |> should equal "it failed"

[<Test>]
let ``MCP unknown methods and bad JSON are JSON-RPC errors`` () =
    let r = handle """{"jsonrpc":"2.0","id":5,"method":"resources/list"}""" |> parse
    r.["error"].["code"].GetValue<int>() |> should equal -32601
    let p = handle "{not json" |> parse
    p.["error"].["code"].GetValue<int>() |> should equal -32700

[<Test>]
let ``MCP serve answers one line per request`` () =
    let input = new StringReader("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}\n{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}\n")
    let output = new StringWriter()
    Mcp.serve "1.0" "instructions" noTools echoTool input output
    let lines = output.ToString().Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
    lines.Length |> should equal 2
    (JsonNode.Parse lines.[1]).["id"].GetValue<int>() |> should equal 2

// ---------------------------------------------------------------------------------------------
// Tools

[<Test>]
let ``Tools report when no project was compiled`` () =
    AgentTools.projects.Clear()
    let ctx : AgentTools.BoosterContext = { IsOutdated = (fun _ -> None); Build = (fun _ -> 0) }
    match AgentTools.callTool ctx "get_signature" """{"name":"List.map"}""" with
    | Error msg -> msg |> should haveSubstring "has not compiled any project"
    | Ok text -> Assert.Fail text
    match AgentTools.callTool ctx "no_such_tool" "" with
    | Error msg -> msg |> should haveSubstring "Unknown tool"
    | Ok text -> Assert.Fail text

[<Test>]
let ``Names are normalized for lookup`` () =
    AgentTools.normalizeName "System.Collections.Generic.List`1" |> should equal "System.Collections.Generic.List"
    AgentTools.normalizeName "List<'T>" |> should equal "List"
    AgentTools.normalizeName "Dictionary<string, List<int>>" |> should equal "Dictionary"
    AgentTools.normalizeName "Outer+Inner`2" |> should equal "Outer.Inner"

let private sample = """
module Sample

/// Adds one to a number.
let addOne (x: int) = x + 1

type Shape =
    | Circle of radius: float
    | Square of side: float

type Counter() =
    member this.Value = 1
    member this.Add(n: int) = n + this.Value

let useIt () = addOne (Counter().Add 2)
"""

let private checkSample =
    lazy (
        let dir = Directory.CreateTempSubdirectory("ws-agent-tools-test")
        let file = Path.Combine(dir.FullName, "Sample.fsx")
        File.WriteAllText(file, sample)
        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let options, _ =
            checker.GetProjectOptionsFromScript(file, SourceText.ofString sample, assumeDotNetFramework = false, useSdkRefs = true)
            |> Async.RunSynchronously
        let results = checker.ParseAndCheckProject options |> Async.RunSynchronously
        results.HasCriticalErrors |> should equal false
        AgentTools.buildIndex results
    )

let private signatureOf name =
    match AgentTools.resolve checkSample.Value name with
    | [] -> failwithf "%s not found" name
    | r :: _ -> AgentTools.formatResolved r

[<Test>]
let ``Signature of a project function, with its doc`` () =
    let s = signatureOf "Sample.addOne"
    s |> should haveSubstring "addOne"
    s |> should haveSubstring "int -> int"
    s |> should haveSubstring "Adds one to a number."
    s |> should haveSubstring "Sample.fsx"

[<Test>]
let ``Signature of a project union and a member`` () =
    let union = signatureOf "Sample.Shape"
    union |> should haveSubstring "union"
    union |> should haveSubstring "Circle"
    let m = signatureOf "Sample.Counter.Add"
    m |> should haveSubstring "Add"
    m |> should haveSubstring "int"

[<Test>]
let ``Signature of referenced symbols by partial names`` () =
    let map = signatureOf "List.map"
    map |> should haveSubstring "FSharp.Core"
    map |> should haveSubstring "('T -> 'U)"
    let split = signatureOf "System.String.Split"
    split |> should haveSubstring "Split"
    let list = signatureOf "System.Collections.Generic.List`1"
    list |> should haveSubstring "class"

[<Test>]
let ``Search finds project symbols first`` () =
    let results = AgentTools.search checkSample.Value "addOne" true false 10
    results |> should not' (be Empty)
    let (name, kind, _, own) = results.Head
    name |> should equal "Sample.addOne"
    own |> should equal true
    kind |> should equal "function"

// ---------------------------------------------------------------------------------------------
// Lookup and diagnostics

[<Test>]
let ``Loose lookup drops a wrong namespace and suggests close names`` () =
    match AgentTools.resolveLoose checkSample.Value "Wrong.Namespace.Sample.addOne" with
    | r :: _, Some note ->
        note |> should haveSubstring "No exact match"
        AgentTools.formatResolved r |> should haveSubstring "addOne"
    | _ -> Assert.Fail "expected a loose match"
    let suggestions = AgentTools.suggest checkSample.Value "Sample.addOen" AgentTools.search
    suggestions |> should contain "Sample.addOne"

[<Test>]
let ``A namespace lists its types and modules`` () =
    match AgentTools.namespaceListing checkSample.Value "System.Collections.Generic" with
    | Some text ->
        text |> should haveSubstring "namespace System.Collections.Generic"
        text |> should haveSubstring "class List"
        text |> should haveSubstring "class Dictionary"
    | None -> Assert.Fail "expected a listing"
    AgentTools.namespaceListing checkSample.Value "No.Such.Namespace" |> should equal None

[<Test>]
let ``Diagnostics explain common errors`` () =
    let state =
        { AgentTools.createState "/p/App.fsproj" None 1 System.TimeSpan.Zero
            [| true, "/p/Client.fs(23,17,23,39): error FS0001: This expression was expected to have type     'Todo'     but here has type     'unit'"
               true, "/p/Client.fs(13,13,13,37): warning FS3221: This expression returns a value of type 'Attr' but is implicitly discarded." |]
          with ProjectType = "Library" }
    let text = AgentTools.diagnostics state None true 100
    text |> should haveSubstring "error FS0001"
    text |> should haveSubstring "hint (FS0001): if the line is a do! on an RPC"
    text |> should haveSubstring "hint (FS3221): a list with an explicit yield"


// ---------------------------------------------------------------------------------------------
// Extensions

/// An extension like the ones other packages add, found by loadExtensions in this test assembly.
type TestExtension() =
    interface AgentTools.IAgentExtension with
        member _.Name = "test"
        member _.Tools =
            [ { Name = "echo"; Description = "Echoes text."; Parameters = [ "text", "string", "The text.", true ]
                ReadOnly = true; Run = fun _ args -> defaultArg (args.String "text") "" } ]
        member _.Instructions = "Use echo."
        member _.Decorate name _ text = if name = "get_diagnostics" then text + "\n  more in echo" else text
        member _.LocalTools = [ "echo" ]

[<Test>]
let ``Without extensions, only the free tools are offered`` () =
    let state =
        { AgentTools.createState "/p/App.fsproj" None 1 System.TimeSpan.Zero
            [| true, "/p/Remoting.fs(5,1): warning WS9002: Remote method is returning a type which is not fully supported on client side." |]
          with ProjectType = "Library" }
    let text = AgentTools.diagnostics state None true 100
    text |> should haveSubstring "hint (WS9002)"
    text |> should not' (haveSubstring "more in")

[<Test>]
let ``Extensions add tools, instructions and pointers`` () =
    match AgentTools.loadExtensions (typeof<TestExtension>.Assembly.Location) with
    | Ok exts -> exts |> List.map (fun e -> e.Name) |> should equal [ "test" ]
    | Error e -> Assert.Fail e
    let edition = AgentTools.edition ()
    let names = edition.Tools |> List.map (fun t -> t.Name)
    for t in AgentTools.freeTools do names |> should contain t.Name
    names |> should contain "echo"
    edition.Instructions |> should haveSubstring "Use echo."
    edition.LocalTools |> should equal [ "echo" ]
    edition.Decorate "get_diagnostics" (AgentTools.ToolArgs(null)) "x" |> should equal "x\n  more in echo"
    let ctx : AgentTools.BoosterContext = { IsOutdated = (fun _ -> None); Build = (fun _ -> -1) }
    AgentTools.callTool ctx "echo" """{"text":"hi"}""" |> should equal (Ok "hi" : Result<string, string>)
    AgentTools.loadExtensions "/no/such/extension.dll" |> Result.isError |> should equal true

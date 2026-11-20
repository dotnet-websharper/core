# The WebSharper booster as an MCP server for AI agents

The WebSharper booster (`wsfscservice`) is the long-running process that compiles WebSharper F# projects
quickly. After each compilation it keeps what it learned about the project in memory: the F# type check,
WebSharper's translation, and the metadata of the references. AI agents (Claude Code, Copilot, Cursor and
other [Model Context Protocol](https://modelcontextprotocol.io) clients) can ask it about boosted projects,
instead of reading sources, decompiling references or running full builds to find out the same things.

## Connecting an agent

The simplest way is `dotnet ws mcp`, from the [dotnet-ws](https://github.com/dotnet-websharper/dotnet-ws) tool.
Configure it as a stdio MCP server started in the project's folder, for example in `.mcp.json`:

```json
{
  "mcpServers": {
    "websharper": { "command": "dotnet", "args": [ "ws", "mcp" ] }
  }
}
```

It runs the booster of the WebSharper.FSharp version the projects in the folder reference, so it talks to the booster
that compiles them. `--project`, `--version` and `--booster` (a path, for example a local build of the compiler) choose
another one.

Without dotnet-ws, run the booster with `--mcp`: `wsfscservice.dll` is in the `tools/net11.0` folder of the
`WebSharper.FSharp` package (or of the `WebSharper.FSharp.Booster` tool, when the project uses it):

```json
{
  "mcpServers": {
    "websharper": {
      "command": "dotnet",
      "args": [ "/Users/me/.nuget/packages/websharper.fsharp/<version>/tools/net11.0/wsfscservice.dll", "--mcp" ]
    }
  }
}
```

`wsfscservice --mcp` talks to the booster of the same package
version over the booster's named pipe. It does not start the booster: build the project once (`dotnet build` or
`dotnet ws build`) to start it and record the project. Projects built with `WebSharperStandalone=true` do not go
through the booster.

Tool calls are answered from the last compilation of each project, while compilations of other projects can run.
After editing source files, `build_project` compiles again in a fraction of a `dotnet build`.

## Tools

All tools take an optional `project`: a project file path, file name or assembly name. It defaults to the most
recently compiled project.

| Tool | What it answers |
|---|---|
| `list_projects` | The projects compiled since the booster started: type, last compilation time, exit code, error and warning counts. |
| `get_diagnostics` | Errors and warnings of the last compilation, F# and WebSharper, as printed by the build. The first errors come with their source line and the signatures of the functions and values used on it, and common ones with a hint. Filter by `file`, leave out warnings with `include_warnings: false`. |
| `get_signature` | The F# signature of a type, module, member, record field or union case, by full or partial name (`List.map`, `WebSharper.UI.Doc.Element`, `System.String.Split`), from the project or any reference, with its XML doc summary, declaration location, WebSharper attributes and deprecation. Extension members are found on the type they extend (`attr.classDynPred`), template types show their generated members (`Templates.Main.MainForm`). A name in the wrong namespace still finds the member (`WebSharper.UI.Doc.EmbedView`), and misses suggest close names. A namespace (`WebSharper.UI.Client`) lists its types and modules. |
| `search_symbols` | Types, modules and members whose name contains a text, project symbols first. Members are searched in the project and in WebSharper libraries. |
| `get_js_translation` | How WebSharper translates a type or member: its JavaScript address and the compiled form of each member (instance or static method, function, inline, macro, RPC), or that it has no translation and can't be used in `[<JavaScript>]` code. |
| `find_usages` | Where a type, module or member is used in the project, with its definition. |
| `get_file_signature` | The `.fsi` signature of a source file: every type, module and value it declares. |
| `list_rpc_methods` | The project's RPC methods with their remoting paths, and its Website (sitelet) definition. |
| `build_project` | Compiles the project again with the arguments of its last build, like the fast path of `dotnet ws build`, and returns the exit code and diagnostics. It does not run MSBuild: after changing the project file or references, use `dotnet build`. |

The server's instructions tell agents to prefer these tools over web searches, and online to use
[docs.websharper.com](https://docs.websharper.com), never the superseded 4.x documentation
(developers.websharper.com/docs/v4.x). `get_diagnostics` adds hints for common WebSharper errors and warnings.

## Extensions

Other packages can add tools. An extension is a .NET assembly with a public class that implements
`WebSharper.FSharp.Service.AgentTools.IAgentExtension` and has a constructor without arguments:

* `Tools`: the tools it adds. A tool with the name of one of the tools above replaces it.
* `Instructions`: added to the server instructions.
* `Decorate`: can add to the answers of the other tools, for example pointers to its own tools.
* `LocalTools`: its tools that need no booster, such as static content: `wsfscservice --mcp` answers them itself, also
  before the first build.

Both sides load it into the default load context, next to the compiler, with its dependencies from its own folder:

* the booster, from the `WebSharperAgentExtensions` MSBuild property of the project (paths separated by semicolons),
  which the compiler gets as `--agentextension:<path>`; a package sets it in its `.props` or `.targets`;
* `wsfscservice --mcp`, from `--extension <path>` arguments and the `WebSharperAgentExtensions` environment variable
  (paths separated by `:`, `;` on Windows).

`wsfscservice --mcp` sends the paths of its extensions with each tool call, and the booster loads them too: an
extension given only to the front end works without the MSBuild property.

An extension must be built against the same WebSharper version as the booster that loads it.

An extension that also implements `IAgentExtensionWithHost` gets an `IAgentHost` in `Initialize` when it is loaded:
its statistics (`Stats`, see below), the process's log (`Log`), the booster's `WebSharperVersion`, and `IsBooster`
(false in `wsfscservice --mcp`, where its local tools and pointers run). Members are only added to `IAgentHost`, so
extensions built for an earlier version keep working.

## Logging tool calls

With the `WebSharperMcpLog` environment variable set to a file path, `wsfscservice --mcp` appends a JSON line to it
for each tool call: the tool, its arguments, the duration, whether it answered (`ok`) and the start of the answer.
Misses (`"ok": false`) show what agents look for and don't find. For example, in `.mcp.json`:

```json
{ "mcpServers": { "websharper": { "command": "dotnet", "args": [ "ws", "mcp" ], "env": { "WebSharperMcpLog": "/tmp/websharper-mcp.jsonl" } } } }
```

## Statistics

The booster keeps statistics of what it does, and they stay on the machine: in memory, in a file next to its
`websharper-boosters` record (`websharper-boosters/stats/`, written every minute when they change and when the booster
exits), and for `dotnet ws stats`, which asks the running boosters through their pipe (the `stats` message) and
reads the files of stopped ones. They hold counts, durations and sizes under names chosen by the code that records
them, never source, symbol names, paths, project names or the arguments of tool calls.

* Every message the booster handles: `msg.<kind>.wait_ms` in the queue (compilations are serialized),
  `msg.<kind>_ms` processing and `msg.<kind>.total_ms`, how it ended (`msg.<kind>.exit.<code>`) and the size of the
  reply, for compilations (`compile`), replays of `dotnet ws build` (`replay`), `exit` and agent tool calls (`mcp`).
* Compilations: each stage `logger.TimedStage` reports (`compile.stage.<stage>_ms`, without the file and assembly
  names some stages print), and errors and warnings by code (`compile.error.FS0001`, `compile.warning.WS9002`).
* Agent tools, core's and extensions' alike: `tool.<source>.<tool>_ms`, how they ended (`ok`, `not_answered`,
  `failed`) and the size of the answer. `wsfscservice --mcp` adds each MCP request end to end
  (`mcp.request.<method>_ms`, `mcp.call.<source>.<tool>_ms`), the pipe and process overhead of booster calls
  (`.overhead_ms`, from the booster's time in its reply), calls made while no booster runs, the client name and
  protocol version from `initialize`, session lengths and call counts, consecutive tools (`mcp.sequence.*`), the
  same call failing twice in a row (`.repeated_failure`), and pointers that extensions add to answers
  (`mcp.pointer.shown.<tool>`) and whether the agent called that tool within the next three calls
  (`mcp.pointer.followed.<tool>`). It sends them to the booster after each request, and keeps them for later while
  no booster runs.
* The booster: uptime, memory, projects kept for the tools and evicted, extensions loaded or failed.
* Extensions, through `IAgentHost.Stats` (`Count`, `Time`, `Value`, `Event`): under `ext.<extension name>.`. Names
  must be lower case and fixed by the extension's code, event fields numbers or booleans; anything else is dropped
  and counted as `stats.dropped`.

Durations keep their count, total and maximum since the booster started, and the median, p90 and p99 of the last
1000 samples.

## Memory

The booster keeps the last compilation of the 10 most recently compiled projects for these tools. Set the
`WebSharperAgentProjects` environment variable of the booster to keep more or fewer, or to `0` to keep none (the tools
then have nothing to answer from). The booster inherits the environment of the build that starts it: stop it with
`dotnet ws stop` for a change to apply.

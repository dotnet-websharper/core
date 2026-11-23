// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

/// The agent tools of the C# booster: answers from what wscsc reports after each compilation (its output and
/// paths) and from the WebSharper metadata of the project and its references, read from their files. It hosts no
/// C# compiler, so it answers what WebSharper knows: its diagnostics, translations and RPC methods.
module WebSharper.CSharp.Service.AgentTools

open System
open System.IO
open System.Text
open System.Text.Json.Nodes
open System.Collections.Generic
open System.Collections.Concurrent
open WebSharper.Compiler
open WebSharper.Compiler.AgentTranslation

module M = WebSharper.Core.Metadata
type TypeDefinition = WebSharper.Core.AST.TypeDefinition

let private tryOr (f: unit -> 'a) (fallback: 'a) = try f () with _ -> fallback

// ---------------------------------------------------------------------------------------------
// Compilations reported by wscsc

/// What wscsc reports after a compilation (the "record" message).
[<CLIMutable>]
type CompileReport =
    {
        Project : string
        /// The intermediate assembly WebSharper compiled (obj/), its sidecar holds the project's metadata.
        Assembly : string
        References : string[]
        ProjectType : string
        ExitCode : int
        DurationMs : float
        /// The WebSharper outputs were up to date, nothing was compiled (WebSharperCompile runs when csc did not).
        Skipped : bool
        SourceFiles : int
        /// The printed lines, IsError for the error channel (errors and warnings).
        Log : LogLine[]
    }

and [<CLIMutable>] LogLine = { IsError : bool; Text : string }

/// WebSharper metadata of an assembly, read from its sidecar (or resources) without expressions: the tools only
/// need the compiled forms. Cached by path while the assembly and its sidecar are unchanged.
let private metadataCache = ConcurrentDictionary<string, (DateTime * DateTime) * Result<M.Info, string> option>(StringComparer.OrdinalIgnoreCase)

let private readOptions : WebSharper.Core.MetadataIO.ReadOptions = { InlineExpressions = false; OtherExpressions = false; Graph = false }

let private stamp (path: string) = if File.Exists path then File.GetLastWriteTimeUtc path else DateTime.MinValue

let readMetadata (path: string) : Result<M.Info, string> option =
    let stamps = stamp path, stamp (Sidecar.PathFor path)
    match metadataCache.TryGetValue path with
    | true, (s, r) when s = stamps -> r
    | _ ->
        let result =
            if not (File.Exists path) then None
            else
                try
                    let aR = AssemblyResolver.Create().SearchPaths([ path ])
                    let loader = Loader.Create aR ignore
                    let a = loader.LoadFile(path, false)
                    FrontEnd.TryReadFromAssemblyWith readOptions a
                with e -> Some (Error e.Message)
        // errors are not cached: a sidecar can appear later
        match result with
        | Some (Error _) -> ()
        | _ -> metadataCache.[path] <- (stamps, result)
        result

type ProjectState =
    {
        Report : CompileReport
        Finished : DateTime
        /// The compilation before, when this one was skipped (its diagnostics still apply).
        DiagnosticsFrom : DateTime
        CurrentMetadata : Lazy<Result<M.Info, string> option>
        Translations : Lazy<TranslationIndex option>
    }
    member this.ProjectFile = this.Report.Project
    member this.AssemblyName = Path.GetFileNameWithoutExtension this.Report.Assembly

let createState (report: CompileReport) (previous: ProjectState option) =
    let current = lazy (readMetadata report.Assembly)
    let report, diagnosticsFrom =
        match previous with
        | Some p when report.Skipped && report.ExitCode = 0 -> { report with Log = p.Report.Log }, p.DiagnosticsFrom
        | _ -> report, DateTime.Now
    {
        Report = report
        Finished = DateTime.Now
        DiagnosticsFrom = diagnosticsFrom
        CurrentMetadata = current
        Translations =
            lazy (
                let refs =
                    report.References |> Array.choose (fun r ->
                        match readMetadata r with
                        | Some (Ok m) -> Some m
                        | _ -> None)
                let sources =
                    [
                        match current.Value with
                        | Some (Ok m) -> "this project", m
                        | _ -> ()
                        if refs.Length > 0 then
                            "references", tryOr (fun () -> M.Info.UnionWithoutDependencies refs) M.Info.Empty
                    ]
                if sources.IsEmpty then None else Some (buildTranslationIndex sources)
            )
    }

/// How many projects keep their last compilation for the tools.
let maxProjects =
    match Environment.GetEnvironmentVariable "WebSharperAgentProjects" with
    | null -> 10
    | v -> match Int32.TryParse v with | true, n when n >= 0 -> n | _ -> 10

let projects = ConcurrentDictionary<string, ProjectState>(StringComparer.OrdinalIgnoreCase)

let record (report: CompileReport) =
    if maxProjects > 0 && not (String.IsNullOrEmpty report.Project) then
        let previous = match projects.TryGetValue report.Project with | true, p -> Some p | _ -> None
        projects.[report.Project] <- createState report previous
        while projects.Count > maxProjects do
            let oldest = projects.Values |> Seq.minBy (fun p -> p.Finished)
            if projects.TryRemove(oldest.ProjectFile) |> fst then Stats.count "agent.projects_evicted"

let projectCount () = projects.Count

// ---------------------------------------------------------------------------------------------
// Tool plumbing

exception ToolError of string

let fail msg = raise (ToolError msg)

let private noProjects =
    "The C# booster has not recorded any compilation since it started. Build a WebSharper C# project (dotnet build) first; "
    + "projects built with WebSharperStandalone=true don't report to the booster, and the booster only hears of a build "
    + "when the C# compiler succeeded (csc errors stop the build before WebSharper runs)."

let findProject (name: string option) =
    let all = projects.Values |> Seq.sortByDescending (fun p -> p.Finished) |> List.ofSeq
    if all.IsEmpty then fail noProjects
    match name with
    | None | Some "" -> all.Head
    | Some n ->
        let n = n.Trim()
        let byPath = all |> List.tryFind (fun p -> String.Equals(p.ProjectFile, tryOr (fun () -> Path.GetFullPath n) n, StringComparison.OrdinalIgnoreCase))
        let byFile = all |> List.tryFind (fun p -> String.Equals(Path.GetFileName p.ProjectFile, n, StringComparison.OrdinalIgnoreCase) || String.Equals(Path.GetFileNameWithoutExtension p.ProjectFile, n, StringComparison.OrdinalIgnoreCase))
        let byAssembly = all |> List.tryFind (fun p -> String.Equals(p.AssemblyName, n, StringComparison.OrdinalIgnoreCase))
        match byPath |> Option.orElse byFile |> Option.orElse byAssembly with
        | Some p -> p
        | None -> fail (sprintf "No compiled project matches \"%s\". Compiled projects: %s" n (all |> List.map (fun p -> p.ProjectFile) |> String.concat ", "))

type ToolArgs(args: JsonNode) =
    member this.String(name: string) =
        match args with
        | null -> None
        | a ->
            match a.[name] with
            | null -> None
            | v ->
                let s = try v.GetValue<string>() with _ -> v.ToJsonString()
                if String.IsNullOrWhiteSpace s then None else Some s
    member this.Required(name: string) =
        match this.String name with
        | Some s -> s
        | None -> fail (sprintf "Missing argument: %s" name)
    member this.Bool(name: string, def: bool) =
        match args with
        | null -> def
        | a ->
            match a.[name] with
            | null -> def
            | v -> try v.GetValue<bool>() with _ -> (match this.String name with Some "true" -> true | Some "false" -> false | _ -> def)
    member this.Int(name: string, def: int) =
        match args with
        | null -> def
        | a ->
            match a.[name] with
            | null -> def
            | v -> try v.GetValue<int>() with _ -> (match Int32.TryParse(defaultArg (this.String name) "") with | true, i -> i | _ -> def)
    member this.Project = findProject (this.String "project")
    /// The "names" array, else "name".
    member this.Names =
        let fromArray =
            match args with
            | null -> []
            | a ->
                match a.["names"] with
                | :? JsonArray as arr ->
                    [ for v in arr do
                        match (try v.GetValue<string>() with _ -> null) with
                        | null -> ()
                        | s when String.IsNullOrWhiteSpace s -> ()
                        | s -> yield s.Trim() ]
                | null -> []
                | v ->
                    match (try v.GetValue<string>() with _ -> null) with
                    | null -> []
                    | s -> s.Split([| ','; '\n' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.Trim()) |> Array.filter ((<>) "") |> List.ofArray
        match fromArray, this.String "name" with
        | [], None -> fail "Missing argument: name (or names, a list of names)"
        | [], Some n -> [ n ]
        | names, Some n when not (List.contains n names) -> n :: names
        | names, _ -> List.distinct names

/// The answer budget of one call; a call with several names shares it.
let maxResultChars = 60000

/// Runs a tool for one name once per name of the call: a section per name, then the names that found nothing.
let batched (run: ProjectState -> string -> string) (a: ToolArgs) =
    let p = a.Project
    match a.Names with
    | [ name ] -> run p name
    | names ->
        Stats.value "tool.names_per_call" (float names.Length)
        let budget = max 4000 (maxResultChars / names.Length)
        let results =
            names |> List.map (fun n ->
                try
                    let text = run p n
                    Ok (n, (if text.Length > budget then text.Substring(0, budget) + "\n... (truncated: ask for this name alone for all of it)" else text))
                with ToolError msg -> Error (n, msg))
        let found = results |> List.choose (function Ok r -> Some r | Error _ -> None)
        let missed = results |> List.choose (function Error r -> Some r | Ok _ -> None)
        if not missed.IsEmpty then Stats.countBy "tool.names_missed" (int64 missed.Length)
        let missedText = missed |> List.map (fun (n, msg) -> sprintf "=== %s: not found\n%s" n msg) |> String.concat "\n\n"
        if found.IsEmpty then fail missedText
        else
            (found |> List.map (fun (n, text) -> sprintf "=== %s\n%s" n text) |> String.concat "\n\n")
            + (if missed.IsEmpty then "" else "\n\n" + missedText)

// ---------------------------------------------------------------------------------------------
// Diagnostics

let private errorLines (p: ProjectState) = p.Report.Log |> Array.filter (fun l -> l.IsError) |> Array.map (fun l -> l.Text)
let isErrorLine (l: string) = l.Contains "error WS" || l.Contains "WebSharper error:" || l.Contains ": error "
let isWarningLine (l: string) = l.Contains "warning WS" || l.Contains "WebSharper warning:" || l.Contains ": warning "

let private projectHeader (p: ProjectState) =
    sprintf "Project %s (assembly %s, %s), WebSharper compiled %s in %.1f s, exit code %d"
        p.ProjectFile p.AssemblyName p.Report.ProjectType (p.Finished.ToString("yyyy-MM-dd HH:mm:ss")) (p.Report.DurationMs / 1000.) p.Report.ExitCode

/// file(line,col[,endLine,endCol]): WebSharper error WS9001: ...
let private diagnosticLocation = Text.RegularExpressions.Regex(@"^\s*(?<file>.+?)\((?<line>\d+),(?<col>\d+)(,\d+,\d+)?\):\s+(WebSharper\s+)?(error|warning)\s+\w+")

/// The source line of a diagnostic.
let private sourceLine (diagnostic: string) =
    let m = diagnosticLocation.Match diagnostic
    if not m.Success then [] else
    let file = m.Groups.["file"].Value
    let line = int m.Groups.["line"].Value
    try
        if File.Exists file then
            let lines = File.ReadAllLines file
            if line >= 1 && line <= lines.Length then [ sprintf "      %d | %s" line (lines.[line - 1].TrimEnd()) ] else []
        else []
    with _ -> []

let hints =
    [
        "WS9002", "Remote method is returning a type", "add [JavaScript] to the type the RPC takes or returns"
        "WS9001", "not found in JavaScript compilation", "client code uses something without a JavaScript translation: check it with get_js_translation, move server-only code into a [Remote] method, or mark your own code [JavaScript]"
        "WS9001", "Duplicate remote method path", "RPC paths are <class>/<method> without the namespace: rename one class or method"
    ]

let diagnostics (p: ProjectState) (file: string option) (includeWarnings: bool) (limit: int) =
    let sb = StringBuilder()
    sb.AppendLine(projectHeader p) |> ignore
    if p.Report.Skipped && p.DiagnosticsFrom < p.Finished then
        sb.AppendLine(sprintf "The WebSharper outputs were up to date: these are the diagnostics of the compilation of %s." (p.DiagnosticsFrom.ToString("yyyy-MM-dd HH:mm:ss"))) |> ignore
    let lines =
        errorLines p
        |> Array.filter (fun l -> isErrorLine l || (includeWarnings && isWarningLine l) || not (isWarningLine l))
        |> Array.filter (fun l -> match file with Some f -> l.Contains(f, StringComparison.OrdinalIgnoreCase) | None -> true)
        |> Array.distinct
    let found = hints |> List.filter (fun (_, text, _) -> lines |> Array.exists (fun l -> l.Contains text)) |> List.map (fun (code, _, hint) -> sprintf "  hint (%s): %s" code hint) |> List.distinct
    if lines.Length = 0 then
        sb.AppendLine(if p.Report.ExitCode = 0 then "No WebSharper errors or warnings." else "No errors were printed, see the build output.") |> ignore
    else
        let mutable explained = 0
        for l in Array.truncate limit lines do
            sb.AppendLine(l) |> ignore
            if explained < 8 && isErrorLine l then
                explained <- explained + 1
                for e in sourceLine l do sb.AppendLine(e) |> ignore
        if lines.Length > limit then sb.AppendLine(sprintf "... %d more, raise limit to see them" (lines.Length - limit)) |> ignore
        for h in found do sb.AppendLine(h) |> ignore
    sb.AppendLine("C# compiler diagnostics (CS....) are not included: they come from csc, see the build output.") |> ignore
    sb.ToString().TrimEnd()

// ---------------------------------------------------------------------------------------------
// Translations

/// C# keywords for types, as an agent writes them.
let private aliases =
    dict [
        "string", "System.String"; "object", "System.Object"; "bool", "System.Boolean"; "char", "System.Char"
        "byte", "System.Byte"; "sbyte", "System.SByte"; "short", "System.Int16"; "ushort", "System.UInt16"
        "int", "System.Int32"; "uint", "System.UInt32"; "long", "System.Int64"; "ulong", "System.UInt64"
        "float", "System.Single"; "double", "System.Double"; "decimal", "System.Decimal"
        "nint", "System.IntPtr"; "nuint", "System.UIntPtr"; "void", "System.Void"
    ]

let private requireTranslations (p: ProjectState) =
    match p.Translations.Value with
    | Some t -> t
    | None ->
        let why =
            match p.CurrentMetadata.Value with
            | Some (Error e) -> sprintf "its WebSharper metadata could not be read (%s)" e
            | _ -> sprintf "no WebSharper metadata was found for it or its references (exit code %d)" p.Report.ExitCode
        fail (sprintf "No translations for %s: %s. See get_diagnostics." p.ProjectFile why)

/// Types by full or partial name (Dictionary, Generic.Dictionary, System.Collections.Generic.Dictionary<K,V>).
let lookupTypes (index: TranslationIndex) (typeName: string) =
    let n = normalizeName typeName
    let n = match aliases.TryGetValue n with | true, full -> full | _ -> n
    let get (d: Dictionary<string, ResizeArray<_>>) = match d.TryGetValue n with | true, l -> List.ofSeq l | _ -> []
    match get index.Classes @ get index.Interfaces with
    | _ :: _ as l -> l
    | [] ->
        let suffix = "." + n
        let fromKeys (cmp: StringComparison) =
            Seq.append index.Classes index.Interfaces
            |> Seq.filter (fun kv -> kv.Key.EndsWith(suffix, cmp) || String.Equals(kv.Key, n, cmp))
            |> Seq.collect (fun kv -> kv.Value)
            |> Seq.distinctBy (fun (t, _, _) -> t)
            |> List.ofSeq
        match fromKeys StringComparison.Ordinal with
        | [] -> fromKeys StringComparison.OrdinalIgnoreCase
        | l -> l

let translationOf (p: ProjectState) (name: string) =
    let index = requireTranslations p
    let name = name.Trim()
    let stripGenerics (s: string) = if s.EndsWith ">" && s.Contains "<" then s.Substring(0, s.LastIndexOf '<') else s
    let memberFilter, found =
        match lookupTypes index name with
        | _ :: _ as l -> None, l
        | [] ->
            match splitLast (stripGenerics name) with
            | Some (owner, m) ->
                match lookupTypes index owner with
                | _ :: _ as l ->
                    let m = if m = "ctor" || m = "#ctor" || m = "new" || m = ".ctor" then ".ctor" else m
                    let names = [ m ]
                    let having = l |> List.filter (fun (t, _, info) -> hasTranslatedMemberNamed t info names)
                    Some (m, names), (if having.IsEmpty then [ List.head l ] else having)
                | [] -> None, []
            | None -> None, []
    match found with
    | [] ->
        let last = match splitLast (normalizeName name) with Some (_, l) -> l | None -> normalizeName name
        // close short names first (typos: Lst is List), then names containing it
        let shortOf (k: string) = match k.LastIndexOf '.' with -1 -> k | i -> k.Substring(i + 1)
        let keys = Seq.append index.Classes.Keys index.Interfaces.Keys |> Seq.distinct |> Array.ofSeq
        let close =
            keys
            |> Array.choose (fun k -> let d = editDistance (shortOf k) last in if d <= max 1 (last.Length / 4) then Some (d, k) else None)
            |> Array.sortBy (fun (d, k) -> d, (if k.StartsWith "System." || k.StartsWith "WebSharper." then 0 else 1), k.Length)
            |> Array.map snd
        let containing = keys |> Array.filter (fun k -> (shortOf k).Contains(last, StringComparison.OrdinalIgnoreCase)) |> Array.sortBy (fun k -> k.Length)
        let similar = Array.append close containing |> Array.distinct |> Array.truncate 10 |> List.ofArray
        sprintf "%s has no WebSharper translation in %s or its references: it can't be used in client-side ([JavaScript]) code.%s"
            name p.AssemblyName
            (if similar.IsEmpty then " (Check the name with search_symbols.)" else "\nTranslated types with similar names:\n" + (similar |> List.map (sprintf "  %s") |> String.concat "\n"))
    | found ->
        let shown = found |> List.truncate 5
        let text =
            shown
            |> List.map (fun (t, src, info) -> formatTranslationWith "[JavaScript]" t src info memberFilter)
            |> List.filter (fun s -> s <> "")
            |> String.concat "\n\n"
        if found.Length > shown.Length then
            text + sprintf "\n\n... %d more types match %s, give its full name: %s" (found.Length - shown.Length) name
                (found |> List.skip shown.Length |> List.truncate 10 |> List.map (fun (t, _, _) -> t.Value.FullName) |> String.concat ", ")
        else text

/// Translated types and members whose name contains the query: what client-side code can use.
let searchTranslations (p: ProjectState) (query: string) (includeMembers: bool) (projectOnly: bool) (limit: int) =
    let index = requireTranslations p
    let q = query.Trim()
    let rank (full: string) (short: string) =
        if String.Equals(short, q, StringComparison.OrdinalIgnoreCase) then Some 0
        elif short.StartsWith(q, StringComparison.OrdinalIgnoreCase) then Some 1
        elif short.Contains(q, StringComparison.OrdinalIgnoreCase) then Some 2
        elif full.Contains(q, StringComparison.OrdinalIgnoreCase) then Some 3
        else None
    let shortName (n: string) = match n.LastIndexOf '.' with -1 -> n | i -> n.Substring(i + 1)
    let results = ResizeArray()
    let scan (d: Dictionary<string, ResizeArray<TypeDefinition * string * M.Info>>) (kind: string) =
        for kv in d do
            for (t, src, info) in kv.Value do
                let own = src = "this project"
                if not projectOnly || own then
                    match rank kv.Key (shortName kv.Key) with
                    | Some r -> results.Add((r, (if own then 0 else 1), kv.Key, kind, t.Value.Assembly))
                    | None -> ()
                    if includeMembers && (own || t.Value.Assembly.StartsWith "WebSharper") then
                        let names =
                            match info.Classes.TryGetValue t with
                            | true, (_, _, Some cls) -> cls.Methods.Keys |> Seq.map (fun m -> m.Value.MethodName) |> List.ofSeq
                            | _ ->
                                match info.Interfaces.TryGetValue t with
                                | true, intf -> intf.Methods.Keys |> Seq.map (fun m -> m.Value.MethodName) |> List.ofSeq
                                | _ -> []
                        for m in names |> List.distinct do
                            // accessors as the property: get_Length is Length
                            let display = if m.StartsWith "get_" || m.StartsWith "set_" then m.Substring 4 else m
                            match rank (kv.Key + "." + display) display with
                            | Some r -> results.Add((r, (if own then 0 else 1), kv.Key + "." + display, "member", t.Value.Assembly))
                            | None -> ()
    scan index.Classes "class"
    scan index.Interfaces "interface"
    results
    |> Seq.distinctBy (fun (_, _, n, k, a) -> n, k, a)
    |> Seq.sortBy (fun (r, own, n, _, _) -> r, own, n.Length, n)
    |> Seq.truncate limit
    |> Seq.map (fun (_, own, n, k, a) -> sprintf "%s  (%s, %s%s)" n k a (if own = 0 then ", this project" else ""))
    |> List.ofSeq

// ---------------------------------------------------------------------------------------------
// Tools

let listProjects (_: ToolArgs) =
    let all = projects.Values |> Seq.sortByDescending (fun p -> p.Finished) |> List.ofSeq
    if all.IsEmpty then noProjects
    else
    let sb = StringBuilder()
    for p in all do
        let errors = errorLines p |> Array.filter isErrorLine |> Array.length
        let warnings = errorLines p |> Array.filter isWarningLine |> Array.length
        sb.AppendLine(projectHeader p) |> ignore
        sb.AppendLine(sprintf "  %d WebSharper error(s), %d warning(s) printed%s" errors warnings (if p.Report.Skipped then " (outputs were up to date)" else "")) |> ignore
        sb.AppendLine(sprintf "  %d source files, %d references" p.Report.SourceFiles p.Report.References.Length) |> ignore
    sb.ToString().TrimEnd()

let getDiagnostics (a: ToolArgs) =
    diagnostics a.Project (a.String "file") (a.Bool("include_warnings", true)) (a.Int("limit", 100))

let getJsTranslation (a: ToolArgs) = batched translationOf a

let searchSymbols (a: ToolArgs) =
    let p = a.Project
    let query = a.Required "query"
    match searchTranslations p query (a.Bool("include_members", true)) (a.Bool("project_only", false)) (a.Int("limit", 50)) with
    | [] -> sprintf "No type or member translated by WebSharper matches \"%s\" in %s or its references." query p.AssemblyName
    | l -> String.concat "\n" l

let listRpcMethods (a: ToolArgs) =
    let p = a.Project
    match p.CurrentMetadata.Value with
    | Some (Ok info) ->
        let rpcs = rpcMethods info
        let sitelet =
            match info.SiteletDefinition with
            | Some t -> sprintf "Website (sitelet) definition: %s\n" t.Value.FullName
            | None -> ""
        if rpcs.IsEmpty then sitelet + sprintf "%s has no RPC methods ([Remote])." p.AssemblyName
        else sitelet + sprintf "RPC methods of %s (%d):\n" p.AssemblyName rpcs.Length + String.concat "\n" rpcs
    | Some (Error e) -> fail (sprintf "The WebSharper metadata of %s could not be read: %s" p.Report.Assembly e)
    | None -> fail (sprintf "%s has no WebSharper metadata (exit code %d), see get_diagnostics." p.Report.Assembly p.Report.ExitCode)

type Tool =
    {
        Name : string
        Description : string
        /// name, type, description, required
        Parameters : (string * string * string * bool) list
        Run : ToolArgs -> string
    }

let private projectParam = "project", "string", "Project file path, file name or assembly name. Defaults to the most recently compiled project.", false
let private namesParam = "names", "string[]", "Several names at once, instead of name: one call answers for all of them.", false

let tools : Tool list =
    [
        {
            Name = "list_projects"
            Description = "List the WebSharper C# projects whose compilation the booster recorded since it started, with their type, last compilation time, exit code and WebSharper error/warning counts. Start here to find project names for the other tools."
            Parameters = []
            Run = listProjects
        }
        {
            Name = "get_diagnostics"
            Description = "WebSharper errors and warnings of the last compilation of a C# project, as printed by the build: file(line,col): WebSharper error WS9001: message, with the source line of the first errors. C# compiler errors are not included (they come from csc)."
            Parameters = [ projectParam; "file", "string", "Only diagnostics of source files whose path contains this text.", false; "include_warnings", "boolean", "Include warnings (default true).", false; "limit", "integer", "Maximum number of lines (default 100).", false ]
            Run = getDiagnostics
        }
        {
            Name = "get_js_translation"
            Description = "How WebSharper translates a type or member to JavaScript: its JavaScript address and the compiled form of each member (instance/static method, inline, macro, RPC), from the metadata of the project and its references. Tells whether the type or member can be used in client-side ([JavaScript]) code at all. Takes C# type keywords (string, int) and partial names (Dictionary, Doc.Element). Pass several names with names to check them all in one call."
            Parameters = [ projectParam; "name", "string", "Type (System.Collections.Generic.Dictionary, string) or Type.Member (System.String.Split, List.ctor).", false; namesParam ]
            Run = getJsTranslation
        }
        {
            Name = "search_symbols"
            Description = "Find types and members translated by WebSharper (usable in client-side code) by part of their name, in the project and its references (members only in the project and WebSharper libraries). Exact matches first, project symbols first."
            Parameters = [ projectParam; "query", "string", "Part of the name to look for, case-insensitive.", true; "include_members", "boolean", "Also search members (default true).", false; "project_only", "boolean", "Only types of the project (default false).", false; "limit", "integer", "Maximum number of results (default 50).", false ]
            Run = searchSymbols
        }
        {
            Name = "list_rpc_methods"
            Description = "The RPC methods ([Remote]) the project exposes to client-side code, with their remoting paths, and its Website (sitelet) definition."
            Parameters = [ projectParam ]
            Run = listRpcMethods
        }
    ]

let toolsJson () =
    let arr = JsonArray()
    for t in tools do
        let props = JsonObject()
        for (name, typ, desc, _) in t.Parameters do
            props.[name] <-
                if typ = "string[]" then
                    JsonObject(dict [ "type", JsonValue.Create("array") :> JsonNode; "items", JsonObject(dict [ "type", JsonValue.Create("string") :> JsonNode ]) :> JsonNode; "description", JsonValue.Create(desc) :> JsonNode ])
                else
                    JsonObject(dict [ "type", JsonValue.Create(typ) :> JsonNode; "description", JsonValue.Create(desc) :> JsonNode ])
        let required = JsonArray(t.Parameters |> List.filter (fun (_, _, _, r) -> r) |> List.map (fun (n, _, _, _) -> JsonValue.Create(n) :> JsonNode) |> Array.ofList)
        let schema = JsonObject(dict [ "type", JsonValue.Create("object") :> JsonNode; "properties", props :> JsonNode; "required", required :> JsonNode ])
        let annotations = JsonObject(dict [ "readOnlyHint", JsonValue.Create(true) :> JsonNode ])
        arr.Add(JsonObject(dict [ "name", JsonValue.Create(t.Name) :> JsonNode; "description", JsonValue.Create(t.Description) :> JsonNode; "inputSchema", schema :> JsonNode; "annotations", annotations :> JsonNode ]))
    arr

let instructions =
    "Answers questions about the WebSharper C# projects built on this machine, from what the WebSharper C# booster "
    + "(wscscservice) recorded after each build: WebSharper diagnostics, and how WebSharper translates types and members "
    + "to JavaScript, from the exact WebSharper versions the project uses. "
    + "Use list_projects first; the other tools default to the most recently compiled project. "
    + "Information is as of the last build of a project: run dotnet build after editing source files. "
    + "Use get_js_translation (several names at once with names) before using a .NET API in [JavaScript] code, "
    + "and search_symbols to find what client-side code can use. "
    + "For online documentation use docs.websharper.com; never use developers.websharper.com/docs/v4.x or other 4.x pages, they are superseded and their APIs differ."

/// Runs a tool, Ok with its text or Error with a message for the agent.
let callTool (name: string) (arguments: string) : Result<string, string> =
    match tools |> List.tryFind (fun t -> t.Name = name) with
    | None -> Error (sprintf "Unknown tool: %s" name)
    | Some tool ->
        let key = sprintf "tool.core.%s" (Stats.part name)
        let sw = Diagnostics.Stopwatch.StartNew()
        let result, outcome =
            try
                let args = if String.IsNullOrWhiteSpace arguments then null else JsonNode.Parse arguments
                let text = tool.Run (ToolArgs args)
                let text = if text.Length > maxResultChars then text.Substring(0, maxResultChars) + "\n... (truncated)" else text
                Ok text, "ok"
            with
            | ToolError msg -> Error msg, "not_answered"
            | e -> Error (sprintf "%s failed: %s" name e.Message), "failed"
        Stats.ms (key + "_ms") sw.Elapsed
        Stats.count (key + "." + outcome)
        Stats.value (key + ".chars") (float (match result with Ok t | Error t -> t.Length))
        result

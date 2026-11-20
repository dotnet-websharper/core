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

/// Answers questions of AI agents (and other tools) about the projects compiled by the booster,
/// from what the last compilation of each project left in memory. See Mcp.fs for the protocol.
module WebSharper.FSharp.Service.AgentTools

open System
open System.IO
open System.Text
open System.Text.Json.Nodes
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open WebSharper.Compiler
open WebSharper.Compiler.FSharp.AgentSnapshot

module M = WebSharper.Core.Metadata
type TypeDefinition = WebSharper.Core.AST.TypeDefinition

// ---------------------------------------------------------------------------------------------
// Names

/// Strips generic arity and arguments, uses '.' for nesting: List`1, List<'T> and List<int> are List.
let normalizeName (name: string) =
    let sb = StringBuilder()
    let mutable depth = 0
    let mutable i = 0
    while i < name.Length do
        let c = name.[i]
        if c = '<' then depth <- depth + 1
        elif c = '>' then depth <- max 0 (depth - 1)
        elif depth = 0 then
            if c = '`' then
                // skip the arity digits
                while i + 1 < name.Length && Char.IsDigit name.[i + 1] do i <- i + 1
            elif c = '+' || c = '/' then sb.Append('.') |> ignore
            elif not (Char.IsWhiteSpace c) then sb.Append(c) |> ignore
        i <- i + 1
    sb.ToString().TrimEnd('.')

/// Splits "A.B.c" into ("A.B", "c"), member names can be operators containing dots: ( |>. )
let splitLast (name: string) =
    let i = name.LastIndexOf '.'
    if i <= 0 || i = name.Length - 1 then None else Some (name.Substring(0, i), name.Substring(i + 1))

let private tryOr (f: unit -> 'a) (fallback: 'a) = try f () with _ -> fallback

// ---------------------------------------------------------------------------------------------
// F# symbols

/// Type names as written in code: int, string list, Doc, not Microsoft.FSharp.Core.int
let displayContext = FSharpDisplayContext.Empty.WithShortTypeNames(true)

type SymbolIndex =
    {
        /// Entities by normalized full, display and compiled names.
        ByName : Dictionary<string, ResizeArray<FSharpEntity>>
        /// All entities, project entities first.
        All : FSharpEntity[]
        /// Entities declared in the project.
        IsProjectEntity : FSharpEntity -> bool
        /// Extension members by the normalized names of the type they extend (attr.classDyn is in HtmlExtensions).
        Extensions : Dictionary<string, ResizeArray<FSharpEntity * FSharpMemberOrFunctionOrValue>>
        /// Top-level types and modules by namespace.
        Namespaces : Dictionary<string, ResizeArray<FSharpEntity>>
    }

/// Namespace or module path and name: type abbreviations have no full name in FCS (RpcAttribute, templates).
let private pathName (e: FSharpEntity) =
    match tryOr (fun () -> e.AccessPath) "" with
    | "" | "global" -> e.DisplayName
    | ap -> ap + "." + e.DisplayName

let entityNames (e: FSharpEntity) =
    [
        tryOr (fun () -> e.TryFullName) None
        tryOr (fun () -> e.TryGetFullDisplayName()) None
        tryOr (fun () -> e.TryGetFullCompiledName()) None
    ]
    |> List.choose id
    |> function [] -> [ tryOr (fun () -> pathName e) e.DisplayName ] | l -> l
    |> List.map normalizeName
    |> List.distinct

let entityName (e: FSharpEntity) =
    match tryOr (fun () -> e.TryGetFullDisplayName()) None with
    | Some n -> n
    | None ->
        match tryOr (fun () -> e.TryFullName) None with
        | Some n -> n
        | None -> tryOr (fun () -> pathName e) e.DisplayName

let buildIndex (check: FSharpCheckProjectResults) =
    let all = ResizeArray<FSharpEntity>()
    let own = HashSet<FSharpEntity>(HashIdentity.Reference)
    let namespaces = Dictionary<string, ResizeArray<FSharpEntity>>(StringComparer.Ordinal)
    let rec collect isOwn (e: FSharpEntity) =
        all.Add e
        if isOwn then own.Add e |> ignore
        for n in tryOr (fun () -> Seq.toArray e.NestedEntities) [||] do
            collect isOwn n
    let top (e: FSharpEntity) =
        match tryOr (fun () -> e.Namespace) None with
        | Some ns ->
            match namespaces.TryGetValue ns with
            | true, l -> l.Add e
            | _ -> namespaces.[ns] <- ResizeArray [ e ]
        | None -> ()
    for e in check.AssemblySignature.Entities do
        top e
        collect true e
    for a in tryOr (fun () -> check.ProjectContext.GetReferencedAssemblies()) [] do
        for e in tryOr (fun () -> Seq.toArray a.Contents.Entities) [||] do
            top e
            collect false e
    let byName = Dictionary<string, ResizeArray<FSharpEntity>>(StringComparer.Ordinal)
    let add (n: string) (e: FSharpEntity) =
        match byName.TryGetValue n with
        | true, l -> if not (l.Contains e) then l.Add e
        | _ -> byName.[n] <- ResizeArray [ e ]
    for e in all do
        for n in entityNames e do add n e
    // templates: type Page = Template<"Main.html">, the provided nested types as Page.TodoForm
    for e in own do
        if tryOr (fun () -> e.IsFSharpAbbreviation) false then
            match tryOr (fun () -> if e.AbbreviatedType.HasTypeDefinition then Some e.AbbreviatedType.TypeDefinition else None) None with
            | Some target when tryOr (fun () -> target.IsProvided) false ->
                let rec nested (prefixes: string list) (t: FSharpEntity) =
                    for n in tryOr (fun () -> Seq.toArray t.NestedEntities) [||] do
                        let names = prefixes |> List.map (fun p -> p + "." + n.DisplayName)
                        for name in names do add name n
                        nested names n
                let prefixes =
                    (entityNames e @ [ (match tryOr (fun () -> e.AccessPath) "" with "" -> e.DisplayName | ap -> ap + "." + e.DisplayName) ])
                    |> List.map normalizeName |> List.distinct
                for pr in prefixes do add pr e
                nested prefixes target
            | _ -> ()
    // extension members of the project and of WebSharper libraries (the others are too many to scan)
    let extensions = Dictionary<string, ResizeArray<FSharpEntity * FSharpMemberOrFunctionOrValue>>(StringComparer.Ordinal)
    for e in all do
        let scan = own.Contains e || (tryOr (fun () -> e.Assembly.SimpleName.StartsWith "WebSharper") false)
        if scan && tryOr (fun () -> e.IsFSharpModule) false then
            for m in tryOr (fun () -> Seq.toArray e.MembersFunctionsAndValues) [||] do
                if tryOr (fun () -> m.IsExtensionMember) false then
                    match tryOr (fun () -> m.ApparentEnclosingEntity) None with
                    | Some target ->
                        for n in entityNames target do
                            match extensions.TryGetValue n with
                            | true, l -> l.Add((e, m))
                            | _ -> extensions.[n] <- ResizeArray [ (e, m) ]
                    | None -> ()
    {
        ByName = byName
        All = all.ToArray()
        IsProjectEntity = fun e -> own.Contains e
        Extensions = extensions
        Namespaces = namespaces
    }

type ResolvedSymbol =
    | REntity of FSharpEntity
    | RMember of FSharpEntity * FSharpMemberOrFunctionOrValue
    | RField of FSharpEntity * FSharpField
    | RUnionCase of FSharpEntity * FSharpUnionCase

let symbolOf r : FSharpSymbol =
    match r with
    | REntity e -> upcast e
    | RMember (_, m) -> upcast m
    | RField (_, f) -> upcast f
    | RUnionCase (_, c) -> upcast c

let private findEntities (index: SymbolIndex) (name: string) =
    let n = normalizeName name
    match index.ByName.TryGetValue n with
    | true, l -> List.ofSeq l
    | _ ->
        // a partially qualified name: List.map, Doc.Element, Collections.Generic.List
        let suffix = "." + n
        let fromKeys (cmp: StringComparison) =
            index.ByName
            |> Seq.filter (fun kv -> kv.Key.EndsWith(suffix, cmp) || String.Equals(kv.Key, n, cmp))
            |> Seq.collect (fun kv -> kv.Value)
            |> Seq.distinct
            |> List.ofSeq
        match fromKeys StringComparison.Ordinal with
        | [] -> fromKeys StringComparison.OrdinalIgnoreCase
        | l -> l

/// The type a type abbreviation stands for: the provided type of a template (type Page = Template<"Main.html">)
let abbreviatedEntity (e: FSharpEntity) =
    if tryOr (fun () -> e.IsFSharpAbbreviation) false then
        tryOr (fun () -> if e.AbbreviatedType.HasTypeDefinition then Some e.AbbreviatedType.TypeDefinition else None) None
    else None

let private isAccessor (m: FSharpMemberOrFunctionOrValue) =
    tryOr (fun () -> m.IsPropertyGetterMethod || m.IsPropertySetterMethod || m.IsEventAddMethod || m.IsEventRemoveMethod) false

/// Finds a type, module or member by name, most specific results first, project symbols first.
let resolve (index: SymbolIndex) (name: string) : ResolvedSymbol list =
    let name = name.Trim()
    let projectFirst (l: ResolvedSymbol list) =
        let entityOf = function REntity e | RMember (e, _) | RField (e, _) | RUnionCase (e, _) -> e
        l |> List.sortBy (fun r -> if index.IsProjectEntity (entityOf r) then 0 else 1)
    match findEntities index name with
    | _ :: _ as es -> es |> List.map REntity |> projectFirst
    | [] ->
    // a member: the name is split before normalizing, generic arguments can contain dots
    let memberSplit =
        let n = name
        // strip generic arguments of the member: map<'T>
        let n = if n.EndsWith ">" && n.Contains "<" then n.Substring(0, n.LastIndexOf '<') else n
        splitLast n
    match memberSplit with
    | None -> []
    | Some (owner, memberName) ->
    let memberName = if memberName = "ctor" || memberName = "#ctor" || memberName = "new" then ".ctor" else memberName
    // owners through type abbreviations too: Templates.Page.TodoForm is in the provided Template type
    let owners =
        findEntities index owner
        |> List.collect (fun e -> e :: (Option.toList (abbreviatedEntity e)))
        |> List.distinct
    [
        for e in owners do
            let members = tryOr (fun () -> Seq.toArray e.MembersFunctionsAndValues) [||]
            let matching =
                members |> Array.filter (fun m ->
                    if memberName = ".ctor" then tryOr (fun () -> m.IsConstructor) false
                    else
                        let names = [ tryOr (fun () -> m.DisplayName) ""; tryOr (fun () -> m.CompiledName) ""; tryOr (fun () -> m.LogicalName) "" ]
                        names |> List.exists (fun n -> n = memberName)
                )
            // a property and its get_X/set_X accessors: keep the property
            let props = matching |> Array.exists (fun m -> tryOr (fun () -> m.IsProperty) false)
            for m in matching do
                if not (props && isAccessor m) then
                    yield RMember (e, m)
            for f in tryOr (fun () -> Seq.toArray e.FSharpFields) [||] do
                if f.Name = memberName then yield RField (e, f)
            if tryOr (fun () -> e.IsFSharpUnion) false then
                for c in e.UnionCases do
                    if c.Name = memberName then yield RUnionCase (e, c)
            for n in tryOr (fun () -> Seq.toArray e.NestedEntities) [||] do
                if n.DisplayName = memberName || n.CompiledName = memberName then yield REntity n
            // extension members of the type: attr.classDyn
            for n in entityNames e do
                match index.Extensions.TryGetValue n with
                | true, l ->
                    for (ext, m) in l do
                        if tryOr (fun () -> m.DisplayName = memberName || m.LogicalName = memberName || m.CompiledName = memberName) false then
                            yield RMember (ext, m)
                | _ -> ()
    ]
    |> List.distinctBy (function RMember (_, m) -> box m | REntity e -> box e | RField (_, f) -> box f | RUnionCase (_, c) -> box c)
    |> projectFirst

/// Edit distance, for "did you mean" suggestions on typos (Server.ToggleTdo).
let editDistance (a: string) (b: string) =
    let a, b = a.ToLowerInvariant(), b.ToLowerInvariant()
    let d = Array2D.init (a.Length + 1) (b.Length + 1) (fun i j -> if i = 0 then j elif j = 0 then i else 0)
    for i in 1 .. a.Length do
        for j in 1 .. b.Length do
            let cost = if a.[i - 1] = b.[j - 1] then 0 else 1
            d.[i, j] <- min (min (d.[i - 1, j] + 1) (d.[i, j - 1] + 1)) (d.[i - 1, j - 1] + cost)
    d.[a.Length, b.Length]

/// Like resolve, and when nothing matches: the name without its leading namespace segments
/// (WebSharper.UI.Doc.EmbedView finds WebSharper.UI.Client.Doc.EmbedView), or with an Attribute suffix (Rpc).
/// Returns the results and a note when they are for another name than given.
let resolveLoose (index: SymbolIndex) (name: string) : ResolvedSymbol list * string option =
    match resolve index name with
    | _ :: _ as r -> r, None
    | [] ->
    let segments = (normalizeName name).Split('.')
    let candidates =
        [
            if not (name.EndsWith "Attribute") then yield name + "Attribute"
            for k in 1 .. segments.Length - 2 do
                let rest = String.Join(".", segments.[k..])
                yield rest
                if not (rest.EndsWith "Attribute") then yield rest + "Attribute"
            // attributes by their name alone: WebSharper.Remoting.Rpc is WebSharper.RpcAttribute
            if segments.Length > 1 && not (segments.[segments.Length - 1].EndsWith "Attribute") then
                yield segments.[segments.Length - 1] + "Attribute"
        ]
    candidates
    |> List.tryPick (fun c ->
        match resolve index c with
        | [] -> None
        | r -> Some (r, Some (sprintf "No exact match for %s, these match %s:" name c)))
    |> Option.defaultValue ([], None)

/// Suggestions for a name that matches nothing: members of the owner with a close name, then similar names anywhere.
let suggest (index: SymbolIndex) (name: string) (search: SymbolIndex -> string -> bool -> bool -> int -> (string * string * string * bool) list) =
    let close =
        match splitLast (normalizeName name) with
        | Some (owner, m) ->
            let owners =
                match resolveLoose index owner with
                | rs, _ -> rs |> List.choose (function REntity e -> Some e | _ -> None)
            [
                for e in owners do
                    for mfv in tryOr (fun () -> Seq.toArray e.MembersFunctionsAndValues) [||] do
                        let n = tryOr (fun () -> mfv.DisplayName) ""
                        let dist = editDistance n m
                        if n <> "" && dist <= max 2 (m.Length / 4) then
                            yield dist, sprintf "%s.%s" (entityName e) n
            ]
            |> List.sortBy fst |> List.map snd |> List.distinct |> List.truncate 5
        | None -> []
    let last = match splitLast (normalizeName name) with Some (_, l) -> l | None -> normalizeName name
    let similar = search index last true false 10 |> List.map (fun (n, k, asm, _) -> sprintf "%s (%s, %s)" n k asm)
    (close @ similar) |> List.distinct |> List.truncate 10

// ---------------------------------------------------------------------------------------------
// Documentation

let private xmlFiles = ConcurrentDictionary<string, Dictionary<string, Xml.Linq.XElement> option>(StringComparer.OrdinalIgnoreCase)

let private docText (e: Xml.Linq.XElement) =
    let sb = StringBuilder()
    let rec walk (n: Xml.Linq.XNode) =
        match n with
        | :? Xml.Linq.XText as t -> sb.Append(t.Value) |> ignore
        | :? Xml.Linq.XElement as el ->
            match el.Name.LocalName with
            | "see" | "seealso" | "paramref" | "typeparamref" ->
                let a =
                    [ "cref"; "langword"; "name"; "href" ]
                    |> List.tryPick (fun a -> match el.Attribute(Xml.Linq.XName.Get a) with null -> None | x -> Some x.Value)
                match a with
                | Some v ->
                    let v = if v.Length > 2 && v.[1] = ':' then v.Substring 2 else v
                    sb.Append(if el.IsEmpty then v else el.Value) |> ignore
                | None -> sb.Append(el.Value) |> ignore
            | "para" | "br" ->
                sb.Append(' ') |> ignore
                for c in el.Nodes() do walk c
            | _ -> for c in el.Nodes() do walk c
        | _ -> ()
    walk e
    Text.RegularExpressions.Regex.Replace(sb.ToString(), @"\s+", " ").Trim()

let private summaryOf (doc: Xml.Linq.XElement) =
    match doc.Element(Xml.Linq.XName.Get "summary") with
    | null -> None
    | s ->
        let t = docText s
        if t = "" then None else Some t

let private tryLoadXml (path: string) =
    xmlFiles.GetOrAdd(path, fun path ->
        try
            if File.Exists path then
                let d = Dictionary<string, Xml.Linq.XElement>(StringComparer.Ordinal)
                for m in Xml.Linq.XDocument.Load(path).Descendants(Xml.Linq.XName.Get "member") do
                    match m.Attribute(Xml.Linq.XName.Get "name") with
                    | null -> ()
                    | n -> d.[n.Value] <- m
                Some d
            else None
        with _ -> None
    )

/// The summary of the XML documentation, from the source or from the xml file next to a referenced dll.
let docSummary (doc: FSharpXmlDoc) =
    try
        match doc with
        | FSharpXmlDoc.FromXmlText t ->
            let text = String.concat "\n" t.UnprocessedLines
            if String.IsNullOrWhiteSpace text then None
            else
                try summaryOf (Xml.Linq.XElement.Parse("<doc>" + text + "</doc>"))
                    |> Option.orElse (Some (Text.RegularExpressions.Regex.Replace(text, @"\s+", " ").Trim()))
                with _ -> Some (Text.RegularExpressions.Regex.Replace(text, @"\s+", " ").Trim())
        | FSharpXmlDoc.FromXmlFile (dll, xmlSig) ->
            tryLoadXml (Path.ChangeExtension(dll, ".xml"))
            |> Option.bind (fun d -> match d.TryGetValue xmlSig with | true, m -> summaryOf m | _ -> None)
        | _ -> None
    with _ -> None

// ---------------------------------------------------------------------------------------------
// Formatting F# symbols

let private webSharperAttributes (attrs: seq<FSharpAttribute>) =
    attrs
    |> Seq.filter (fun a ->
        match tryOr (fun () -> a.AttributeType.TryFullName) None with
        | Some n -> n.StartsWith "WebSharper."
        | None -> false)
    |> Seq.map (fun a -> tryOr (fun () -> a.Format displayContext) a.AttributeType.DisplayName)
    |> List.ofSeq

let private location (r: FSharp.Compiler.Text.range) =
    if (r.FileName.EndsWith ".fs" || r.FileName.EndsWith ".fsi" || r.FileName.EndsWith ".fsx") && File.Exists r.FileName then
        Some (sprintf "%s(%d,%d)" r.FileName r.StartLine (r.StartColumn + 1))
    else None

let entityKind (e: FSharpEntity) =
    tryOr (fun () ->
        if e.IsNamespace then "namespace"
        elif e.IsFSharpModule then "module"
        elif e.IsFSharpAbbreviation then "type abbreviation"
        elif e.IsFSharpRecord then "record"
        elif e.IsFSharpUnion then "union"
        elif e.IsFSharpExceptionDeclaration then "exception"
        elif e.IsInterface then "interface"
        elif e.IsEnum then "enum"
        elif e.IsDelegate then "delegate"
        elif e.IsValueType then "struct"
        elif e.IsMeasure then "unit of measure"
        else "class") "type"

let memberSignature (m: FSharpMemberOrFunctionOrValue) =
    let fallback () =
        let name = if tryOr (fun () -> m.IsConstructor) false then "new" else m.DisplayName
        sprintf "%s: %s" name (tryOr (fun () -> m.FullType.Format displayContext) "?")
    let text =
        tryOr (fun () -> m.GetValSignatureText(displayContext, FSharp.Compiler.Text.Range.range0)) None
        |> Option.defaultWith fallback
    let text = text.Trim()
    let isStatic = tryOr (fun () -> m.IsMember && not m.IsInstanceMember && not m.IsConstructor) false
    if tryOr (fun () -> m.IsInstanceMember) false && not (text.StartsWith "member") && not (text.StartsWith "abstract") then
        "member " + text
    elif isStatic && text.StartsWith "member " then
        "static " + text
    elif isStatic && not (text.StartsWith "static") then
        "static member " + text
    else text

let private sourceText (t: FSharp.Compiler.Text.ISourceText) = t.GetSubTextString(0, t.Length)

/// The message of an [<Obsolete>] attribute, so that agents don't write deprecated code (Doc.CheckBox).
let obsoleteOf (attrs: seq<FSharpAttribute>) =
    attrs
    |> Seq.tryFind (fun a -> tryOr (fun () -> a.AttributeType.TryFullName = Some "System.ObsoleteAttribute") false)
    |> Option.map (fun a ->
        match tryOr (fun () -> a.ConstructorArguments |> Seq.tryHead |> Option.map (fun (_, v) -> string v)) None with
        | Some m when not (String.IsNullOrWhiteSpace m) -> m
        | _ -> "deprecated")

let private header (sb: StringBuilder) (kind: string) (name: string) (assembly: string) (loc: string option) (attrs: string list) (doc: string option) =
    sb.AppendLine(sprintf "%s %s" kind name) |> ignore
    sb.AppendLine(sprintf "  assembly: %s" assembly) |> ignore
    loc |> Option.iter (fun l -> sb.AppendLine(sprintf "  declared at: %s" l) |> ignore)
    if not attrs.IsEmpty then
        sb.AppendLine(sprintf "  WebSharper attributes: %s" (String.concat " " attrs)) |> ignore
    doc |> Option.iter (fun d -> sb.AppendLine(sprintf "  summary: %s" d) |> ignore)

let maxSignatureChars = 30000

let formatResolved (r: ResolvedSymbol) =
    let sb = StringBuilder()
    match r with
    | REntity e ->
        let deprecated = tryOr (fun () -> obsoleteOf e.Attributes) None
        header sb (entityKind e) (entityName e) (tryOr (fun () -> e.Assembly.SimpleName) "?")
            (tryOr (fun () -> location e.DeclarationLocation) None)
            (tryOr (fun () -> webSharperAttributes e.Attributes) [])
            (tryOr (fun () -> docSummary e.XmlDoc) None)
        deprecated |> Option.iter (fun d -> sb.AppendLine(sprintf "  DEPRECATED: %s" d) |> ignore)
        sb.AppendLine() |> ignore
        let text =
            match tryOr (fun () -> e.TryGetMetadataText()) None with
            | Some t when t.Length > 0 ->
                (sourceText t).Split('\n')
                |> Array.map (fun l -> l.TrimEnd('\r'))
                // the header comments (assembly name and path), empty comments and opens
                |> Array.skipWhile (fun l -> l.TrimStart().StartsWith "//" || String.IsNullOrWhiteSpace l)
                |> Array.filter (fun l -> not (l.TrimStart().StartsWith "//" && l.Trim().Length <= 3) && not (l.StartsWith "open "))
                |> String.concat "\n"
                |> fun t -> Text.RegularExpressions.Regex.Replace(t, @"\n{3,}", "\n\n").Trim()
            | _ ->
                // project entities: list the members
                let members =
                    tryOr (fun () -> Seq.toList e.MembersFunctionsAndValues) []
                    |> List.filter (fun m -> not (isAccessor m) && not (tryOr (fun () -> m.IsCompilerGenerated) false))
                    |> List.map (fun m -> "    " + memberSignature m)
                let fields =
                    tryOr (fun () -> Seq.toList e.FSharpFields) []
                    |> List.map (fun f -> sprintf "    %s: %s" f.Name (tryOr (fun () -> f.FieldType.Format displayContext) "?"))
                let cases =
                    if tryOr (fun () -> e.IsFSharpUnion) false then
                        e.UnionCases |> Seq.map (fun c ->
                            if c.Fields.Count = 0 then "    | " + c.Name
                            else sprintf "    | %s of %s" c.Name (c.Fields |> Seq.map (fun f -> sprintf "%s: %s" f.Name (tryOr (fun () -> f.FieldType.Format displayContext) "?")) |> String.concat " * ")
                        ) |> List.ofSeq
                    else []
                let nested =
                    tryOr (fun () -> Seq.toList e.NestedEntities) []
                    |> List.map (fun n -> sprintf "    %s %s" (entityKind n) n.DisplayName)
                String.concat "\n" (cases @ fields @ members @ nested)
        let text =
            match abbreviatedEntity e with
            | Some target when tryOr (fun () -> target.IsProvided) false ->
                // provided types (templates) have no metadata text: list their members and nested types
                let members =
                    tryOr (fun () -> Seq.toList target.MembersFunctionsAndValues) []
                    |> List.filter (fun m -> not (isAccessor m))
                    |> List.map (fun m -> "    " + memberSignature m)
                let nested = tryOr (fun () -> Seq.toList target.NestedEntities) [] |> List.map (fun n -> sprintf "    nested type %s (get_signature %s.%s for its members)" n.DisplayName (entityName e) n.DisplayName)
                text + sprintf "\n\nprovided type %s:\n" (tryOr (fun () -> target.DisplayName) "?") + String.concat "\n" (members @ nested)
            | _ -> text
        if text.Length > maxSignatureChars then
            sb.Append(text.Substring(0, maxSignatureChars)).AppendLine().AppendLine("... (truncated, ask for a member to see it in full)") |> ignore
        else
            sb.Append(text) |> ignore
    | RMember (e, m) ->
        let deprecated = tryOr (fun () -> obsoleteOf m.Attributes) None
        let extended = if tryOr (fun () -> m.IsExtensionMember) false then tryOr (fun () -> m.ApparentEnclosingEntity) None else None
        let kind =
            if extended.IsSome then
                sprintf "extension member of %s (open %s), defined in" (entityName extended.Value) (tryOr (fun () -> e.AccessPath) "?")
            elif tryOr (fun () -> e.IsFSharpModule) false then
                if tryOr (fun () -> m.FullType.IsFunctionType) false then "function in module" else "value in module"
            elif tryOr (fun () -> m.IsConstructor) false then "constructor of"
            elif tryOr (fun () -> m.IsProperty) false then "property of"
            else "member of"
        header sb kind (entityName e) (tryOr (fun () -> e.Assembly.SimpleName) "?")
            (tryOr (fun () -> location m.DeclarationLocation) None)
            (tryOr (fun () -> webSharperAttributes m.Attributes) [])
            (tryOr (fun () -> docSummary m.XmlDoc) None)
        deprecated |> Option.iter (fun d -> sb.AppendLine(sprintf "  DEPRECATED: %s" d) |> ignore)
        sb.AppendLine().Append(memberSignature m) |> ignore
    | RField (e, f) ->
        header sb "field of" (entityName e) (tryOr (fun () -> e.Assembly.SimpleName) "?")
            (tryOr (fun () -> location f.DeclarationLocation) None)
            (tryOr (fun () -> webSharperAttributes f.PropertyAttributes) [])
            (tryOr (fun () -> docSummary f.XmlDoc) None)
        sb.AppendLine().Append(sprintf "%s: %s" f.Name (tryOr (fun () -> f.FieldType.Format displayContext) "?")) |> ignore
    | RUnionCase (e, c) ->
        header sb "union case of" (entityName e) (tryOr (fun () -> e.Assembly.SimpleName) "?")
            (tryOr (fun () -> location c.DeclarationLocation) None)
            (tryOr (fun () -> webSharperAttributes c.Attributes) [])
            (tryOr (fun () -> docSummary c.XmlDoc) None)
        let fields = c.Fields |> Seq.map (fun f -> sprintf "%s: %s" f.Name (tryOr (fun () -> f.FieldType.Format displayContext) "?")) |> String.concat " * "
        sb.AppendLine().Append(if fields = "" then "| " + c.Name else sprintf "| %s of %s" c.Name fields) |> ignore
    sb.ToString().TrimEnd()

/// Search results: (full name, kind, assembly, is from the project)
let search (index: SymbolIndex) (query: string) (includeMembers: bool) (projectOnly: bool) (limit: int) =
    let q = query.Trim()
    let rank (name: string) (short: string) =
        if String.Equals(short, q, StringComparison.OrdinalIgnoreCase) then Some 0
        elif short.StartsWith(q, StringComparison.OrdinalIgnoreCase) then Some 1
        elif short.Contains(q, StringComparison.OrdinalIgnoreCase) then Some 2
        elif name.Contains(q, StringComparison.OrdinalIgnoreCase) then Some 3
        else None
    let results = ResizeArray()
    for e in index.All do
        let isOwn = index.IsProjectEntity e
        if not projectOnly || isOwn then
            let full = entityName e
            let accessible = tryOr (fun () -> e.Accessibility.IsPublic || isOwn) true
            if accessible then
                match rank full e.DisplayName with
                | Some r -> results.Add((r, (if isOwn then 0 else 1), full, entityKind e, tryOr (fun () -> e.Assembly.SimpleName) "?"))
                | None -> ()
                // members of project entities and WebSharper libraries, others are too many
                let asm = tryOr (fun () -> e.Assembly.SimpleName) ""
                if includeMembers && (isOwn || asm.StartsWith "WebSharper") then
                    for m in tryOr (fun () -> Seq.toArray e.MembersFunctionsAndValues) [||] do
                        if not (isAccessor m) && tryOr (fun () -> m.Accessibility.IsPublic || isOwn) true then
                            let mname = tryOr (fun () -> m.DisplayName) ""
                            match rank (full + "." + mname) mname with
                            | Some r -> results.Add((r, (if isOwn then 0 else 1), full + "." + mname, (if tryOr (fun () -> m.IsProperty) false then "property" elif tryOr (fun () -> m.IsConstructor) false then "constructor" elif m.IsMember then "member" else "function"), asm))
                            | None -> ()
    results
    |> Seq.distinctBy (fun (_, _, n, k, a) -> n, k, a)
    |> Seq.sortBy (fun (r, own, n, _, _) -> r, own, n.Length, n)
    |> Seq.truncate limit
    |> Seq.map (fun (_, own, n, k, a) -> n, k, a, own = 0)
    |> List.ofSeq

// ---------------------------------------------------------------------------------------------
// WebSharper translation

/// Short type text for WebSharper method keys: FSharpAsync<FSharpList<Todo>>, string[], (int -> unit)
let rec typeText (t: WebSharper.Core.AST.Type) =
    let short (fullName: string) =
        let n = normalizeName fullName
        match n.LastIndexOf '.' with
        | -1 -> n
        | i -> n.Substring(i + 1)
    match t with
    | WebSharper.Core.AST.ConcreteType c ->
        let n = short c.Entity.Value.FullName
        match c.Generics with
        | [] -> n
        | gs -> n + "<" + (gs |> List.map typeText |> String.concat ", ") + ">"
    | WebSharper.Core.AST.TypeParameter i -> sprintf "'T%d" i
    | WebSharper.Core.AST.ArrayType (e, 1) -> typeText e + "[]"
    | WebSharper.Core.AST.ArrayType (e, rank) -> typeText e + "[" + String(',', rank - 1) + "]"
    | WebSharper.Core.AST.TupleType (ts, isStruct) -> (if isStruct then "struct (" else "(") + (ts |> List.map typeText |> String.concat " * ") + ")"
    | WebSharper.Core.AST.FSharpFuncType (a, r) -> "(" + typeText a + " -> " + typeText r + ")"
    | WebSharper.Core.AST.ByRefType e -> "byref<" + typeText e + ">"
    | WebSharper.Core.AST.VoidType -> "unit"
    | WebSharper.Core.AST.StaticTypeParameter i -> sprintf "^T%d" i
    | WebSharper.Core.AST.LocalTypeParameter n -> "'" + n
    | other -> string other

let methodText (m: WebSharper.Core.AST.Method) =
    let m = m.Value
    sprintf "%s%s(%s): %s" m.MethodName
        (if m.Generics > 0 then "<" + String.concat ", " (List.init m.Generics (sprintf "'M%d")) + ">" else "")
        (m.Parameters |> List.map typeText |> String.concat ", ")
        (typeText m.ReturnType)

let constructorText (c: WebSharper.Core.AST.Constructor) =
    sprintf "new(%s)" (c.Value.CtorParameters |> List.map typeText |> String.concat ", ")

let describeCompiled (c: M.CompiledMember) =
    let rec d c =
        match c with
        | M.Instance (name, _, _) -> sprintf "instance method \"%s\"" name
        | M.Static (name, fromInstance, _) -> sprintf "static method \"%s\"%s" name (if fromInstance then " (instance member compiled as static)" else "")
        | M.Func (name, fromInstance) -> sprintf "function \"%s\"%s" name (if fromInstance then " (instance member compiled as function)" else "")
        | M.GlobalFunc (address, _) -> sprintf "global function %O" address
        | M.New (Some name) -> sprintf "constructor \"%s\"" name
        | M.New None -> "constructor"
        | M.Inline (true, _) -> "inlined (JavaScript compiled from the .NET code)"
        | M.Inline (false, _) -> "inline JavaScript"
        | M.Macro (macroType, _, fallback) ->
            sprintf "macro %s%s" macroType.Value.FullName (match fallback with Some f -> ", fallback: " + d f | None -> "")
        | M.Remote (_, path, _) -> sprintf "RPC \"%s\" (runs on the server)" path
    d c

let describeField (f: M.CompiledField) =
    match f with
    | M.InstanceField n -> sprintf "field \"%s\"" n
    | M.OptionalField n -> sprintf "optional field \"%s\"" n
    | M.StaticField n -> sprintf "static field \"%s\"" n
    | M.IndexedField i -> sprintf "indexed field [%d]" i
    | M.VarField v -> sprintf "variable %O" v

let describeCustomType (c: M.CustomTypeInfo) =
    match c with
    | M.NotCustomType -> None
    | M.FSharpRecordInfo fields -> Some (sprintf "F# record, JavaScript fields: %s" (fields |> List.map (fun f -> f.JSName) |> String.concat ", "))
    | M.FSharpUnionInfo u -> Some (sprintf "F# union, cases: %s" (u.Cases |> List.map (fun c -> c.Name) |> String.concat ", "))
    | M.FSharpUnionCaseInfo _ -> Some "F# union case"
    | M.FSharpAnonRecordInfo names -> Some (sprintf "F# anonymous record: %s" (String.concat ", " names))
    | M.DelegateInfo _ -> Some "delegate"
    | M.EnumInfo t -> Some (sprintf "enum of %s" t.Value.FullName)
    | M.StructInfo -> Some "struct"

type TranslationIndex =
    {
        Classes : Dictionary<string, ResizeArray<TypeDefinition * string * M.Info>>
        Interfaces : Dictionary<string, ResizeArray<TypeDefinition * string * M.Info>>
    }

let buildTranslationIndex (sources: (string * M.Info) list) =
    let classes = Dictionary<string, ResizeArray<_>>(StringComparer.Ordinal)
    let interfaces = Dictionary<string, ResizeArray<_>>(StringComparer.Ordinal)
    let add (d: Dictionary<string, ResizeArray<_>>) (t: TypeDefinition) src info =
        let n = normalizeName t.Value.FullName
        match d.TryGetValue n with
        | true, l -> l.Add((t, src, info))
        | _ -> d.[n] <- ResizeArray [ (t, src, info) ]
    for (src, info) in sources do
        for kv in info.Classes do add classes kv.Key src info
        for kv in info.Interfaces do add interfaces kv.Key src info
    { Classes = classes; Interfaces = interfaces }

/// The member names to look for: as given, and the compiled names of matching F# members (List.map is Map)
type MemberFilter = { Given : string; Names : string list }

let private memberMatches (filter: MemberFilter option) (methodName: string) =
    match filter with
    | None -> true
    | Some f ->
        f.Names |> List.exists (fun f ->
            methodName = f || methodName = "get_" + f || methodName = "set_" + f || methodName = "op_" + f
            || methodName = "add_" + f || methodName = "remove_" + f)

/// Whether the type has a translated member matching the filter
let hasTranslatedMember (t: TypeDefinition) (info: M.Info) (filter: MemberFilter) =
    let f = Some filter
    match info.Classes.TryGetValue t with
    | true, (_, _, Some cls) ->
        (filter.Names |> List.contains ".ctor" && cls.Constructors.Count > 0)
        || cls.Methods.Keys |> Seq.exists (fun m -> memberMatches f m.Value.MethodName)
        || cls.Implementations.Keys |> Seq.exists (fun (_, m) -> memberMatches f m.Value.MethodName)
        || cls.Fields.Keys |> Seq.exists (fun n -> filter.Names |> List.contains n)
    | _ ->
    match info.Interfaces.TryGetValue t with
    | true, intf -> intf.Methods.Keys |> Seq.exists (fun m -> memberMatches f m.Value.MethodName)
    | _ -> false

let formatTranslation (t: TypeDefinition) (src: string) (info: M.Info) (memberFilter: MemberFilter option) =
    let sb = StringBuilder()
    let line (s: string) = sb.AppendLine(s) |> ignore
    let describe () =
        match info.Classes.TryGetValue t with
        | true, (address, custom, cls) ->
            line (sprintf "%s (%s), translated by WebSharper (metadata of %s)" t.Value.FullName t.Value.Assembly src)
            line (sprintf "  JavaScript address: %O" address)
            describeCustomType custom |> Option.iter (fun d -> line ("  " + d))
            match cls with
            | None -> line "  no class information: only the type is known, its members are translated where they are used (custom type)"
            | Some cls ->
                if cls.IsStub then line "  stub: declares an existing JavaScript class"
                cls.BaseClass |> Option.iter (fun b -> line (sprintf "  base class: %s" b.Entity.Value.FullName))
                if not cls.Macros.IsEmpty then
                    line (sprintf "  type macros: %s" (cls.Macros |> List.map (fun (m, _) -> m.Value.FullName) |> String.concat ", "))
                let ctors =
                    if memberFilter.IsNone || (memberFilter.Value.Names |> List.contains ".ctor") then
                        cls.Constructors |> Seq.map (fun kv -> sprintf "    %s: %s" (constructorText kv.Key) (describeCompiled kv.Value.CompiledForm)) |> List.ofSeq
                    else []
                let methods =
                    cls.Methods
                    |> Seq.filter (fun kv -> memberMatches memberFilter kv.Key.Value.MethodName)
                    |> Seq.map (fun kv -> sprintf "    %s: %s" (methodText kv.Key) (describeCompiled kv.Value.CompiledForm))
                    |> Seq.sort |> List.ofSeq
                let impls =
                    cls.Implementations
                    |> Seq.filter (fun kv -> memberMatches memberFilter (snd kv.Key).Value.MethodName)
                    |> Seq.map (fun kv -> sprintf "    %s.%s: %s" (fst kv.Key).Value.FullName (methodText (snd kv.Key)) (describeCompiled kv.Value.CompiledForm))
                    |> Seq.sort |> List.ofSeq
                let fields =
                    cls.Fields
                    |> Seq.filter (fun kv -> memberFilter.IsNone || (memberFilter.Value.Names |> List.contains kv.Key))
                    |> Seq.map (fun kv -> sprintf "    %s: %s" kv.Key (describeField kv.Value.CompiledForm))
                    |> List.ofSeq
                let section title (items: string list) =
                    if not items.IsEmpty then
                        line (sprintf "  %s (%d):" title items.Length)
                        for i in items do line i
                section "constructors" ctors
                section "methods" methods
                section "interface implementations" impls
                section "fields" fields
                if memberFilter.IsSome && ctors.IsEmpty && methods.IsEmpty && impls.IsEmpty && fields.IsEmpty then
                    line (sprintf "  member %s has no WebSharper translation: it can't be used in client-side ([<JavaScript>]) code" memberFilter.Value.Given)
        | _ ->
        match info.Interfaces.TryGetValue t with
        | true, intf ->
            line (sprintf "%s (%s), interface translated by WebSharper (metadata of %s)" t.Value.FullName t.Value.Assembly src)
            line (sprintf "  JavaScript address: %O" intf.Address)
            let methods =
                intf.Methods
                |> Seq.filter (fun kv -> memberMatches memberFilter kv.Key.Value.MethodName)
                |> Seq.map (fun kv -> let (name, _, _) = kv.Value in sprintf "    %s: \"%s\"" (methodText kv.Key) name)
                |> List.ofSeq
            if not methods.IsEmpty then
                line (sprintf "  methods (%d):" methods.Length)
                for m in methods do line m
        | _ -> ()
    describe ()
    sb.ToString().TrimEnd()

// ---------------------------------------------------------------------------------------------
// Projects

type ProjectState =
    {
        ProjectFile : string
        AssemblyName : string
        ProjectType : string
        Finished : DateTime
        Duration : TimeSpan
        ExitCode : int
        /// Lines printed by the compilation, true for the error channel (errors and warnings).
        Log : (bool * string)[]
        Snapshot : CompileSnapshot option
        Symbols : Lazy<SymbolIndex option>
        Translations : Lazy<TranslationIndex option>
        CurrentMetadata : Lazy<M.Info option>
    }

let createState projectFile (snapshot: CompileSnapshot option) exitCode duration log =
    let currentMeta =
        lazy (
            snapshot
            |> Option.bind (fun s -> s.Compilation)
            |> Option.bind (fun c -> tryOr (fun () -> Some (c.ToCurrentMetadata(true))) None)
        )
    let config = snapshot |> Option.map (fun s -> s.Config)
    {
        ProjectFile = projectFile
        AssemblyName =
            match config with
            | Some c when not (isNull c.AssemblyFile) -> Path.GetFileNameWithoutExtension c.AssemblyFile
            | _ -> Path.GetFileNameWithoutExtension projectFile
        ProjectType =
            match config with
            | Some c -> match c.ProjectType with Some t -> string t | None -> "Library"
            | None -> "?"
        Finished = DateTime.Now
        Duration = duration
        ExitCode = exitCode
        Log = log
        Snapshot = snapshot
        Symbols = lazy (snapshot |> Option.bind (fun s -> s.Check) |> Option.map (fun (_, check) -> buildIndex check))
        Translations =
            lazy (
                let sources =
                    [
                        match currentMeta.Value with
                        | Some m -> "this project", m
                        | None -> ()
                        match snapshot |> Option.bind (fun s -> s.ReferenceMetadata) with
                        | Some m -> "references", m
                        | None -> ()
                    ]
                if sources.IsEmpty then None else Some (buildTranslationIndex sources)
            )
        CurrentMetadata = currentMeta
    }

/// How many projects keep their last compilation in memory for agent tools.
let maxProjects =
    match Environment.GetEnvironmentVariable "WebSharperAgentProjects" with
    | null -> 10
    | v -> match Int32.TryParse v with | true, n when n >= 0 -> n | _ -> 10

let projects = ConcurrentDictionary<string, ProjectState>(StringComparer.OrdinalIgnoreCase)

let record (state: ProjectState) =
    if maxProjects > 0 then
        projects.[state.ProjectFile] <- state
        while projects.Count > maxProjects do
            let oldest = projects.Values |> Seq.minBy (fun p -> p.Finished)
            if projects.TryRemove(oldest.ProjectFile) |> fst then Stats.count "agent.projects_evicted"

/// How many compilations the agent tools keep now.
let projectCount () = projects.Count

/// What the booster provides to the tools besides the recorded compilations.
type BoosterContext =
    {
        /// Whether the cached arguments of a project are outdated (project file or references changed), None if not cached.
        IsOutdated : string -> bool option
        /// Compiles a project again with its cached arguments, like `dotnet ws build`, returns the exit code.
        Build : string -> int
    }

exception ToolError of string

/// Ends a tool with an error message for the agent.
let fail msg = raise (ToolError msg)

let findProject (name: string option) =
    let all = projects.Values |> Seq.sortByDescending (fun p -> p.Finished) |> List.ofSeq
    if all.IsEmpty then
        fail "The booster has not compiled any project since it started. Build a WebSharper project (dotnet build or dotnet ws build) first; projects built with WebSharperStandalone=true don't go through the booster."
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

/// The F# symbols of a project's last compilation, or a tool error if it did not get to the type check.
let requireSymbols (p: ProjectState) =
    match p.Symbols.Value with
    | Some s -> s
    | None -> fail (sprintf "The last compilation of %s did not get to the F# type check (exit code %d), see get_diagnostics." p.ProjectFile p.ExitCode)

// ---------------------------------------------------------------------------------------------
// Tools

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

let private errorLines (p: ProjectState) = p.Log |> Array.filter fst |> Array.map snd
let private isErrorLine (l: string) = l.Contains ": error " || l.Contains "error WS" || l.StartsWith "error "
let private isWarningLine (l: string) = l.Contains ": warning " || l.Contains "warning WS"

let private projectHeader (p: ProjectState) =
    sprintf "Project %s (assembly %s, %s), compiled %s in %.1f s, exit code %d"
        p.ProjectFile p.AssemblyName p.ProjectType (p.Finished.ToString("yyyy-MM-dd HH:mm:ss")) p.Duration.TotalSeconds p.ExitCode

let listProjects (ctx: BoosterContext) (_: ToolArgs) =
    let all = projects.Values |> Seq.sortByDescending (fun p -> p.Finished) |> List.ofSeq
    if all.IsEmpty then
        "The booster has not compiled any project since it started. Build a WebSharper project (dotnet build or dotnet ws build) first."
    else
    let sb = StringBuilder()
    for p in all do
        let errors = errorLines p |> Array.filter isErrorLine |> Array.length
        let warnings = errorLines p |> Array.filter isWarningLine |> Array.length
        sb.AppendLine(projectHeader p) |> ignore
        sb.AppendLine(sprintf "  %d error(s), %d warning(s) printed" errors warnings) |> ignore
        match p.Snapshot |> Option.bind (fun s -> s.Check) with
        | Some (options, _) ->
            sb.AppendLine(sprintf "  %d source files, %d references" options.SourceFiles.Length (options.OtherOptions |> Array.filter (fun o -> o.StartsWith "-r:") |> Array.length)) |> ignore
        | None -> ()
        match ctx.IsOutdated p.ProjectFile with
        | Some true -> sb.AppendLine("  the project file or a reference changed since: build_project falls back, run dotnet build") |> ignore
        | Some false -> sb.AppendLine("  can be rebuilt with build_project") |> ignore
        | None -> ()
    sb.ToString().TrimEnd()

let private diagnosticLocation = Text.RegularExpressions.Regex(@"^\s*(?<file>.+?)\((?<line>\d+),(?<col>\d+)(,\d+,\d+)?\):\s+(error|warning)\s+(?<code>\w+)")

/// The source line of a diagnostic and the signatures of the symbols used on it, from the type check: the types
/// an agent needs to understand "expected 'Todo' but here has type 'unit'" without reading other files.
let explainDiagnostic (p: ProjectState) (fileResults: Dictionary<string, FSharpCheckFileResults option>) (diagnostic: string) : string list =
    let m = diagnosticLocation.Match diagnostic
    match m.Success, p.Snapshot with
    | true, Some ({ Check = Some (options, _) } as snapshot) ->
        let line = int m.Groups.["line"].Value
        let col = int m.Groups.["col"].Value - 1
        let reported = m.Groups.["file"].Value.Replace('\\', '/')
        let file =
            options.SourceFiles
            |> Array.tryFind (fun f -> String.Equals(f.Replace('\\', '/'), reported, StringComparison.OrdinalIgnoreCase))
            |> Option.orElse (options.SourceFiles |> Array.tryFind (fun f -> String.Equals(Path.GetFileName f, Path.GetFileName reported, StringComparison.OrdinalIgnoreCase)))
        match file with
        | Some f when File.Exists f ->
            let lines = File.ReadAllLines f
            let source = if line >= 1 && line <= lines.Length then [ sprintf "      %d | %s" line (lines.[line - 1].TrimEnd()) ] else []
            let results =
                match fileResults.TryGetValue f with
                | true, r -> r
                | _ ->
                    let r = tryOr (fun () -> Some (snd (snapshot.Checker.GetBackgroundCheckResultsForFileInProject(f, options) |> Async.RunSynchronously))) None
                    fileResults.[f] <- r
                    r
            let noise (fullName: string) =
                fullName.StartsWith "Microsoft.FSharp.Core.Operators" || fullName.StartsWith "Microsoft.FSharp.Core.ExtraTopLevelOperators"
                || fullName.StartsWith "Microsoft.FSharp.Core.LanguagePrimitives"
            let symbols =
                match results with
                | Some r ->
                    tryOr (fun () -> r.GetAllUsesOfAllSymbolsInFile() |> Array.ofSeq) [||]
                    |> Array.filter (fun u -> u.Range.StartLine = line && not u.IsFromDefinition)
                    |> Array.sortBy (fun u -> abs (u.Range.StartColumn - col))
                    |> Array.choose (fun u ->
                        match u.Symbol with
                        | :? FSharpMemberOrFunctionOrValue as m when not (tryOr (fun () -> m.IsCompilerGenerated) false) ->
                            let full = tryOr (fun () -> m.FullName) m.DisplayName
                            if noise full || tryOr (fun () -> m.DeclaringEntity |> Option.exists (fun e -> e.DisplayName.EndsWith "Builder")) false then None
                            elif tryOr (fun () -> m.IsModuleValueOrMember) false then Some (sprintf "%s  (%s)" (memberSignature m) full)
                            else Some (sprintf "%s: %s  (local)" m.DisplayName (tryOr (fun () -> m.FullType.Format displayContext) "?"))
                        | :? FSharpUnionCase as c -> Some (sprintf "union case %s" c.FullName)
                        | :? FSharpField as fld -> Some (sprintf "field %s: %s" fld.Name (tryOr (fun () -> fld.FieldType.Format displayContext) "?"))
                        | _ -> None)
                    |> Array.distinct
                    |> Array.truncate 5
                    |> Array.map (fun s -> "      uses " + s)
                    |> List.ofArray
                | None -> []
            source @ symbols
        | _ -> []
    | _ -> []

let diagnostics (p: ProjectState) (file: string option) (includeWarnings: bool) (limit: int) =
    let sb = StringBuilder()
    sb.AppendLine(projectHeader p) |> ignore
    let lines =
        errorLines p
        |> Array.filter (fun l -> isErrorLine l || (includeWarnings && isWarningLine l) || not (isWarningLine l))
        |> Array.filter (fun l -> match file with Some f -> l.Contains(f, StringComparison.OrdinalIgnoreCase) | None -> true)
        |> Array.distinct
    let fromLog = lines.Length > 0
    // the F# type check, when the compilation stopped there without printing (unusual)
    let typeCheck =
        if fromLog then [||] else
        match p.Snapshot |> Option.bind (fun s -> s.Check) with
        | Some (_, check) ->
            check.Diagnostics
            |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error || (includeWarnings && d.Severity = FSharpDiagnosticSeverity.Warning))
            |> Array.filter (fun d -> match file with Some f -> d.FileName.Contains(f, StringComparison.OrdinalIgnoreCase) | None -> true)
            |> Array.map (fun d ->
                sprintf "%s(%d,%d): %s %s: %s" d.FileName d.StartLine (d.StartColumn + 1)
                    (if d.Severity = FSharpDiagnosticSeverity.Error then "error" else "warning") d.ErrorNumberText d.Message)
        | None -> [||]
    let all = Array.append lines typeCheck
    let hints =
        [
            "WS9002", "Remote method is returning a type", "add [<JavaScript>] to the type the RPC takes or returns"
            "WS9001", "not found in JavaScript compilation", "client code uses something without a JavaScript translation: check it with get_js_translation, move server-only code into an RPC, or mark your own code [<JavaScript>]"
            "WS9001", "Duplicate remote method path", "RPC paths are <module>/<function> without the namespace: rename one module or function"
            "FS0044", "deprecated", "use the replacement the message names"
            "FS3221", "is implicitly discarded", "a list with an explicit yield turns implicit yields off for all its elements: drop the yield (if c then attr.x), or yield every element"
            "FS0046", "The identifier 'checked' is reserved", "write attr.``checked`` (like attr.``type`` and attr.``class``)"
            "FS0001", "but here has type     'unit'", "if the line is a do! on an RPC or other Async<'T>, use let! x = ... (do! is for Async<unit>), or ... |> Async.Ignore"
        ]
        |> List.filter (fun (_, text, _) -> all |> Array.exists (fun l -> l.Contains text))
        |> List.map (fun (code, _, hint) -> sprintf "  hint (%s): %s" code hint)
        |> List.distinct
    let all = if hints.IsEmpty then all else Array.append all (Array.ofList hints)
    if all.Length = 0 then
        sb.AppendLine(if p.ExitCode = 0 then "No errors or warnings." else "No errors were printed, see the build output.") |> ignore
    else
        // the first errors (and discarded-value warnings, they hide lost code) with their line and the symbols on it
        let fileResults = Dictionary<string, FSharpCheckFileResults option>()
        let mutable explained = 0
        for l in Array.truncate limit all do
            sb.AppendLine(l) |> ignore
            if explained < 8 && (isErrorLine l || l.Contains "FS3221") then
                explained <- explained + 1
                for e in explainDiagnostic p fileResults l do sb.AppendLine(e) |> ignore
        if all.Length > limit then sb.AppendLine(sprintf "... %d more, raise limit to see them" (all.Length - limit)) |> ignore
    sb.ToString().TrimEnd()

let getDiagnostics (_: BoosterContext) (a: ToolArgs) =
    diagnostics a.Project (a.String "file") (a.Bool("include_warnings", true)) (a.Int("limit", 100))

/// A tool error for a name that matches nothing, with suggestions.
let notFound index (p: ProjectState) (name: string) =
    let suggestions = suggest index name search
    let s = if suggestions.IsEmpty then "" else "\nDid you mean:\n" + (suggestions |> List.map (sprintf "  %s") |> String.concat "\n")
    fail (sprintf "No type, module or member named %s in %s or its references.%s" name p.AssemblyName s)

/// The types and modules of a namespace (WebSharper.UI.Client), with the first sentence of their doc, and its sub-namespaces.
let namespaceListing (index: SymbolIndex) (name: string) =
    let n = normalizeName name
    let keys =
        match index.Namespaces.ContainsKey n with
        | true -> [ n ]
        | false -> index.Namespaces.Keys |> Seq.filter (fun k -> k.EndsWith("." + n, StringComparison.OrdinalIgnoreCase) || String.Equals(k, n, StringComparison.OrdinalIgnoreCase)) |> List.ofSeq
    match keys with
    | [] -> None
    | keys ->
        let sb = StringBuilder()
        for ns in keys |> List.truncate 3 do
            let entities =
                index.Namespaces.[ns]
                |> Seq.filter (fun e -> tryOr (fun () -> e.Accessibility.IsPublic) true && not (e.DisplayName.StartsWith "<"))
                |> Seq.distinctBy (fun e -> e.DisplayName, entityKind e)
                |> Seq.sortBy (fun e -> (if tryOr (fun () -> e.IsFSharpModule) false then 0 else 1), e.DisplayName)
                |> List.ofSeq
            let assemblies = entities |> List.map (fun e -> tryOr (fun () -> e.Assembly.SimpleName) "?") |> List.distinct
            sb.AppendLine(sprintf "namespace %s: %d types and modules (%s)" ns entities.Length (String.concat ", " assemblies)) |> ignore
            for e in entities |> List.truncate 200 do
                let summary =
                    match tryOr (fun () -> docSummary e.XmlDoc) None with
                    | Some d ->
                        let first = match d.IndexOf(". ") with -1 -> d | i -> d.Substring(0, i + 1)
                        " - " + (if first.Length > 110 then first.Substring(0, 110) + "..." else first)
                    | None -> ""
                sb.AppendLine(sprintf "  %s %s%s" (entityKind e) e.DisplayName summary) |> ignore
            let subs = index.Namespaces.Keys |> Seq.filter (fun k -> k.StartsWith(ns + ".") && not (k.Substring(ns.Length + 1).Contains ".")) |> Seq.sort |> List.ofSeq
            if not subs.IsEmpty then sb.AppendLine(sprintf "  namespaces: %s" (String.concat ", " subs)) |> ignore
            sb.AppendLine() |> ignore
        Some (sb.ToString().TrimEnd())

let getSignature (_: BoosterContext) (a: ToolArgs) =
    let p = a.Project
    let name = a.Required "name"
    let index = requireSymbols p
    match resolveLoose index name with
    | [], _ ->
        match namespaceListing index name with
        | Some listing -> listing
        | None -> notFound index p name
    | results, note ->
        let shown = results |> List.truncate 8
        let text = shown |> List.map formatResolved |> String.concat "\n\n"
        let text = match note with Some n -> n + "\n\n" + text | None -> text
        if results.Length > shown.Length then text + sprintf "\n\n... %d more matches" (results.Length - shown.Length) else text

let searchSymbols (_: BoosterContext) (a: ToolArgs) =
    let p = a.Project
    let query = a.Required "query"
    let index = requireSymbols p
    let results = search index query (a.Bool("include_members", true)) (a.Bool("project_only", false)) (a.Int("limit", 50))
    if results.IsEmpty then sprintf "Nothing matches \"%s\" in %s or its references." query p.AssemblyName
    else
        results
        |> List.map (fun (n, k, asm, own) -> sprintf "%s  (%s, %s%s)" n k asm (if own then ", this project" else ""))
        |> String.concat "\n"

let findUsages (_: BoosterContext) (a: ToolArgs) =
    let p = a.Project
    let name = a.Required "name"
    let index = requireSymbols p
    let check = p.Snapshot.Value.Check.Value |> snd
    let limit = a.Int("limit", 200)
    match resolveLoose index name with
    | [], _ -> notFound index p name
    | results, note ->
        let sb = StringBuilder()
        note |> Option.iter (fun n -> sb.AppendLine(n) |> ignore)
        for r in results |> List.truncate 5 do
            let sym = symbolOf r
            let uses =
                check.GetUsesOfSymbol(sym)
                |> Array.sortBy (fun u -> u.Range.FileName, u.Range.StartLine, u.Range.StartColumn)
            sb.AppendLine(sprintf "%s: %d use(s)" (tryOr (fun () -> sym.FullName) sym.DisplayName) uses.Length) |> ignore
            for u in Array.truncate limit uses do
                sb.AppendLine(sprintf "  %s(%d,%d)%s" u.Range.FileName u.Range.StartLine (u.Range.StartColumn + 1) (if u.IsFromDefinition then " definition" else "")) |> ignore
            if uses.Length > limit then sb.AppendLine(sprintf "  ... %d more" (uses.Length - limit)) |> ignore
        sb.ToString().TrimEnd()

let getFileSignature (_: BoosterContext) (a: ToolArgs) =
    let p = a.Project
    let file = a.Required "file"
    match p.Snapshot with
    | Some ({ Check = Some (options, _) } as s) ->
        let matches =
            options.SourceFiles |> Array.filter (fun f ->
                String.Equals(f, tryOr (fun () -> Path.GetFullPath file) file, StringComparison.OrdinalIgnoreCase)
                || f.EndsWith(Path.DirectorySeparatorChar.ToString() + file.TrimStart('/', '\\'), StringComparison.OrdinalIgnoreCase)
                || String.Equals(Path.GetFileName f, file, StringComparison.OrdinalIgnoreCase))
        match matches with
        | [||] -> fail (sprintf "%s is not a source file of %s. Source files: %s" file p.AssemblyName (options.SourceFiles |> Array.map Path.GetFileName |> String.concat ", "))
        | [| f |] ->
            let _, fileResults = s.Checker.GetBackgroundCheckResultsForFileInProject(f, options) |> Async.RunSynchronously
            match fileResults.GenerateSignature() with
            | Some t -> sprintf "// signature of %s\n%s" f (sourceText t)
            | None -> fail (sprintf "Could not generate the signature of %s (it may have type errors)" f)
        | many -> fail (sprintf "%s matches several source files: %s" file (String.concat ", " many))
    | _ -> fail (sprintf "The last compilation of %s did not get to the F# type check (exit code %d), see get_diagnostics." p.ProjectFile p.ExitCode)

let getJsTranslation (_: BoosterContext) (a: ToolArgs) =
    let p = a.Project
    let name = a.Required "name"
    let index =
        match p.Translations.Value with
        | Some t -> t
        | None -> fail (sprintf "The last compilation of %s did not get to the WebSharper translation (exit code %d), see get_diagnostics." p.ProjectFile p.ExitCode)
    let lookup (typeName: string) =
        let n = normalizeName typeName
        let get (d: Dictionary<string, ResizeArray<_>>) = match d.TryGetValue n with | true, l -> List.ofSeq l | _ -> []
        match get index.Classes @ get index.Interfaces with
        | [] ->
            // F# names: modules (List for ListModule), abbreviations (list for FSharpList`1), partial names
            match p.Symbols.Value with
            | Some symbols ->
                fst (resolveLoose symbols typeName)
                |> List.choose (function REntity e -> Some e | _ -> None)
                |> List.collect (fun e ->
                    let names = [ tryOr (fun () -> e.TryGetFullCompiledName()) None; tryOr (fun () -> e.TryFullName) None ] |> List.choose id |> List.map normalizeName
                    let names =
                        if tryOr (fun () -> e.IsFSharpAbbreviation) false then
                            match tryOr (fun () -> Some e.AbbreviatedType.TypeDefinition) None with
                            | Some t -> names @ ([ tryOr (fun () -> t.TryFullName) None ] |> List.choose id |> List.map normalizeName)
                            | None -> names
                        else names
                    names |> List.distinct |> List.collect (fun n ->
                        (match index.Classes.TryGetValue n with | true, l -> List.ofSeq l | _ -> [])
                        @ (match index.Interfaces.TryGetValue n with | true, l -> List.ofSeq l | _ -> [])))
                |> List.distinctBy (fun (t, _, _) -> t)
            | None -> []
        | l -> l
    let typeName, memberFilter, found =
        match lookup name with
        | _ :: _ as l -> name, None, l
        | [] ->
            match splitLast (if name.EndsWith ">" && name.Contains "<" then name.Substring(0, name.LastIndexOf '<') else name) with
            | Some (owner, m) ->
                match lookup owner with
                | _ :: _ as l ->
                    let m = if m = "ctor" || m = "#ctor" || m = "new" then ".ctor" else m
                    // the compiled names of the F# members: List.map is ListModule.Map
                    let compiledNames =
                        match p.Symbols.Value with
                        | Some symbols ->
                            fst (resolveLoose symbols name) |> List.choose (function
                                | RMember (_, mfv) -> tryOr (fun () -> Some mfv.CompiledName) None
                                | _ -> None)
                        | None -> []
                    let filter = { Given = m; Names = m :: compiledNames |> List.distinct }
                    // only the types having the member, or the first one to say it is missing
                    let having = l |> List.filter (fun (t, _, info) -> hasTranslatedMember t info filter)
                    owner, Some filter, (if having.IsEmpty then [ List.head l ] else having)
                | [] -> name, None, []
            | None -> name, None, []
    match found with
    | [] ->
        let last = match splitLast (normalizeName name) with Some (_, l) -> l | None -> normalizeName name
        let similar =
            Seq.append index.Classes.Keys index.Interfaces.Keys
            |> Seq.filter (fun k -> k.EndsWith("." + last, StringComparison.OrdinalIgnoreCase) || k.Contains(last, StringComparison.OrdinalIgnoreCase))
            |> Seq.distinct |> Seq.truncate 10 |> List.ofSeq
        sprintf "%s has no WebSharper translation in %s or its references: it can't be used in client-side ([<JavaScript>]) code.%s"
            name p.AssemblyName
            (if similar.IsEmpty then " (Check the name with search_symbols.)" else "\nTranslated types with similar names:\n" + (similar |> List.map (sprintf "  %s") |> String.concat "\n"))
    | found ->
        found
        |> List.map (fun (t, src, info) -> formatTranslation t src info memberFilter)
        |> List.filter (fun s -> s <> "")
        |> String.concat "\n\n"

let listRpcMethods (_: BoosterContext) (a: ToolArgs) =
    let p = a.Project
    match p.CurrentMetadata.Value with
    | None -> fail (sprintf "The last compilation of %s did not get to the WebSharper translation (exit code %d), see get_diagnostics." p.ProjectFile p.ExitCode)
    | Some info ->
        let rpcs =
            [
                for kv in info.Classes do
                    match kv.Value with
                    | _, _, Some cls ->
                        for m in cls.Methods do
                            match m.Value.CompiledForm with
                            | M.Remote (_, path, _) -> yield sprintf "%s.%s  ->  %s" kv.Key.Value.FullName (methodText m.Key) path
                            | _ -> ()
                    | _ -> ()
            ]
            |> List.sort
        let sitelet =
            match info.SiteletDefinition with
            | Some t -> sprintf "Website (sitelet) definition: %s\n" t.Value.FullName
            | None -> ""
        if rpcs.IsEmpty then sitelet + sprintf "%s has no RPC methods ([<Rpc>] or [<Remote>])." p.AssemblyName
        else sitelet + sprintf "RPC methods of %s (%d):\n" p.AssemblyName rpcs.Length + String.concat "\n" rpcs

let buildProject (ctx: BoosterContext) (a: ToolArgs) =
    let p = a.Project
    match ctx.IsOutdated p.ProjectFile with
    | None -> fail (sprintf "%s has no cached compiler arguments, run dotnet build on it" p.ProjectFile)
    | Some true -> fail (sprintf "The project file or a reference of %s changed since its last build: run dotnet build (or dotnet ws build) instead" p.ProjectFile)
    | Some false ->
    let exitCode = ctx.Build p.ProjectFile
    let p = findProject (Some p.ProjectFile)
    sprintf "Exit code %d\n%s" exitCode (diagnostics p None true 100)

// ---------------------------------------------------------------------------------------------
// Tool descriptions (MCP tools/list)

type Tool =
    {
        Name : string
        Description : string
        /// name, type, description, required
        Parameters : (string * string * string * bool) list
        ReadOnly : bool
        Run : BoosterContext -> ToolArgs -> string
    }

/// The optional project parameter of the tools.
let projectParam = "project", "string", "Project file path, file name or assembly name. Defaults to the most recently compiled project.", false

/// The tools of the free edition: what keeps agents from failing on WebSharper code.
let freeTools : Tool list =
    [
        {
            Name = "list_projects"
            Description = "List the WebSharper projects the booster compiled since it started, with their type, last compilation time, exit code and error/warning counts. Start here to find project names for the other tools."
            Parameters = []
            ReadOnly = true
            Run = listProjects
        }
        {
            Name = "get_diagnostics"
            Description = "Errors and warnings of the last compilation of a project (F# and WebSharper), as printed by the build: file(line,col): error CODE: message."
            Parameters = [ projectParam; "file", "string", "Only diagnostics of source files whose path contains this text.", false; "include_warnings", "boolean", "Include warnings (default true).", false; "limit", "integer", "Maximum number of lines (default 100).", false ]
            ReadOnly = true
            Run = getDiagnostics
        }
        {
            Name = "get_signature"
            Description = "F# signature of a type, module, member, record field or union case, by full or partial name (System.Collections.Generic.List, List.map, WebSharper.UI.Doc.Element), from the project or any of its references, with its XML doc summary, declaration location and WebSharper attributes."
            Parameters = [ projectParam; "name", "string", "Name of the type, module or member, e.g. Namespace.Type, Module.function, Type.Member.", true ]
            ReadOnly = true
            Run = getSignature
        }
        {
            Name = "search_symbols"
            Description = "Find types, modules and members by part of their name in the project and its references (members only in the project and WebSharper libraries). Exact matches first, project symbols first."
            Parameters = [ projectParam; "query", "string", "Part of the name to look for, case-insensitive.", true; "include_members", "boolean", "Also search members and functions (default true).", false; "project_only", "boolean", "Only symbols declared in the project (default false).", false; "limit", "integer", "Maximum number of results (default 50).", false ]
            ReadOnly = true
            Run = searchSymbols
        }
        {
            Name = "get_js_translation"
            Description = "How WebSharper translates a type or member to JavaScript: its JavaScript address and the compiled form of each member (instance/static method, inline, macro, RPC), from the metadata of the project and its references. Tells whether the type or member can be used in client-side ([<JavaScript>]) code at all."
            Parameters = [ projectParam; "name", "string", "Type (System.Collections.Generic.Dictionary, List) or Type.Member (System.String.Split).", true ]
            ReadOnly = true
            Run = getJsTranslation
        }
        {
            Name = "find_usages"
            Description = "Where a type, module or member is used in the project's source files, with the location of its definition."
            Parameters = [ projectParam; "name", "string", "Name of the type, module or member.", true; "limit", "integer", "Maximum number of locations per symbol (default 200).", false ]
            ReadOnly = true
            Run = findUsages
        }
        {
            Name = "get_file_signature"
            Description = "The F# signature (.fsi) of a source file of the project: every type, module and value it declares, with types, without the implementation."
            Parameters = [ projectParam; "file", "string", "Source file path, relative path or file name.", true ]
            ReadOnly = true
            Run = getFileSignature
        }
        {
            Name = "list_rpc_methods"
            Description = "The RPC methods ([<Rpc>]) the project exposes to client-side code, with their remoting paths, and its Website (sitelet) definition."
            Parameters = [ projectParam ]
            ReadOnly = true
            Run = listRpcMethods
        }
        {
            Name = "build_project"
            Description = "Compile a project again with the arguments of its last build, like the fast path of `dotnet ws build`: runs the F# compiler and the WebSharper translation, writes their outputs, and returns the exit code and diagnostics. Does not run MSBuild (no restore, no copying to bin), so after changing the project file or references use dotnet build."
            Parameters = [ projectParam ]
            ReadOnly = false
            Run = buildProject
        }
    ]

let toolsJsonOf (tools: Tool list) =
    let arr = JsonArray()
    for t in tools do
        let props = JsonObject()
        for (name, typ, desc, _) in t.Parameters do
            props.[name] <- JsonObject(dict [ "type", JsonValue.Create(typ) :> JsonNode; "description", JsonValue.Create(desc) :> JsonNode ])
        let required = JsonArray(t.Parameters |> List.filter (fun (_, _, _, r) -> r) |> List.map (fun (n, _, _, _) -> JsonValue.Create(n) :> JsonNode) |> Array.ofList)
        let schema = JsonObject(dict [ "type", JsonValue.Create("object") :> JsonNode; "properties", props :> JsonNode; "required", required :> JsonNode ])
        let annotations = JsonObject(dict [ "readOnlyHint", JsonValue.Create(t.ReadOnly) :> JsonNode ])
        arr.Add(JsonObject(dict [ "name", JsonValue.Create(t.Name) :> JsonNode; "description", JsonValue.Create(t.Description) :> JsonNode; "inputSchema", schema :> JsonNode; "annotations", annotations :> JsonNode ]))
    arr

// ---------------------------------------------------------------------------------------------
// Extensions: more tools from other packages, loaded by the booster from the assemblies given with the
// --agentextension compile argument, and by `wsfscservice --mcp` from --extension or WebSharperAgentExtensions.

/// An extension of the agent tools.
type IAgentExtension =
    abstract Name : string
    /// Tools added to the free ones; a tool with the name of a free one replaces it.
    abstract Tools : Tool list
    /// Added to the server instructions.
    abstract Instructions : string
    /// Adds pointers to the extension's tools to the answer of a tool: tool name, arguments, answer.
    abstract Decorate : toolName: string -> args: ToolArgs -> answer: string -> string
    /// Tools that need no booster (static content): `wsfscservice --mcp` answers them itself.
    abstract LocalTools : string list

/// What the booster (or `wsfscservice --mcp`) gives an extension that implements IAgentExtensionWithHost.
/// Members are only added here, so extensions built against an earlier version keep working.
type IAgentHost =
    /// The extension's statistics, under "ext.<extension name>.", kept with the booster's (Stats.fs).
    abstract Stats : Stats.IAgentStats
    /// Writes to the log of the process (the booster's log, or stderr of `wsfscservice --mcp`).
    abstract Log : string -> unit
    /// The WebSharper version of the booster.
    abstract WebSharperVersion : string
    /// True in the booster, false in `wsfscservice --mcp`.
    abstract IsBooster : bool

/// An extension that gets an IAgentHost when it is loaded, before its tools are used.
type IAgentExtensionWithHost =
    inherit IAgentExtension
    abstract Initialize : host: IAgentHost -> unit

/// Set by the process: its log, its WebSharper version, and whether it is the booster.
let mutable hostLog : string -> unit = ignore
let mutable hostVersion = ""
let mutable hostIsBooster = true

let private hostFor (e: IAgentExtension) =
    let stats = Stats.forExtension e.Name
    { new IAgentHost with
        member _.Stats = stats
        member _.Log msg = hostLog (sprintf "[%s] %s" e.Name msg)
        member _.WebSharperVersion = hostVersion
        member _.IsBooster = hostIsBooster }

let private extensions = ConcurrentDictionary<string, IAgentExtension list>(StringComparer.OrdinalIgnoreCase)
let private extensionFolders = ConcurrentDictionary<string, bool>(StringComparer.OrdinalIgnoreCase)

/// The dependencies of an extension are next to it.
let private resolveFromExtensionFolders =
    lazy
        System.Runtime.Loader.AssemblyLoadContext.Default.add_Resolving(fun ctx name ->
            extensionFolders.Keys
            |> Seq.map (fun d -> Path.Combine(d, name.Name + ".dll"))
            |> Seq.tryFind File.Exists
            |> Option.map ctx.LoadFromAssemblyPath
            |> Option.toObj)

/// Loads the IAgentExtension implementations of an assembly into the default context (they share the compiler's
/// types), once per path.
let loadExtensions (path: string) : Result<IAgentExtension list, string> =
    try
        let full = Path.GetFullPath path
        if not (File.Exists full) then Error (sprintf "The agent extension %s does not exist" full) else
        resolveFromExtensionFolders.Force()
        extensionFolders.[Path.GetDirectoryName full] <- true
        Ok (extensions.GetOrAdd(full, fun p ->
            let asm = System.Runtime.Loader.AssemblyLoadContext.Default.LoadFromAssemblyPath p
            let types =
                try asm.GetTypes()
                with :? System.Reflection.ReflectionTypeLoadException as e -> e.Types |> Array.filter (isNull >> not)
            types
            |> Array.filter (fun t -> typeof<IAgentExtension>.IsAssignableFrom t && not t.IsAbstract && not t.IsInterface)
            |> Array.map (fun t ->
                let e = Activator.CreateInstance t :?> IAgentExtension
                match e with
                | :? IAgentExtensionWithHost as h -> h.Initialize (hostFor e)
                | _ -> ()
                Stats.count ("extensions.loaded." + Stats.part e.Name)
                e)
            |> List.ofArray))
    with e ->
        Stats.count "extensions.failed"
        Error (sprintf "Could not load the agent extension %s: %s" path e.Message)

/// The paths of the assemblies loadExtensions loaded.
let loadedExtensionPaths () = extensions.Keys |> List.ofSeq

let loadedExtensions () =
    extensions.Values |> Seq.concat |> Seq.distinctBy (fun e -> e.Name) |> List.ofSeq

/// The free tools and those of the loaded extensions.
let allTools () =
    let fromExtensions = loadedExtensions () |> List.collect (fun e -> e.Tools)
    (freeTools |> List.filter (fun t -> not (fromExtensions |> List.exists (fun x -> x.Name = t.Name)))) @ fromExtensions

let toolsJson () = toolsJsonOf (allTools ())

/// Where a tool comes from: "core" or the name of the extension that has it.
let toolSource (name: string) =
    match loadedExtensions () |> List.tryFind (fun e -> e.Tools |> List.exists (fun t -> t.Name = name)) with
    | Some e -> e.Name
    | None -> "core"

let baseInstructions =
    "Answers questions about the WebSharper projects compiled by the WebSharper booster (wsfscservice) "
    + "from what it keeps in memory after each build: diagnostics, F# signatures, symbol search and usages, "
    + "and how WebSharper translates types and members to JavaScript. "
    + "Use list_projects first; the other tools default to the most recently compiled project. "
    + "Information is as of the last build of a project; use build_project after editing source files, and get_diagnostics for its errors. "
    + "These tools answer from the exact WebSharper versions the project uses: prefer them over web searches and over reading package files. "
    + "For online documentation use docs.websharper.com; never use developers.websharper.com/docs/v4.x or other 4.x pages, they are superseded and their APIs differ. "
    + "For APIs use get_signature (partial names work: Doc.BindView; a namespace lists its contents), search_symbols, find_usages, "
    + "and get_js_translation before using a .NET API in client code."

/// What `wsfscservice --mcp` offers: the free tools, with the loaded extensions' tools, instructions and pointers.
type Edition =
    {
        Name : string
        Tools : Tool list
        Instructions : string
        Decorate : string -> ToolArgs -> string -> string
        LocalTools : string list
    }

let edition () =
    match loadedExtensions () with
    | [] -> { Name = "free"; Tools = freeTools; Instructions = baseInstructions; Decorate = (fun _ _ text -> text); LocalTools = [] }
    | exts ->
        {
            Name = exts |> List.map (fun e -> e.Name) |> String.concat ", "
            Tools = allTools ()
            Instructions = baseInstructions + " " + (exts |> List.map (fun e -> e.Instructions) |> String.concat " ")
            Decorate = fun name args text -> exts |> List.fold (fun t e -> e.Decorate name args t) text
            LocalTools = exts |> List.collect (fun e -> e.LocalTools)
        }

let maxResultChars = 60000

/// Runs a tool, Ok with its text result or Error with a message for the agent.
let callTool (ctx: BoosterContext) (name: string) (arguments: string) : Result<string, string> =
    match allTools () |> List.tryFind (fun t -> t.Name = name) with
    | None ->
        if List.isEmpty (loadedExtensions ()) then
            Error (sprintf "Unknown tool: %s. If it comes from an agent extension, build the project (dotnet build) so that the booster loads it." name)
        else Error (sprintf "Unknown tool: %s" name)
    | Some tool ->
        // statistics: tool.<source>.<name>: its duration, how it ended, the size of its answer
        let key = sprintf "tool.%s.%s" (Stats.part (toolSource name)) (Stats.part name)
        let sw = Diagnostics.Stopwatch.StartNew()
        let result, outcome =
            try
                let args = if String.IsNullOrWhiteSpace arguments then null else JsonNode.Parse arguments
                let text = tool.Run ctx (ToolArgs args)
                let text = if text.Length > maxResultChars then text.Substring(0, maxResultChars) + "\n... (truncated)" else text
                Ok text, "ok"
            with
            | ToolError msg -> Error msg, "not_answered"
            | e -> Error (sprintf "%s failed: %s" name e.Message), "failed"
        Stats.ms (key + "_ms") sw.Elapsed
        Stats.count (key + "." + outcome)
        Stats.value (key + ".chars") (float (match result with Ok t | Error t -> t.Length))
        result

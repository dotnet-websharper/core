// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module IntelliFactory.WebSharper.Compiler.FrontEnd

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Web.UI
open Mono.Cecil
module M = IntelliFactory.WebSharper.Core.Metadata
module P = IntelliFactory.JavaScript.Packager
module R = IntelliFactory.WebSharper.Compiler.ReflectionLayer
module Re = IntelliFactory.WebSharper.Core.Reflection
module Res = IntelliFactory.WebSharper.Core.Resources
module W = IntelliFactory.JavaScript.Writer
type Path = string
type Pref = IntelliFactory.JavaScript.Preferences

[<Literal>]
let EMBEDDED_METADATA = "WebSharper.dep"

[<Literal>]
let EMBEDDED_JS = "WebSharper.js"

[<Literal>]
let EMBEDDED_MINJS = "WebSharper.min.js"

let readResource name (def: AssemblyDefinition) =
    def.MainModule.Resources
    |> Seq.tryPick (function
        | :? EmbeddedResource as r when r.Name = name ->
            use r = new StreamReader(r.GetResourceStream())
            Some (r.ReadToEnd())
        | _ -> None)

type Symbols =
    | Mdb of byte []
    | Pdb of byte []

type Assembly =
    {
        Debug : option<Symbols>
        Definition : AssemblyDefinition
    }

    member this.OutputParameters(keyPair) =
        let par = WriterParameters()
        match keyPair with
        | Some kp -> par.StrongNameKeyPair <- kp
        | None -> ()
        par

    member this.RawBytes(kP: option<StrongNameKeyPair>) =
        use s = new System.IO.MemoryStream(16 * 1024)
        this.Definition.Write(s, this.OutputParameters kP)
        s.ToArray()

    member this.Symbols = this.Debug

    member this.Write(kP: option<StrongNameKeyPair>)(path: Path) =
        let par = this.OutputParameters kP
        match this.Debug with
        | Some (Mdb _) ->
            par.WriteSymbols <- true
            par.SymbolWriterProvider <- Mdb.MdbWriterProvider()
        | Some (Pdb _) ->
            par.WriteSymbols <- true
            par.SymbolWriterProvider <- Pdb.PdbWriterProvider()
        | None -> ()
        this.Definition.Write(path, par)

    member this.ReadableJavaScript =
        readResource EMBEDDED_JS this.Definition

    member this.CompressedJavaScript =
        readResource EMBEDDED_MINJS this.Definition

[<Sealed>]
type Resolver(paths: Set<Path>) =
    inherit DefaultAssemblyResolver()
    do for p in paths do base.AddSearchDirectory p

[<Sealed>]
type Loader(paths: Set<Path>, log: string -> unit) =

    let load (bytes: byte[]) (symbols: option<Symbols>) (paths: Set<Path>) =
        use str = new MemoryStream(bytes)
        let par = ReaderParameters()
        par.AssemblyResolver <- Resolver paths
        par.ReadingMode <- ReadingMode.Deferred
        match symbols with
        | Some (Pdb bytes) ->
            par.ReadSymbols <- true
            par.SymbolReaderProvider <- new Pdb.PdbReaderProvider()
            par.SymbolStream <- new MemoryStream(bytes)
        | Some (Mdb bytes) ->
            par.ReadSymbols <- true
            par.SymbolReaderProvider <- new Mdb.MdbReaderProvider()
            par.SymbolStream <- new MemoryStream(bytes)
        | None -> ()
        let def = AssemblyDefinition.ReadAssembly(str, par)
        {
            Debug = symbols
            Definition = def
        }

    static member Create(paths)(log) =
        Loader(paths, log)

    member this.LoadRaw(bytes)(symbols) =
        load bytes symbols paths

    member this.LoadFile(path: Path) =
        let bytes = File.ReadAllBytes path
        let p ext = Path.ChangeExtension(path, ext)
        let ex x = File.Exists(p x)
        let rd x = File.ReadAllBytes(p x)
        let symbolsPath =
            if ex ".pdb" then Some (p ".pdb")
            elif ex ".mdb" then Some (p ".mdb")
            else None
        let symbols =
            if ex ".pdb" then Some (Pdb (rd ".pdb"))
            elif ex ".mdb" then Some (Mdb (rd ".mdb"))
            else None
        try
            load bytes symbols (Set.add path paths)
        with :? InvalidOperationException ->
            if symbolsPath.IsSome then
                "Failed to load symbols: " + symbolsPath.Value
                |> log
            load bytes None (Set.add path paths)

module CecilTools =

    let writeCompiledMetadata (a: AssemblyDefinition) (rm: M.AssemblyInfo) (meta: Metadata.T) (pkg: P.Module) =
        let pub = ManifestResourceAttributes.Public
        let dep =
            use s = new MemoryStream(8 * 1024)
            Metadata.Serialize s meta
            s.ToArray()
        let prog = P.Package pkg
        let js pref =
            use s = new MemoryStream(8 * 1024)
            let () =
                use w = new StreamWriter(s)
                W.WriteProgram pref w (prog pref)
            s.ToArray()
        let rmdata =
            use s = new MemoryStream(8 * 1024)
            rm.ToStream s
            s.ToArray()
        let rmname = M.AssemblyInfo.EmbeddedResourceName
        EmbeddedResource(rmname, pub, rmdata)
        |> a.MainModule.Resources.Add
        EmbeddedResource(EMBEDDED_METADATA, pub, dep)
        |> a.MainModule.Resources.Add
        if not pkg.IsEmpty then
            EmbeddedResource(EMBEDDED_MINJS, pub, js Pref.Compact)
            |> a.MainModule.Resources.Add
            EmbeddedResource(EMBEDDED_JS, pub, js Pref.Readable)
            |> a.MainModule.Resources.Add

    let readRuntimeMetadata (a: AssemblyDefinition) =
        let key = M.AssemblyInfo.EmbeddedResourceName
        a.MainModule.Resources
        |> Seq.tryPick (function
            | :? EmbeddedResource as r when r.Name = key ->
                use s = r.GetResourceStream()
                try
                    Some (M.AssemblyInfo.FromStream s)
                with e ->
                    failwithf "Failed to read assembly metadata for: %s" a.FullName
            | _ -> None)

    let readCompiledMetadata (a: AssemblyDefinition) =
        let key = EMBEDDED_METADATA
        a.MainModule.Resources
        |> Seq.tryPick (function
            | :? EmbeddedResource as r when r.Name = key ->
                try
                    use s = r.GetResourceStream()
                    Some (Metadata.Deserialize s)
                with _ ->
                    failwithf "Failed to deserialize metadata for: %s" a.FullName
             | _ ->
                None)

type Options =
    {
        ErrorLimit : int
        KeyPair : option<StrongNameKeyPair>
        References : list<Assembly>
    }

    static member Default =
        {
            ErrorLimit = 20
            KeyPair = None
            References = []
        }

type Context =
    {
        Code : IDictionary<Re.AssemblyName, Assembly>
        Infos : list<M.AssemblyInfo>
        Metas : list<Metadata.T>
    }

    member this.LookupAssemblyCode(debug: bool, name: Re.AssemblyName) =
        match this.Code.TryGetValue(name) with
        | true, a -> if debug then a.ReadableJavaScript else a.CompressedJavaScript
        | _ -> None

type ResourceContent =
    {
        Content : string
        ContentType : string
        Name : string
    }

type ResourceContext =
    {
        CreateUri : ResourceContent -> string
        DebuggingEnabled : bool
        GetSetting : string -> option<string>
    }

[<Sealed>]
type CompiledAssembly
    (
        context: Context,
        source: R.AssemblyDefinition,
        meta: Metadata.T,
        aInfo: M.AssemblyInfo,
        mInfo: M.Info,
        pkg: P.Module
    ) =

    let getJS (pref: Pref) =
        use w = new StringWriter()
        W.WriteProgram pref w (P.Package pkg pref)
        w.ToString()

    let compressedJS = lazy getJS Pref.Compact
    let readableJS = lazy getJS Pref.Readable

    let nameOfSelf = Re.AssemblyName.Convert(source.Name)

    let deps =
        lazy
        let self = M.Node.AssemblyNode(nameOfSelf, M.AssemblyMode.CompiledAssembly)
        mInfo.GetDependencies([self])

    member this.AssemblyInfo = aInfo
    member this.CompressedJavaScript = compressedJS.Value
    member this.Info = mInfo
    member this.Metadata = meta
    member this.Package = pkg
    member this.ReadableJavaScript = readableJS.Value

    member this.Dependencies = deps.Value

    member this.RenderDependencies(ctx: ResourceContext, writer: HtmlTextWriter) =
        let cache = Dictionary()
        let createUri (content: ResourceContent) =
            match cache.TryGetValue(content) with
            | true, y -> y
            | _ ->
                let y = ctx.CreateUri(content)
                cache.Add(content, y)
                y
        let readWebResource (ty: System.Type) (name: string) =
            try
                let content =
                    let content =
                        ty.Assembly.GetManifestResourceNames()
                        |> Seq.tryFind (fun x -> x.Contains(name))
                        |> Option.bind (fun name ->
                            use s = ty.Assembly.GetManifestResourceStream(name)
                            use r = new StreamReader(s)
                            Some (r.ReadToEnd()))
                    defaultArg content ""
                let contentType =
                    let cT =
                        System.Reflection.CustomAttributeData.GetCustomAttributes(ty.Assembly)
                        |> Seq.tryPick (fun attr ->
                            if attr.Constructor.DeclaringType = typeof<System.Web.UI.WebResourceAttribute> then
                                match [for a in attr.ConstructorArguments -> a.Value] with
                                | [(:? string as n); (:? string as contentType)] ->
                                    if n.Contains(name)
                                        then Some contentType
                                        else None
                                | _ -> None
                            else None)
                    defaultArg cT "text/plain"
                (content, contentType)
            with e ->
                ("", "text/plain")
        let makeJsUri name js =
            createUri {
                Content = js
                ContentType = "text/javascript"
                Name =
                    let ext = if ctx.DebuggingEnabled then ".dll.js" else ".dll.min.js"
                    name + ext
            }
        let ctx : Res.Context =
            {
                DebuggingEnabled = ctx.DebuggingEnabled
                GetAssemblyUrl = fun name ->
                    if name = nameOfSelf then
                        (if ctx.DebuggingEnabled then Pref.Readable else Pref.Compact)
                        |> getJS
                        |> makeJsUri name.Name
                    else
                        match context.LookupAssemblyCode(ctx.DebuggingEnabled, name) with
                        | Some x -> makeJsUri name.Name x
                        | None -> ""
                GetSetting = ctx.GetSetting
                GetWebResourceUrl = fun ty name ->
                    let (c, cT) = readWebResource ty name
                    createUri {
                        Content = c
                        ContentType = cT
                        Name = name
                    }
            }
        this.RenderDependencies(ctx, writer)

    member this.RenderDependencies(ctx, writer: HtmlTextWriter) =
        this.Dependencies
        |> Seq.iter (fun r -> r.Render ctx writer)
        writer.WriteLine()
        writer.WriteLine("<script type='text/javascript'>")
        writer.WriteLine @"if (typeof IntelliFactory !=='undefined')"
        writer.WriteLine @"  IntelliFactory.Runtime.Start();"
        writer.WriteLine("</script>")

[<Sealed>]
type Compiler(errorLimit: int, log: Message -> unit, ctx: Context) =

    member this.Compile(quotation: Quotations.Expr, context: System.Reflection.Assembly, ?name) : option<CompiledAssembly> =
        this.CompileAssembly(R.Dynamic.FromQuotation quotation context (defaultArg name "Example"))

    member this.Compile(quotation: Quotations.Expr, ?name) : option<CompiledAssembly> =
        this.Compile(quotation, System.Reflection.Assembly.GetCallingAssembly(), ?name = name)

    member this.CompileAssembly(assembly: R.AssemblyDefinition) : option<CompiledAssembly> =
        let succ = ref true
        let err (m: Message) =
            match m.Priority with
            | Priority.Warning -> ()
            | _ -> succ := false
            log m
        let logger = Logger.Create err errorLimit
        let meta = Metadata.Union logger ctx.Metas
        let pool = Inlining.Pool.Create logger
        let macros = Reflector.Pool.Create logger
        try
            let ra = Reflector.Reflect logger assembly
            let pkg = Resolver.Resolve logger ra
            let va = Validator.Validate logger pool macros ra
            let rm = Analyzer.Analyze ctx.Infos va
            let local = Metadata.Parse logger va
            let joined = Metadata.Union logger [meta; local]
            Assembler.Assemble logger pool macros joined va
            if !succ then
                let mInfo = M.Info.Create (rm :: ctx.Infos)
                Some (CompiledAssembly(ctx, assembly, local, rm, mInfo, pkg.Value))
            else None
        with ErrorLimitExceeded -> None

    member this.CompileAndModify(assembly: Assembly) : bool =
        match this.CompileAssembly(R.Cecil.AdaptAssembly assembly.Definition) with
        | None -> false
        | Some a ->
            CecilTools.writeCompiledMetadata assembly.Definition
                a.AssemblyInfo a.Metadata a.Package
            true

let Prepare (options: Options) (log: Message -> unit) : Compiler =
    let refs =
        options.References
        |> Seq.distinctBy (fun a -> a.Definition.Name.Name)
        |> Seq.toList
    let cM = List.choose (fun a -> CecilTools.readCompiledMetadata a.Definition) refs
    let rM = List.choose (fun a -> CecilTools.readRuntimeMetadata a.Definition) refs
    let code =
        dict [|
            for a in options.References do
                yield (Re.AssemblyName.Parse(a.Definition.FullName), a)
        |]
    let ctx = { Metas = cM; Infos = rM; Code = code }
    Compiler(options.ErrorLimit, log, ctx)

let Compile (options: Options) (log: Message -> unit) : Assembly -> bool =
    let c = Prepare options log
    fun aF -> c.CompileAndModify(aF)

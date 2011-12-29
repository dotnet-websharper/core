// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

type Dictionary<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>
type Queue<'T> = System.Collections.Generic.Queue<'T>

module M = IntelliFactory.WebSharper.Core.Metadata
module P = IntelliFactory.JavaScript.Packager
module W = IntelliFactory.JavaScript.Writer
type Pref = IntelliFactory.JavaScript.Preferences
type Key = System.Reflection.StrongNameKeyPair
type Path = string

[<Literal>]
let EMBEDDED_METADATA = "WebSharper.dep"

[<Literal>]
let EMBEDDED_JS = "WebSharper.js"

[<Literal>]
let EMBEDDED_MINJS = "WebSharper.min.js"

type Symbols =
    | Mdb of byte []
    | Pdb of byte []

let readResource name (def: Mono.Cecil.AssemblyDefinition) =
    def.MainModule.Resources
    |> Seq.tryPick (function
        | :? Mono.Cecil.EmbeddedResource as r when r.Name = name ->
            use r = new System.IO.StreamReader(r.GetResourceStream())
            Some (r.ReadToEnd())
        | _ -> None)

type Assembly =
    {
        Debug : option<Symbols>
        Definition : Mono.Cecil.AssemblyDefinition
    }

    member this.OutputParameters keyPair =
        let par = Mono.Cecil.WriterParameters()
        match keyPair with
        | Some kp -> par.StrongNameKeyPair <- kp
        | None -> ()
        par

    member this.RawBytes (kP: option<Key>) =
        use s = new System.IO.MemoryStream(16 * 1024)
        this.Definition.Write(s, this.OutputParameters kP)
        s.ToArray()

    member this.Symbols = this.Debug

    member this.Write (kP: option<Key>) (path: Path) =
        let par = this.OutputParameters kP
        match this.Debug with
        | Some (Mdb _) ->
            par.WriteSymbols <- true
            par.SymbolWriterProvider <- Mono.Cecil.Mdb.MdbWriterProvider()
        | Some (Pdb _) ->
            par.WriteSymbols <- true
            par.SymbolWriterProvider <- Mono.Cecil.Pdb.PdbWriterProvider()
        | None -> ()
        this.Definition.Write(path, par)

    member this.ReadableJavaScript =
        readResource EMBEDDED_JS this.Definition

    member this.CompressedJavaScript =
        readResource EMBEDDED_MINJS this.Definition

[<Sealed>]
type Resolver(paths: Set<Path>) =
    inherit Mono.Cecil.DefaultAssemblyResolver()
    do for p in paths do base.AddSearchDirectory p

/// Loads assemblies.
[<Sealed>]
type Loader(paths: Set<Path>, log: string -> unit) =

    let load (bytes: byte[]) (symbols: option<Symbols>) (paths: Set<Path>) =
        use str = new System.IO.MemoryStream(bytes)
        let par = Mono.Cecil.ReaderParameters()
        par.AssemblyResolver <- Resolver paths
        par.ReadingMode <- Mono.Cecil.ReadingMode.Deferred
        match symbols with
        | Some (Pdb bytes) ->
            par.ReadSymbols <- true
            par.SymbolReaderProvider <- new Mono.Cecil.Pdb.PdbReaderProvider()
            par.SymbolStream <- new System.IO.MemoryStream(bytes)
        | Some (Mdb bytes) ->
            par.ReadSymbols <- true
            par.SymbolReaderProvider <- new Mono.Cecil.Mdb.MdbReaderProvider()
            par.SymbolStream <- new System.IO.MemoryStream(bytes)
        | None ->
            ()
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly(str, par)
        {
            Debug = symbols
            Definition = def
        }

    static member Create paths log =
        Loader(paths, log)

    member this.LoadRaw bytes symbols =
        load bytes symbols paths

    member this.LoadFile (path: Path) =
        let bytes = System.IO.File.ReadAllBytes path
        let p ext = System.IO.Path.ChangeExtension(path, ext)
        let ex x = System.IO.File.Exists(p x)
        let rd x = System.IO.File.ReadAllBytes(p x)
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
        with :? System.InvalidOperationException ->
            if symbolsPath.IsSome then
                "Failed to load symbols: " + symbolsPath.Value
                |> log
            load bytes None (Set.add path paths)

type Options =
    {
        ErrorLimit : int
        KeyPair : option<System.Reflection.StrongNameKeyPair>
        References : list<Assembly>
    }

    static member Default =
        {
            ErrorLimit = 20
            KeyPair = None
            References = []
        }

let readRuntimeMetadata (a: Mono.Cecil.AssemblyDefinition) =
    let key = M.AssemblyInfo.EmbeddedResourceName
    a.MainModule.Resources
    |> Seq.tryPick (function
        | :? Mono.Cecil.EmbeddedResource as r when r.Name = key ->
            use s = r.GetResourceStream()
            Some (M.AssemblyInfo.FromStream s)
        | _ -> None)

let readCompiledMetadata (a: Mono.Cecil.AssemblyDefinition) =
    let key = EMBEDDED_METADATA
    a.MainModule.Resources
    |> Seq.tryPick (function
        | :? Mono.Cecil.EmbeddedResource as r when r.Name = key ->
            use s = r.GetResourceStream()
            Some (Metadata.Deserialize s)
        | _ ->
            None)

let writeCompiledMetadata (a: Mono.Cecil.AssemblyDefinition)
    (rm: M.AssemblyInfo) (meta: Metadata.T) (pkg: P.Module) =
    let pub = Mono.Cecil.ManifestResourceAttributes.Public
    let dep =
        use s = new System.IO.MemoryStream(8 * 1024)
        Metadata.Serialize s meta
        s.ToArray()
    let prog = P.Package pkg
    let js pref =
        use s = new System.IO.MemoryStream(8 * 1024)
        let () =
            use w = new System.IO.StreamWriter(s)
            W.WriteProgram pref w (prog pref)
        s.ToArray()
    let rmdata =
        use s = new System.IO.MemoryStream(8 * 1024)
        rm.ToStream s
        s.ToArray()
    let rmname = M.AssemblyInfo.EmbeddedResourceName
    Mono.Cecil.EmbeddedResource(rmname, pub, rmdata)
    |> a.MainModule.Resources.Add
    Mono.Cecil.EmbeddedResource(EMBEDDED_METADATA, pub, dep)
    |> a.MainModule.Resources.Add
    if not pkg.IsEmpty then
        Mono.Cecil.EmbeddedResource(EMBEDDED_MINJS, pub, js Pref.Compact)
        |> a.MainModule.Resources.Add
        Mono.Cecil.EmbeddedResource(EMBEDDED_JS, pub, js Pref.Readable)
        |> a.MainModule.Resources.Add

let Compile (options: Options) (log: Message -> unit) : Assembly -> bool =
    let refs =
        options.References
        |> Seq.distinctBy (fun a -> a.Definition.Name.Name)
        |> Seq.toList
    let cM =
        refs
        |> List.choose (fun a -> readCompiledMetadata a.Definition)
    let rM =
        refs
        |> List.choose (fun a -> readRuntimeMetadata a.Definition)
    fun assembly ->
        let succ = ref true
        let err (m: Message) =
            match m.Priority with
            | Priority.Warning -> ()
            | _ -> succ := false
            log m
        let logger = Logger.Create err options.ErrorLimit
        let meta = Metadata.Union logger cM
        let pool = Inlining.Pool.Create logger
        let macros = Reflector.Pool.Create logger
        try
            let ra = Reflector.Reflect logger assembly.Definition
            let pkg = Resolver.Resolve logger ra
            let va = Validator.Validate logger pool macros ra
            let rm = Analyzer.Analyze rM va
            let local = Metadata.Parse logger va
            let joined = Metadata.Union logger [meta; local]
            Assembler.Assemble logger pool macros joined va
            writeCompiledMetadata assembly.Definition rm local pkg.Value
            !succ
        with ErrorLimitExceeded ->
            false

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper.Compiler

[<AutoOpen>]
module LoaderUtility =

#if !NET461 // TODO dotnet: Mono.Cecil.DefaultAssemblyResolver
    open System
    open System.Collections.Generic
    open Microsoft.Extensions.DependencyModel
    open Mono.Cecil

    // Taken from https://github.com/jbevain/cecil/issues/306#issuecomment-263157799
    type private DotNetCoreAssemblyResolver() as this =
        let libraries = Dictionary<string, Lazy<AssemblyDefinition>>()

        do match DependencyContext.Default with
            | null -> ()
            | d ->
            for library in d.CompileLibraries do
                let paths = [|
                    for p in library.ResolveReferencePaths() do
                        if not (String.IsNullOrEmpty p) then yield p
                |]
                if not (Array.isEmpty paths) then
                    libraries.Add(library.Name,
                        lazy
                        paths |> Array.pick (fun p ->
                            try AssemblyDefinition.ReadAssembly(p, ReaderParameters(AssemblyResolver = this)) |> Some
                            with e -> None)
                    )

        member this.Resolve(fullName: AssemblyNameReference) =
            this.Resolve(fullName, ReaderParameters())

        member this.Resolve(fullName: AssemblyNameReference, parameters: ReaderParameters) =
            match fullName with
            | null -> nullArg "name"
            | fullName ->
                match libraries.TryGetValue(fullName.Name) with
                | true, x -> x.Value
                | false, _ ->
                    let assemblies =
                        Array.append
                            (AppDomain.CurrentDomain.GetAssemblies())
                            (AppDomain.CurrentDomain.ReflectionOnlyGetAssemblies())
                    for asm in assemblies do
                        let name = asm.GetName().Name
                        if not (libraries.ContainsKey name || isNull asm.Location) then
                            libraries.Add(name,
                                lazy AssemblyDefinition.ReadAssembly(asm.Location,
                                        ReaderParameters(AssemblyResolver = this)))

                    match DependencyContext.Default with
                    | null -> ()
                    | d ->
                        for library in d.RuntimeLibraries do
                            match library.ResourceAssemblies |> Seq.tryHead with
                            | Some r when not (String.IsNullOrEmpty r.Path) ->
                                libraries.Add(library.Name,
                                    lazy AssemblyDefinition.ReadAssembly(r.Path,
                                            ReaderParameters(AssemblyResolver = this)))
                            | _ -> ()

                    match libraries.TryGetValue(fullName.Name) with
                    | true, x -> x.Value
                    | false, _ ->
                        raise (AssemblyResolutionException fullName)

        member this.Dispose(disposing: bool) =
            if disposing then
                for l in libraries.Values do
                    if l.IsValueCreated then
                        l.Value.Dispose()

        interface IDisposable with
            member this.Dispose() =
                this.Dispose(true)
                GC.SuppressFinalize(this)

        interface IAssemblyResolver with
            member this.Resolve(n) = this.Resolve(n)
            member this.Resolve(n, p) = this.Resolve(n, p)
#endif

    [<Sealed>]
    type Resolver(aR: AssemblyResolver) =
#if NET461 // TODO dotnet: Mono.Cecil.DefaultAssemblyResolver
        let def = new Mono.Cecil.DefaultAssemblyResolver()
#else
        let def = new DotNetCoreAssemblyResolver()
#endif
        let defResolve (name: Mono.Cecil.AssemblyNameReference) = def.Resolve name

        let resolve (ref: string) (par: option<Mono.Cecil.ReaderParameters>) =
            let n = AssemblyName(ref)
            match aR.ResolvePath n with
            | Some x ->
                try
                    if x = null || not (FileInfo(x).Exists) then
                        failwithf "Invalid file resolution: %s" (string x)
                with :? System.ArgumentException ->
                    failwithf "Invalid file resolution: [%s]" (string x)
                match par with
                | None -> Mono.Cecil.AssemblyDefinition.ReadAssembly(x)
                | Some par -> Mono.Cecil.AssemblyDefinition.ReadAssembly(x, par)
            | None -> defResolve(Mono.Cecil.AssemblyNameReference.Parse ref)

        interface Mono.Cecil.IAssemblyResolver with

            member x.Resolve(ref: Mono.Cecil.AssemblyNameReference, par: Mono.Cecil.ReaderParameters) =
                let ref = ref.FullName
                resolve ref (Some par)

            member x.Resolve(ref: Mono.Cecil.AssemblyNameReference) =
                let ref = ref.FullName
                resolve ref None

            member x.Dispose() =
#if NET461 // TODO dotnet: Mono.Cecil.DefaultAssemblyResolver
                def.Dispose()
#else
                ()
#endif

[<Sealed>]
type Loader(aR: AssemblyResolver, log: string -> unit) =

    let load flp (bytes: byte[]) (symbols: option<Symbols>) (aR: AssemblyResolver) =
        let str = new MemoryStream(bytes)
        let par = Mono.Cecil.ReaderParameters()
        par.AssemblyResolver <- new Resolver(aR)
        par.ReadingMode <- Mono.Cecil.ReadingMode.Deferred
        // match symbols with
        // | Some (Pdb bytes) ->
        //     par.ReadSymbols <- true
        //     par.SymbolReaderProvider <- new Mono.Cecil.Pdb.PdbReaderProvider()
        //     par.SymbolStream <- new MemoryStream(bytes)
        // | Some (Mdb bytes) ->
        //     par.ReadSymbols <- true
        //     par.SymbolReaderProvider <- new Mono.Cecil.Mdb.MdbReaderProvider()
        //     par.SymbolStream <- new MemoryStream(bytes)
        // | None -> ()
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly(str, par)
        Assembly.Create(def, ?loadPath = flp, ?symbols = symbols)

    static member Create(res: AssemblyResolver)(log) =
        Loader(res, log)

    member this.LoadRaw(bytes)(symbols) =
        load None bytes symbols aR

    member this.LoadFile(path: string, ?loadSymbols) =
        let loadSymbols = defaultArg loadSymbols true
        let bytes = File.ReadAllBytes path
        let p ext = Path.ChangeExtension(path, ext)
        let ex x = File.Exists(p x)
        let rd x = File.ReadAllBytes(p x)
        let symbolsPath =
            if loadSymbols then
                if ex ".pdb" then Some (p ".pdb")
                elif ex ".mdb" then Some (p ".mdb")
                else None
            else None
        let symbols =
            if loadSymbols then
                if ex ".pdb" then Some (Pdb (rd ".pdb"))
                elif ex ".mdb" then Some (Mdb (rd ".mdb"))
                else None
            else None
        let aR = aR.SearchPaths [path]
        let fP = Some (Path.GetFullPath path)
        try
            load fP bytes symbols aR
        with _ ->
            if symbolsPath.IsSome then
                "Failed to load symbols: " + symbolsPath.Value
                |> log
                load fP bytes None aR
            else reraise()

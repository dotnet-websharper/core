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

    [<Sealed>]
    type Resolver(aR: AssemblyResolver) =
        let def = Mono.Cecil.DefaultAssemblyResolver()

        let resolve (ref: string) (par: option<Mono.Cecil.ReaderParameters>) =
            let n = AssemblyName(ref)
            match aR.ResolvePath n with
            | Some x ->
                match par with
                | None -> Mono.Cecil.AssemblyDefinition.ReadAssembly(x)
                | Some par -> Mono.Cecil.AssemblyDefinition.ReadAssembly(x, par)
            | None -> def.Resolve(ref)

        interface Mono.Cecil.IAssemblyResolver with

            member x.Resolve(name) =
                resolve name None

            member x.Resolve(name: string, par) =
                resolve name (Some par)

            member x.Resolve(ref: Mono.Cecil.AssemblyNameReference, par: Mono.Cecil.ReaderParameters) =
                let ref = ref.FullName
                resolve ref (Some par)

            member x.Resolve(ref: Mono.Cecil.AssemblyNameReference) =
                let ref = ref.FullName
                resolve ref None

[<Sealed>]
type Loader(aR: AssemblyResolver, log: string -> unit) =

    let load flp (bytes: byte[]) (symbols: option<Symbols>) (aR: AssemblyResolver) =
        use str = new MemoryStream(bytes)
        let par = Mono.Cecil.ReaderParameters()
        par.AssemblyResolver <- Resolver aR
        par.ReadingMode <- Mono.Cecil.ReadingMode.Deferred
        match symbols with
        | Some (Pdb bytes) ->
            par.ReadSymbols <- true
            par.SymbolReaderProvider <- new Mono.Cecil.Pdb.PdbReaderProvider()
            par.SymbolStream <- new MemoryStream(bytes)
        | Some (Mdb bytes) ->
            par.ReadSymbols <- true
            par.SymbolReaderProvider <- new Mono.Cecil.Mdb.MdbReaderProvider()
            par.SymbolStream <- new MemoryStream(bytes)
        | None -> ()
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

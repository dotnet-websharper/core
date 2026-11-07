// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

open System.Collections.Concurrent

[<Sealed>]
type Loader(aR: AssemblyResolver, log: string -> unit, cache: ConcurrentDictionary<string, Assembly>) =

    let load flp (bytes: byte[]) (symbols: option<Symbols>) =
        let def = PEMetadata.PEAssembly.Read bytes
        Assembly.Create(def, ?loadPath = flp, ?symbols = symbols)

    static member Create(resolver: AssemblyResolver)(log) =
        let cache = ConcurrentDictionary<string, Assembly>()
        Loader(resolver, log, cache)

    member this.WithAssemblyResolver(resolver: AssemblyResolver) =
        Loader(resolver, log, cache)

    member this.LoadRaw(bytes)(symbols) =
        load None bytes symbols

    member this.LoadFile(path: string, ?loadSymbols) =
        let loadSymbols = defaultArg loadSymbols true
        
        let loadFile path =
            let p ext = Path.ChangeExtension(path, ext)
            let ex x = File.Exists(p x)
            let rd x = File.ReadAllBytes(p x)
            let symbols =
                if loadSymbols then
                    if ex ".pdb" then Some (Pdb (rd ".pdb"))
                    elif ex ".mdb" then Some (Mdb (rd ".mdb"))
                    else None
                else None
            let def = PEMetadata.PEAssembly.ReadFile path
            Assembly.Create(def, loadPath = Path.GetFullPath path, ?symbols = symbols)

        cache.GetOrAdd(path, loadFile)

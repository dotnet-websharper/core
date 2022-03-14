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

namespace WebSharper.Compiler.AssemblyResolution

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Reflection
open System.Runtime.Loader

module internal Net4xImplemetnation =

    let isCompatible (ref: AssemblyName) (def: AssemblyName) =
        ref.Name = def.Name && (ref.Version = null || def.Version = null || ref.Version = def.Version)

    let tryFindAssembly (dom: AppDomain) (name: AssemblyName) =
        dom.GetAssemblies()
        |> Seq.tryFind (fun a ->
            a.GetName()
            |> isCompatible name)

    let loadInto (baseDir: string) (dom: AppDomain) (path: string) =
        let f = FileInfo path
        if f.DirectoryName = baseDir then
            dom.Load(AssemblyName.GetAssemblyName path)
        else
            File.ReadAllBytes path
            |> dom.Load

    type Resolver(reso: AssemblyResolution, dom: AppDomain) =

        let baseDir = dom.BaseDirectory

        member this.Install() =
            let resolveAssembly(name: AssemblyName) =
                match tryFindAssembly dom name with
                | None ->
                    match reso.ResolvePath name with
                    | None -> None
                    | Some r -> Some (loadInto baseDir dom r)
                | r -> r

            let resolve (x: obj) (a: ResolveEventArgs) =
                let name = AssemblyName(a.Name)
                match resolveAssembly name with
                | None -> null
                | Some r -> r

            let handler = ResolveEventHandler(resolve)

            dom.add_AssemblyResolve(handler)

            fun () -> 
                dom.remove_AssemblyResolve(handler)

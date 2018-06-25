// Copyright 2013-2015 IntelliFactory
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

namespace WebSharper.Compiler

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Reflection

[<AutoOpen>]
module Implemetnation =

    let isCompatible (ref: AssemblyName) (def: AssemblyName) =
        ref.Name = def.Name && (ref.Version = null || def.Version = null || ref.Version = def.Version)

    let tryFindAssembly (dom: AppDomain) (name: AssemblyName) =
        dom.GetAssemblies()
        |> Seq.tryFind (fun a ->
            a.GetName()
            |> isCompatible name)

    let loadInto (baseDir: string) (dom: AppDomain) (path: string) =
        File.ReadAllBytes path
        |> dom.Load

    type AssemblyResolution =
        {
            ResolvePath : AssemblyName -> option<string>
        }

        member r.ResolveAssembly(bD: string, dom: AppDomain, name: AssemblyName) =
            match tryFindAssembly dom name with
            | None ->
                match r.ResolvePath name with
                | None -> None
                | Some r -> Some (loadInto bD dom r)
            | r -> r

    let combine a b =
        {
            ResolvePath = fun name ->
                match a.ResolvePath name with
                | None -> b.ResolvePath name
                | r -> r
        }

    let first xs =
        xs
        |> Seq.tryFind (fun x -> true)

    let isMatchingFile name path =
        let f = FileInfo path
        if f.Exists then
            let n = AssemblyName.GetAssemblyName f.FullName
            isCompatible n name
        else false

    let searchPaths (paths: seq<string>) =
        let paths =
            paths
            |> Seq.map Path.GetFullPath
            |> Seq.toArray
        {
            ResolvePath = fun name ->
                seq {
                    for path in paths do
                        for ext in [".dll"; ".exe"] do
                            if String.Equals(Path.GetFileName(path), name.Name + ext, StringComparison.OrdinalIgnoreCase) then
                                if isMatchingFile name path then
                                    yield path
                }
                |> first
        }

    let searchDirs (dirs: seq<string>) =
        let dirs =
            dirs
            |> Seq.map Path.GetFullPath
            |> Seq.toArray
        {
            ResolvePath = fun name ->
                seq {
                    for dir in dirs do
                        for ext in [".dll"; ".exe"] do
                            let p = Path.Combine(dir, name.Name + ext)
                            if isMatchingFile name p then
                                yield p
                }
                |> first
        }

    let memoize getKey f =
        let cache = ConcurrentDictionary()
        fun x -> cache.GetOrAdd(getKey x, valueFactory = Func<_,_>(fun _ -> f x))

    let memoizeResolution (r: AssemblyResolution) =
        let key (n: AssemblyName) = (n.Name, string n.Version)
        { ResolvePath = memoize key r.ResolvePath }

    let zero =
        { ResolvePath = fun name -> None }

    let inline ( ++ ) a b = combine a b

/// An utility for resolving assemblies from non-standard contexts.
[<Sealed>]
type AssemblyResolver(baseDir: string, dom: AppDomain, reso: AssemblyResolution) =

    let reso = memoizeResolution reso

    static let get (x: AssemblyResolver) : AssemblyResolution = x.Resolution

    let resolve (x: obj) (a: ResolveEventArgs) =
        let name = AssemblyName(a.Name)
        match reso.ResolveAssembly(baseDir, dom, name) with
        | None -> null
        | Some r -> r

    let handler = ResolveEventHandler(resolve)

    member r.Install() =
        dom.add_AssemblyResolve(handler)

    member r.Remove() =
        dom.remove_AssemblyResolve(handler)

    member r.Wrap(action: unit -> 'T) =
        try
            r.Install()
            action ()
        finally
            r.Remove()

    member r.SearchDirectories ds = AssemblyResolver(baseDir, dom, reso ++ searchDirs ds)
    member r.SearchPaths ps = AssemblyResolver(baseDir, dom, reso ++ searchPaths ps)
    member r.Resolve name = reso.ResolveAssembly(baseDir, dom, name)
    member r.ResolvePath name = reso.ResolvePath name
    member r.Resolution = reso
    member r.WithBaseDirectory bD = AssemblyResolver(Path.GetFullPath bD, dom, reso)

    static member Create(?domain) =
        let dom = defaultArg domain AppDomain.CurrentDomain
        AssemblyResolver(dom.BaseDirectory, dom, zero)

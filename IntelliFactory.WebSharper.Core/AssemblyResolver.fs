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

namespace IntelliFactory.WebSharper.Core

open System
open System.Collections.Generic
open System.IO
open System.Reflection

[<AutoOpen>]
module private AssemblyResolverModule =

    type AssemblyContext =
        | ReflectionOnlyContext
        | RegularContext

    type AssemblyResolution =
        AssemblyContext -> AssemblyName -> option<Assembly>

    let isCompatible (ref: AssemblyName) (def: AssemblyName) =
        ref.Name = def.Name
        && ref.Version = def.Version

    let combine a b : AssemblyResolution =
        fun ctx name ->
            match a ctx name with
            | None -> b ctx name
            | r -> r

    let searchDirs (dirs: seq<string>) : AssemblyResolution =
        fun ctx name ->
            seq {
                for dir in dirs do
                    for ext in [".dll"; ".exe"] do
                        let p = Path.Combine(dir, name.Name + ext)
                        let f = FileInfo p
                        if f.Exists then
                            let n = AssemblyName.GetAssemblyName f.FullName
                            if isCompatible name n then
                                match ctx with
                                | ReflectionOnlyContext ->
                                    yield Assembly.ReflectionOnlyLoadFrom(f.FullName)
                                | RegularContext ->
                                    yield Assembly.LoadFrom(f.FullName)
            }
            |> Seq.tryFind (fun x -> true)

    let searchAssemblies (all: seq<Assembly>) : AssemblyResolution =
        fun ctx name ->
            all
            |> Seq.tryFind (fun a ->
                match ctx with
                | ReflectionOnlyContext -> a.ReflectionOnly
                | _ -> true
                && isCompatible name (a.GetName()))

    let searchDomain (dom: AppDomain) : AssemblyResolution =
        fun ctx name ->
            let a = dom.GetAssemblies()
            searchAssemblies a ctx name

/// An utility for resolving assemblies from non-standard contexts.
[<Sealed>]
type AssemblyResolver private (resolve: AssemblyResolution) =

    let root = obj ()
    let cache = Dictionary()

    let resolve (ctx: AssemblyContext) (x: AssemblyName) =
        lock root <| fun () ->
            let key = (ctx, string x.Name, string x.Version)
            match cache.TryGetValue key with
            | true, y -> y
            | _ ->
                let y = resolve ctx x
                cache.[key] <- y
                y

    static let zero =
        AssemblyResolver(fun _ _ -> None)

    static let get (x: AssemblyResolver) : AssemblyResolution =
        x.Resolve

    let handler1 =
        ResolveEventHandler(fun _ a ->
            let name = AssemblyName(a.Name)
            match resolve RegularContext name with
            | None -> null
            | Some r -> r)

    let handler2 =
        ResolveEventHandler(fun _ a ->
            let name = AssemblyName(a.Name)
            match resolve ReflectionOnlyContext name with
            | None -> null
            | Some r -> r)

    /// Installs the resolver into an `AppDomain`.
    member r.Install(?domain) =
        let domain = defaultArg domain AppDomain.CurrentDomain
        domain.add_AssemblyResolve(handler1)
        domain.add_ReflectionOnlyAssemblyResolve(handler2)

    /// Uninstalls the resolver from an `AppDomain`.
    member r.Remove(?domain) =
        let domain = defaultArg domain AppDomain.CurrentDomain
        domain.remove_AssemblyResolve(handler1)
        domain.remove_ReflectionOnlyAssemblyResolve(handler2)

    /// Wraps an action in `Install/Remove`.
    member r.With(?domain) =
        fun action ->
            try
                r.Install(?domain = domain)
                action ()
            finally
                r.Remove(?domain = domain)

    member private r.Resolve = resolve

    /// Combines two resolvers with the second one acting as fallback.
    static member Fallback(a, b) =
        AssemblyResolver(combine (get a) (get b))

    /// Searches the given AppDomain.
    static member SearchDomain(?domain)=
        let domain = defaultArg domain AppDomain.CurrentDomain
        AssemblyResolver(combine (searchDomain domain) (searchDirs [domain.BaseDirectory]))

    /// Creates an assembly resolver based on the given search paths.
    static member SearchPaths(searchPaths) =
        AssemblyResolver(searchDirs searchPaths)

    /// Alias for `Fallback`.
    static member ( + ) (a, b) =
        AssemblyResolver.Fallback(a, b)

    /// The `Zero` resolver always refueses to resolve.
    static member Zero =
        zero

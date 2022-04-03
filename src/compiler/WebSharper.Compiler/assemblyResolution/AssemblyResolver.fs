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

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Reflection
open System.Runtime.Loader
open WebSharper.Compiler.AssemblyResolution

/// An utility for resolving assemblies from non-standard contexts.
[<Sealed>]
type AssemblyResolver(dom: AppDomain, reso: AssemblyResolution) =

    let mutable remove = fun () -> ()

    member r.Install() =
        if System.Environment.Version.Major = 4 then 
            remove <- Net4xImplemetnation.Resolver(reso, dom).Install()         
        else
            remove <- NetCoreImplemetnation.Resolver(reso, dom).Install()

    member r.Remove() =
        remove()

    member r.Wrap(action: unit -> 'T) =
        try
            r.Install()
            action ()
        finally
            r.Remove()

    member r.SearchDirectories searchPaths = AssemblyResolver(dom, reso ++ searchDirs searchPaths)
    member r.SearchPaths (searchPaths: seq<string>) = AssemblyResolver(dom, reso ++ Common.searchPaths searchPaths)
    member r.ResolvePath name = reso.ResolvePath name

    static member Create(?domain) =
        let dom = defaultArg domain AppDomain.CurrentDomain
        AssemblyResolver(dom, zero)

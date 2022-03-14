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

type internal AssemblyResolution =
    {
        Cache : ConcurrentDictionary<string, Assembly>
        ResolvePath : AssemblyName -> option<string>
    }

[<AutoOpen>]
module internal Common =
    let isCompatibleForLoad (ref: AssemblyName) (def: AssemblyName) =
        ref.Name = def.Name && 
            (ref.Version = null || def.Version = null || ref.Version = def.Version)

    let amsToDictByName paths =
        paths
        |> Seq.choose (fun path ->
            try
                let assemblyName = AssemblyName.GetAssemblyName path
                Some (assemblyName.Name, path)
            with _ -> None
        )
        |> dict

    let combine a b =
        {
            Cache = ConcurrentDictionary()
            ResolvePath = fun name ->
                match a.ResolvePath name with
                | None -> b.ResolvePath name
                | r -> r
        }

    let isMatchingFile name path =
        let f = FileInfo path
        if f.Exists then
            let n = AssemblyName.GetAssemblyName f.FullName
            let isCompat = isCompatibleForLoad n name
#if DEBUG
            if isCompat && n.Version > name.Version then
                LoggerBase.Current.Out <| sprintf "AssemblyResolver loading higher version: %A instead of %A" n.Version name.Version 
#endif
            isCompat
        else false

    let searchPaths (paths: seq<string>) =
        let asmsDict = amsToDictByName paths
#if DEBUG
        for path in paths do
            LoggerBase.Current.Out <| sprintf "AssemblyResolver added search path: %s" path
#endif
        {
            Cache = ConcurrentDictionary()
            ResolvePath = fun name ->
                match asmsDict.TryGetValue(name.Name) with
                | true, path when isMatchingFile name path ->
                    Some path
                | _ ->
                    None
        }

    let searchDirs (dirs: seq<string>) =
        let paths =
            dirs
            |> Seq.collect (fun dir ->
                Seq.append (Directory.EnumerateFiles(dir, "*.dll")) (Directory.EnumerateFiles(dir, "*.exe"))
            )
#if DEBUG
        for dir in dirs do
            LoggerBase.Current.Out <| sprintf "AssemblyResolver added search dirs: %s" dir
#endif
        searchPaths paths

    let zero =
        { Cache = ConcurrentDictionary(); ResolvePath = fun name -> None }

    let inline ( ++ ) a b = combine a b

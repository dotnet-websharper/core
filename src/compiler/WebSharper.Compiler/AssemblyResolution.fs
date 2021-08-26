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

[<AutoOpen>]
module Implemetnation =

    let forceNonCompatible (ref: AssemblyName) =
        match ref.Name with
        | "FSharp.Core" 
        | "System.Runtime"
        | "netstandard"
            -> true
        | _ -> false
    
    let isCompatibleForLoad (ref: AssemblyName) (def: AssemblyName) =
        ref.Name = def.Name && 
            (ref.Version = null || def.Version = null || ref.Version = def.Version)

    let isCompatibleRuntimeForLoad (ref: AssemblyName) (def: AssemblyName) =
        ref.Name = def.Name && 
            (ref.Version = null || def.Version = null || ref.Version >= def.Version)

    let isCompatibleForInherit (ref: AssemblyName) (def: AssemblyName) =
        ref.Name = def.Name && 
            (forceNonCompatible ref || ref.Version = null || def.Version = null || ref.Version = def.Version)
            
    let isMatchingRuntimeFile name path =
        let n = AssemblyName.GetAssemblyName path
        let isCompat = isCompatibleRuntimeForLoad n name
#if DEBUG
        if isCompat && n.Version > name.Version then
            LoggerBase.Current.Out <| sprintf "AssemblyResolver loading higher version from runtime: %A instead of %A" n name
#endif
        isCompat

    let asmsProp = typeof<AssemblyLoadContext>.GetProperty("Assemblies", [||])

    let amsToDictByName paths =
        paths
        |> Seq.choose (fun path ->
            let asmName = 
                try
                    Some (AssemblyName.GetAssemblyName path)
                with _ -> None
            match asmName with
            | None -> None
            | Some n -> Some (n.Name, path)
        )
        |> dict

    let runtimeAsmDict =
        let sysRuntimePath =
            let assemblies = asmsProp.GetMethod.Invoke(AssemblyLoadContext.Default, [||])
            let sysRuntimeAsm =
                assemblies :?> seq<Assembly>
                |> Seq.find (fun a -> a.GetName().Name = "System.Runtime")    
            sysRuntimeAsm.Location
        let sysRuntimeDir = DirectoryInfo(Path.GetDirectoryName(sysRuntimePath))
        let runtimeVersion = sysRuntimeDir.Name
        sysRuntimeDir.Parent.Parent.GetDirectories()
        |> Seq.choose (fun fwdir ->
            fwdir.GetDirectories() |> Seq.tryFind (fun vdir -> vdir.Name = runtimeVersion)
        )
        |> Seq.collect (fun vdir ->
            vdir.EnumerateFiles("*.dll") |> Seq.map (fun fi -> fi.FullName)
        )
        |> amsToDictByName

    let loadIntoAppDomain (dom: AppDomain) (path: string) =
        try File.ReadAllBytes path |> dom.Load
        with :? System.BadImageFormatException -> null

    let loadIntoAssemblyLoadContext (loadContext: AssemblyLoadContext) (path: string) =
        let fs = new MemoryStream (File.ReadAllBytes path)
        try loadContext.LoadFromStream fs 
        with :? System.BadImageFormatException -> null

    let tryFindAssembly (dom: AppDomain) (name: AssemblyName) =
        let assemblies = asmsProp.GetMethod.Invoke(AssemblyLoadContext.Default, [||])
        let inMainContext =
            assemblies :?> seq<Assembly>
            |> Seq.tryFind (fun a ->
                a.GetName()
                |> isCompatibleForInherit name)
        match inMainContext with
        | None -> 
            match runtimeAsmDict.TryGetValue(name.Name) with
            | true, p when isMatchingRuntimeFile name p -> 
#if DEBUG
                LoggerBase.Current.Out <| sprintf "AssemblyResolver loading assembly as collectible from runtime: %s" p
#endif
                loadIntoAppDomain dom p |> Some 
            | _ ->
                None
        | res -> res

    type AssemblyResolution =
        {
            Cache : ConcurrentDictionary<string, Assembly>
            ResolvePath : AssemblyName -> option<string>
        }

        member r.ResolveAssembly(dom: AppDomain, loadContext: option<AssemblyLoadContext>, asmNameOrPath: string) =
            let resolve (x: string) =
                let isFilePath =
                   x.EndsWith(".dll", StringComparison.InvariantCultureIgnoreCase) 
                   || x.EndsWith(".exe", StringComparison.InvariantCultureIgnoreCase) 
                let name =
                    if isFilePath then
                        AssemblyName(Path.GetFileNameWithoutExtension x)
                    else
                        AssemblyName(x)
                match tryFindAssembly dom name with
                | None ->
                    if isFilePath then
                        let p = Path.GetFullPath x
#if DEBUG
                        LoggerBase.Current.Out <| sprintf "AssemblyResolver loading assembly as collectible from path: %s" p
#endif
                        let asm =
                            match loadContext with
                            | Some alc ->
                                loadIntoAssemblyLoadContext alc p
                            | None ->
                                loadIntoAppDomain dom p
                        asm
                    else
                        match r.ResolvePath name with
                        | None -> 
#if DEBUG
                            LoggerBase.Current.Out <| sprintf "AssemblyResolver could not resolve assembly: %s" x
#endif
                            null
                        | Some p -> 
#if DEBUG
                            LoggerBase.Current.Out <| sprintf "AssemblyResolver loading assembly as collectible: %s" x
#endif
                            match loadContext with
                            | Some alc ->
                                loadIntoAssemblyLoadContext alc p
                            | None ->
                                loadIntoAppDomain dom p
                | Some r -> 
#if DEBUG
                    LoggerBase.Current.Out <| sprintf "AssemblyResolver resolved assembly from main context: %s" x
#endif
                    r

            r.Cache.GetOrAdd(asmNameOrPath, valueFactory = Func<_,_>(resolve))

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

/// An utility for resolving assemblies from non-standard contexts.
[<Sealed>]
type AssemblyResolver(dom: AppDomain, reso: AssemblyResolution) =

    let mutable loadContext = None
    let mutable entered = null
    let mutable domHandler = null 

    member r.Install() =
        if Option.isNone loadContext then
            loadContext <-
                // hack to create a .NET 5 AssemblyLoadContext if we are on .NET 5
                let ctor = typeof<AssemblyLoadContext>.GetConstructor([| typeof<string>; typeof<bool> |])
                if isNull ctor then
                    None
                else
                    let alc = ctor.Invoke([| null; true |]) :?> AssemblyLoadContext
                    let resolve = Func<_,_,_>(fun (thisAlc: AssemblyLoadContext) (assemblyName: AssemblyName) -> 
                        reso.ResolveAssembly(dom, Some thisAlc, assemblyName.FullName)
                    )
                    alc.add_Resolving resolve
                            
                    Some alc

        let enterContextualReflection() =
            match loadContext with
            | Some alc ->
                let meth = typeof<AssemblyLoadContext>.GetMethod("EnterContextualReflection", [||])
                if not (isNull meth) then
                    entered <- meth.Invoke(alc, [||]) :?> IDisposable
            | _ -> ()

        let domResolve (x: obj) (a: ResolveEventArgs) =
            reso.ResolveAssembly(dom, loadContext, a.Name)

        domHandler <- ResolveEventHandler(domResolve)

        let resolve x =
            reso.ResolveAssembly(dom, loadContext, x)
        
        WebSharper.Core.Reflection.OverrideAssemblyResolve <-  Some resolve
        enterContextualReflection()
        dom.add_AssemblyResolve(domHandler)    

    member r.Remove() =
        let exitContextualReflection() =
            if not (isNull entered) then
                entered.Dispose()

        let unload() =
            let meth = typeof<AssemblyLoadContext>.GetMethod("Unload")
            meth.Invoke(loadContext.Value, [||]) |> ignore

        WebSharper.Core.Reflection.OverrideAssemblyResolve <- None
        exitContextualReflection()
        dom.remove_AssemblyResolve(domHandler)    

    member r.Wrap(action: unit -> 'T) =
        try
            r.Install()
            action ()
        finally
            r.Remove()

    member r.SearchDirectories searchPaths = AssemblyResolver(dom, reso ++ searchDirs searchPaths)
    member r.SearchPaths (searchPaths: seq<string>) = AssemblyResolver(dom, reso ++ Implemetnation.searchPaths searchPaths)
    member r.ResolvePath name = reso.ResolvePath name

    static member Create(?domain) =
        let dom = defaultArg domain AppDomain.CurrentDomain
        AssemblyResolver(dom, zero)

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

namespace WebSharper.Sitelets.Offline

open System
open System.IO
open System.Reflection
open WebSharper.Core
open WebSharper.Compiler
open WebSharper.Sitelets
module C = WebSharper.Compiler.Commands
module H = WebSharper.Compiler.HtmlCommand
module D = WebSharper.Compiler.DownloadResources
module PC = WebSharper.PathConventions

/// Implements the WebSharper executable plugin for generating
/// offline pages with sitelets. To use, run `WebSharper.exe sitelets`.
[<Sealed>]
type HtmlCommand() =
    interface H.IHtmlCommand with
        member this.Execute(env, options) =
            let errors = ResizeArray()
            let aR = AssemblyResolver.Create()
            Directory.CreateDirectory options.OutputDirectory |> ignore
            // process extra.files
            Extra.CopyFiles
                options.ProjectDirectory
                options.OutputDirectory
            let scriptDir =
                System.IO.Path.Combine(options.OutputDirectory, "Scripts")
                |> Directory.CreateDirectory
            let aR =
                let aR = aR.SearchPaths(options.ReferenceAssemblyPaths)
                options.ReferenceAssemblyPaths
                |> Seq.map Path.GetDirectoryName
                |> Seq.append [Path.GetDirectoryName(options.MainAssemblyPath)]
                |> aR.SearchDirectories
            
            try
                aR.Install()

                // Load the sitelet
                let loadSite (file: string) =
                    let assembly = WebSharper.Core.Reflection.LoadAssembly file
                    match assembly with
                    | null ->
                        failwithf "Failed to load %s" file
                    | assembly ->
                        let aT = typeof<WebsiteAttribute>
                        match Attribute.GetCustomAttribute(assembly, aT) with
                        | :? WebsiteAttribute as attr ->
                            attr.Run ()
                        |_  ->
                            failwithf "Failed to find WebSiteAttribute \
                                on the processed assembly: %s"
                                file
                let (sitelet, actions) = loadSite options.MainAssemblyPath

                if options.DownloadResources then
                    let assemblies = [options.MainAssemblyPath] @ options.ReferenceAssemblyPaths
                    for p in assemblies do
                        D.DownloadResource p options.OutputDirectory |> errors.AddRange

                // Write site content.
                Output.WriteSite aR {
                    Sitelet = sitelet
                    Options = options
                    Actions = actions
                    UnpackSourceMap = options.UnpackSourceMap
                    UnpackTypeScript = options.UnpackTypeScript
                }
                |> Async.RunSynchronously
            
            finally
                aR.Remove()

            if errors.Count = 0 then
                C.Ok
            else
                C.Errors (List.ofSeq errors)

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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
open IntelliFactory.Core
open WebSharper.Core
open WebSharper.Sitelets

/// Implements the WebSharper executable plugin for generating
/// offline pages with sitelets. To use, run `WebSharper.exe sitelets`.
[<Sealed>]
type HtmlCommand =
    /// <summary>
    /// Generate an offline HTML application.
    /// </summary>
    /// <param name="options">The configuration for the application.</param>
    /// <returns>List errors on failure, or None on success.</returns>
    static member Execute
        (
            projectDirectory: string,
            outputDirectory: string,
            referenceAssemblyPaths: list<string>,
            mainAssemblyPath: string,
            unpackSourceMap: bool,
            unpackTypeScript: bool,
            debugMode: bool
        ) : option<list<string>> =
        let baseDir = typeof<HtmlCommand>.Assembly.Location
        let aR =
            AssemblyResolver.Create()
                .WithBaseDirectory(baseDir)
                .SearchDirectories([baseDir])
        // process extra.files
        Extra.CopyFiles projectDirectory outputDirectory
        let scriptDir =
            Path.Combine(outputDirectory, "Scripts")
            |> Directory.CreateDirectory
        let aR =
            let aR = aR.SearchPaths(referenceAssemblyPaths)
            referenceAssemblyPaths
            |> Seq.map Path.GetDirectoryName
            |> Seq.append [Path.GetDirectoryName(mainAssemblyPath)]
            |> aR.SearchDirectories
        aR.Wrap <| fun () ->
            // Load the sitelet
            let loadSite (file: string) =
                let assemblyName = AssemblyName.GetAssemblyName(file)
                let assembly = aR.Resolve(assemblyName)
                match assembly with
                | None ->
                    failwithf "Failed to load %s" file
                | Some assembly ->
                    let aT = typeof<WebsiteAttribute>
                    match Attribute.GetCustomAttribute(assembly, aT) with
                    | :? WebsiteAttribute as attr ->
                        attr.Run ()
                    |_  ->
                        failwithf "Failed to find WebSiteAttribute \
                            on the processed assembly: %s"
                            file
            let (sitelet, actions) = loadSite mainAssemblyPath
            // Write site content.
            Output.WriteSite aR {
                Sitelet = sitelet
                MainAssemblyPath = mainAssemblyPath
                OutputDirectory = outputDirectory
                ProjectDirectory = projectDirectory
                ReferenceAssemblyPaths = referenceAssemblyPaths
                Actions = actions
                DebugMode = debugMode
                UnpackSourceMap = unpackSourceMap
                UnpackTypeScript = unpackTypeScript
            }
            |> Async.RunSynchronously
            None

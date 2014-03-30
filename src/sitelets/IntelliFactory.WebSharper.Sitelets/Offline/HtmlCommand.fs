// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Sitelets.Offline

open System
open System.IO
open System.Reflection
open IntelliFactory.Core
open IntelliFactory.WebSharper.Core
open IntelliFactory.WebSharper.Sitelets
module C = IntelliFactory.WebSharper.Compiler.Commands
module H = IntelliFactory.WebSharper.Compiler.HtmlCommand

/// Implements the WebSharper executable plugin for generating
/// offline pages with sitelets. To use, run `WebSharper.exe sitelets`.
[<Sealed>]
type HtmlCommand() =
    interface H.IHtmlCommand with
        member this.Execute(env, options) =
            let baseDir = typeof<HtmlCommand>.Assembly.Location
            let aR =
                AssemblyResolver.Create()
                    .WithBaseDirectory(baseDir)
                    .SearchDirectories([baseDir])
            // process extra.files
            Extra.CopyFiles
                options.ProjectDirectory
                options.OutputDirectory
            let scriptDir =
                Path.Combine(options.OutputDirectory, "Scripts")
                |> Directory.CreateDirectory
            let aR =
                let aR = aR.SearchPaths(options.ReferenceAssemblyPaths)
                options.ReferenceAssemblyPaths
                |> Seq.map Path.GetDirectoryName
                |> Seq.append [Path.GetDirectoryName(options.MainAssemblyPath)]
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
                let (sitelet, actions) = loadSite options.MainAssemblyPath
                // Write site content.
                Output.WriteSite aR {
                    Sitelet = sitelet
                    Options = options
                    Actions = actions
                }
                |> Async.RunSynchronously
                C.Ok

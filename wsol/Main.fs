// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

module IntelliFactory.WebSharper.Sitelets.Offline.Main

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open IntelliFactory.WebSharper.Sitelets

[<EntryPoint>]
let Main args =

    // Tries to load the site from the assembly
    let loadSite (file: FileInfo) =
        let assembly = Assembly.LoadFile(file.FullName)
        let aT = typeof<WebsiteAttribute>
        match Attribute.GetCustomAttribute(assembly, aT) with
        | :? WebsiteAttribute as attr ->
            attr.Run ()
        |_  ->
            failwithf "Failed to find WebSiteAttribute \
                on the processed assembly: %s"
                file.FullName

    try
        let options = Options.Parse args

        // process extra.files
        Extra.CopyFiles
            options.ProjectDirectory.FullName
            options.OutputDirectory.FullName

        let assembly =
            Assembly.LoadFile(options.SourceAssembly.FullName)

        let scriptDir =
            Path.Combine(options.OutputDirectory.FullName, "Scripts")
            |> Directory.CreateDirectory

        // Add source directories to search path
        for dir in options.SourceDirectories do
            Loader.AddSearchPath dir.FullName

        // Load the sitelet
        let (sitelet, actions) = loadSite options.SourceAssembly

        // Compute the available assembly files and embedded resources.
        let assFiles =
            let m =
                Output.ComputeResources options.Mode
                    options.SourceDirectories
            let d = Dictionary()
            Map.iter (fun k v -> d.[k] <- v) m
            d

        // Write site content.
        Output.WriteSite
            {
                AssemblyFiles = assFiles
                Sitelet = sitelet
                Mode = options.Mode
                SrcDir = options.SourceDirectories
                TargetDir = options.OutputDirectory
                Actions = actions
            }

        // Write resources.
        Output.OutputResources assFiles scriptDir
        0
    with
    | Options.BadOptions message ->
        stderr.WriteLine message
        stderr.WriteLine Options.Help
        -1
    | exn ->
        let temp = Path.GetTempFileName()
        File.WriteAllText(temp, string exn)
        stdout.WriteLine("[Error] {0}(1,1): {1}", temp, string exn)
        1

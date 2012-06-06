// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

namespace IntelliFactory.WebSharper.Sitelets

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open IntelliFactory.WebSharper.Core.Plugins
open IntelliFactory.WebSharper.Sitelets.Offline

/// Implements the WebSharper executable plugin for generating
/// offline pages with sitelets. To use, run WebSharper.exe sitelets.
[<Sealed>]
type Plugin() =

    // Tries to load the site from the assembly
    static let loadSite (file: FileInfo) =
        let assembly = Assembly.LoadFile(file.FullName)
        let aT = typeof<WebsiteAttribute>
        match Attribute.GetCustomAttribute(assembly, aT) with
        | :? WebsiteAttribute as attr ->
            attr.Run ()
        |_  ->
            failwithf "Failed to find WebSiteAttribute \
                on the processed assembly: %s"
                file.FullName

    static let run (env: IEnvironment) =
        let args =
            Array.sub env.CommandLineArgs 2
                (env.CommandLineArgs.Length - 2)
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
                env.AddAssemblySearchPath(dir.FullName)

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
            Result.Success
        with
        | Options.BadOptions message ->
            stderr.WriteLine message
            stderr.WriteLine Options.Help
            Result.Error
        | exn ->
            let temp = Path.GetTempFileName()
            File.WriteAllText(temp, string exn)
            stdout.WriteLine("[Error] {0}(1,1): {1}", temp, string exn)
            Result.Error

    interface IPlugin with
        member this.Run(env) =
            if env.CommandLineArgs.Length > 2
                && env.CommandLineArgs.[1] = "sitelets" then
                run env
            else
                Result.Pass

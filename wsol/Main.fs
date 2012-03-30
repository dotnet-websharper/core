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

// Tries to load the site from the assembly
let TryLoadSite (file: System.IO.FileInfo) =
    try
        let assembly = System.Reflection.Assembly.LoadFile(file.FullName)
        let aT = typeof<IntelliFactory.WebSharper.Sitelets.WebsiteAttribute>
        match System.Attribute.GetCustomAttribute(assembly, aT) with
        | :? IntelliFactory.WebSharper.Sitelets.WebsiteAttribute as attr ->
            attr.Run () |> Some
        |_  -> None
    with
    | _ -> None

let guard action =
    try action () with exn ->
        let temp = System.IO.Path.GetTempFileName()
        System.IO.File.WriteAllText(temp, string exn)
        stdout.WriteLine("[Error] {0}(1,1): {1}: {2}", temp,
            exn.GetType().FullName, exn.Message)
        1


[<EntryPoint>]
let main args =
    try
        let options = Options.Parse args

        guard <| fun _ ->

            // process extra.files
            Extra.CopyFiles options.ProjectDirectory.FullName options.OutputDirectory.FullName //"E:\\ifp-hplogin\\HPLogin\\WebSharperMobileProject"

            let assembly =
                System.Reflection.Assembly.LoadFile(options.SourceAssembly.FullName)

            let scriptDir =
                System.IO.Path.Combine(options.OutputDirectory.FullName, "Scripts")
                |> System.IO.Directory.CreateDirectory

            // Add source directories to search path
            for dir in options.SourceDirectories do
                Loader.AddSearchPath dir.FullName

            // Load the sitelet
            let (sitelet, actions) = (TryLoadSite options.SourceAssembly).Value
            //Compute the available assembly files and embedded resources.
            let assFiles =
                let m =
                    Output.ComputeResources options.Mode
                        options.SourceDirectories
                let d = System.Collections.Generic.Dictionary()
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
    with Options.BadOptions message ->
        stderr.WriteLine message
        stderr.WriteLine Options.Help
        -1

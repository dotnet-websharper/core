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

module IntelliFactory.WebSharper.Sitelets.Templating.Main

module Tool = IntelliFactory.WebSharper.Sitelets.Templating.Tool

module Options =
    let Namespace = ref Tool.Options.Default.Namespace
    let Page = ref Tool.Options.Default.Page
    let PrintHelp = ref false

let PrintHelp () =
    let a = System.Reflection.Assembly.GetExecutingAssembly()
    let v = System.Diagnostics.FileVersionInfo.GetVersionInfo a.Location
    [
        "IntelliFactory WebSharper(TM) XHTML to F# Compiler build " + v.ProductVersion
        "Copyright (c) IntelliFactory. All Rights Reserved."
        ""
        "Usage: xhtml2fs.exe [INPUT] [OPTIONS] "
        ""
        ""
        "--namespace <name>    Generate code inside the given namespace."
        "                      Short form: --ns or -ns."
        "--page <name>         Generate page with the given name."
        "                      Short form: --p or -p."
        "--help                Displays this message."
        "                      Short form: ---help or -help or /? or -?."
        ""
    ]
    |> String.concat System.Environment.NewLine
    |> printf "%s"

let guard action =
    try action (); 0 with exn ->
        let temp = System.IO.Path.GetTempFileName()
        System.IO.File.WriteAllText(temp, string exn)
        stdout.WriteLine("[Error] {0}(1,1): {1}: {2}", temp,
            exn.GetType().FullName, exn.Message)
        1

[<EntryPoint>]
let main _ =
    guard <| fun () ->
    let args = System.Environment.GetCommandLineArgs()
    let optionList =
        if args.Length > 2 then List.ofArray args.[2..] else []
    if args.Length > 1 then
        match args.[1] with
        | "--help" | "-help" | "/?" | "-?" ->
            PrintHelp ()
        | _ ->
            let filename = args.[1]
            let page = Tool.GetPageName filename
            Options.Page := page
            let rec PO args =
                match args with
                | "--namespace" :: ns :: rest | "--ns" :: ns :: rest | "-ns" :: ns :: rest ->
                    Options.Namespace := ns
                    PO rest
                | "--page" :: ns :: rest | "--p" :: ns :: rest | "-p" :: ns :: rest ->
                    Options.Page := ns
                    PO rest
                | "--help" :: rest | "-help" :: rest  | "/?" :: rest  ->
                    Options.PrintHelp := true
                    PO rest
                | [] ->
                    { Namespace = !(Options.Namespace)
                      Page = !(Options.Page) } : Tool.Options
                | _ ->
                    printf "warning: could not interpret all options\n"
                    { Namespace = !(Options.Namespace)
                      Page = !(Options.Page) } : Tool.Options
            let options = PO optionList
            if !(Options.PrintHelp) then
                PrintHelp ()
            printf "Processing '%s'...\n" filename
            try
                use writer = System.IO.File.CreateText(filename+".fs")
                use reader = System.IO.File.OpenText(filename)
                Tool.ParseToWriter options writer reader
            with
                | exc ->
                    printf "Error: %s" exc.Message
    else
        PrintHelp ()

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

namespace IntelliFactory.WebSharper.Sitelets.Build

open System.IO
open System.Xml
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
module Tool = IntelliFactory.WebSharper.Sitelets.Templating.Tool

type BuildPageTemplates() = 
    inherit Task()

    let mutable nameSpace = "Website"
    let mutable sources : ITaskItem[] = [||]

    let chooseTemplate (p: ITaskItem) =
        if p.ItemSpec.EndsWith ".template.xml"
        then Some (p.ItemSpec.Replace(".template.xml", ".template.fs"))
        else None

    member this.Namespace
        with get () = nameSpace
        and set x = nameSpace <- x

    [<Required>]
    member this.Source
        with set (value) = sources <- value

    [<Output>]
    member this.Output = 
        sources
        |> Array.map (fun x -> 
            match chooseTemplate x with
            | Some v -> v
            | None -> x.ItemSpec
            )

    member this.Convert(source : string, result : string) = 
        this.Log.LogMessage("Converting {0} to fs...", source)
        let isNewer a b =
            not (File.Exists a)
            || not (File.Exists b)
            || File.GetLastWriteTimeUtc a > File.GetLastWriteTimeUtc b
        try
            if isNewer source result then
                let opts =
                    {
                        Tool.Options.Default with
                            Page = Tool.GetPageName source
                            Namespace = nameSpace
                    }
                use reader = File.OpenText(source)
                use writer = File.CreateText(result)
                Tool.ParseToWriter opts writer reader
                this.Log.LogMessage("Conversion completed")
            true
        with
            | :? XmlException as xmlex ->
                this.Log.LogError(null, null, null, source, xmlex.LineNumber, xmlex.LinePosition, 0, 0, xmlex.Message, [||])
                false
            | e -> 
                this.Log.LogError(null, null, null, source, 0, 0, 0, 0, e.Message, [||])
                false

    override this.Execute() =
        try
            let ok = ref true
            for src in sources do
                match chooseTemplate src with
                | Some resultName -> 
                    let result = this.Convert(src.ItemSpec, resultName)
                    ok := !ok && result
                | None -> ()
            !ok
        with
            e -> this.Log.LogErrorFromException e; false

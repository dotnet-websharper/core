// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

/// Implements utilities for use by the current assembly.
namespace IntelliFactory.WebSharper.Sitelets

[<AutoOpen>]
module internal Extensions =
    open System.Diagnostics

    let private source =
        TraceSource("WebSharper", SourceLevels.All)

    let Timed message action =
        let sw = Stopwatch()
        sw.Start()
        let r = action()
        source.TraceInformation("{0} in {1} sec.",
            message, sw.Elapsed.TotalSeconds)
        r

    let joinWithSlash (a: string) (b: string) =
        let startsWithSlash (s: string) =
            s.Length > 0
            && s.[0] = '/'
        let endsWithSlash (s: string) =
            s.Length > 0
            && s.[s.Length - 1] = '/'
        match endsWithSlash a, startsWithSlash b with
        | true, true -> a + b.Substring(1)
        | false, false -> a + "/" + b
        | _ -> a + b

module internal ResourceContext =
    open System
    open System.Collections.Generic
    open System.Configuration
    open System.Diagnostics
    open System.IO
    open System.Reflection
    open System.Web
    open System.Web.Hosting
    module M = IntelliFactory.WebSharper.Core.Metadata
    module R = IntelliFactory.WebSharper.Core.Remoting
    module Re = IntelliFactory.WebSharper.Core.Resources

    let ServerRootPath =
        HostingEnvironment.MapPath "~/bin"

    let MetaData () : M.Info =
        IntelliFactory.WebSharper.Web.Shared.Metadata

    let SharedJson () =
        IntelliFactory.WebSharper.Web.Shared.Json

    let ResourceContext (appPath: string) : Re.Context =
        let page = new UI.Page()
        let isDebug = HttpContext.Current.IsDebuggingEnabled
        {
            DebuggingEnabled = isDebug
            DefaultToHttp = false
            GetSetting = fun (name: string) ->
                match ConfigurationManager.AppSettings.[name] with
                | null -> None
                | x -> Some x
            GetAssemblyRendering = fun name ->
                let suffix = if isDebug then ".dll.js" else ".dll.min.js"
                let url =
                    String.Format("Scripts/{0}{1}", page.Server.UrlEncode name.Name, suffix)
                    |> joinWithSlash appPath
                Re.RenderLink url
            GetWebResourceRendering = fun ty resource ->
                let url = page.ClientScript.GetWebResourceUrl(ty, resource)
                Re.RenderLink url
        }

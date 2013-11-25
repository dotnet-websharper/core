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

namespace IntelliFactory.WebSharper.Web

module J = IntelliFactory.WebSharper.Core.Json
module M = IntelliFactory.WebSharper.Core.Metadata
module R = IntelliFactory.WebSharper.Core.Reflection
module Re = IntelliFactory.WebSharper.Core.Resources

type private Conf = System.Configuration.ConfigurationManager
type private D<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>
type private Q<'T> = System.Collections.Generic.Queue<'T>

/// The script manager control takes care of providing resources
/// for the WebSharper pagelets used on the page. Every
/// ASP.NET page should have one instance of this
/// control in the head section.
[<Sealed>]
type ScriptManager() =
    inherit System.Web.UI.Control()
    do base.ID <- Shared.SCRIPT_MANAGER_ID
    let registry = D()
    let nodes = Q()
    let mutable k = 0
    let next () = k <- k + 1; k

    let getId id =
        match id with
        | Some id -> id
        | None -> System.String.Format("ws{0}", next ())

    member private this.ResourceContext : Re.Context =
        {
            DebuggingEnabled =
                this.Context.IsDebuggingEnabled
            DefaultToHttp = false
            GetSetting = fun key ->
                match Conf.AppSettings.[key] with
                | null -> None
                | x -> Some x
            GetAssemblyRendering = fun name ->
                let ext =
                    if System.Web.HttpContext.Current.IsDebuggingEnabled
                    then ".dll.js"
                    else ".dll.min.js"
                let url =
                    System.String.Format("~/Scripts/WebSharper/{0}{1}", name.Name, ext)
                    |> this.ResolveUrl
                Re.RenderLink url
            GetWebResourceRendering = fun t name ->
                let url = this.Page.ClientScript.GetWebResourceUrl(t, name)
                Re.RenderLink url
        }

    /// Registers a pagelet with the manager.
    member this.Register (id: option<string>) (c: System.Web.UI.Control) =
        let t = c.GetType()
        let t = if t.IsGenericType then t.GetGenericTypeDefinition() else t
        nodes.Enqueue(M.TypeNode (R.TypeDefinition.FromType t))
        let id  = getId id
        let enc = Shared.Json.GetEncoder t
        registry.[id] <- enc.Encode c
        id

    /// Renders the resources.
    override this.Render writer =
        writer.WriteLine()
        let content =
            J.Encoded.Object [for kv in registry -> (kv.Key, kv.Value)]
            |> Shared.Json.Pack
            |> J.Stringify
        let encode (text: string) =
            let ev =
                System.Text.RegularExpressions.MatchEvaluator(fun x ->
                    match x.Groups.[0].Value with
                    | "'" -> "&#39;"
                    | "<" -> "&lt;"
                    | ">" -> "&gt;"
                    | _ -> "&amp;")
            System.Text.RegularExpressions.Regex.Replace(text, @"['<>&]", ev)
        writer.WriteLine("<meta id='{0}' name='{0}' content='{1}' />",
            IntelliFactory.WebSharper.Html.Activator.META_ID, encode content)
        let ctx = this.ResourceContext
        Shared.Metadata.GetDependencies(Seq.toList nodes)
        |> Seq.iter (fun r -> r.Render ctx writer)
        writer.WriteLine()
        writer.WriteLine("<script type='text/javascript'>")
        writer.WriteLine @"if (typeof IntelliFactory !=='undefined')"
        writer.WriteLine @"  IntelliFactory.Runtime.Start();"
        writer.WriteLine("</script>")

    /// Searches the page for a ScriptManager.
    static member private TryFind(page: System.Web.UI.Page) =
        match page.Header with
        | null   -> None
        | header ->
            match header.FindControl Shared.SCRIPT_MANAGER_ID with
            | :? ScriptManager as m -> Some m
            | _ -> None

    /// Finds an instance of ScriptManager on the page, throwing an
    /// exception if it is not present.
    static member Find(page: System.Web.UI.Page) =
        match ScriptManager.TryFind page with
        | None ->
            failwith
                "WebSharper ScriptManager control was not found on the page. \
                 Please add an instance of the control to the head section of \
                 the page. The head section must have the RUNAT attribute \
                 set to SERVER."
        | Some c ->
            c

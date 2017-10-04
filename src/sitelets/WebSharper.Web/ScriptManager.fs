// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper.Web

module CT = WebSharper.Core.ContentTypes
module J = WebSharper.Core.Json
module M = WebSharper.Core.Metadata
module P = WebSharper.PathConventions
//module R = WebSharper.Core.Reflection
module Re = WebSharper.Core.Resources

type private Conf = System.Configuration.ConfigurationManager
type private D<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>
type private Q<'T> = System.Collections.Generic.Queue<'T>

/// The script manager control takes care of providing resources
/// for the WebSharper pagelets used on the page. Every
/// ASP.NET page should have one instance of this
/// control in the head section.
[<Sealed>]
type ScriptManager() =
#if NET461 // ASP.NET: Control
    inherit System.Web.UI.Control()
    do base.ID <- Shared.SCRIPT_MANAGER_ID
#endif

    let registry = D()
    let nodes = Q()
    let mutable k = 0
    let next () = k <- k + 1; k

    let getId id =
        match id with
        | Some id -> id
        | None -> System.String.Format("ws{0}", next ())

    member private this.ResourceContext : Re.Context =
        ResourceContext.ResourceContext "/"

    /// Registers a pagelet with the manager.
    member this.Register (id: option<string>) (c: WebSharper.IRequiresResources) =
        Seq.iter nodes.Enqueue c.Requires
        let id  = getId id
        c.Encode(Shared.Metadata, Shared.Json)
        |> List.iter (fun (k, v) -> registry.[k] <- v)
        id

#if NET461 // ASP.NET: Control
    override this.Render writer =
        this.Render(new Re.HtmlTextWriter(writer))
#endif

    /// Renders the resources.
    member this.Render (writer: Re.HtmlTextWriter) =
        let encode (text: string) =
            let ev =
                System.Text.RegularExpressions.MatchEvaluator(fun x ->
                    match x.Groups.[0].Value with
                    | "'" -> "&#39;"
                    | "<" -> "&lt;"
                    | ">" -> "&gt;"
                    | _ -> "&amp;")
            System.Text.RegularExpressions.Regex.Replace(text, @"['<>&]", ev)
        let ctx = this.ResourceContext
        let resources = 
            ctx.ResourceDependencyCache.GetOrAdd(Set nodes, fun nodes ->
                Shared.Dependencies.GetResources nodes
            )
        if not (List.isEmpty resources) then
            let content =
                J.Encoded.Object [for kv in registry -> (kv.Key, kv.Value)]
                |> Shared.Json.Pack
                |> J.Stringify
            writer.WriteLine()
            writer.WriteLine("<meta id='{0}' name='{0}' content='{1}' />",
                WebSharper.Activator.META_ID, encode content)
            resources |> Seq.iter (fun r -> Re.Rendering.RenderCached(ctx, r, (fun _ -> writer)))
            writer.WriteLine()
            writer.WriteLine("<script type='{0}'>", CT.Text.JavaScript.Text)
            writer.WriteLine @"if (typeof IntelliFactory !=='undefined')"
            writer.WriteLine @"  IntelliFactory.Runtime.Start();"
            writer.WriteLine("</script>")

#if NET461 // ASP.NET: Control
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
#endif

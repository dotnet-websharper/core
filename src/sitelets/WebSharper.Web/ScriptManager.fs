// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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
module R = WebSharper.Core.Reflection
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
        let isDebug = this.Context.IsDebuggingEnabled
        let pu = P.PathUtility.VirtualPaths("/")
        {
            DebuggingEnabled = isDebug
            DefaultToHttp = false
            GetSetting = fun (name: string) ->
                match Conf.AppSettings.[name] with
                | null -> None
                | x -> Some x
            GetAssemblyRendering = fun name ->
                let aid = P.AssemblyId.Create(name.FullName)
                let url = if isDebug then pu.JavaScriptPath(aid) else pu.MinifiedJavaScriptPath(aid)
                Re.RenderLink url
            GetWebResourceRendering = fun ty resource ->
                let id = P.AssemblyId.Create(ty)
                let kind =
                    if resource.EndsWith(".js") || resource.EndsWith(".ts")
                        then P.ResourceKind.Script
                        else P.ResourceKind.Content
                P.EmbeddedResource.Create(kind, id, resource)
                |> pu.EmbeddedPath
                |> Re.RenderLink
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
            WebSharper.Html.Client.Activator.META_ID, encode content)
        let ctx = this.ResourceContext
        Shared.Metadata.GetDependencies(Seq.toList nodes)
        |> Seq.iter (fun r -> r.Render ctx writer)
        writer.WriteLine()
        writer.WriteLine("<script type='{0}'>", CT.Text.JavaScript.Text)
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

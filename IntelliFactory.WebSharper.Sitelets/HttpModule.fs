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

namespace IntelliFactory.WebSharper.Sitelets

open System
open System.Collections.Generic
open System.Configuration
open System.Diagnostics
open System.IO
open System.Reflection
open System.Web
open System.Web.Hosting

module internal SiteLoading =

    let TryLoadSite (assembly: Assembly) =
        let aT = typeof<WebsiteAttribute>
        match System.Attribute.GetCustomAttribute(assembly, aT) with
        | :? WebsiteAttribute as attr ->
            attr.Run () |> Some
        |_ -> None

    let LoadFromAssemblies () =
        Timed "Initialized sitelets" <| fun () ->
            let assemblies =
                System.Web.Compilation.BuildManager.GetReferencedAssemblies()
                |> Seq.cast<System.Reflection.Assembly>
            let pairs = Seq.choose TryLoadSite assemblies
            let sitelets = Seq.map fst pairs
            let actions = Seq.map snd pairs
            (Sitelet.Sum sitelets, Seq.concat actions)

module private Site =
    let internal Current =
        SiteLoading.LoadFromAssemblies()

/// IIS module, processing the URLs and serving the pages.
type HttpModule() =
    let mutable dispose = ignore

    interface IHttpModule with
        member this.Init app =

            // Load sitelet
            let (site, actions) = Site.Current

            /// Handler for processing begin requests and respond with a page if one exists with the current URL.
            let beginhandler =
                EventHandler(fun (x: obj) (e: EventArgs)->
                    let app = (x :?> HttpApplication)

                    let METHOD = function
                        | "CONNECT" -> Http.Method.Connect
                        | "DELETE" -> Http.Method.Delete
                        | "GET" -> Http.Method.Get
                        | "HEAD" -> Http.Method.Head
                        | "OPTIONS" -> Http.Method.Options
                        | "POST" -> Http.Method.Post
                        | "PUT" -> Http.Method.Put
                        | "TRACE" -> Http.Method.Trace
                        | rest -> Http.Method.Custom rest

                    // Construct the request
                    let request : Http.Request=
                        let headers =
                            seq {
                                for key in app.Context.Request.Headers.AllKeys do
                                    yield Http.Header.Custom key app.Context.Request.Headers.[key]
                            }

                        // app.Context.Request.Cookies
                        let parameters =
                            seq {
                                for p in app.Context.Request.Params.AllKeys do
                                    yield (p, app.Context.Request.[p])
                            }
                        {
                            Method  = METHOD app.Context.Request.HttpMethod
                            Uri     = app.Context.Request.Url
                            Headers = headers
                            Body    = app.Context.Response.OutputStream
                            Post    = new Http.ParameterCollection(app.Context.Request.Form)
                            Get     = new Http.ParameterCollection(app.Context.Request.QueryString)
                            Cookies = app.Context.Request.Cookies
                            ServerVariables = new Http.ParameterCollection(app.Context.Request.ServerVariables)
                            Files =
                                let fs = app.Context.Request.Files
                                seq {
                                    for k in fs.Keys do
                                        yield fs.[k]
                                }
                        }

                    let appPath =
                        if app.Context.Request.ApplicationPath = "/" then
                            ""
                        else
                            app.Context.Request.ApplicationPath

                    let site = Sitelet.Shift appPath site

                    // Get the action
                    match site.Router.Route request with
                    | Some action ->
                        // Create a context
                        let context =
                            {
                                ApplicationPath = appPath
                                ResolveUrl = fun url ->
                                    if url.StartsWith ("~") then
                                        appPath + url.Substring(1)
                                    else
                                        url
                                Json = ResourceContext.SharedJson()
                                Link = fun action ->
                                    match site.Router.Link action  with
                                    | Some loc -> loc.ToString()
                                    | None -> failwith "Failed to link to action"

                                Metadata = ResourceContext.MetaData()
                                ResourceContext = ResourceContext.ResourceContext appPath
                                Request = request
                            }

                        // Handle action
                        match site.Controller.Handle action |> Content.ToCustomContent with
                        | CustomContent genResponse ->
                            let response = genResponse context
                            app.Context.Response.Status <- response.Status.ToString()
                            for header in response.Headers do
                                app.Context.Response.AddHeader(header.Name, header.Value)

                            response.WriteBody app.Context.Response.OutputStream
                            app.Response.End()
                        | _ ->
                            ()
                    | None ->
                        ()
                )

            let endhandler =
                EventHandler(fun (x: obj) (e: EventArgs)-> () )
            app.add_AuthorizeRequest beginhandler
            app.add_EndRequest endhandler

        /// Release the handlers
        member this.Dispose() = ()

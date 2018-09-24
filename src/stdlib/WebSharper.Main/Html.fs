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

namespace WebSharper

open WebSharper
open WebSharper.JavaScript
module M = WebSharper.Core.Metadata
module J = WebSharper.Core.Json

/// An interface that has to be implemented by controls
/// that depend on resources.
type IRequiresResources =
    abstract member Requires : M.Info -> seq<M.Node>
    abstract member Encode : M.Info * J.Provider -> list<string * J.Encoded>

/// HTML content that can be used as the Body of a web Control.
/// Can be zero, one or many DOM nodes.
type IControlBody =
    /// Replace the given node with the HTML content.
    /// The node is guaranteed to be present in the DOM.
    /// Called exactly once on startup on an IControl's Body.
    [<JavaScript; Name "ReplaceInDom">]
    abstract ReplaceInDom : Dom.Node -> unit

/// An interface that has to be implemented by controls that
/// are subject to activation, ie. server-side controls that
/// contain client-side elements.
type IControl =
    inherit IRequiresResources
    [<JavaScript; Name "Body">]
    abstract member Body : IControlBody
    abstract member Id : string

[<AutoOpen>]
module HtmlContentExtensions =

    [<JavaScript>]
    type private SingleNode(node: Dom.Node) =
        interface IControlBody with
            member this.ReplaceInDom(old) =
                node.ParentNode.ReplaceChild(node, old) |> ignore

    [<JavaScript>]
    type IControlBody with
        /// Create HTML content comprised of a single DOM node.
        static member SingleNode (node: Dom.Node) =
            new SingleNode(node) :> IControlBody

[<JavaScript>]
module Activator =

    /// The identifier of the meta tag holding the controls.
    [<Literal>]
    let META_ID = "websharper-data"

    let mutable Instances : obj = null

    let private onReady (f: unit -> unit) =
        let mutable readyFired = false
        let rec ready() =
            if not readyFired then
                readyFired <- true
                f()
                JS.Document.RemoveEventListener("DOMContentLoaded", ready, false)
                JS.Window.RemoveEventListener("load", ready, false)
        if JS.Document?readyState = "complete" then
            ready()
        else
            JS.Document.AddEventListener("DOMContentLoaded", ready, false)
            JS.Window.AddEventListener("load", ready, false)

    let private Activate() =
        if As JS.Document then
            let meta = JS.Document.GetElementById(META_ID)
            if (As meta) then
                onReady <| fun () ->
                    let text = meta.GetAttribute("content")
                    let obj = Json.Activate (Json.Parse text)
                    JS.GetFields obj
                    |> Array.iter (fun (k, v) ->
                        match v with
                        | :? IControl as v ->
                            let p = v.Body
                            let old = JS.Document.GetElementById k
                            p.ReplaceInDom old
                        | _ -> ()
                    )
                    Instances <- obj

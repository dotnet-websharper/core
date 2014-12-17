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

/// Initializes all controls found on the page.
module IntelliFactory.WebSharper.Html.Activator

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JavaScript
module H = IntelliFactory.WebSharper.Html.Default

/// The identifier of the meta tag holding the controls.
[<Literal>]
let META_ID = "websharper-data"

/// An interface that has to be implemented by controls that
/// are subject to activation.
type IControl =
    abstract member Body : IPagelet
    abstract member Id : string

[<Direct "typeof document !== 'undefined'">]
let private hasDocument () = false

[<JavaScript>]
let private Activate =
    if hasDocument () then
        let meta = JS.Document.GetElementById(META_ID)
        if (As meta) then
            H.OnLoad (fun () ->
                let text = meta.GetAttribute("content")
                let obj = Json.Activate (Json.Parse text)
                JS.GetFields obj
                |> Array.iter (fun (k, v) ->
                    let p : IPagelet = (As<IControl> v).Body
                    let old = JS.Document.GetElementById k
                    ignore (old.ParentNode.ReplaceChild(p.Body, old))
                    p.Render()))

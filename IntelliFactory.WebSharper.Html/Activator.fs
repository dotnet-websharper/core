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

/// Initializes all controls found on the page.
module IntelliFactory.WebSharper.Html.Activator

open IntelliFactory.WebSharper
module H = IntelliFactory.WebSharper.Html.Default
module J = IntelliFactory.WebSharper.JavaScript

/// The identifier of the meta tag holding the controls.
[<Literal>]
let META_ID = "websharper-data"

/// An interface that has to be implemented by controls that
/// are subject to activation.
type IControl =
    abstract member Body : IPagelet

[<JavaScript>]
let private Activate =
    H.OnLoad (fun () ->
        let doc = Dom.Document.Current
        let meta = doc.GetElementById META_ID
        let text = meta.GetAttribute "content"
        let obj = Json.Activate (Json.Parse text)
        J.GetFields obj
        |> Array.iter (fun (k, v) ->
            let p : IPagelet = (As<IControl> v).Body
            let old = doc.GetElementById k
            ignore (old.ParentNode.ReplaceChild(p.Body, old))
            p.Render()))

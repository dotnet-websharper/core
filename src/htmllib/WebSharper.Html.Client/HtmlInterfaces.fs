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

namespace WebSharper.Html.Client

open WebSharper
open WebSharper.JavaScript

[<AbstractClass>]
[<JavaScript>]
type Pagelet() =

    abstract member Body : Dom.Node

    abstract member Render : unit -> unit

    interface IControlBody with
        member this.ReplaceInDom(node: Dom.Node) =
            node.ParentNode.ReplaceChild(this.Body, node) |> ignore
            this.Render()

    member this.AppendTo(targetId: string) =
        let target = JS.Document.GetElementById(targetId)
        target.AppendChild(this.Body) |> ignore
        this.Render()

module Interfaces =

    /// Interface for required HTML/DOM functionality.
    type IHtmlProvider =

        // Construct
        abstract member CreateTextNode  : string -> Dom.Text
        abstract member CreateElement   : string -> Dom.Element

        // Attributes
        abstract member SetAttribute    : Dom.Node -> string -> string -> unit
        abstract member AppendAttribute : Dom.Node -> Dom.Attr -> unit
        abstract member RemoveAttribute : Dom.Node -> string -> unit
        abstract member GetAttribute    : Dom.Node -> string -> string
        abstract member HasAttribute    : Dom.Node -> string -> bool
        abstract member CreateAttribute : string -> Dom.Attr

        // Property
        abstract member GetProperty<'T> : Dom.Node -> string -> 'T
        abstract member SetProperty<'T> : Dom.Node -> string -> 'T -> unit

        // Append/Remove
        abstract member AppendNode      : Dom.Node -> Dom.Node -> unit
        abstract member Clear           : Dom.Node -> unit
        abstract member Remove          : Dom.Node -> unit

        // Text
        abstract member SetText         : Dom.Node -> string -> unit
        abstract member GetText         : Dom.Node -> string

        // Html
        abstract member SetHtml         : Dom.Node -> string -> unit
        abstract member GetHtml         : Dom.Node -> string

        // Value
        abstract member SetValue        : Dom.Node -> string -> unit
        abstract member GetValue        : Dom.Node -> string

        // Style
        abstract member SetStyle        : Dom.Node -> string -> unit

        // Css
        abstract member SetCss          : Dom.Node -> string -> string -> unit

        // Class
        abstract member AddClass        : Dom.Node -> string -> unit
        abstract member RemoveClass     : Dom.Node -> string -> unit

        abstract member OnLoad          : Dom.Node -> (unit -> unit) -> unit

        abstract member OnDocumentReady : (unit -> unit) -> unit







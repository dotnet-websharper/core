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

namespace IntelliFactory.WebSharper.Html

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Dom

/// IPagelet
type IPagelet =
    abstract member Render : unit -> unit
    abstract member Body : Dom.Node

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







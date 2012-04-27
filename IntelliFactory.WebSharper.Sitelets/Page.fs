// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

open System.Collections.Generic
open System.Web.UI
open IntelliFactory.Html
open IntelliFactory.WebSharper
type private Writer = HtmlTextWriter -> unit
type private Control = IntelliFactory.WebSharper.Web.Control

/// Represents HTML pages with embedded WebSharper controls.
type Page =
    {
        Doctype : option<string>
        Title : option<string>
        Renderer : option<string> -> option<string> -> Writer -> Writer ->
            HtmlTextWriter -> unit
        Head : seq<Element<unit>>
        Body : seq<Element<Control>>
    }

    static member Default =
        let renderer (doctype : option<string>) (title: option<string>)
            writeHead writeBody (writer: System.Web.UI.HtmlTextWriter) =
            // Doctype
            match doctype with
            | Some dt -> writer.WriteLine dt
            | None -> ()
            writer.RenderBeginTag HtmlTextWriterTag.Html
            // Head section
            writer.RenderBeginTag HtmlTextWriterTag.Head
            writer.WriteLine()
            writeHead writer
            match title with
            | Some t ->
                writer.RenderBeginTag HtmlTextWriterTag.Title
                writer.Write t
                writer.RenderEndTag ()
            | None -> ()
            writer.RenderEndTag()
            // Body section
            writer.RenderBeginTag HtmlTextWriterTag.Body
            writer.WriteLine()
            writeBody writer
            writer.RenderEndTag()
            writer.RenderEndTag()
        {
            Doctype = Some "<!DOCTYPE html>"
            Title = None
            Head = []
            Renderer = renderer
            Body = []
        }

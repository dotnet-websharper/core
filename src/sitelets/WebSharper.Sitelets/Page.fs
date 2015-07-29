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

namespace WebSharper.Sitelets

open System.Collections.Generic
open System.Web.UI
open WebSharper.Html.Server
open WebSharper
type private Writer = HtmlTextWriter -> unit
type private IControl = WebSharper.Html.Client.IControl

type Page =
    {
        Doctype : option<string>
        Title : option<string>
        Renderer : option<string> -> option<string> -> Writer -> Writer ->
            HtmlTextWriter -> unit
        Head : seq<Element>
        Body : seq<Element>
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
            match title with
            | Some t ->
                writer.WriteFullBeginTag "title"
                writer.Write t
                writer.WriteEndTag "title"
                writer.WriteLine()
            | None -> ()
            writeHead writer
            writer.RenderEndTag()
            // Body section
            writer.RenderBeginTag HtmlTextWriterTag.Body
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

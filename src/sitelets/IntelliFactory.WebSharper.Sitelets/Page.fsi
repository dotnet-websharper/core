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

namespace IntelliFactory.WebSharper.Sitelets

open System.Web.UI
open IntelliFactory.WebSharper.Html.Server
open IntelliFactory.WebSharper
type private Writer = HtmlTextWriter -> unit
type private IControl = IntelliFactory.WebSharper.Html.Client.IControl

/// Represents HTML pages with embedded WebSharper controls.
type Page =
    {
        /// Doctype tag.
        /// Default is the standard HTML5 doctype: "<!DOCTYPE html>"
        Doctype : option<string>
        /// Title of the page, ie. contents of the <title> tag.
        Title : option<string>
        Renderer : option<string> -> option<string> -> Writer -> Writer ->
            HtmlTextWriter -> unit
        /// Head of the page, ie. contents of the <head> tag.
        /// WebSharper-generated tags, such as script dependencies,
        /// are appended to this head.
        Head : seq<Element>
        /// Body of the page, ie. contents of the <body> tag.
        Body : seq<Element>
    }

    /// A default, empty page.
    /// Use the `{ Page.Default with ... }` syntax to create your own pages.
    static member Default : Page

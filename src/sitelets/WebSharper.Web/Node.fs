// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

open System.Web.UI
open WebSharper
module M = WebSharper.Core.Metadata

type INode =
    inherit IRequiresResources

    abstract member Write : M.Info * HtmlTextWriter -> unit

    abstract member Name : string

    abstract member IsAttribute : bool

    abstract member AttributeValue : string option

[<AutoOpen>]
module HtmlTextWriterExtensions =

    type HtmlTextWriter with

        /// Checks whether an element should be rendered as self-closing,
        /// ie. <x /> instead of <x></x>
        static member IsSelfClosingTag (name: string) =
            List.exists ((=) (name.ToLower())) [
                "area"
                "base"
                "basefont"
                "br"
                "col"
                "frame"
                "hr"
                "img"
                "input"
                "isindex"
                "link"
                "meta"
                "param"
            ]

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
open WebSharper.Html.Client.Interfaces

/// Represents HTML attributes.
[<JavaScript>]
type internal Attribute private (HtmlProvider) =
    inherit Pagelet()

    [<DefaultValue>]
    val mutable Name : string

    [<DefaultValue>]
    val mutable Value: string

    static member New(htmlProvider: IHtmlProvider, name: string, value: string) =
        let a = new Attribute(htmlProvider)
        a.Name <- name
        a.Value <- value
        a

    override this.Body =
        let attr = HtmlProvider.CreateAttribute this.Name
        attr.Value <- this.Value
        attr :> _

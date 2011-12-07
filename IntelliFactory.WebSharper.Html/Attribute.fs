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
open IntelliFactory.WebSharper.Html.Interfaces

/// Represents HTML attributes.
type internal Attribute [<JavaScript>] private (HtmlProvider) =

    [<DefaultValue>]
    val mutable Name : string

    [<DefaultValue>]
    val mutable Value: string

    [<JavaScript>]
    static member New(htmlProvider: IHtmlProvider, name: string, value: string) =
        let a = new Attribute(htmlProvider)
        a.Name <- name
        a.Value <- value
        a

    interface IPagelet with
        [<JavaScript>]
        member this.Body =
            let attr = HtmlProvider.CreateAttribute this.Name
            attr.NodeValue <- this.Value
            attr :> _

        [<JavaScript>]
        member this.Render () = ()

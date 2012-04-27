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

namespace IntelliFactory.WebSharper.Html

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html.Interfaces

[<AutoOpen>]
module Operators =

    [<JavaScript>]
    [<Name "add">]
    let ( -< ) (el: Element) (inner: seq<#IPagelet>) =
        for pl in inner do
            el.Append (pl :> IPagelet)
        el

    [<Inline>]
    [<JavaScript>]
    let ( -- ) (el: Element) (inner: #IPagelet) =
        el -< [inner]

    [<Inline "$f.apply($o)">]
    let private Apply f o = ()

    /// Destructively adds an function to be invoked after the render is called on
    /// the pagelet.
    [<JavaScript>]
    let OnAfterRender<'T when 'T :> IPagelet> (f: 'T -> unit) (w: 'T) : unit =
        let r = w?Render
        w?Render <- fun () ->
            Apply r w
            f w

    /// Destructively adds an function to be invoked before the render is called on
    /// the pagelet.
    [<JavaScript>]
    let OnBeforeRender<'T when 'T :> IPagelet> (f: 'T -> unit) (w: 'T) : unit =
        let r = w?Render
        w?Render <- fun () ->
            f w
            Apply r w

    // Generates a new unique id.
    [<JavaScript>]
    [<Inline>]
    let NewId () = Utils.NewId ()

    /// Looks up dom element by id
    [<Inline "document.getElementById($name)">]
    let ById (name: string) : Dom.Element = Unchecked.defaultof<_>()




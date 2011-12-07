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

module internal Utils =

    [<Inline "'id' + Math.round(Math.random() * 1E8)">]
    let NewId () = ""

type Element [<JavaScript>] (HtmlProvider : IHtmlProvider) =

    [<DefaultValue>]
    val mutable private RenderInternal : unit -> unit

    [<DefaultValue>]
    val mutable Body : Dom.Element

    [<DefaultValue>]
    val mutable private IsRendered : bool

    /// An alias for Body.
    [<JavaScript>]
    member this.Dom with [<Inline>] get () = this.Body

    [<JavaScript>]
    static member New(html: IHtmlProvider, name: string) : Element =
        let el = new Element(html)
        let dom = Dom.Document.Current.CreateElement(name)
        el.RenderInternal <- ignore
        el.Body <- dom
        el.IsRendered <- false
        el

    interface IPagelet with

        [<JavaScript>]
        member this.Render () =
            if not this.IsRendered then
                this.RenderInternal ()
                this.IsRendered <- true

        [<JavaScript>]
        member this.Body = this.Body :> _

    [<JavaScript>]
    member this.Text
        with get () = HtmlProvider.GetText this.Body
        and set x = HtmlProvider.SetText this.Body x

    [<JavaScript>]
    member this.Html
        with get () = HtmlProvider.GetHtml this.Body
        and set x = HtmlProvider.SetHtml this.Body x

    [<JavaScript>]
    member this.Value
        with get () = HtmlProvider.GetValue this.Body
        and set (x: string) = HtmlProvider.SetValue this.Body x

    [<JavaScript>]
    member this.Id
        with get () =
            let id = HtmlProvider.GetProperty this.Body "id"
            // Sets the id if empty
            if id = JavaScript.Undefined<string> || id = "" then
                let newId = Utils.NewId ()
                HtmlProvider.SetProperty this.Body "id"  newId
                newId
            else
                id

    [<JavaScript>]
    member this.OnLoad f =
        HtmlProvider.OnLoad this.Body f

    [<JavaScript>]
    member this.HtmlProvider
        with get () = HtmlProvider

    [<JavaScript>]
    [<Name "AppendI">]
    member this.Append(pl: IPagelet) =
        // Check if attribute
        if (unbox pl.Body.NodeType) = 2 then
            let attr = pl.Body :?> Dom.Attr
            HtmlProvider.AppendAttribute this.Body attr
        else
            HtmlProvider.AppendNode this.Body pl.Body
        // Call render if outer component already is rendered.
        if this.IsRendered then
            pl.Render()
        else
            let r = this.RenderInternal
            this.RenderInternal <- fun () ->
                r ()
                pl.Render ()

    [<JavaScript>]
    [<Name "AppendN">]
    member this.Append(node: Dom.Node) =
        HtmlProvider.AppendNode this.Body node

    [<Inline>]
    [<JavaScript>]
    [<Name "AppendT">]
    member this.Append (text: string) =
        HtmlProvider.AppendNode this.Body (HtmlProvider.CreateTextNode text)

    [<Inline>]
    [<JavaScript>]
    member this.Clear () =
        HtmlProvider.Clear this.Body

    [<Inline>]
    [<JavaScript>]
    member this.Remove() =
        HtmlProvider.Remove this.Body

    [<Inline>]
    [<JavaScript>]
    member this.SetAttribute(name: string, value: string) =
        HtmlProvider.SetAttribute this.Body name value

    [<Inline>]
    [<JavaScript>]
    member this.HasAttribute(name: string) =
        HtmlProvider.HasAttribute this.Body name

    [<Inline>]
    [<JavaScript>]
    member this.GetAttribute(name: string) =
        HtmlProvider.GetAttribute this.Body name

    [<Inline>]
    [<JavaScript>]
    member this.RemoveAttribute(name: string) =
        HtmlProvider.RemoveAttribute this.Body name

    [<Inline>]
    [<JavaScript>]
    member this.SetCss(name:string, prop: string) =
        HtmlProvider.SetCss this.Body name prop

    [<Inline>]
    [<JavaScript>]
    member this.SetStyle(style: string) =
        HtmlProvider.SetStyle this.Body style


    [<Inline>]
    [<JavaScript>]
    member this.AddClass(cls: string) =
        HtmlProvider.AddClass this.Body cls

    [<Inline>]
    [<JavaScript>]
    member this.RemoveClass(cls: string) =
        HtmlProvider.RemoveClass this.Body cls

    [<Inline>]
    [<JavaScript>]
    member this.GetProperty(name: string) =
        HtmlProvider.GetProperty this.Body name

    [<Inline>]
    [<JavaScript>]
    member this.SetProperty(name: string,  value)=
        HtmlProvider.SetProperty this.Body name value

    
    [<JavaScript>]
    member this.Item
        with get (name: string) =
            let prop = HtmlProvider.GetAttribute this.Body name
            HtmlProvider.GetAttribute this.Body name
        and set (name: string) (value: string) =
            HtmlProvider.SetAttribute this.Body name value

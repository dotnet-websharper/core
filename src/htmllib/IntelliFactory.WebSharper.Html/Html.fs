// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

module internal Implementation =
    open IntelliFactory.WebSharper
    open Interfaces
    open IntelliFactory.WebSharper.JQuery

    /// jQuery based HTMLProvider.
    type JQueryHtmlProvider[<JavaScript>]() =
        interface IHtmlProvider with

            [<JavaScript>]
            member this.CreateTextNode str =
                Dom.Document.Current.CreateTextNode(str)

            [<JavaScript>]
            member this.CreateAttribute str =
                Dom.Document.Current.CreateAttribute str

            [<JavaScript>]
            member this.CreateElement name =
                Dom.Document.Current.CreateElement(name)

            [<JavaScript>]
            member this.AppendAttribute node attr =
                (this :> IHtmlProvider).SetAttribute node attr.NodeName attr.Value

            [<JavaScript>]
            member this.AppendNode node el =
                JQuery.Of(node).Append(JQuery.Of(el)).Ignore

            [<JavaScript>]
            member this.GetText node =
                node.TextContent

            [<JavaScript>]
            member this.SetText node text =
                node.TextContent <- text

            [<JavaScript>]
            member this.AddClass node cls =
                JQuery.Of(node).AddClass(cls).Ignore

            [<JavaScript>]
            member this.Clear node =
                JQuery.Of(node).Contents().Detach().Ignore

            [<JavaScript>]
            member this.GetHtml node =
                JQuery.Of(node).Html()

            [<JavaScript>]
            member this.SetHtml node text =
                JQuery.Of(node).Html(text).Ignore

            [<JavaScript>]
            member this.GetValue node =
                As <| JQuery.Of(node).Val()

            [<JavaScript>]
            member this.SetValue node value =
                JQuery.Of(node).Val(value).Ignore

            [<JavaScript>]
            member this.Remove node =
                JQuery.Of(node).Remove().Ignore

            [<JavaScript>]
            member this.SetAttribute node name value =
                JQuery.Of(node).Attr(name, value).Ignore

            [<JavaScript>]
            member this.HasAttribute node name =
                JQuery.Of(node).Attr(name) !=. null

            [<JavaScript>]
            member this.GetAttribute node name =
                JQuery.Of(node).Attr(name)

            [<JavaScript>]
            member this.GetProperty node name =
                As <| JQuery.Of(node).Attr(name)

            [<JavaScript>]
            member this.SetProperty node name value =
                As <| JQuery.Of(node).Prop(name, value)

            [<JavaScript>]
            member this.RemoveAttribute node name =
                JQuery.Of(node).RemoveAttr(name).Ignore

            [<JavaScript>]
            member this.SetCss node name prop =
                JQuery.Of(node).Css(name, prop).Ignore

            [<JavaScript>]
            member this.SetStyle node style =
                JQuery.Of(node).Attr("style", style).Ignore

            [<JavaScript>]
            member this.RemoveClass node cls =
                JQuery.Of(node).RemoveClass(cls).Ignore

            [<JavaScript>]
            member this.OnLoad node f =
                JQuery.Of(node).Ready(f).Ignore

            [<JavaScript>]
            member this.OnDocumentReady f =
                JQuery.Of(Dom.Document.Current).Ready(f).Ignore


    [<JavaScript>]
    let private HtmlProvider = new JQueryHtmlProvider()

    [<JavaScript>]
    let Attr = new AttributeBuilder(HtmlProvider)

    [<JavaScript>]
    let Tags = new TagBuilder(HtmlProvider)

    module HTML5 =

        [<JavaScript>]
        let private Html5Provider = new JQueryHtmlProvider()

        [<JavaScript>]
        let Tags = new Html5TagBuilder(Html5Provider)

        [<JavaScript>]
        let Attr = new Html5AttributeBuilder(Html5Provider)
        
    [<JavaScript>]
    let DeprecatedHtml = DeprecatedTagBuilder(HtmlProvider)

    [<Inline>]
    [<JavaScript>]
    let OnDocumentReady f =
        (HtmlProvider :> IHtmlProvider).OnDocumentReady f

[<AutoOpen>]
module Default =
    open IntelliFactory.WebSharper

    module HTML5 =

        [<JavaScript>]
        let Tags = Implementation.HTML5.Tags

        [<JavaScript>]
        let Attr = Implementation.HTML5.Attr

    [<JavaScript>]
    let Tags = Implementation.Tags

    [<JavaScript>]
    let Deprecated = Implementation.DeprecatedHtml

    [<JavaScript>]
    let OnLoad (init: unit -> unit) : unit =
        Implementation.OnDocumentReady init

    [<JavaScript>]
    let Attr = Implementation.Attr

    [<JavaScript>]
    let Text x = Tags.Text x

    [<JavaScript>]
    let A x = Tags.A x

    [<Inline>]
    [<JavaScript>]
    let Abbr x = Tags.Abbr x

    [<Inline>]
    [<JavaScript>]
    let Acronym x = Tags.Acronym x

    [<Inline>]
    [<JavaScript>]
    let Address x = Tags.Address x

    [<Inline>]
    [<JavaScript>]
    let Anchor x = Tags.Anchor x

    [<Inline>]
    [<JavaScript>]
    let Area x = Tags.Area x

    [<JavaScript>]
    let B x = Tags.B x

    [<Inline>]
    [<JavaScript>]
    let Base x = Tags.Base x

    [<Inline>]
    [<JavaScript>]
    let Bdo x = Tags.Bdo

    [<Inline>]
    [<JavaScript>]
    let Big x = Tags.Big x

    [<Inline>]
    [<JavaScript>]
    let BlockQuote x = Tags.BlockQuote x

    [<JavaScript>]
    let Body x = Tags.Body x

    [<JavaScript>]
    let Br x = Tags.Br x

    [<JavaScript>]
    let Button x = Tags.Button x

    [<Inline>]
    [<JavaScript>]
    let Caption x = Tags.Caption x

    [<JavaScript>]
    let Code x = Tags.Code x

    [<Inline>]
    [<JavaScript>]
    let Cite x= Tags.Cite x

    [<Inline>]
    [<JavaScript>]
    let Col x = Tags.Col x

    [<Inline>]
    [<JavaScript>]
    let ColGroup x = Tags.ColGroup x

    [<Inline>]
    [<JavaScript>]
    let DD x = Tags.DD x

    [<Inline>]
    [<JavaScript>]
    let Del x = Tags.Del x

    [<JavaScript>]
    let Div x = Tags.Div x

    [<Inline>]
    [<JavaScript>]
    let Dfn x = Tags.Dfn x

    [<Inline>]
    [<JavaScript>]
    let DL x = Tags.DL x

    [<Inline>]
    [<JavaScript>]
    let DT x = Tags.DT x

    [<JavaScript>]
    let Em x = Tags.Em x

    [<Inline>]
    [<JavaScript>]
    let FieldSet x = Tags.FieldSet x

    [<JavaScript>]
    let Form x = Tags.Form x

    [<Inline>]
    [<JavaScript>]
    let Frame x  = Tags.Frame x

    [<Inline>]
    [<JavaScript>]
    let FrameSet x = Tags.FrameSet x

    [<JavaScript>]
    let H1 x = Tags.H1 x

    [<JavaScript>]
    let H2 x = Tags.H2 x

    [<JavaScript>]
    let H3 x = Tags.H3 x

    [<JavaScript>]
    let H4 x = Tags.H4 x

    [<Inline>]
    [<JavaScript>]
    let H5 x = Tags.H5 x

    [<Inline>]
    [<JavaScript>]
    let H6 x = Tags.H6 x

    [<JavaScript>]
    let Head x = Tags.Head x

    [<JavaScript>]
    let Hr x = Tags.Hr x

    [<JavaScript>]
    let I x = Tags.I x

    [<JavaScript>]
    let IFrame x = Tags.IFrame x

    [<JavaScript>]
    let Img x = Tags.Img x

    [<JavaScript>]
    let Input x = Tags.Input x

    [<Inline>]
    [<JavaScript>]
    let Ins x = Tags.Ins x

    [<Inline>]
    [<JavaScript>]
    let Kbd x = Tags.Kbd x

    [<Inline>]
    [<JavaScript>]
    let Label x = Tags.Label x

    [<Inline>]
    [<JavaScript>]
    let Legend x = Tags.Legend x

    [<JavaScript>]
    let LI x = Tags.LI x

    [<Inline>]
    [<JavaScript>]
    let Link x = Tags.Link x

    [<Inline>]
    [<JavaScript>]
    let Meta x = Tags.Meta x

    [<Inline>]
    [<JavaScript>]
    let NoFrames x = Tags.NoFrames x

    [<Inline>]
    [<JavaScript>]
    let NoScript x = Tags.NoScript x

    [<JavaScript>]
    let OL x =  Tags.OL x

    [<Inline>]
    [<JavaScript>]
    let OptGroup x = Tags.OptGroup x

    [<JavaScript>]
    let P x = Tags.P x

    [<Inline>]
    [<JavaScript>]
    let Param x = Tags.Param x

    [<JavaScript>]
    let Pre x = Tags.Pre x

    [<Inline>]
    [<JavaScript>]
    let Q x = Tags.Q x

    [<Inline>]
    [<JavaScript>]
    let Samp x = Tags.Samp x

    [<JavaScript>]
    let Script x = Tags.Script x

    [<JavaScript>]
    let Select x = Tags.Select x

    [<Inline>]
    [<JavaScript>]
    let Small x = Tags.Small x

    [<JavaScript>]
    let Span x = Tags.Span x

    [<Inline>]
    [<JavaScript>]
    let Strong x = Tags.Strong x

    [<Inline>]
    [<JavaScript>]
    let Sub x = Tags.Sub x

    [<Inline>]
    [<JavaScript>]
    let Sup x = Tags.Sup x

    [<JavaScript>]
    let Table x  = Tags.Table x

    [<JavaScript>]
    let TBody x = Tags.TBody x

    [<JavaScript>]
    let TD x = Tags.TD x

    [<JavaScript>]
    let TextArea x = Tags.TextArea x

    [<JavaScript>]
    let TFoot x = Tags.TFoot x

    [<JavaScript>]
    let TH x = Tags.TH x

    [<JavaScript>]
    let THead x = Tags.THead x

    [<JavaScript>]
    let TR x = Tags.TR x

    [<Inline>]
    [<JavaScript>]
    let TT  x = Tags.TT x

    [<JavaScript>]
    let UL x = Tags.UL x

    (* Attributes *)

    [<JavaScript>]
    let NewAttr x = Attr.NewAttr x

    [<JavaScript>]
    let Action x = Attr.Action x

    [<JavaScript>]
    let Align x = Attr.Align x

    [<JavaScript>]
    let Alt x = Attr.Alt x

    [<Inline>]
    [<JavaScript>]
    let AltCode x = Attr.AltCode x

    [<Inline>]
    [<JavaScript>]
    let Archive x = Attr.Archive x

    [<Inline>]
    [<JavaScript>]
    let Border x = Attr.Border x

    [<Inline>]
    [<JavaScript>]
    let BorderColor x = Attr.BorderColor x

    [<Inline>]
    [<JavaScript>]
    let CellPadding x = Attr.CellPadding x

    [<Inline>]
    [<JavaScript>]
    let CellSpacing x = Attr.CellSpacing x

    [<Inline>]
    [<JavaScript>]
    let Checked x = Attr.Checked x

    [<Inline>]
    [<JavaScript>]
    let Codebase x = Attr.Codebase x

    [<Inline>]
    [<JavaScript>]
    let ColSpan x = Attr.ColSpan x

    [<Inline>]
    [<JavaScript>]
    let Cols x = Attr.Cols x

    [<Inline>]
    [<JavaScript>]
    let Content x = Attr.Content x

    [<Inline>]
    [<JavaScript>]
    let Coords x = Attr.Coords x

    [<Inline>]
    [<JavaScript>]
    let Disabled x = Attr.Disabled x

    [<Inline>]
    [<JavaScript>]
    let EncType x = Attr.EncType x

    [<JavaScript>]
    let HRef x = Attr.HRef x

    [<Inline>]
    [<JavaScript>]
    let HRefLang x = Attr.HRefLang x

    [<JavaScript>]
    let Height x = Attr.Height x

    [<Inline>]
    [<JavaScript>]
    let HttpEquiv x = Attr.HttpEquiv x

    [<JavaScript>]
    let Id x = Attr.Id x

    [<Inline>]
    [<JavaScript>]
    let IsMap x = Attr.IsMap x

    [<Inline>]
    [<JavaScript>]
    let Lang x = Attr.Lang x

    [<Inline>]
    [<JavaScript>]
    let MaxLength x = Attr.MaxLength x

    [<Inline>]
    [<JavaScript>]
    let Method x = Attr.Method x

    [<Inline>]
    [<JavaScript>]
    let Multiple x = Attr.Multiple x

    [<JavaScript>]
    let Name x = Attr.Name x

    [<Inline>]
    [<JavaScript>]
    let NoHRef x = Attr.NoHRef x

    [<Inline>]
    [<JavaScript>]
    let Rel x = Attr.Rel x

    [<Inline>]
    [<JavaScript>]
    let Rev x = Attr.Rev x

    [<JavaScript>]
    let RowSpan x = Attr.RowSpan x

    [<Inline>]
    [<JavaScript>]
    let Rows x = Attr.Rows x

    [<Inline>]
    [<JavaScript>]
    let Rules x = Attr.Rules x

    [<JavaScript>]
    let Selected x = Attr.Selected x

    [<Inline>]
    [<JavaScript>]
    let Shape x = Attr.Shape x

    [<Inline>]
    [<JavaScript>]
    let Size x = Attr.Size x

    [<JavaScript>]
    let Src x = Attr.Src x

    [<Inline>]
    [<JavaScript>]
    let UseMap x = Attr.UseMap x

    [<JavaScript>]
    let VAlign x = Attr.VAlign x

    [<JavaScript>]
    let Width x = Attr.Width x

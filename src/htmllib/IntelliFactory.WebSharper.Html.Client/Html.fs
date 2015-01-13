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
namespace IntelliFactory.WebSharper.Html.Client

module internal Implementation =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.JavaScript
    open IntelliFactory.WebSharper.Html.Client.Interfaces
    open IntelliFactory.WebSharper.JQuery

    /// jQuery based HTMLProvider.
    type JQueryHtmlProvider[<JavaScript>]() =
        interface IHtmlProvider with

            [<JavaScript>]
            member this.CreateTextNode str =
                JS.Document.CreateTextNode(str)

            [<JavaScript>]
            member this.CreateAttribute str =
                JS.Document.CreateAttribute str

            [<JavaScript>]
            member this.CreateElement name =
                JS.Document.CreateElement(name)

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
                JQuery.Of(JS.Document).Ready(f).Ignore


    [<JavaScript>]
    let private HtmlProvider = new JQueryHtmlProvider()

    [<JavaScript>]
    let Attr = new AttributeBuilder(HtmlProvider)

    [<JavaScript>]
    let Tags = new TagBuilder(HtmlProvider)
        
    [<JavaScript>]
    let DeprecatedHtml = DeprecatedTagBuilder(HtmlProvider)

    [<Inline>]
    [<JavaScript>]
    let OnDocumentReady f =
        (HtmlProvider :> IHtmlProvider).OnDocumentReady f

[<AutoOpen>]
module Default =
    open IntelliFactory.WebSharper

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

    [<Inline>]
    [<JavaScript>]
    let Article x = Tags.Article x

    [<Inline>]
    [<JavaScript>]
    let Aside x = Tags.Aside x

    [<Inline>]
    [<JavaScript>]
    let Audio x = Tags.Audio x

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
    let Canvas x = Tags.Canvas x

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
    let Command x = Tags.Command x

    [<Inline>]
    [<JavaScript>]
    let DataList x = Tags.DataList x

    [<Inline>]
    [<JavaScript>]
    let DD x = Tags.DD x

    [<Inline>]
    [<JavaScript>]
    let Del x = Tags.Del x

    [<Inline>]
    [<JavaScript>]
    let Details x = Tags.Details x

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
    let Embed x = Tags.Embed x

    [<Inline>]
    [<JavaScript>]
    let FieldSet x = Tags.FieldSet x

    [<Inline>]
    [<JavaScript>]
    let FigCaption x = Tags.FigCaption x

    [<Inline>]
    [<JavaScript>]
    let Figure x = Tags.Figure x

    [<Inline>]
    [<JavaScript>]
    let Footer x = Tags.Footer x

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

    [<Inline>]
    [<JavaScript>]
    let Header x = Tags.Header x

    [<Inline>]
    [<JavaScript>]
    let HGroup x = Tags.HGroup x

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
    let KeyGen x = Tags.KeyGen x

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
    let Mark x = Tags.Mark x

    [<Inline>]
    [<JavaScript>]
    let Meta x = Tags.Meta x

    [<Inline>]
    [<JavaScript>]
    let Meter x = Tags.Meter x

    [<Inline>]
    [<JavaScript>]
    let Nav x = Tags.Nav x

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

    [<Inline>]
    [<JavaScript>]
    let Output x = Tags.Output x

    [<JavaScript>]
    let P x = Tags.P x

    [<Inline>]
    [<JavaScript>]
    let Param x = Tags.Param x

    [<JavaScript>]
    let Pre x = Tags.Pre x

    [<Inline>]
    [<JavaScript>]
    let Progress x = Tags.Progress x

    [<Inline>]
    [<JavaScript>]
    let Q x = Tags.Q x

    [<Inline>]
    [<JavaScript>]
    let Rp x = Tags.Rp x

    [<Inline>]
    [<JavaScript>]
    let Rt x = Tags.Rt x

    [<Inline>]
    [<JavaScript>]
    let Ruby x = Tags.Ruby x

    [<Inline>]
    [<JavaScript>]
    let Samp x = Tags.Samp x

    [<JavaScript>]
    let Script x = Tags.Script x

    [<Inline>]
    [<JavaScript>]
    let Section x = Tags.Section x

    [<JavaScript>]
    let Select x = Tags.Select x

    [<Inline>]
    [<JavaScript>]
    let Small x = Tags.Small x

    [<Inline>]
    [<JavaScript>]
    let Source x = Tags.Source x

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
    let Summary x = Tags.Summary x

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

    [<Inline>]
    [<JavaScript>]
    let Time x = Tags.Time x

    [<JavaScript>]
    let TR x = Tags.TR x

    [<Inline>]
    [<JavaScript>]
    let TT  x = Tags.TT x

    [<JavaScript>]
    let UL x = Tags.UL x

    [<Inline>]
    [<JavaScript>]
    let Video x = Tags.Video x

    [<Inline>]
    [<JavaScript>]
    let Wbr x = Tags.Wbr x

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
    let AutoComplete x = Attr.AutoComplete x

    [<Inline>]
    [<JavaScript>]
    let AutoFocus x = Attr.AutoFocus x

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
    let ContentEditable x = Attr.ContentEditable x

    [<Inline>]
    [<JavaScript>]
    let ContextMenu x = Attr.ContextMenu x

    [<Inline>]
    [<JavaScript>]
    let Coords x = Attr.Coords x

    [<Inline>]
    [<JavaScript>]
    let Disabled x = Attr.Disabled x

    [<Inline>]
    [<JavaScript>]
    let Draggable x = Attr.Draggable x

    [<Inline>]
    [<JavaScript>]
    let EncType x = Attr.EncType x

    [<Inline>]
    [<JavaScript>]
    let FormAction x = Attr.FormAction x

    [<Inline>]
    [<JavaScript>]
    let FormEncType x = Attr.FormEncType x

    [<Inline>]
    [<JavaScript>]
    let FormMethod x = Attr.FormMethod x

    [<Inline>]
    [<JavaScript>]
    let FormNoValidate x = Attr.FormNoValidate x

    [<Inline>]
    [<JavaScript>]
    let FormTarget x = Attr.FormTarget x

    [<JavaScript>]
    let HRef x = Attr.HRef x

    [<Inline>]
    [<JavaScript>]
    let HRefLang x = Attr.HRefLang x

    [<JavaScript>]
    let Height x = Attr.Height x

    [<Inline>]
    [<JavaScript>]
    let Hidden x = Attr.Hidden x

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
    let ItemProp x = Attr.ItemProp x

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
    let NoValidate x = Attr.NoValidate x

    [<Inline>]
    [<JavaScript>]
    let Pattern x = Attr.Pattern x

    [<Inline>]
    [<JavaScript>]
    let PlaceHolder x = Attr.PlaceHolder x

    [<Inline>]
    [<JavaScript>]
    let Rel x = Attr.Rel x

    [<Inline>]
    [<JavaScript>]
    let Required x = Attr.Required x

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

    [<Inline>]
    [<JavaScript>]
    let SpellCheck x = Attr.SpellCheck x

    [<JavaScript>]
    let Src x = Attr.Src x

    [<Inline>]
    [<JavaScript>]
    let Step x = Attr.Step x

    [<Inline>]
    [<JavaScript>]
    let Subject x = Attr.Subject x

    [<Inline>]
    [<JavaScript>]
    let UseMap x = Attr.UseMap x

    [<JavaScript>]
    let VAlign x = Attr.VAlign x

    [<JavaScript>]
    let Width x = Attr.Width x

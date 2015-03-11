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
open WebSharper.Html.Client.Interfaces

/// Deprecated HTML combinators
// These are listed under a dedicated builder type for no reason
// other than readability.
type DeprecatedTagBuilder [<JavaScript>](HtmlProvider: IHtmlProvider) =

    [<JavaScript>]
    member this.NewTag (name: string) (children: seq<#Pagelet>) : Element =
        let el = Element.New (HtmlProvider, name)
        for pl in children do
            el.Append (pl :> Pagelet)
        el

    // {{ tag deprecated
    [<Inline>]
    [<JavaScript>]
    member this.Acronym x = this.NewTag "acronym" x
    [<Inline>]
    [<JavaScript>]
    member this.Applet x = this.NewTag "applet" x
    [<Inline>]
    [<JavaScript>]
    member this.BaseFont x = this.NewTag "basefont" x
    [<Inline>]
    [<JavaScript>]
    member this.Big x = this.NewTag "big" x
    [<Inline>]
    [<JavaScript>]
    member this.Center x = this.NewTag "center" x
    [<Inline>]
    [<JavaScript>]
    member this.Dir x = this.NewTag "dir" x
    [<Inline>]
    [<JavaScript>]
    member this.Font x = this.NewTag "font" x
    [<Inline>]
    [<JavaScript>]
    member this.Frame x = this.NewTag "frame" x
    [<Inline>]
    [<JavaScript>]
    member this.FrameSet x = this.NewTag "frameset" x
    [<Inline>]
    [<JavaScript>]
    member this.IsIndex x = this.NewTag "isindex" x
    [<Inline>]
    [<JavaScript>]
    member this.Menu x = this.NewTag "menu" x
    [<Inline>]
    [<JavaScript>]
    member this.MenuItem x = this.NewTag "menuitem" x
    [<Inline>]
    [<JavaScript>]
    member this.S x = this.NewTag "s" x
    [<Inline>]
    [<JavaScript>]
    member this.Strike x = this.NewTag "strike" x
    [<Inline>]
    [<JavaScript>]
    member this.TT x = this.NewTag "tt" x
    [<Inline>]
    [<JavaScript>]
    member this.U x = this.NewTag "u" x
    // }}

/// HTML combinators
type TagBuilder [<JavaScript>](HtmlProvider: IHtmlProvider) =

    [<JavaScript>]
    member this.NewTag (name: string) (children: seq<#Pagelet>) : Element =
        let el = Element.New (HtmlProvider, name)
        for pl in children do
            el.Append (pl :> Pagelet)
        el

    /// Creates a Text node.
    [<JavaScript>]
    [<Name "text">]
    member this.Text (data: string) = Text(data) :> Pagelet

    // {{ tag normal colliding
    [<Inline>]
    [<JavaScript>]
    member this.A x = this.NewTag "a" x
    [<Inline>]
    [<JavaScript>]
    member this.Abbr x = this.NewTag "abbr" x
    [<Inline>]
    [<JavaScript>]
    member this.Address x = this.NewTag "address" x
    [<Inline>]
    [<JavaScript>]
    member this.Area x = this.NewTag "area" x
    [<Inline>]
    [<JavaScript>]
    member this.Article x = this.NewTag "article" x
    [<Inline>]
    [<JavaScript>]
    member this.Aside x = this.NewTag "aside" x
    [<Inline>]
    [<JavaScript>]
    member this.Audio x = this.NewTag "audio" x
    [<Inline>]
    [<JavaScript>]
    member this.B x = this.NewTag "b" x
    [<Inline>]
    [<JavaScript>]
    member this.Base x = this.NewTag "base" x
    [<Inline>]
    [<JavaScript>]
    member this.BDI x = this.NewTag "bdi" x
    [<Inline>]
    [<JavaScript>]
    member this.BDO x = this.NewTag "bdo" x
    [<Inline>]
    [<JavaScript>]
    member this.BlockQuote x = this.NewTag "blockquote" x
    [<Inline>]
    [<JavaScript>]
    member this.Body x = this.NewTag "body" x
    [<Inline>]
    [<JavaScript>]
    member this.Br x = this.NewTag "br" x
    [<Inline>]
    [<JavaScript>]
    member this.Button x = this.NewTag "button" x
    [<Inline>]
    [<JavaScript>]
    member this.Canvas x = this.NewTag "canvas" x
    [<Inline>]
    [<JavaScript>]
    member this.Caption x = this.NewTag "caption" x
    [<Inline>]
    [<JavaScript>]
    member this.Cite x = this.NewTag "cite" x
    [<Inline>]
    [<JavaScript>]
    member this.Code x = this.NewTag "code" x
    [<Inline>]
    [<JavaScript>]
    member this.Col x = this.NewTag "col" x
    [<Inline>]
    [<JavaScript>]
    member this.ColGroup x = this.NewTag "colgroup" x
    [<Inline>]
    [<JavaScript>]
    member this.Command x = this.NewTag "command" x
    [<Inline>]
    [<JavaScript>]
    member this.Content x = this.NewTag "content" x
    [<Inline>]
    [<JavaScript>]
    member this.Data x = this.NewTag "data" x
    [<Inline>]
    [<JavaScript>]
    member this.DataList x = this.NewTag "datalist" x
    [<Inline>]
    [<JavaScript>]
    member this.DD x = this.NewTag "dd" x
    [<Inline>]
    [<JavaScript>]
    member this.Del x = this.NewTag "del" x
    [<Inline>]
    [<JavaScript>]
    member this.Details x = this.NewTag "details" x
    [<Inline>]
    [<JavaScript>]
    member this.Dfn x = this.NewTag "dfn" x
    [<Inline>]
    [<JavaScript>]
    member this.Div x = this.NewTag "div" x
    [<Inline>]
    [<JavaScript>]
    member this.DL x = this.NewTag "dl" x
    [<Inline>]
    [<JavaScript>]
    member this.DT x = this.NewTag "dt" x
    [<Inline>]
    [<JavaScript>]
    member this.Em x = this.NewTag "em" x
    [<Inline>]
    [<JavaScript>]
    member this.Embed x = this.NewTag "embed" x
    [<Inline>]
    [<JavaScript>]
    member this.FieldSet x = this.NewTag "fieldset" x
    [<Inline>]
    [<JavaScript>]
    member this.FigCaption x = this.NewTag "figcaption" x
    [<Inline>]
    [<JavaScript>]
    member this.Figure x = this.NewTag "figure" x
    [<Inline>]
    [<JavaScript>]
    member this.Footer x = this.NewTag "footer" x
    [<Inline>]
    [<JavaScript>]
    member this.Form x = this.NewTag "form" x
    [<Inline>]
    [<JavaScript>]
    member this.H1 x = this.NewTag "h1" x
    [<Inline>]
    [<JavaScript>]
    member this.H2 x = this.NewTag "h2" x
    [<Inline>]
    [<JavaScript>]
    member this.H3 x = this.NewTag "h3" x
    [<Inline>]
    [<JavaScript>]
    member this.H4 x = this.NewTag "h4" x
    [<Inline>]
    [<JavaScript>]
    member this.H5 x = this.NewTag "h5" x
    [<Inline>]
    [<JavaScript>]
    member this.H6 x = this.NewTag "h6" x
    [<Inline>]
    [<JavaScript>]
    member this.Head x = this.NewTag "head" x
    [<Inline>]
    [<JavaScript>]
    member this.Header x = this.NewTag "header" x
    [<Inline>]
    [<JavaScript>]
    member this.HGroup x = this.NewTag "hgroup" x
    [<Inline>]
    [<JavaScript>]
    member this.HR x = this.NewTag "hr" x
    [<Inline>]
    [<JavaScript>]
    member this.HTML x = this.NewTag "html" x
    [<Inline>]
    [<JavaScript>]
    member this.I x = this.NewTag "i" x
    [<Inline>]
    [<JavaScript>]
    member this.IFrame x = this.NewTag "iframe" x
    [<Inline>]
    [<JavaScript>]
    member this.Img x = this.NewTag "img" x
    [<Inline>]
    [<JavaScript>]
    member this.Input x = this.NewTag "input" x
    [<Inline>]
    [<JavaScript>]
    member this.Ins x = this.NewTag "ins" x
    [<Inline>]
    [<JavaScript>]
    member this.Kbd x = this.NewTag "kbd" x
    [<Inline>]
    [<JavaScript>]
    member this.KeyGen x = this.NewTag "keygen" x
    [<Inline>]
    [<JavaScript>]
    member this.Label x = this.NewTag "label" x
    [<Inline>]
    [<JavaScript>]
    member this.Legend x = this.NewTag "legend" x
    [<Inline>]
    [<JavaScript>]
    member this.LI x = this.NewTag "li" x
    [<Inline>]
    [<JavaScript>]
    member this.Link x = this.NewTag "link" x
    [<Inline>]
    [<JavaScript>]
    member this.Main x = this.NewTag "main" x
    [<Inline>]
    [<JavaScript>]
    member this.Map x = this.NewTag "map" x
    [<Inline>]
    [<JavaScript>]
    member this.Mark x = this.NewTag "mark" x
    [<Inline>]
    [<JavaScript>]
    member this.Meta x = this.NewTag "meta" x
    [<Inline>]
    [<JavaScript>]
    member this.Meter x = this.NewTag "meter" x
    [<Inline>]
    [<JavaScript>]
    member this.Nav x = this.NewTag "nav" x
    [<Inline>]
    [<JavaScript>]
    member this.NoFrames x = this.NewTag "noframes" x
    [<Inline>]
    [<JavaScript>]
    member this.NoScript x = this.NewTag "noscript" x
    [<Inline>]
    [<JavaScript>]
    member this.Object x = this.NewTag "object" x
    [<Inline>]
    [<JavaScript>]
    member this.OL x = this.NewTag "ol" x
    [<Inline>]
    [<JavaScript>]
    member this.OptGroup x = this.NewTag "optgroup" x
    [<Inline>]
    [<JavaScript>]
    member this.Option x = this.NewTag "option" x
    [<Inline>]
    [<JavaScript>]
    member this.Output x = this.NewTag "output" x
    [<Inline>]
    [<JavaScript>]
    member this.P x = this.NewTag "p" x
    [<Inline>]
    [<JavaScript>]
    member this.Param x = this.NewTag "param" x
    [<Inline>]
    [<JavaScript>]
    member this.Picture x = this.NewTag "picture" x
    [<Inline>]
    [<JavaScript>]
    member this.Pre x = this.NewTag "pre" x
    [<Inline>]
    [<JavaScript>]
    member this.Progress x = this.NewTag "progress" x
    [<Inline>]
    [<JavaScript>]
    member this.Q x = this.NewTag "q" x
    [<Inline>]
    [<JavaScript>]
    member this.RP x = this.NewTag "rp" x
    [<Inline>]
    [<JavaScript>]
    member this.RT x = this.NewTag "rt" x
    [<Inline>]
    [<JavaScript>]
    member this.RTC x = this.NewTag "rtc" x
    [<Inline>]
    [<JavaScript>]
    member this.Ruby x = this.NewTag "ruby" x
    [<Inline>]
    [<JavaScript>]
    member this.Samp x = this.NewTag "samp" x
    [<Inline>]
    [<JavaScript>]
    member this.Script x = this.NewTag "script" x
    [<Inline>]
    [<JavaScript>]
    member this.Section x = this.NewTag "section" x
    [<Inline>]
    [<JavaScript>]
    member this.Select x = this.NewTag "select" x
    [<Inline>]
    [<JavaScript>]
    member this.Shadow x = this.NewTag "shadow" x
    [<Inline>]
    [<JavaScript>]
    member this.Small x = this.NewTag "small" x
    [<Inline>]
    [<JavaScript>]
    member this.Source x = this.NewTag "source" x
    [<Inline>]
    [<JavaScript>]
    member this.Span x = this.NewTag "span" x
    [<Inline>]
    [<JavaScript>]
    member this.Strong x = this.NewTag "strong" x
    [<Inline>]
    [<JavaScript>]
    member this.Style x = this.NewTag "style" x
    [<Inline>]
    [<JavaScript>]
    member this.Sub x = this.NewTag "sub" x
    [<Inline>]
    [<JavaScript>]
    member this.Summary x = this.NewTag "summary" x
    [<Inline>]
    [<JavaScript>]
    member this.Sup x = this.NewTag "sup" x
    [<Inline>]
    [<JavaScript>]
    member this.Table x = this.NewTag "table" x
    [<Inline>]
    [<JavaScript>]
    member this.TBody x = this.NewTag "tbody" x
    [<Inline>]
    [<JavaScript>]
    member this.TD x = this.NewTag "td" x
    [<Inline>]
    [<JavaScript>]
    member this.Template x = this.NewTag "template" x
    [<Inline>]
    [<JavaScript>]
    member this.TextArea x = this.NewTag "textarea" x
    [<Inline>]
    [<JavaScript>]
    member this.TFoot x = this.NewTag "tfoot" x
    [<Inline>]
    [<JavaScript>]
    member this.TH x = this.NewTag "th" x
    [<Inline>]
    [<JavaScript>]
    member this.THead x = this.NewTag "thead" x
    [<Inline>]
    [<JavaScript>]
    member this.Time x = this.NewTag "time" x
    [<Inline>]
    [<JavaScript>]
    member this.Title x = this.NewTag "title" x
    [<Inline>]
    [<JavaScript>]
    member this.TR x = this.NewTag "tr" x
    [<Inline>]
    [<JavaScript>]
    member this.Track x = this.NewTag "track" x
    [<Inline>]
    [<JavaScript>]
    member this.UL x = this.NewTag "ul" x
    [<Inline>]
    [<JavaScript>]
    member this.Var x = this.NewTag "var" x
    [<Inline>]
    [<JavaScript>]
    member this.Video x = this.NewTag "video" x
    [<Inline>]
    [<JavaScript>]
    member this.WBR x = this.NewTag "wbr" x
    // }}

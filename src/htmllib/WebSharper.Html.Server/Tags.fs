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
namespace WebSharper.Html.Server

/// Exposes common HTML tags.
// List is from:
// http://www.w3.org/TR/html4/index/elements.html
[<AutoOpen>]
module Tags =

    let Comment x = Html.CommentContent x
    let Text x = Html.TextContent x

    // {{ tag normal
    let A x = Html.NewTag "a" x
    let Abbr x = Html.NewTag "abbr" x
    let Address x = Html.NewTag "address" x
    let Area x = Html.NewTag "area" x
    let Article x = Html.NewTag "article" x
    let Aside x = Html.NewTag "aside" x
    let Audio x = Html.NewTag "audio" x
    let B x = Html.NewTag "b" x
    let Base x = Html.NewTag "base" x
    let BDI x = Html.NewTag "bdi" x
    let BDO x = Html.NewTag "bdo" x
    let BlockQuote x = Html.NewTag "blockquote" x
    let Body x = Html.NewTag "body" x
    let Br x = Html.NewTag "br" x
    let Button x = Html.NewTag "button" x
    let Canvas x = Html.NewTag "canvas" x
    let Caption x = Html.NewTag "caption" x
    let Cite x = Html.NewTag "cite" x
    let Code x = Html.NewTag "code" x
    let Col x = Html.NewTag "col" x
    let ColGroup x = Html.NewTag "colgroup" x
    let Command x = Html.NewTag "command" x
    let DataList x = Html.NewTag "datalist" x
    let DD x = Html.NewTag "dd" x
    let Del x = Html.NewTag "del" x
    let Details x = Html.NewTag "details" x
    let Dfn x = Html.NewTag "dfn" x
    let Div x = Html.NewTag "div" x
    let DL x = Html.NewTag "dl" x
    let DT x = Html.NewTag "dt" x
    let Em x = Html.NewTag "em" x
    let Embed x = Html.NewTag "embed" x
    let FieldSet x = Html.NewTag "fieldset" x
    let FigCaption x = Html.NewTag "figcaption" x
    let Figure x = Html.NewTag "figure" x
    let Footer x = Html.NewTag "footer" x
    let Form x = Html.NewTag "form" x
    let H1 x = Html.NewTag "h1" x
    let H2 x = Html.NewTag "h2" x
    let H3 x = Html.NewTag "h3" x
    let H4 x = Html.NewTag "h4" x
    let H5 x = Html.NewTag "h5" x
    let H6 x = Html.NewTag "h6" x
    let Head x = Html.NewTag "head" x
    let Header x = Html.NewTag "header" x
    let HGroup x = Html.NewTag "hgroup" x
    let HR x = Html.NewTag "hr" x
    let HTML x = Html.NewTag "html" x
    let I x = Html.NewTag "i" x
    let IFrame x = Html.NewTag "iframe" x
    let Img x = Html.NewTag "img" x
    let Input x = Html.NewTag "input" x
    let Ins x = Html.NewTag "ins" x
    let Kbd x = Html.NewTag "kbd" x
    let KeyGen x = Html.NewTag "keygen" x
    let Label x = Html.NewTag "label" x
    let Legend x = Html.NewTag "legend" x
    let LI x = Html.NewTag "li" x
    let Link x = Html.NewTag "link" x
    let Mark x = Html.NewTag "mark" x
    let Meta x = Html.NewTag "meta" x
    let Meter x = Html.NewTag "meter" x
    let Nav x = Html.NewTag "nav" x
    let NoFrames x = Html.NewTag "noframes" x
    let NoScript x = Html.NewTag "noscript" x
    let OL x = Html.NewTag "ol" x
    let OptGroup x = Html.NewTag "optgroup" x
    let Output x = Html.NewTag "output" x
    let P x = Html.NewTag "p" x
    let Param x = Html.NewTag "param" x
    let Picture x = Html.NewTag "picture" x
    let Pre x = Html.NewTag "pre" x
    let Progress x = Html.NewTag "progress" x
    let Q x = Html.NewTag "q" x
    let RP x = Html.NewTag "rp" x
    let RT x = Html.NewTag "rt" x
    let RTC x = Html.NewTag "rtc" x
    let Ruby x = Html.NewTag "ruby" x
    let Samp x = Html.NewTag "samp" x
    let Script x = Html.NewTag "script" x
    let Section x = Html.NewTag "section" x
    let Select x = Html.NewTag "select" x
    let Shadow x = Html.NewTag "shadow" x
    let Small x = Html.NewTag "small" x
    let Source x = Html.NewTag "source" x
    let Span x = Html.NewTag "span" x
    let Strong x = Html.NewTag "strong" x
    let Sub x = Html.NewTag "sub" x
    let Summary x = Html.NewTag "summary" x
    let Sup x = Html.NewTag "sup" x
    let Table x = Html.NewTag "table" x
    let TBody x = Html.NewTag "tbody" x
    let TD x = Html.NewTag "td" x
    let TextArea x = Html.NewTag "textarea" x
    let TFoot x = Html.NewTag "tfoot" x
    let TH x = Html.NewTag "th" x
    let THead x = Html.NewTag "thead" x
    let Time x = Html.NewTag "time" x
    let TR x = Html.NewTag "tr" x
    let Track x = Html.NewTag "track" x
    let UL x = Html.NewTag "ul" x
    let Var x = Html.NewTag "var" x
    let Video x = Html.NewTag "video" x
    let WBR x = Html.NewTag "wbr" x
    // }}

    [<RequireQualifiedAccess>]
    module Deprecated =
        // {{ tag deprecated
        let Acronym x = Html.NewTag "acronym" x
        let Applet x = Html.NewTag "applet" x
        let BaseFont x = Html.NewTag "basefont" x
        let Big x = Html.NewTag "big" x
        let Center x = Html.NewTag "center" x
        let Dir x = Html.NewTag "dir" x
        let Font x = Html.NewTag "font" x
        let Frame x = Html.NewTag "frame" x
        let FrameSet x = Html.NewTag "frameset" x
        let IsIndex x = Html.NewTag "isindex" x
        let Menu x = Html.NewTag "menu" x
        let MenuItem x = Html.NewTag "menuitem" x
        let S x = Html.NewTag "s" x
        let Strike x = Html.NewTag "strike" x
        let TT x = Html.NewTag "tt" x
        let U x = Html.NewTag "u" x
        // }}

    module Tags =
        // {{ tag colliding
        let Content x = Html.NewTag "content" x
        let Data x = Html.NewTag "data" x
        let Main x = Html.NewTag "main" x
        let Map x = Html.NewTag "map" x
        let Object x = Html.NewTag "object" x
        let Option x = Html.NewTag "option" x
        let Style x = Html.NewTag "style" x
        let Template x = Html.NewTag "template" x
        let Title x = Html.NewTag "title" x
        // }}

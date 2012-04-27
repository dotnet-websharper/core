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
namespace IntelliFactory.Html

/// Exposes common HTML tags.
// List is from:
// http://www.w3.org/TR/html4/index/elements.html
[<AutoOpen>]
module Tags =

    module HTML5 =
        let Article x = Html.NewElement "article" x
        let Aside x = Html.NewElement "aside" x
        let Audio x = Html.NewElement "audio" x
        let Canvas x = Html.NewElement "canvas" x
        let Command x = Html.NewElement "command" x
        let DataList x = Html.NewElement "datalist" x
        let Details x = Html.NewElement "details" x
        let Embed x = Html.NewElement "embed" x
        let FigCaption x = Html.NewElement "figcaption" x
        let Figure x = Html.NewElement "figure" x
        let Footer x = Html.NewElement "footer" x
        let Header x = Html.NewElement "header" x
        let HGroup x = Html.NewElement "hgroup" x
        let KeyGen x = Html.NewElement "keygen" x
        let Mark x = Html.NewElement "mark" x
        let Meter x = Html.NewElement "meter" x
        let Nav x = Html.NewElement "nav" x
        let Output x = Html.NewElement "output" x
        let Progress x = Html.NewElement "progress" x
        let Rp x = Html.NewElement "rp" x
        let Rt x = Html.NewElement "rt" x
        let Ruby x = Html.NewElement "ruby" x
        let Section x = Html.NewElement "section" x
        let Source x = Html.NewElement "source" x
        let Summary x = Html.NewElement "summary" x
        let Time x = Html.NewElement "time" x
        let Video x = Html.NewElement "video" x
        let Wbr x = Html.NewElement "wbr" x

    let Comment x = Html.CommentContent x
    let Text x = Html.TextContent x

    let A x = Html.NewElement "a" x
    let Abbr x = Html.NewElement "abbr" x
    let Acronym x = Html.NewElement "acronym"
    let Address x = Html.NewElement "address" x
    // ?
    let Anchor x = Html.NewElement "anchor" x
    let Area x = Html.NewElement "area" x
    let B x = Html.NewElement "b" x
    let Base x = Html.NewElement "base" x
    let Bdo x = Html.NewElement "bdo" x
    let Big x = Html.NewElement "big" x
    let BlockQuote x = Html.NewElement "blockquote" x
    let Body x = Html.NewElement "body" x
    let Br x = Html.NewElement "br" x
    let Button x = Html.NewElement "button" x

    let Caption x = Html.NewElement "caption" x
    let Cite x = Html.NewElement "cite" x
    let Code x = Html.NewElement "code" x
    let Col x = Html.NewElement "col" x
    let ColGroup x = Html.NewElement "colgroup" x

    let DD x = Html.NewElement "dd" x
    let Del x = Html.NewElement "del" x
    let Dfn x = Html.NewElement "dfn" x
    let Div x = Html.NewElement "div" x
    let DL x = Html.NewElement "dl" x
    let DT x = Html.NewElement "dt" x

    let Em x = Html.NewElement "em" x

    let FieldSet x = Html.NewElement "fieldset" x
    let Form x = Html.NewElement "form" x
    let Frame x = Html.NewElement "frame" x
    let FrameSet x = Html.NewElement "frameset" x

    let H1 x = Html.NewElement "h1" x
    let H2 x = Html.NewElement "h2" x
    let H3 x = Html.NewElement "h3" x
    let H4 x = Html.NewElement "h4" x
    let H5 x = Html.NewElement "h5" x
    let H6 x = Html.NewElement "h6" x
    let Head x = Html.NewElement "head" x
    let HR x = Html.NewElement "hr" x
    let HTML x = Html.NewElement "html" x

    let I x = Html.NewElement "i" x
    let IFrame x = Html.NewElement "iframe" x
    let Img x = Html.NewElement "img" x
    let Input x = Html.NewElement "input" x
    let Ins x = Html.NewElement "ins" x
    
    let Kbd x = Html.NewElement "kbd" x

    let Label x = Html.NewElement "label" x
    let Legend x = Html.NewElement "legend" x
    let LI x = Html.NewElement "li" x
    let Link x = Html.NewElement "link" x



    let NoFrames x = Html.NewElement "noframes" x
    let NoScript x = Html.NewElement "noscript" x

    
    let OL x = Html.NewElement "ol" x
    let OptGroup x = Html.NewElement "optgroup" x

    let P x = Html.NewElement "p" x
    let Param x = Html.NewElement "param" x
    let Pre x = Html.NewElement "pre" x

    let Q x = Html.NewElement "q" x

    let Samp x = Html.NewElement "samp" x
    let Script x = Html.NewElement "script" x
    let Select x = Html.NewElement "select" x
    let Small x = Html.NewElement "small" x
    let Span x = Html.NewElement "span" x
    let Strong x = Html.NewElement "strong" x
    let Style x = Html.NewElement "style" x
    let Sub x = Html.NewElement "sub" x
    let Sup x = Html.NewElement "sup" x

    let Table x = Html.NewElement "table" x
    let TBody x = Html.NewElement "tbody" x
    let TD x = Html.NewElement "td" x
    let TextArea x = Html.NewElement "textarea" x
    let TFoot x = Html.NewElement "tfoot" x
    let TH x = Html.NewElement "th" x
    let THead x = Html.NewElement "thead" x
    let TR x = Html.NewElement "tr" x
    let TT x = Html.NewElement "tt" x

    let UL x = Html.NewElement "ul" x

    let Var x = Html.NewElement "var" x

    [<RequireQualifiedAccess>]
    module Deprecated =
        let Applet x = Html.NewElement "applet" x
        let BaseFont x = Html.NewElement "basefont" x
        let Center x = Html.NewElement "center" x
        let Dir x = Html.NewElement "dir" x
        let Font x = Html.NewElement "font" x
        let IsIndex x = Html.NewElement "isindex" x
        let Menu x = Html.NewElement "menu" x
        let S x = Html.NewElement "s" x
        let Strike x = Html.NewElement "strike" x
        let U x = Html.NewElement "u" x

    module Tags =
        let Title x = Html.NewElement "title" x
        let Option x = Html.NewElement "option" x
        let Map x = Html.NewElement "map" x
        let Meta x = Html.NewElement "meta" x
        let Object x = Html.NewElement "object" x

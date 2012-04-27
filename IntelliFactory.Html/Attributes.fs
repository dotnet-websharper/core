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

// Exposes common HTML attributes.
// List is obtained from:
// http://www.w3.org/TR/html4/index/attributes.html
[<AutoOpen>]
module Attributes =

    module HTML5 =
        let ContentEditable x = Html.NewAttribute "contenteditable" x
        let ContextMenu x = Html.NewAttribute "contextmenu" x
        let Data key x = Html.NewAttribute ("data-"+key) x
        let Draggable x = Html.NewAttribute "draggable" x
        let Hidden x = Html.NewAttribute "hidden" x
        let Item x = Html.NewAttribute "item" x
        let ItemProp x = Html.NewAttribute "itemprop" x
        let SpellCheck x = Html.NewAttribute "spellcheck" x
        let Subject x = Html.NewAttribute "subject" x
        let AutoComplete x = Html.NewAttribute "autocomplete" x
        let AutoFocus x = Html.NewAttribute "autofocus" x
        let Form x = Html.NewAttribute "form" x
        let FormAction x = Html.NewAttribute "formaction" x
        let FormEncType x = Html.NewAttribute "formenctype" x
        let FormMethod x = Html.NewAttribute "formmethod" x
        let FormNoValidate x = Html.NewAttribute "formnovalidate" x
        let FormTarget x = Html.NewAttribute "formtarget" x
        let Height x = Html.NewAttribute "height" x
        let List x = Html.NewAttribute "list" x
        let Max x = Html.NewAttribute "max" x
        let Min x = Html.NewAttribute "min" x
        let Multiple x = Html.NewAttribute "multiple" x
        let NoValidate x = Html.NewAttribute "novalidate" x
        let Pattern x = Html.NewAttribute "pattern" x
        let PlaceHolder x = Html.NewAttribute "placeholder" x
        let Required x = Html.NewAttribute "required" x
        let Step x = Html.NewAttribute "step" x

    let Abbr x = Html.NewAttribute "abbr" x
    let AcceptCharSet x = Html.NewAttribute "accept-charset" x
    let Accept x = Html.NewAttribute "accept" x
    let AccessKey x = Html.NewAttribute "accesskey" x
    let Align x = Html.NewAttribute "align" x
    let Alt x = Html.NewAttribute "alt" x
    // ?
    let AltCode x = Html.NewAttribute "altcode" x
    let Archive x = Html.NewAttribute "archive" x
    let Axis x = Html.NewAttribute "axis" x
    let Border x = Html.NewAttribute "border" x
    // ?
    let BorderColor x = Html.NewAttribute "bordercolor" x
    let CellPadding x = Html.NewAttribute "cellpadding" x
    let CellSpacing x = Html.NewAttribute "cellspacing" x
    let Char x = Html.NewAttribute "char" x
    let CharOff x = Html.NewAttribute "charoff" x
    let CharSet x = Html.NewAttribute "charset" x
    let Checked x = Html.NewAttribute "checked" x
    let Class x = Html.NewAttribute "class" x
    let ClassId x = Html.NewAttribute "classid" x
    let CodeBase x = Html.NewAttribute "codebase" x
    let CodeType x = Html.NewAttribute "codetype" x
    let Cols x = Html.NewAttribute "cols" x
    let ColSpan x = Html.NewAttribute "colspan" x
    
    let Coords x = Html.NewAttribute "coords" x
    let Declare x = Html.NewAttribute "declare" x
    let Defer x = Html.NewAttribute "defer" x
    let Disabled x = Html.NewAttribute "disabled" x
    let EncType x = Html.NewAttribute "enctype" x
    let For x = Html.NewAttribute "for" x
    let FrameBorder x = Html.NewAttribute "frameborder" x
    let Headers x = Html.NewAttribute "headers" x
    let Height x = Html.NewAttribute "height" x
    let HRef x = Html.NewAttribute "href" x
    let HRefLang x = Html.NewAttribute "hreflang" x
    let HttpEquiv x = Html.NewAttribute "http-equiv" x
    let Id x = Html.NewAttribute "id" x
    let IsMap x = Html.NewAttribute "ismap" x
    
    let Lang x = Html.NewAttribute "lang" x
    let LongDesc x = Html.NewAttribute "longdesc" x
    let MarginHeight x = Html.NewAttribute "marginheight" x
    let MarginWidth x = Html.NewAttribute "marginwidth" x
    let MaxLength x = Html.NewAttribute "maxlength" x
    let Media x = Html.NewAttribute "media" x
    let Method x = Html.NewAttribute "method" x
    let Multiple x = Html.NewAttribute "multiple" x
    let Name x = Html.NewAttribute "name" x
    let NoHRef x = Html.NewAttribute "nohref" x
    let NoResize x = Html.NewAttribute "noresize" x
    let OnBlur x = Html.NewAttribute "onblur" x
    let OnChange x = Html.NewAttribute "onchange" x
    let OnClick x = Html.NewAttribute "onclick" x
    let OnDbClick x = Html.NewAttribute "ondbclick" x
    let OnFocus x = Html.NewAttribute "onfocus" x
    let OnKeyDown x = Html.NewAttribute "onkeydown" x
    let OnKeyPress x = Html.NewAttribute "onkeypress" x
    let OnKeyUp x = Html.NewAttribute "onkeyup" x
    let OnLoad x = Html.NewAttribute "onload" x
    let OnMouseDown x = Html.NewAttribute "onmousedown" x
    let OnMouseMove x = Html.NewAttribute "onmousemove" x
    let OnMouseOut x = Html.NewAttribute "onmouseout" x
    let OnMouseOver x = Html.NewAttribute "onmouseover" x
    let OnMouseUp x = Html.NewAttribute "onmouseup" x
    let OnReset x = Html.NewAttribute "onreset" x
    let OnSelect x = Html.NewAttribute "onselect" x
    let OnSubmit x = Html.NewAttribute "onsubmit" x
    let OnUnload x = Html.NewAttribute "onunload" x
    let Profile x = Html.NewAttribute "profile" x
    let ReadOnly x = Html.NewAttribute "readonly" x
    let Rel x = Html.NewAttribute "rel" x
    let Rev x = Html.NewAttribute "rev" x
    let Rows x = Html.NewAttribute "rows" x
    let RowSpan x = Html.NewAttribute "rowspan" x
    let Rules x = Html.NewAttribute "rules" x
    let Scheme x = Html.NewAttribute "scheme" x
    let Scope x = Html.NewAttribute "scope" x
    let Scrolling x = Html.NewAttribute "scrolling" x
    let Selected x = Html.NewAttribute "selected" x
    let Shape x = Html.NewAttribute "shape" x
    let Size x = Html.NewAttribute "size" x
    let Src x = Html.NewAttribute "src" x
    let StandBy x = Html.NewAttribute "standby" x
    let Style x = Html.NewAttribute "style" x
    let Summary x = Html.NewAttribute "summary" x
    let TabIndex x = Html.NewAttribute "tabindex" x
    let Target x = Html.NewAttribute "target" x
    let Title x = Html.NewAttribute "title" x
    let Type x = Html.NewAttribute "type" x
    let UseMap x = Html.NewAttribute "usemap" x
    let VAlign x = Html.NewAttribute "valign" x
    let Value x = Html.NewAttribute "value" x
    let ValueType x = Html.NewAttribute "valuetype" x
    let Width x = Html.NewAttribute "width" x
    // Custom types
    //------------------------------
    let CheckBox<'T> = Html.NewAttribute "type" "checkbox"
    let Hidden<'T> = Html.NewAttribute "type" "hidden"
    let Radio<'T> = Html.NewAttribute "type" "radio"
    let Reset<'T> = Html.NewAttribute "type" "reset"
    let Submit<'T> = Html.NewAttribute "type" "submit"
    let Password<'T> = Html.NewAttribute "type" "password"
    let TextField<'T> = Html.NewAttribute "type" "textfield"

    [<RequireQualifiedAccess>]
    module Deprecated =
        let Alink x = Html.NewAttribute "alink" x
        let Background x = Html.NewAttribute "background" x
        let BgColor x = Html.NewAttribute "bgcolor" x
        let Clear x = Html.NewAttribute "clear" x
        let Code x = Html.NewAttribute "code" x
        let Color x = Html.NewAttribute "color" x
        let Compact x = Html.NewAttribute "compact" x
        let Face x = Html.NewAttribute "face" x
        let HSpace x = Html.NewAttribute "hspace" x
        let Language x = Html.NewAttribute "language" x
        let Link x = Html.NewAttribute "link" x
        let NoShade x = Html.NewAttribute "noshade" x
        let NoWrap x = Html.NewAttribute "nowrap" x
        let Object x = Html.NewAttribute "object" x
        let Prompt x = Html.NewAttribute "prompt" x
        let Start x = Html.NewAttribute "start" x
        let Text x = Html.NewAttribute "text" x
        let Version x = Html.NewAttribute "version" x
        let VLink x = Html.NewAttribute "vlink" x
        let VSpace x = Html.NewAttribute "vspace" x

    module Attr =
        let Action x = Html.NewAttribute "action" x
        let Label x = Html.NewAttribute "label" x
        let Content x = Html.NewAttribute "content" x
        let Cite x = Html.NewAttribute "cite" x
        let Data x = Html.NewAttribute "data" x
        let DateTime x = Html.NewAttribute "datetime" x
        let Dir x = Html.NewAttribute "dir" x
        let Frame x = Html.NewAttribute "frame" x
        let Span x = Html.NewAttribute "span" x

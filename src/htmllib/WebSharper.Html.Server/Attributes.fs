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

// Exposes common HTML attributes.
// List is obtained from:
// http://www.w3.org/TR/html4/index/attributes.html
[<AutoOpen>]
module Attr =

    // {{ attr normal
    let Accept x = Html.NewAttr "accept" x
    let AcceptCharSet x = Html.NewAttr "accept-charset" x
    let AccessKey x = Html.NewAttr "accesskey" x
    let Align x = Html.NewAttr "align" x
    let Alt x = Html.NewAttr "alt" x
    let AltCode x = Html.NewAttr "altcode" x
    let Archive x = Html.NewAttr "archive" x
    let AutoComplete x = Html.NewAttr "autocomplete" x
    let AutoFocus x = Html.NewAttr "autofocus" x
    let AutoPlay x = Html.NewAttr "autoplay" x
    let AutoSave x = Html.NewAttr "autosave" x
    let Axis x = Html.NewAttr "axis" x
    let Border x = Html.NewAttr "border" x
    let BorderColor x = Html.NewAttr "bordercolor" x
    let Buffered x = Html.NewAttr "buffered" x
    let CellPadding x = Html.NewAttr "cellpadding" x
    let CellSpacing x = Html.NewAttr "cellspacing" x
    let Challenge x = Html.NewAttr "challenge" x
    let Char x = Html.NewAttr "char" x
    let CharOff x = Html.NewAttr "charoff" x
    let CharSet x = Html.NewAttr "charset" x
    let Checked x = Html.NewAttr "checked" x
    let Class x = Html.NewAttr "class" x
    let ClassId x = Html.NewAttr "classid" x
    let CodeBase x = Html.NewAttr "codebase" x
    let CodeType x = Html.NewAttr "codetype" x
    let Cols x = Html.NewAttr "cols" x
    let ColSpan x = Html.NewAttr "colspan" x
    let ContentEditable x = Html.NewAttr "contenteditable" x
    let ContextMenu x = Html.NewAttr "contextmenu" x
    let Coords x = Html.NewAttr "coords" x
    let Declare x = Html.NewAttr "declare" x
    let Default x = Html.NewAttr "default" x
    let Defer x = Html.NewAttr "defer" x
    let Disabled x = Html.NewAttr "disabled" x
    let Download x = Html.NewAttr "download" x
    let Draggable x = Html.NewAttr "draggable" x
    let DropZone x = Html.NewAttr "dropzone" x
    let EncType x = Html.NewAttr "enctype" x
    let For x = Html.NewAttr "for" x
    let FormAction x = Html.NewAttr "formaction" x
    let FormEncType x = Html.NewAttr "formenctype" x
    let FormMethod x = Html.NewAttr "formmethod" x
    let FormNoValidate x = Html.NewAttr "formnovalidate" x
    let FormTarget x = Html.NewAttr "formtarget" x
    let FrameBorder x = Html.NewAttr "frameborder" x
    let Headers x = Html.NewAttr "headers" x
    let Height x = Html.NewAttr "height" x
    let Hidden x = Html.NewAttr "hidden" x
    let High x = Html.NewAttr "high" x
    let HRef x = Html.NewAttr "href" x
    let HRefLang x = Html.NewAttr "hreflang" x
    let HttpEquiv x = Html.NewAttr "http" x
    let Icon x = Html.NewAttr "icon" x
    let Id x = Html.NewAttr "id" x
    let IsMap x = Html.NewAttr "ismap" x
    let ItemProp x = Html.NewAttr "itemprop" x
    let Lang x = Html.NewAttr "lang" x
    let LongDesc x = Html.NewAttr "longdesc" x
    let Loop x = Html.NewAttr "loop" x
    let Low x = Html.NewAttr "low" x
    let Manifest x = Html.NewAttr "manifest" x
    let MarginHeight x = Html.NewAttr "marginheight" x
    let MarginWidth x = Html.NewAttr "marginwidth" x
    let MaxLength x = Html.NewAttr "maxlength" x
    let Media x = Html.NewAttr "media" x
    let Method x = Html.NewAttr "method" x
    let Multiple x = Html.NewAttr "multiple" x
    let Name x = Html.NewAttr "name" x
    let NoHRef x = Html.NewAttr "nohref" x
    let NoResize x = Html.NewAttr "noresize" x
    let NoValidate x = Html.NewAttr "novalidate" x
    let Pattern x = Html.NewAttr "pattern" x
    let Ping x = Html.NewAttr "ping" x
    let PlaceHolder x = Html.NewAttr "placeholder" x
    let Poster x = Html.NewAttr "poster" x
    let Preload x = Html.NewAttr "preload" x
    let Profile x = Html.NewAttr "profile" x
    let PubDate x = Html.NewAttr "pubdate" x
    let RadioGroup x = Html.NewAttr "radiogroup" x
    let ReadOnly x = Html.NewAttr "readonly" x
    let Rel x = Html.NewAttr "rel" x
    let Required x = Html.NewAttr "required" x
    let Rev x = Html.NewAttr "rev" x
    let Reversed x = Html.NewAttr "reversed" x
    let Rows x = Html.NewAttr "rows" x
    let RowSpan x = Html.NewAttr "rowspan" x
    let Rules x = Html.NewAttr "rules" x
    let Sandbox x = Html.NewAttr "sandbox" x
    let Scheme x = Html.NewAttr "scheme" x
    let Scope x = Html.NewAttr "scope" x
    let Scoped x = Html.NewAttr "scoped" x
    let Scrolling x = Html.NewAttr "scrolling" x
    let Seamless x = Html.NewAttr "seamless" x
    let Selected x = Html.NewAttr "selected" x
    let Shape x = Html.NewAttr "shape" x
    let Size x = Html.NewAttr "size" x
    let Sizes x = Html.NewAttr "sizes" x
    let SpellCheck x = Html.NewAttr "spellcheck" x
    let Src x = Html.NewAttr "src" x
    let SrcDoc x = Html.NewAttr "srcdoc" x
    let SrcLang x = Html.NewAttr "srclang" x
    let StandBy x = Html.NewAttr "standby" x
    let Step x = Html.NewAttr "step" x
    let Style x = Html.NewAttr "style" x
    let Subject x = Html.NewAttr "subject" x
    let TabIndex x = Html.NewAttr "tabindex" x
    let Target x = Html.NewAttr "target" x
    let Title x = Html.NewAttr "title" x
    let Type x = Html.NewAttr "type" x
    let UseMap x = Html.NewAttr "usemap" x
    let VAlign x = Html.NewAttr "valign" x
    let Value x = Html.NewAttr "value" x
    let ValueType x = Html.NewAttr "valuetype" x
    let Width x = Html.NewAttr "width" x
    let Wrap x = Html.NewAttr "wrap" x
    // }}

    [<RequireQualifiedAccess>]
    module Deprecated =
        // {{ attr deprecated
        let Alink x = Html.NewAttr "alink" x
        let Background x = Html.NewAttr "background" x
        let BgColor x = Html.NewAttr "bgcolor" x
        let Clear x = Html.NewAttr "clear" x
        let Code x = Html.NewAttr "code" x
        let Color x = Html.NewAttr "color" x
        let Compact x = Html.NewAttr "compact" x
        let Data x = Html.NewAttr "data" x
        let Face x = Html.NewAttr "face" x
        let HSpace x = Html.NewAttr "hspace" x
        let Language x = Html.NewAttr "language" x
        let Link x = Html.NewAttr "link" x
        let NoShade x = Html.NewAttr "noshade" x
        let NoWrap x = Html.NewAttr "nowrap" x
        let Object x = Html.NewAttr "object" x
        let Open x = Html.NewAttr "open" x
        let Optimum x = Html.NewAttr "optimum" x
        let Prompt x = Html.NewAttr "prompt" x
        let Start x = Html.NewAttr "start" x
        let Summary x = Html.NewAttr "summary" x
        let Text x = Html.NewAttr "text" x
        let Version x = Html.NewAttr "version" x
        let VLink x = Html.NewAttr "vlink" x
        let VSpace x = Html.NewAttr "vspace" x
        // }}

    module Attr =
        let Data key x = Html.NewAttr ("data-"+key) x
        // {{ attr colliding
        let Action x = Html.NewAttr "action" x
        let Async x = Html.NewAttr "async" x
        let Cite x = Html.NewAttr "cite" x
        let Content x = Html.NewAttr "content" x
        let Controls x = Html.NewAttr "controls" x
        let DateTime x = Html.NewAttr "datetime" x
        let Dir x = Html.NewAttr "dir" x
        let Form x = Html.NewAttr "form" x
        let Frame x = Html.NewAttr "frame" x
        let KeyType x = Html.NewAttr "keytype" x
        let Kind x = Html.NewAttr "kind" x
        let Label x = Html.NewAttr "label" x
        let List x = Html.NewAttr "list" x
        let Max x = Html.NewAttr "max" x
        let Min x = Html.NewAttr "min" x
        let Span x = Html.NewAttr "span" x
        // }}

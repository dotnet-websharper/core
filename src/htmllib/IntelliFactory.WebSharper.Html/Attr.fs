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

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html.Interfaces

/// Exposes HTML5 attributes.
type Html5AttributeBuilder [<JavaScript>] (HtmlProvider: IHtmlProvider) =

    [<JavaScript>]
    member this.NewAttr (name: string) (value: string) : IPagelet =
        let a = Attribute.New(HtmlProvider, name, value)
        a :> _

    [<Inline>]
    [<JavaScript>]
    member this.ContentEditable x = this.NewAttr "contenteditable" x

    [<Inline>]
    [<JavaScript>]
    member this.ContextMenu x = this.NewAttr "contextmenu" x

    [<Inline>]
    [<JavaScript>]
    member this.Data key x =
        // TODO: needs to clean the key name
        this.NewAttr ("data-"+key) x

    [<Inline>]
    [<JavaScript>]
    member this.Draggable x = this.NewAttr "draggable" x

    [<Inline>]
    [<JavaScript>]
    member this.Hidden x = this.NewAttr "hidden" x

    [<Inline>]
    [<JavaScript>]
    member this.Item x = this.NewAttr "item" x

    [<Inline>]
    [<JavaScript>]
    member this.ItemProp x = this.NewAttr "itemprop" x

    [<Inline>]
    [<JavaScript>]
    member this.SpellCheck x = this.NewAttr "spellcheck" x

    [<Inline>]
    [<JavaScript>]
    member this.Subject x = this.NewAttr "subject" x

    // Form and input attributes

    [<Inline>]
    [<JavaScript>]
    member this.AutoComplete x = this.NewAttr "autocomplete" x

    [<Inline>]
    [<JavaScript>]
    member this.AutoFocus x = this.NewAttr "autofocus" x

    [<Inline>]
    [<JavaScript>]
    member this.Form x = this.NewAttr "form" x

    [<Inline>]
    [<JavaScript>]
    member this.FormAction x = this.NewAttr "formaction" x

    [<Inline>]
    [<JavaScript>]
    member this.FormEncType x = this.NewAttr "formenctype" x

    [<Inline>]
    [<JavaScript>]
    member this.FormMethod x = this.NewAttr "formmethod" x

    [<Inline>]
    [<JavaScript>]
    member this.FormNoValidate x = this.NewAttr "formnovalidate" x

    [<Inline>]
    [<JavaScript>]
    member this.FormTarget x = this.NewAttr "formtarget" x

    [<Inline>]
    [<JavaScript>]
    member this.Height x = this.NewAttr "height" x

    [<Inline>]
    [<JavaScript>]
    member this.List x = this.NewAttr "list" x

    [<Inline>]
    [<JavaScript>]
    member this.Max x = this.NewAttr "max" x

    [<Inline>]
    [<JavaScript>]
    member this.Min x = this.NewAttr "min" x

    [<Inline>]
    [<JavaScript>]
    member this.Multiple x = this.NewAttr "multiple" x

    [<Inline>]
    [<JavaScript>]
    member this.NoValidate x = this.NewAttr "novalidate" x

    [<Inline>]
    [<JavaScript>]
    member this.Pattern x = this.NewAttr "pattern" x

    [<Inline>]
    [<JavaScript>]
    member this.PlaceHolder x = this.NewAttr "placeholder" x

    [<Inline>]
    [<JavaScript>]
    member this.Required x = this.NewAttr "required" x

    [<Inline>]
    [<JavaScript>]
    member this.Step x = this.NewAttr "step" x

/// Exposes HTML attributes.
type DeprecatedAttributeBuilder [<JavaScript>] (HtmlProvider: IHtmlProvider) =

    [<JavaScript>]
    member this.NewAttr (name: string) (value: string) : IPagelet =
        let a = Attribute.New(HtmlProvider, name, value)
        a :> _

    [<Inline>]
    [<JavaScript>]
    member this.Alink x = this.NewAttr "alink" x

    [<Inline>]
    [<JavaScript>]
    member this.Background x = this.NewAttr "background" x

    [<Inline>]
    [<JavaScript>]
    member this.BgColor x = this.NewAttr "bgcolor" x

    [<Inline>]
    [<JavaScript>]
    member this.Clear x = this.NewAttr "clear" x

    [<Inline>]
    [<JavaScript>]
    member this.Code x = this.NewAttr "code" x

    [<Inline>]
    [<JavaScript>]
    member this.Color x = this.NewAttr "color" x

    [<Inline>]
    [<JavaScript>]
    member this.Compact x = this.NewAttr "compact" x

    [<Inline>]
    [<JavaScript>]
    member this.Face x = this.NewAttr "face" x

    [<Inline>]
    [<JavaScript>]
    member this.HSpace x = this.NewAttr "hspace" x

    [<Inline>]
    [<JavaScript>]
    member this.Language x = this.NewAttr "language" x

    [<Inline>]
    [<JavaScript>]
    member this.Link x = this.NewAttr "link" x

    [<Inline>]
    [<JavaScript>]
    member this.NoShade x = this.NewAttr "noshade" x

    [<Inline>]
    [<JavaScript>]
    member this.NoWrap x = this.NewAttr "nowrap" x

    [<Inline>]
    [<JavaScript>]
    member this.Object x = this.NewAttr "object" x

    [<Inline>]
    [<JavaScript>]
    member this.Prompt x = this.NewAttr "prompt" x

    [<Inline>]
    [<JavaScript>]
    member this.Start x = this.NewAttr "start" x

    [<Inline>]
    [<JavaScript>]
    member this.Text x = this.NewAttr "text" x

    [<Inline>]
    [<JavaScript>]
    member this.Version x = this.NewAttr "version" x

    [<Inline>]
    [<JavaScript>]
    member this.VLink x = this.NewAttr "vlink" x

    [<Inline>]
    [<JavaScript>]
    member this.VSpace x = this.NewAttr "vspace" x

/// Exposes HTML attributes.
type AttributeBuilder [<JavaScript>] (HtmlProvider: IHtmlProvider) =

    [<JavaScript>]
    member this.NewAttr (name: string) (value: string) : IPagelet =
        let a = Attribute.New(HtmlProvider, name, value)
        a :> _

    [<Inline>]
    [<JavaScript>]
    member this.Abbr x = this.NewAttr "abbr" x

    [<Inline>]
    [<JavaScript>]
    member this.AcceptCharSet x = this.NewAttr "accept-charset" x

    [<Inline>]
    [<JavaScript>]
    member this.Accept x = this.NewAttr "accept" x

    [<Inline>]
    [<JavaScript>]
    member this.AccessKey x = this.NewAttr "accesskey" x

    [<Inline>]
    [<JavaScript>]
    member this.Action x = this.NewAttr "action" x

    [<Inline>]
    [<JavaScript>]
    member this.Align x = this.NewAttr "align" x

    [<Inline>]
    [<JavaScript>]
    member this.Alt x = this.NewAttr "alt" x

    [<Inline>]
    [<JavaScript>]
    member this.AltCode x = this.NewAttr "altcode" x

    [<Inline>]
    [<JavaScript>]
    member this.Archive x = this.NewAttr "archive" x

    [<Inline>]
    [<JavaScript>]
    member this.Axis x = this.NewAttr "axis" x

    [<Inline>]
    [<JavaScript>]
    member this.Border x = this.NewAttr "border" x

    [<Inline>]
    [<JavaScript>]
    member this.BorderColor x = this.NewAttr "bordercolor" x

    [<Inline>]
    [<JavaScript>]
    member this.CellPadding x = this.NewAttr "cellpadding" x

    [<Inline>]
    [<JavaScript>]
    member this.CellSpacing x = this.NewAttr "cellspacing" x

    [<Inline>]
    [<JavaScript>]
    member this.Char x = this.NewAttr "char" x

    [<Inline>]
    [<JavaScript>]
    member this.CharOff x = this.NewAttr "charoff" x

    [<Inline>]
    [<JavaScript>]
    member this.CharSet x = this.NewAttr "charset" x

    [<Inline>]
    [<JavaScript>]
    member this.Checked x = this.NewAttr "checked" x

    [<JavaScript>]
    member this.Class x = this.NewAttr "class" x

    [<Inline>]
    [<JavaScript>]
    member this.ClassId x = this.NewAttr "classid" x

    [<Inline>]
    [<JavaScript>]
    member this.Codebase x = this.NewAttr "codebase" x

    [<Inline>]
    [<JavaScript>]
    member this.CodeType x = this.NewAttr "codetype" x

    [<Inline>]
    [<JavaScript>]
    member this.Cols x = this.NewAttr "cols" x

    [<Inline>]
    [<JavaScript>]
    member this.ColSpan x = this.NewAttr "colspan" x

    [<Inline>]
    [<JavaScript>]
    member this.Content x = this.NewAttr "content" x

    [<Inline>]
    [<JavaScript>]
    member this.Coords x = this.NewAttr "coords" x

    [<Inline>]
    [<JavaScript>]
    member this.Declare x = this.NewAttr "declare" x

    [<Inline>]
    [<JavaScript>]
    member this.Defer x = this.NewAttr "defer" x

    [<Inline>]
    [<JavaScript>]
    member this.Disabled x = this.NewAttr "disabled" x

    [<Inline>]
    [<JavaScript>]
    member this.EncType x = this.NewAttr "enctype" x

    [<Inline>]
    [<JavaScript>]
    member this.For x = this.NewAttr "for" x

    [<Inline>]
    [<JavaScript>]
    member this.FrameBorder x = this.NewAttr "frameborder" x

    [<Inline>]
    [<JavaScript>]
    member this.Headers x = this.NewAttr "headers" x

    [<Inline>]
    [<JavaScript>]
    member this.Height x = this.NewAttr "height" x

    [<Inline>]
    [<JavaScript>]
    member this.HRef x = this.NewAttr "href" x

    [<Inline>]
    [<JavaScript>]
    member this.HRefLang x = this.NewAttr "hreflang" x

    [<Inline>]
    [<JavaScript>]
    member this.HttpEquiv x = this.NewAttr "http-equiv" x

    [<Inline>]
    [<JavaScript>]
    member this.Id x = this.NewAttr "id" x

    [<Inline>]
    [<JavaScript>]
    member this.IsMap x = this.NewAttr "ismap" x

    [<Inline>]
    [<JavaScript>]
    member this.Label x = this.NewAttr "label" x

    [<Inline>]
    [<JavaScript>]
    member this.Lang x = this.NewAttr "lang" x

    [<Inline>]
    [<JavaScript>]
    member this.LongDesc x = this.NewAttr "longdesc" x

    [<Inline>]
    [<JavaScript>]
    member this.MarginHeight x = this.NewAttr "marginheight" x

    [<Inline>]
    [<JavaScript>]
    member this.MarginWidth x = this.NewAttr "marginwidth" x

    [<Inline>]
    [<JavaScript>]
    member this.MaxLength x = this.NewAttr "maxlength" x

    [<Inline>]
    [<JavaScript>]
    member this.Media x = this.NewAttr "media" x

    [<Inline>]
    [<JavaScript>]
    member this.Method x = this.NewAttr "method" x

    [<Inline>]
    [<JavaScript>]
    member this.Multiple x = this.NewAttr "multiple" x

    [<Inline>]
    [<JavaScript>]
    member this.Name x = this.NewAttr "name" x

    [<Inline>]
    [<JavaScript>]
    member this.NoHRef x = this.NewAttr "nohref" x

    [<Inline>]
    [<JavaScript>]
    member this.NoResize x = this.NewAttr "noresize" x

    [<Inline>]
    [<JavaScript>]
    member this.OnBlur x = this.NewAttr "onblur" x

    [<Inline>]
    [<JavaScript>]
    member this.OnChange x = this.NewAttr "onchange" x

    [<Inline>]
    [<JavaScript>]
    member this.OnClick x = this.NewAttr "onclick" x

    [<Inline>]
    [<JavaScript>]
    member this.OnDbClick x = this.NewAttr "ondbclick" x

    [<Inline>]
    [<JavaScript>]
    member this.OnFocus x = this.NewAttr "onfocus" x

    [<Inline>]
    [<JavaScript>]
    member this.OnKeyDown x = this.NewAttr "onkeydown" x

    [<Inline>]
    [<JavaScript>]
    member this.OnKeyPress x = this.NewAttr "onkeypress" x

    [<Inline>]
    [<JavaScript>]
    member this.OnKeyUp x = this.NewAttr "onkeyup" x

    [<Inline>]
    [<JavaScript>]
    member this.OnLoad x = this.NewAttr "onload" x

    [<Inline>]
    [<JavaScript>]
    member this.OnMouseDown x = this.NewAttr "onmousedown" x

    [<Inline>]
    [<JavaScript>]
    member this.OnMouseMove x = this.NewAttr "onmousemove" x

    [<Inline>]
    [<JavaScript>]
    member this.OnMouseOut x = this.NewAttr "onmouseout" x

    [<Inline>]
    [<JavaScript>]
    member this.OnMouseOver x = this.NewAttr "onmouseover" x

    [<Inline>]
    [<JavaScript>]
    member this.OnMouseUp x = this.NewAttr "onmouseup" x

    [<Inline>]
    [<JavaScript>]
    member this.OnReset x = this.NewAttr "onreset" x

    [<Inline>]
    [<JavaScript>]
    member this.OnSelect x = this.NewAttr "onselect" x

    [<Inline>]
    [<JavaScript>]
    member this.OnSubmit x = this.NewAttr "onsubmit" x

    [<Inline>]
    [<JavaScript>]
    member this.OnUnload x = this.NewAttr "onunload" x

    [<Inline>]
    [<JavaScript>]
    member this.Profile x = this.NewAttr "profile" x

    [<Inline>]
    [<JavaScript>]
    member this.ReadOnly x = this.NewAttr "readonly" x

    [<Inline>]
    [<JavaScript>]
    member this.Rel x = this.NewAttr "rel" x

    [<Inline>]
    [<JavaScript>]
    member this.Rev x = this.NewAttr "rev" x

    [<Inline>]
    [<JavaScript>]
    member this.Rows x = this.NewAttr "rows" x

    [<Inline>]
    [<JavaScript>]
    member this.RowSpan x = this.NewAttr "rowspan" x

    [<Inline>]
    [<JavaScript>]
    member this.Rules x = this.NewAttr "rules" x

    [<Inline>]
    [<JavaScript>]
    member this.Scheme x = this.NewAttr "scheme" x

    [<Inline>]
    [<JavaScript>]
    member this.Scope x = this.NewAttr "scope" x

    [<Inline>]
    [<JavaScript>]
    member this.Scrolling x = this.NewAttr "scrolling" x

    [<Inline>]
    [<JavaScript>]
    member this.Selected x = this.NewAttr "selected" x

    [<Inline>]
    [<JavaScript>]
    member this.Shape x = this.NewAttr "shape" x

    [<Inline>]
    [<JavaScript>]
    member this.Size x = this.NewAttr "size" x

    [<JavaScript>]
    [<Inline>]
    member this.Src x = this.NewAttr "src" x

    [<Inline>]
    [<JavaScript>]
    member this.StandBy x = this.NewAttr "standby" x

    [<Inline>]
    [<JavaScript>]
    member this.Style x = this.NewAttr "style" x

    [<Inline>]
    [<JavaScript>]
    member this.Summary x = this.NewAttr "summary" x

    [<Inline>]
    [<JavaScript>]
    member this.TabIndex x = this.NewAttr "tabindex" x

    [<Inline>]
    [<JavaScript>]
    member this.Target x = this.NewAttr "target" x

    [<Inline>]
    [<JavaScript>]
    member this.Title x = this.NewAttr "title" x

    [<Inline>]
    [<JavaScript>]
    member this.Type x = this.NewAttr "type" x

    [<Inline>]
    [<JavaScript>]
    member this.UseMap x = this.NewAttr "usemap" x

    [<Inline>]
    [<JavaScript>]
    member this.VAlign x = this.NewAttr "valign" x

    [<Inline>]
    [<JavaScript>]
    member this.Value x = this.NewAttr "value" x

    [<Inline>]
    [<JavaScript>]
    member this.ValueType x = this.NewAttr "valuetype" x

    [<Inline>]
    [<JavaScript>]
    member this.Width x = this.NewAttr "width" x

    [<JavaScript>]
    member this.CheckBox = this.NewAttr "type" "checkbox"

    [<JavaScript>]
    member this.Hidden = this.NewAttr "type" "hidden"

    [<JavaScript>]
    member this.Radio = this.NewAttr "type" "radio"

    [<JavaScript>]
    member this.Reset = this.NewAttr "type" "reset"

    [<JavaScript>]
    member this.Submit = this.NewAttr "type" "submit"

    [<JavaScript>]
    member this.Password = this.NewAttr "type" "password"

    [<JavaScript>]
    member this.TextField = this.NewAttr "type" "textfield"

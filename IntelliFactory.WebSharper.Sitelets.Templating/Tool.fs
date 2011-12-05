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

namespace IntelliFactory.WebSharper.Sitelets.Templating

type internal HoleVarType =
    | InBody
    | InHead
    | Attribute

module private Constants =

    let ToolName = "WebSharper xhtml2fs"

    let ToolVersion =
        let fvi =
            System.Reflection.Assembly.GetExecutingAssembly().Location
            |> System.Diagnostics.FileVersionInfo.GetVersionInfo
        string fvi.FileVersion

module private Utils =

    let LowerCaseCapitalize (s: string) =
        let chars = s.ToLower().ToCharArray()
        if chars.Length > 0 then
            chars.[0] <- System.Char.ToUpper(chars.[0])
        new System.String(chars)

    let Capitalize (s: string) =
        let chars = s.ToCharArray()
        if chars.Length > 0 then
            chars.[0] <- System.Char.ToUpper(chars.[0])
        new System.String(chars)

    let StringTypeOfHoleVarType ((hole: string, ty: HoleVarType)) =
        match ty with
        | HoleVarType.Attribute ->
            Capitalize hole + ": Context<'Action> -> string"
        | HoleVarType.InBody ->
            Capitalize hole + ": Context<'Action> -> list<Element<Web.Control>>"
        | HoleVarType.InHead ->
            Capitalize hole + ": Context<'Action> -> list<Element<unit>>"

[<AutoOpen>]
module internal PrettyPrinting =

    type private Line =
        | Join of Line * Line
        | Spaced of Line * Line
        | Word of string

    type private Lines =
        | Empty
        | Indented of Lines
        | Single of Line
        | Vertical of Lines * Lines

    type Layout =
        private
        | Many of Lines
        | One of Line

    let private toLines layout =
        match layout with
        | Many x -> x
        | One x -> Single x

    let ( @@ ) a b =
        Many (Vertical (toLines a, toLines b))

    let ( @@-- ) a b =
        a @@ Many (Indented (Indented (toLines b)))

    let ( @@- ) a b =
        a @@ Many (Indented (toLines b))

    let ( -- ) a b =
        match a, b with
        | One a, One b -> One (Spaced (a, b))
        | _ -> a @@-- b

    let ( ^^ ) a b =
        match a, b with
        | One a, One b -> One (Join (a, b))
        | _ -> a @@-- b

    let emptyL = Many Empty

    let wordL word =
        One (Word word)

    let private pack layout c1 c2 =
        let ( ^^ ) a b = Join (a, b)
        let ( @@ ) a b = Vertical (a, b)
        match layout with
        | One layout ->
            One (Word c1 ^^ layout ^^ Word c2)
        | Many lines ->
            let sW x = Single (Word x)
            Many (sW c1 @@ Indented lines @@ sW c2)

    let braceL layout =
        pack layout "{" "}"

    let bracketL layout =
        pack layout "(" ")"

    let squareBracketL layout =
        pack layout "[" "]"

    let semiListL layouts =
        match layouts with
        | [] -> emptyL
        | xs -> List.reduce (fun x y -> x ^^ wordL ";" ^^ y) xs

    let aboveListL layouts =
        match layouts with
        | [] -> emptyL
        | _ -> List.reduce (@@) layouts

    let EmitLayout (writer: System.IO.TextWriter) layout =
        let rec emitLines level lines =
            match lines with
            | Empty -> ()
            | Indented lines ->
                emitLines (1 + level) lines
            | Single line ->
                writer.Write("".PadLeft(2 * level))
                emitLine line
                writer.WriteLine()
            | Vertical (x, y) ->
                emitLines level x
                emitLines level y
        and emitLine line =
            match line with
            | Join (x, y) ->
                emitLine x
                emitLine y
            | Spaced (x, y) ->
                emitLine x
                writer.Write ' '
                emitLine y
            | Word x ->
                writer.Write x
        match layout with
        | Many lines -> emitLines 0 lines
        | One line -> emitLine line; writer.WriteLine()

module internal Parsing =

    type Options =
        {
            Namespace : string
            Page : string
        }

    let rec WriteNode (isInsideHead: bool) (isTopLevelBodyNode: bool)
        (options: Options) (holes: (string * HoleVarType) list)
        (doc: System.Xml.XmlNode) : ((string * HoleVarType) list * Layout * string option * bool) =
        // Escape special characters in strings:
        // " -> \"
        let STR_ESCAPE (s: string) =
            let bad = System.Text.RegularExpressions.Regex "[\r\n\"\\\\]"
            bad.Replace(s,
                System.Text.RegularExpressions.MatchEvaluator(fun e ->
                    match e.Groups.[0].Value.[0] with
                    | '\r' -> @"\r"
                    | '\n' -> @"\n"
                    | '\\' -> @"\\"
                    | '"' -> @"\"""
                    | _ -> e.Groups.[0].Value))
        let C (s: string) =
            let pref, tag =
                if s.Contains(".") then
                    let ind = s.LastIndexOf(".")
                    Some <| s.Substring(0, ind), s.Substring(ind+1, s.Length-ind-1)
                else
                    None, s
            tag
            |> Utils.LowerCaseCapitalize
            |> function
                | "Applet" -> "Deprecated.Applet"
                | "Basefont" -> "Deprecated.BaseFont"
                | "Blockquote" -> "BlockQuote"
                | "Center" -> "Deprecated.Center"
                | "Colgroup" -> "ColGroup"
                | "Dd" -> "DD"
                | "Dir" -> "Deprecated.Dir"
                | "Dl" -> "DL"
                | "Dt" -> "DT"
                | "Fieldset" -> "FieldSet"
                | "Font" -> "Deprecated.Font"
                | "Frameset" -> "FrameSet"
                | "Hr" -> "HR"
                | "Html" -> "HTML"
                | "Iframe" -> "IFrame"
                | "Isindex" -> "Deprecated.IsIndex"
                | "Li" -> "LI"
                | "Map" -> "Tags.Map"
                | "Menu" -> "Deprecated.Menu"
                | "Meta" -> "Tags.Meta"
                | "Noframes" -> "NoFrames"
                | "Noscript" -> "NoScript"
                | "Object" -> "Tags.Object"
                | "Ol" -> "OL"
                | "Optgroup" -> "OptGroup"
                | "Option" -> "Tags.Option"
                | "S" -> "Deprecated.S"
                | "Strike" -> "Deprecated.Strike"
                | "Tbody" -> "TBody"
                | "Td" -> "TD"
                | "Textarea" -> "TextArea"
                | "Tfoot" -> "TFoot"
                | "Th" -> "TH"
                | "Thead" -> "THead"
                | "Title" -> "Tags.Title"
                | "Tr" -> "TR"
                | "Tt" -> "TT"
                | "U" -> "Deprecated.U"
                | "Ul" -> "UL"
                | s -> s
            |> fun res ->
                match pref with
                | None ->
                    res
                | Some pref ->
                    pref + "." + res
        let CA (s: string) =
            s
            |> Utils.LowerCaseCapitalize
            |> function
                | "Acceptcharset" -> "AcceptCharSet"
                | "Accesskey" -> "AccessKey"
                | "Action" -> "Attr.Action"
                | "Alink" -> "Deprecated.Alink"
                | "Altcode" -> "AltCode"
                | "Background" -> "Deprecated.Background"
                | "Bgcolor" -> "Deprecated.BgColor"
                | "Bordercolor" -> "BorderColor"
                | "Cellpadding" -> "CellPadding"
                | "Cellspacing" -> "CellSpacing"
                | "Charoff" -> "CharOff"
                | "Charset" -> "CharSet"
                | "Cite" -> "Attr.Cite"
                | "Classid" -> "ClassId"
                | "Clear" -> "Deprecated.Clear"
                | "Code" -> "Deprecated.Code"
                | "Codebase" -> "CodeBase"
                | "Codetype" -> "CodeType"
                | "Color" -> "Deprecated.Color"
                | "Colspan" -> "ColSpan"
                | "Compact" -> "Deprecated.Compact"
                | "Content" -> "Attr.Content"
                | "Data" -> "Attr.Data"
                | "Datetime" -> "Attr.DateTime"
                | "Dir" -> "Attr.Dir"
                | "Enctype" -> "EncType"
                | "Face" -> "Deprecated.Face"
                | "Frame" -> "Attr.Frame"
                | "Frameborder" -> "FrameBorder"
                | "Href" -> "HRef"
                | "Hreflang" -> "HRefLang"
                | "Hspace" -> "Deprecated.HSpace"
                | "Http-equiv" -> "HttpEquiv"
                | "Ismap" -> "IsMap"
                | "Label" -> "Attr.Label"
                | "Language" -> "Deprecated.Language"
                | "Link" -> "Deprecated.Link"
                | "Longdesc" -> "LongDesc"
                | "Marginheight" -> "MarginHeight"
                | "Marginwidth" -> "MarginWidth"
                | "Maxlength" -> "MaxLength"
                | "Nohref" -> "NoHRef"
                | "Noresize" -> "NoResize"
                | "Noshade" -> "Deprecated.NoShade"
                | "Nowrap" -> "Deprecated.NoWrap"
                | "Object" -> "Deprecated.Object"
                | "Onblur" -> "OnBlur"
                | "Onchange" -> "OnChange"
                | "Onclick" -> "OnClick"
                | "Ondbclick" -> "OnDbClick"
                | "Onfocus" -> "OnFocus"
                | "Onkeydown" -> "OnKeyDown"
                | "Onkeypress" -> "OnKeyPress"
                | "Onkeyup" -> "OnKeyUp"
                | "Onload" -> "OnLoad"
                | "Onmousedown" -> "OnMouseDown"
                | "Onmousemove" -> "OnMouseMove"
                | "Onmouseout" -> "OnMouseOut"
                | "Onmouseover" -> "OnMouseOver"
                | "Onmouseup" -> "OnMouseUp"
                | "Onreset" -> "OnReset"
                | "Onselect" -> "OnSelect"
                | "Onsubmit" -> "OnSubmit"
                | "Onunload" -> "OnUnload"
                | "Prompt" -> "Deprecated.Prompt"
                | "Readonly" -> "ReadOnly"
                | "Rowspan" -> "RowSpan"
                | "Span" -> "Attr.Span"
                | "Standby" -> "StandBy"
                | "Start" -> "Deprecated.Start"
                | "Tabindex" -> "TabIndex"
                | "Text" -> "Deprecated.Text"
                | "Usemap" -> "UseMap"
                | "Valign" -> "VAlign"
                | "Valuetype" -> "ValueType"
                | "Version" -> "Deprecated.Version"
                | "Vlink" -> "Deprecated.VLink"
                | "Vspace" -> "Deprecated.VSpace"
                | "Checkbox" -> "CheckBox"
                | "Textfield" -> "TextField"
                | s -> s
        let attrs =
            match doc.Attributes with
            | null ->
                emptyL
            | _ ->
                [ for attr in doc.Attributes do
                    yield attr ]
                |> List.map (fun attr ->
                    match attr.NodeType with
                    | System.Xml.XmlNodeType.Attribute ->
                        Some (CA attr.Name, attr.InnerText)
                    | _ ->
                        None
                )
                |> List.choose id
                |> List.map (fun (k, v) ->
                    let value =
                        match k.ToLower().Trim() with
                        | "href"
                        | "src" ->
                            sprintf "(context.ResolveUrl(\"%s\"))" v
                        | _ ->
                            sprintf "\"%s\"" v
                    wordL k -- wordL value)
                |> semiListL
        let content, isEmptyContent, headOption, titleOption, isEmpty =
            // Inner content
            // Special handle Body/Head sections
            match doc.NodeType with
            // Do we have a document?
            | System.Xml.XmlNodeType.Document ->
                [ for node in doc.ChildNodes do
                    yield node ]
                // Then find the <html> tag
                |> Seq.tryFind (fun node ->
                    node.Name.ToLower() = "html"
                    && node.NodeType = System.Xml.XmlNodeType.Element)
                |> function
                    | None ->
                        failwith "Invalid template file (no <html> node)"
                    | Some html ->
                        // Extract the <head> section and process <body> only
                        // Holes are only respected inside <body>
                        html.ChildNodes
                        |> Seq.cast
                        |> Seq.fold (fun (holes: (string * HoleVarType) list, layouts,
                                            hopt: _ option, topt, isEmpty)
                                        (node: System.Xml.XmlNode) ->
                            // The <Head> section
                            if node.Name.ToLower() = "head" then
                                if hopt.IsSome then
                                    failwith "Multiple <head> sections found"
                                else
                                    node.ChildNodes
                                    |> Seq.cast
                                    |> Seq.fold (fun (_holes, _layouts, topt, isempty) node ->
                                        let holes, layoutHead, topt, isEmpty = WriteNode true false options _holes node
                                        if topt.IsSome then
                                            // We just found the Title tag
                                            // We don't render it, but simply propagate its text
                                            holes, _layouts, topt, isEmpty && isempty
                                        else
                                            // All other tags are rendered
                                            holes, layoutHead :: _layouts, topt, isEmpty && isempty) (holes, [], None, true)
                                    // The <head> section is not rendered but collected
                                    |> fun (headholes, layoutHeadList, topt, isEmpty) ->
                                        let layout = layoutHeadList |> List.rev |> aboveListL
                                        headholes, layouts, Some layout, topt, isEmpty
                            elif node.Name.ToLower() = "body" then
                                node.ChildNodes
                                |> Seq.cast
                                |> Seq.fold (fun (_holes, _layouts, isempty) node ->
                                    let holes, layout, _, isEmpty = WriteNode false true options _holes node
                                    holes (*@ _holes*), layout :: _layouts, isempty && isEmpty) (holes, layouts, true)
                                |> fun (holes, _layouts, isEmpty) ->
                                    let layout = _layouts |> List.rev |> aboveListL |> squareBracketL
                                    holes, layout :: layouts, hopt, None, isEmpty
                            else
                                holes, layouts, hopt, None, isEmpty
                        ) ([], [], None, None, true)
                |> fun (holes, layouts, hopt, topt, isEmpty) ->
                    (holes |> List.rev, layouts |> List.rev |> aboveListL),
                    (layouts |> List.length |> fun l -> l <= 0),
                    hopt, topt, isEmpty
            // Or something else?
            | _ ->
                [ for node in doc.ChildNodes do
                    yield node ]
                |> List.fold (fun (holes, layouts, isempty) node ->
                    let holes, layout, _, isEmpty = WriteNode false false options holes node
                    holes, layout :: layouts, isempty && isEmpty
                ) ([], [], true)
                |> fun (holes, layouts, isEmpty) ->
                    (holes |> List.rev, layouts |> List.rev |> aboveListL),
                    (layouts |> List.length |> fun l -> l <= 0),
                    None, None, isEmpty
        match doc.NodeType with
        | System.Xml.XmlNodeType.Text ->
            let inner = doc.InnerText
            let regex =
                System.Text.RegularExpressions.Regex(
                    "\A((?s).*)(\$\{([A-Za-z_]+[0-9_]*)\})((?s).*)",
                    System.Text.RegularExpressions.RegexOptions.Multiline)
            // Extract the meta variables from the text and create
            // a layout that represents the entire block.
            let rec MAP (acc: ((string * HoleVarType) * string) list) inner =
                let matches = regex.Match inner
                if matches.Success then
                    let ty =
                        if not isInsideHead then
                            HoleVarType.InBody
                        else
                            HoleVarType.InHead
                    MAP (((matches.Groups.[3].Value, ty), matches.Groups.[4].Value) :: acc) matches.Groups.[1].Value
                else
                    let rest =
                        acc
                        |> List.map (fun ((hole, holeTy), followStr) ->
                            // Are we outside the <Head> tag?
                            if not isInsideHead && not isTopLevelBodyNode then
                                [ wordL ("] -< args." + Utils.Capitalize hole + " context -<")
                                  wordL "[ Text" -- (wordL ("\"" + STR_ESCAPE followStr + "\" ]")) -- wordL "-<"
                                  wordL "["]
                            // Are we inside the <Head> tag?
                            else
                                [ wordL ("args." + Utils.Capitalize hole + " context @ [")
                                  wordL " Text" -- (wordL ("\"" + STR_ESCAPE followStr + "\""))
                                  wordL "] @" ]
                        )
                        |> List.concat
                    // Are we outside the <Head> tag?
                    if not isInsideHead && not isTopLevelBodyNode then
                        [wordL "] -< [ Text" -- wordL ("\"" + STR_ESCAPE inner + "\" ]") -- wordL "-<"
                         wordL "["] @ rest
                    // Are we inside the <Head> tag?
                    else
                        [wordL "Text" -- wordL ("\"" + STR_ESCAPE inner + "\" ] @") ]
                        @ rest
                        @ [wordL "["]
                        @ [wordL ""]
                    |> aboveListL
                    |> fun layout ->
                        acc |> List.map fst |> fun new_holes -> holes @ new_holes, layout, None, false
            MAP [] inner
        // ----------------------------------------------------------
        // <title>
        // ----------------------------------------------------------
        // <title> elements are not rendered, their text is collected
        // <title> attributes are ignored, naturally, because they are non-sense.
        // There can not be any holes in <title> tags. (But this could be relaxed.)
        | System.Xml.XmlNodeType.Element when doc.Name.ToLower() = "title" ->
            holes, emptyL, Some doc.InnerText, true
        // ----------------------------------------------------------
        // <script>
        // ----------------------------------------------------------
        // <script> elements are treated specially to avoid Text blocks and
        // are written verbatim.
        // There can not be any holes in <script> tags.
        // 1. No attributes
        | System.Xml.XmlNodeType.Element
            when doc.Name.ToLower() = "script" && (doc.Attributes=null || doc.Attributes.Count < 1) ->
            [ wordL "Script" -- wordL "["
              @@- (doc.InnerText |> STR_ESCAPE |> fun s -> "Html.VerbatimContent \"" + s + "\"" |> wordL)
              wordL "]" ]
            |> aboveListL
            |> fun layout ->
                holes, layout, None, false
        // <script> elements are treated specially to avoid Text blocks
        // 2. With attributes
        | System.Xml.XmlNodeType.Element when doc.Name.ToLower() = "script" ->
            [ wordL "Script" -- squareBracketL attrs ^^ wordL "-<" ^^ wordL "["
              @@- (doc.InnerText |> STR_ESCAPE |> fun s -> "Html.VerbatimContent \"" + s + "\"" |> wordL)
              wordL "]" ]
            |> aboveListL
            |> fun layout ->
                holes, layout, None, false
        // ----------------------------------------------------------
        // Tags without attributes
        | System.Xml.XmlNodeType.Element when doc.Attributes=null || doc.Attributes.Count < 1 && not isEmptyContent->
            [ wordL (C doc.Name)
              @@- wordL "["
              @@- (snd content)
              @@ wordL "]"]
            |> aboveListL
            |> fun layout ->
                holes @ fst content, layout, None, false
        // Tags without attributes and without content
        | System.Xml.XmlNodeType.Element when doc.Attributes=null || doc.Attributes.Count < 1 && isEmptyContent ->
            wordL (C doc.Name + " []")
            |> fun layout ->
                holes @ fst content, layout, None, false
        // Tags with attributes and without content
        | System.Xml.XmlNodeType.Element when doc.ChildNodes=null || doc.ChildNodes.Count < 1 ->
            [ wordL (C doc.Name) -- squareBracketL attrs ]
            |> aboveListL
            |> fun layout ->
                holes, layout, None, false
        // Comments are translated to XML comment constructors
        | System.Xml.XmlNodeType.Comment ->
            holes, wordL ("Tags.Comment \"" + STR_ESCAPE doc.InnerText + "\""), None, false
        // Tags with attributes and with content
        | System.Xml.XmlNodeType.Element ->
            [ wordL (C doc.Name) -- (squareBracketL attrs ^^ wordL "-<")
              @@- wordL "["
              @@ (snd content)
              @@ wordL "]"]
            |> aboveListL
            |> fun layout ->
                holes @ fst content, layout, None, false
        // The main document node
        | System.Xml.XmlNodeType.Document ->
            let ns = options.Namespace |> Utils.Capitalize
            let template = options.Page |> Utils.Capitalize
            let argTyName = template + "Args"
            [
              wordL "//------------------------------------------------------------------------"
              wordL "// <generated>"
              wordL (sprintf "//    This code was generated by %s version %s."
                        Constants.ToolName Constants.ToolVersion)
              wordL "//"
              wordL "//    Changes to this file will be lost is the code is regenerated."
              wordL "// </generated>"
              wordL "//------------------------------------------------------------------------"
              wordL ("namespace " + ns + ".Templates")
              wordL ""
              wordL ("module " + template + " =")
              @@- wordL "open System.Web.UI"
              @@ wordL "open IntelliFactory.Html"
              @@ wordL "open IntelliFactory.WebSharper"
              @@ wordL "open IntelliFactory.WebSharper.Sitelets"
              @@ wordL ""
              @@ (if (content |> fst |> List.length > 0) then
                    wordL ("type " + argTyName + "<'Action> =")
                     @@- (content |> fst |> List.rev |> List.map (fun hole -> wordL (Utils.StringTypeOfHoleVarType hole)) |> aboveListL |> braceL)
                  else
                    wordL ("type " + argTyName + "<'Action> = | NoArgs")
                  )
              @@ wordL ""
              @@ aboveListL [
                (if isEmpty then
                    wordL ("let " + template + "Body") -- wordL ("context") -- (wordL ("args: " + argTyName + "<'Action>") |> bracketL) -- wordL "= []"
                 else
                    wordL ("let " + template + "Body") -- wordL ("context") -- (wordL ("args: " + argTyName + "<'Action>") |> bracketL) -- wordL "=" @@- snd content)
                wordL ""
                wordL ("let " + template) -- wordL "title" -- (wordL ("args: " + argTyName + "<'Action>") |> bracketL) -- wordL "="
                  @@- wordL "PageContent <| fun context ->"
                  @@-
                    braceL (
                        wordL "Sitelets.Page.Default with"
                        @@-- (
                            wordL "Title = title"
                            @@ (if headOption.IsSome
                                then wordL "Head = " -- squareBracketL headOption.Value
                                else wordL "Head = [ ]")
                            @@ wordL ("Body = " + template + "Body context args")
                        )
                    )
                ]
              @@ wordL ""
            ]
            |> aboveListL
            |> fun layout ->
                holes, layout, None, false
        | _ ->
            holes, emptyL, None, true

module Tool =

    let GetPageName filename =
        let fname = System.IO.Path.GetFileName filename
        let ind = fname.LastIndexOf(".template.xml")
        if ind > 0 then fname.Substring(0, ind) else
            let ind = fname.LastIndexOf(".")
            if ind > 0
            then fname.Substring(0, ind)
            else fname
        |> Utils.Capitalize

    let ToolName = Constants.ToolName
    let ToolVersion = Constants.ToolVersion

    type Options =
        {
            Namespace : string
            Page : string
        }

        static member Default =
            {
                Namespace = "Website"
                Page = ""
            }

        member internal this.ParserOptions : Parsing.Options =
            { Namespace = this.Namespace; Page = this.Page }

    let ParseToString (options: Options) (s: string) =
        let doc = System.Xml.XmlDocument()
        use r = System.Xml.XmlTextReader.Create s
        doc.Load r
        let writer = new System.IO.StringWriter() :> System.IO.TextWriter
        Parsing.WriteNode false false options.ParserOptions [] doc
        |> fun (_, layout, _, _) -> layout
        |> EmitLayout writer
        writer.Close()
        writer.ToString()

    let ParseToWriter (options: Options) (writer: System.IO.TextWriter)
        (reader: System.IO.TextReader) =
        let doc = System.Xml.XmlDocument()
        doc.Load(reader)
        doc
        |> Parsing.WriteNode false false options.ParserOptions []
        |> fun (_, layout, _, _) -> layout
        |> EmitLayout writer
        writer.Close()

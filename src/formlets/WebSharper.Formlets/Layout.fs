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
namespace WebSharper.Formlets

open System.Collections.Generic
open WebSharper
open WebSharper.JavaScript
open IntelliFactory.Formlets.Base
open WebSharper.JQuery
open WebSharper.Html.Client

/// Represents the visual element of a formlet.
type Body =
    {
        Element: Element
        Label: option<unit -> Element>
    }
    with
        [<JavaScript>]
        static member New el l =
            {Element = el; Label = l}

/// Class usable for storing elements for removal.
type internal ElementStore<'T when 'T : equality> [<JavaScript>] internal ()=

    [<DefaultValue>]
    val mutable private store : Dictionary<'T, unit -> unit>

    [<JavaScript>]
    member this.Init () =
        this.store <- new Dictionary<'T,unit -> unit>()

    [<JavaScript>]
    [<Name "NewElementStore">]
    static member New<'T>() =
        let store = new ElementStore<'T>()
        store.Init()
        store

    [<JavaScript>]
    member this.RegisterElement(key: 'T, f: unit -> unit) =
        if this.store.ContainsKey key |> not then
            this.store.[key] <- f

    [<JavaScript>]
    member this.Remove(key: 'T) =
        if this.store.ContainsKey key then
            this.store.[key] ()
            this.store.Remove(key)
            |> ignore

type internal LayoutManager =
    {
        Insert : int -> Body -> list<Element>
        Panel : Element
    }

module Layout =
    /// Represents padding settings for an Html element.
    type Padding =
        {
            Left : option<int>
            Right : option<int>
            Top : option<int>
            Bottom : option<int>
        }
        [<JavaScript>]
        static member Default = {
            Left = None
            Right = None
            Top = None
            Bottom = None
        }

    /// Represents horizontal or vertical placement.
    type Placement =
        | Left
        | Right
        | Top
        | Bottom

    /// Represents horizontal alignment.
    type Align =
        | Left
        | Right

    /// Represents vertical alignment.
    type VerticalAlign =
        | Top
        | Middle
        | Bottom

    /// Represents label configuration
    type LabelConfiguration =
        {
            Align: Align
            VerticalAlign: VerticalAlign
            Placement: Placement
        }
        with
            [<JavaScript>]
            static member Default =
                {
                    Align = Left
                    VerticalAlign = Middle
                    Placement = Placement.Left
                }

    /// Represents form row configuration settings.
    type FormRowConfiguration =
        {
            Padding : option<Padding>
            Color : option<int -> string>
            Class : option<int -> string>
            Style : option<int -> string>
            LabelConfiguration : option<LabelConfiguration>
        }
        with
            /// Default form row configuration.
            [<JavaScript>]
            static member Default =
                {
                    Padding = None
                    Color = None
                    Class = None
                    Style = None
                    LabelConfiguration = None
                }

/// Defines formlets and their operations.
type LayoutProvider [<JavaScript>](LayoutUtils: LayoutUtils) =

    [<JavaScript>]
    member private this.HorizontalAlignElem align (el: Element) =
        let float =
            match align with
            | Layout.Align.Right    -> "right"
            | Layout.Align.Left     -> "left"
        Div [Attr.Style ("float:" + float + ";")] -< [el]

    [<JavaScript>]
    member private this.VerticalAlignedTD valign (elem: Element) =
        let valign =
            match valign with
            | Layout.VerticalAlign.Top      -> "top"
            | Layout.VerticalAlign.Middle   -> "middle"
            | Layout.VerticalAlign.Bottom   -> "bottom"
        let cell = TD [elem]
        cell.SetCss("vertical-align", valign)
        cell

    [<JavaScript>]
    member private this.MakeRow (rowConfig: Layout.FormRowConfiguration) (rowIndex: int) (body: Body) =

        // Get padding from rowConfig or default
        let padding =
            rowConfig.Padding
            |> Utils.Maybe Layout.Padding.Default id

        let paddingLeft =
            padding.Left
            |> Utils.Maybe 0 id

        let paddingTop =
            padding.Top
            |> Utils.Maybe 0 id

        let paddingRight =
            padding.Right
            |> Utils.Maybe 0 id

        let paddingBottom =
            padding.Bottom
            |> Utils.Maybe 0 id

        // Create a table-cell with padding
        let makeCell    (l:int)
                        (t:int)
                        (r:int)
                        (b:int)
                        (csp: bool)
                        (valign:option<Layout.VerticalAlign>)
                        (elem: Element) =
            // Padding
            let paddingStyle =
                [
                    "padding-left: " , l
                    "padding-top: "  , t
                    "padding-right: ", r
                    "padding-bottom: " , b
                ]
                |> List.map (fun (k,v) -> k + (string v) + "px;")
                |> Seq.reduce (fun x y -> x  + y)

            // Vertical align
            let valignStyle =
                valign
                |> Utils.Maybe "" (fun valign ->
                    let value =
                        match valign with
                        | Layout.VerticalAlign.Top      -> "top"
                        | Layout.VerticalAlign.Middle   -> "middle"
                        | Layout.VerticalAlign.Bottom   -> "bottom"
                    "vertical-align: " + value + ";"
                )

            let style = Attr.Style (paddingStyle + ";" +  valignStyle)
            // Colspan
            let colSpan =
                if csp then
                    [(Attr.ColSpan "2")]
                else []
            TD <| style :: colSpan @ [elem] :> Pagelet

        let elem = body.Element

        // Cells for the row
        let cells =
            // Do we have a label
            match body.Label with
            | None          ->
                // No Label
                [
                    makeCell    paddingLeft
                                paddingTop
                                paddingRight
                                paddingBottom
                                true
                                None
                                elem
                ]
            | Some labelGen  ->

                // Use label configuration
                let labelConf =
                    rowConfig.LabelConfiguration
                    |> Utils.Maybe Layout.LabelConfiguration.Default id

                // Aligned label
                let label =
                    labelGen ()
                    |> this.HorizontalAlignElem labelConf.Align

                match labelConf.Placement with
                | Layout.Placement.Top ->
                    [
                        Utils.InTable [[label];[elem]]
                        |> makeCell paddingLeft
                                    paddingTop
                                    paddingRight
                                    paddingBottom
                                    true
                                    None
                    ]
                | Layout.Placement.Bottom ->
                    [
                        Utils.InTable [[elem];[label]]
                        |> makeCell paddingLeft
                                    paddingTop
                                    paddingRight
                                    paddingBottom
                                    true
                                    None
                    ]
                | Layout.Placement.Left ->
                    [
                        makeCell    paddingLeft
                                    paddingTop
                                    0
                                    paddingBottom
                                    false
                                    (Some labelConf.VerticalAlign)
                                    label

                        makeCell 0  paddingTop
                                    paddingRight
                                    paddingBottom
                                    false
                                    None
                                    elem
                    ]
                | Layout.Placement.Right ->
                    [
                        makeCell    paddingLeft
                                    paddingTop
                                    0
                                    paddingBottom
                                    false
                                    (Some labelConf.VerticalAlign)
                                    elem

                        makeCell 0  paddingTop
                                    paddingRight
                                    paddingBottom
                                    false
                                    None
                                    label
                    ]

        let rowClass =
            rowConfig.Class
            |> Utils.Maybe [] (fun classGen ->
                [Attr.Class <| classGen rowIndex]
            )

        let rowColorStyleProp =
            rowConfig.Color
            |> Utils.Maybe [] (fun colGen ->
                let col =  colGen rowIndex
                ["background-color: "  + col]
            )
        let rowStyleProp =
            rowConfig.Style
            |> Utils.Maybe [] (fun styleGen ->
                [styleGen rowIndex]
            )
        let rowStyle =
            match rowColorStyleProp @ rowStyleProp with
            | [] -> []
            | vs ->
                [
                    Seq.reduce (fun x y -> x + ";" + y) vs
                    |> Attr.Style
                ]
        TR <| rowClass @ rowStyle @ rowStyle @ cells

    [<JavaScript>]
    member private this.MakeLayout (lm: unit -> LayoutManager) : Layout<Body> =
        LayoutUtils.New <| fun () ->
            let lm = lm ()
            // Store inserted elements
            let store = ElementStore.New ()

            // Insert new body element
            let insert (ix : int) (bd: Body) =
                // New row and key
                let elemId = bd.Element.Id
                // Inserts
                let newElems = lm.Insert ix bd

                // Register element for removal.
                store.RegisterElement(elemId, fun () ->
                    for e in newElems do
                        e.Remove()
                )
            // Remove elements
            let remove (elems: seq<Body>)=
                for b in elems do
                    store.Remove b.Element.Id
            {
                Body = {Element = lm.Panel; Label = None}
                Insert = insert
                Remove = remove
                SyncRoot = null
            }

    [<JavaScript>]
    member internal this.RowLayout (rowConfig: Layout.FormRowConfiguration) : Layout<Body> =
        LayoutUtils.New <| fun () ->
            let panel = TBody []
            let container = Table [panel]

            // Store inserted elements
            let store = ElementStore.New ()

            // Insert new body element
            let insert (rowIx : int) (body: Body) =
                let elemId = body.Element.Id

                // Inserts
                let row = this.MakeRow rowConfig rowIx body
                let jqPanel = JQuery.Of(panel.Body)

                let index = ref 0
                let inserted = ref false
                jqPanel.Children().Each(fun el ->
                    let jqRow = JQuery.Of(el : Dom.Element)
                    if rowIx = index.Value then
                        JQuery.Of(row.Body).InsertBefore(jqRow).Ignore
                        // Since it's parent is already rendered,
                        // explicit rendering is required.
                        row.Render()
                        inserted := true
                    incr index
                ).Ignore
                if not inserted.Value then
                    panel.Append(row)

                // Register element for removal.
                store.RegisterElement(elemId, fun () ->
                    row.Remove ()
                )

            // Remove elements
            let remove (elems: seq<Body>)=
                for b in elems do
                    store.Remove b.Element.Id
            {
                Body = {Element = container; Label = None}
                Insert = insert
                Remove = remove
                SyncRoot = null
            }

    [<JavaScript>]
    member private this.ColumnLayout (rowConfig: Layout.FormRowConfiguration) : Layout<Body> =
        LayoutUtils.New <| fun () ->
            let row = TR []
            let container = Table [TBody [row]]

            // Store inserted elements
            let store = ElementStore.New ()

            // Insert new body element
            let insert (rowIx : int) (body: Body) =

                // New row and key
                let elemId = body.Element.Id

                // Inserts
                let newCol = TD [Table [TBody [this.MakeRow rowConfig rowIx body]]]
                let jqPanel = JQuery.Of(row.Body)
                let index = ref 0
                let inserted = ref false
                jqPanel.Children().Each(fun el ->
                    let jqCol = JQuery.Of(el : Dom.Element)
                    if rowIx = index.Value then
                        JQuery.Of(newCol.Body).InsertBefore(jqCol).Ignore
                        // Since it's parent is already rendered,
                        // explicit rendering is required.
                        newCol.Render()
                        inserted := true
                    incr index
                ).Ignore
                if not inserted.Value then
                    row.Append(newCol)

                // Register element for removal.
                store.RegisterElement(elemId, fun () ->
                    newCol.Remove ()
                )

            // Remove elements
            let remove (elems: seq<Body>)=
                for b in elems do
                    store.Remove b.Element.Id
            {
                Body = {Element = container; Label = None}
                Insert = insert
                Remove = remove
                SyncRoot = null
            }

    /// Flowlet layout for rendering subsequent formlets by replacing
    /// previous ones.
    [<JavaScript>]
    member this.Flowlet : Layout<Body> =
        let lm () =
            // Panel
            let panel = Div []

            // Insert a row
            let insert (_: int) bd =
                let label =
                    if bd.Label.IsSome then
                        bd.Label.Value ()
                    else
                        Span []
                let nextScreen =
                    Utils.InTable [[label; Div [bd.Element]]]
                panel.Clear()
                panel.Append(nextScreen)
                [nextScreen]
            { Insert = insert; Panel = panel}
        this.MakeLayout lm

    /// Produces a layout with the given label configuration.
    [<JavaScript>]
    member this.LabelLayout (lc : Layout.LabelConfiguration) =
        this.RowLayout {Layout.FormRowConfiguration.Default with LabelConfiguration = Some lc}

    /// Vertical layout component.
    [<JavaScript>]
    member this.Vertical  : Layout<Body> =
         this.RowLayout Layout.FormRowConfiguration.Default

    /// Horizontal layout component.
    [<JavaScript>]
    member this.Horizontal : Layout<Body> =
        this.ColumnLayout Layout.FormRowConfiguration.Default

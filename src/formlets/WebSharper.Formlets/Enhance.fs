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

open WebSharper
open WebSharper.JavaScript
open WebSharper.Html.Client
open WebSharper.Html.Client.Events

module H = WebSharper.Html.Client.Default

/// Defines formlets and their operations.
module Enhance =

    open IntelliFactory.Formlets.Base
    open WebSharper.Formlets.Controls
    open IntelliFactory.Reactive

    /// Creates a formlet enhanced with reset capabilities defined by the second argument.
    [<JavaScript>]
    let WithResetFormlet (formlet: Formlet<'T>) (reset: Formlet<'U>) : Formlet<'T> =
        let formlet =
            formlet
            |> Formlet.WithLayoutOrDefault
            |> Formlet.ApplyLayout
            |> Formlet.InitWithFailure
            |> Formlet.LiftResult
            |> Formlet.WithNotificationChannel
        let button = Formlet.LiftResult reset
        Formlet.Do {
            let! (v, notify) = formlet
            let! b = button
            do
                match b with
                | Success _ -> notify ()
                | _         -> ()
            return v
        }
        |> Formlet.MapResult Result.Join
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Invokes the given callback function on every reset. Only if the function returns true
    // the formlet is reset.
    [<JavaScript>]
    let WithResetAction (f: unit -> bool) (formlet: Formlet<'T>) : Formlet<'T> =
        Formlet.New <| fun () ->
            let form = (formlet :> IFormlet<_,_>).Build ()
            let notify (o: obj) =
                if f() then
                    form.Notify o
            {form with Notify = notify}
        |> Formlet.WithLayout (formlet :> IFormlet<_,_>).Layout
        |> PropagateRenderFrom formlet
        |> OfIFormlet


    /// Creates a formlet enhanced with a a submit form depending on the
    // the result of the given first formlet.
    [<JavaScript>]
    let WithSubmitFormlet (formlet: Formlet<'T>)
                          (submit: Result<'T> -> Formlet<unit>) : Formlet<'T> =
        Formlet.Do {
            let! v =
                formlet
                |> Formlet.InitWithFailure
                |> Formlet.LiftResult
            let! s = submit v
            return v
        }
        |> Formlet.MapResult Result.Join
        |> PropagateRenderFrom formlet
        |> OfIFormlet


    /// Enhances a formlet with a submit and a reset formlet depending on
    /// the result of the given formlet and the a reset function.
    [<JavaScript>]
    let WithSubmitAndReset  (formlet: Formlet<'T>)
                            (submReset : (unit -> unit) ->
                            Result<'T> -> Formlet<'U>)  =
        Formlet.Do {
            let! (res, notify) =
                    formlet
                    |> Formlet.InitWithFailure
                    |> Formlet.LiftResult
                    |> Formlet.WithNotificationChannel
            return! submReset notify res :> IFormlet<_,_>
        }
        |> PropagateRenderFrom formlet
        |> OfIFormlet

    /// Represents form button configuration settings.
    type FormButtonConfiguration =
        {
            Label: string option
            Style: option<string>
            Class : option<string>
        }
    with
        /// Default form button configuration.
        [<JavaScript>]
        static member Default =
            {
                Label = None
                Style = None
                Class = None
            }

    /// Creates a button formlet with the given label.
    /// The first time the button is clicked the formlet will yield
    /// the value "0". For every additional click the value is
    /// incremented by one until reset.
    [<JavaScript>]
    let internal InputButton (conf: FormButtonConfiguration) enabled : Formlet<int> =
        MkFormlet <| fun () ->
            let state = HotStream<_>.New(Failure [])
            let count = ref 0
            let submit =
                let label = Utils.Maybe "Submit" id conf.Label
                let submit =
                    H.Input[Attr.Type "button"] -< [
                        Attr.Class CssConstants.SubmitButtonClass; Attr.Value label
                    ]
                    |>! Events.OnClick (fun _ _ ->
                        incr count
                        state.Trigger (Success count.Value)
                    )
                if not enabled then
                    submit.AddClass CssConstants.DisabledButtonClass

                match conf.Style with
                | None ->
                    ()
                | Some style ->
                    submit.SetStyle style

                match conf.Class with
                | None ->
                    ()
                | Some cls ->
                    submit.AddClass cls
                submit
            let reset () =
                count := 0
                state.Trigger (Failure [])
            state.Trigger (Failure [])
            submit, reset, state


    /// Creates a formlet enhanced with submit and reset buttons specified by
    /// the given form button configuration objects.
    [<JavaScript>]
    let WithCustomSubmitAndResetButtons (submitConf: FormButtonConfiguration)
                                        (resetConf: FormButtonConfiguration)
                                        (formlet: Formlet<'T>) : Formlet<'T> =
        let submitReset (reset : unit -> unit) (result : Result<'T>) : Formlet<'T> =
            let submit : Formlet<'T> =
                match result with
                | Success (value: 'T) ->
                    InputButton submitConf true
                    |> Formlet.Map (fun _ -> value)
                | Failure fs ->
                    InputButton submitConf false
                    |> Formlet.MapResult (fun _ -> Failure fs)
            let reset =
                Formlet.Do {
                    let! res = Formlet.LiftResult (InputButton resetConf true)
                    do
                        match res with
                        | Success _  ->
                            reset ()
                        | _          -> ()
                    return ()
                }
            (
                Formlet.Return (fun v _ -> v)
                <*> submit
                <*> reset
            )
            |> Formlet.WithLayout Layout.Horizontal
        WithSubmitAndReset formlet submitReset

    /// Creates a formlet enhanced with submit and reset buttons named
    /// "Submit" and "Reset".
    [<JavaScript>]
    let WithSubmitAndResetButtons (formlet: Formlet<'T>) : Formlet<'T> =
        formlet
        |> WithCustomSubmitAndResetButtons
            { FormButtonConfiguration.Default with Label=Some "Submit" }
            { FormButtonConfiguration.Default with Label=Some "Reset" }


    /// Represents validation icon configuration. Contains
    /// Css classes for "valid icon" and "error icon".
    type ValidationIconConfiguration =
        {
            ValidIconClass: string
            ErrorIconClass: string
        }
    with
        [<JavaScript>]
        static member Default =
            {
                ValidIconClass = CssConstants.ValidIconClass
                ErrorIconClass = CssConstants.ErrorIconClass
            }

    /// Creates a formlet enhanced with validation icon reflecting the current state of the
    /// form value. The validation icon configuration is specified by the given configuration
    /// object.
    [<JavaScript>]
    let WithCustomValidationIcon (vic: ValidationIconConfiguration) (formlet : Formlet<'T>)  : Formlet<'T> =
        let formlet =
            formlet
            |> Formlet.WithLayoutOrDefault
        let valid (res: Result<'T>) =
            fun () ->
                match res with
                | Success _ ->
                    Div [Attr.Class vic.ValidIconClass; Attr.Title ""] -< [Div []]
                | Failure msgs ->
                    let title = List.fold (fun x y -> x + " " + y) "" msgs
                    Div [Attr.Class vic.ErrorIconClass; Attr.Title title] -< [Div []]
            |> Formlet.OfElement
        Formlet.Do {
            let! res = Formlet.LiftResult formlet
            let! _ = valid res
            return res
        }
        |> Formlet.MapResult Result.Join
        |> Formlet.WithLayout Layout.Horizontal


    /// Creates a formlet enhanced with validation icon reflecting the state of the formlet
    /// using the default validation icon configuration settings.
    [<JavaScript>]
    let WithValidationIcon (formlet : Formlet<'T>)  : Formlet<'T> =
        WithCustomValidationIcon ValidationIconConfiguration.Default formlet


    /// Represents validation styling configuration.
    type ValidationFrameConfiguration =
        {
            ValidClass: option<string>
            ValidStyle: option<string>
            ErrorClass: option<string>
            ErrorStyle: option<string>
        }
    with
        /// Default validator configuration.
        [<JavaScript>]
        static member Default =
            {
                ValidClass = Some CssConstants.SuccessFormletClass
                ValidStyle = None
                ErrorClass = Some CssConstants.ErrorFormletClass
                ErrorStyle = None
            }

    [<JavaScript>]
    let internal WrapFormlet
            (wrapper: System.IObservable<Result<'T>> -> Body -> Element)
            (formlet: Formlet<'T>) : Formlet<'T> =
        MkFormlet <| fun () ->

            // Get form with layout
            let formlet = formlet |> Formlet.WithLayoutOrDefault
            let form = Formlet.BuildForm formlet
            let (body, disp) =  (formlet :> IFormlet<_,_>).Layout.Apply(form.Body).Value

            let panel = wrapper form.State body
            panel, (fun () -> form.Notify null) , form.State


    /// Creates a fomlet wrapped inside a validation frame, reflecting the current state of the
    /// form. The validation frame settings are specified by the given configuration object.
    /// This function merges the internal form body components.
    [<JavaScript>]
    let WithCustomValidationFrame  (vc: ValidationFrameConfiguration) (formlet: Formlet<'T>) : Formlet<'T> =
        formlet
        |> WrapFormlet (fun state body ->
            Div [body.Element]
            |>! OnAfterRender (fun panel ->
                state.Subscribe(fun res ->
                    match res with
                    | Success _ ->
                        // Remove error class
                        match vc.ErrorClass with
                        | None      -> ()
                        | Some cls  ->
                            panel.RemoveClass(cls)
                        // Add valid class
                        match vc.ValidClass with
                        | None      -> ()
                        | Some cls  ->
                            panel.AddClass(cls)
                        // Add valid style
                        match vc.ValidStyle with
                        | None          ->
                            panel.SetStyle ""
                        | Some style    ->
                            panel.SetStyle style
                    | Failure msgs ->
                        // Remove valid class
                        match vc.ValidClass with
                        | None      -> ()
                        | Some cls  ->
                            panel.RemoveClass cls
                        // Add error class
                        match vc.ErrorClass with
                        | None      -> ()
                        | Some cls  ->
                            panel.AddClass cls
                        // Add error style
                        match vc.ErrorStyle with
                        | None          ->
                            panel.SetStyle ""
                        | Some style    ->
                            panel.SetStyle style
                ) |> ignore
            )
        )

    /// Creates a formlet enhanced with a reset button specified according to
    /// the given form button configuration object.
    [<JavaScript>]
    let WithCustomResetButton (buttonConf: FormButtonConfiguration) (formlet: Formlet<'T>) : Formlet<'T> =
        let buttonConf =
            match buttonConf.Label with
            | Some label -> buttonConf
            | None       -> {buttonConf with Label = Some "Reset"}
        InputButton buttonConf true
        |> WithResetFormlet formlet

    /// Creates a formlet enhanced with a reset button.
    [<JavaScript>]
    let WithResetButton (formlet: Formlet<'T>) : Formlet<'T> =
        WithCustomResetButton FormButtonConfiguration.Default formlet

    /// Creates a formlet enhanced with a submit button specified according to
    /// the given form button configuration object.
    [<JavaScript>]
    let WithCustomSubmitButton (buttonConf: FormButtonConfiguration) (formlet: Formlet<'T>) : Formlet<'T> =
        let buttonConf =
            match buttonConf.Label with
            | Some label -> buttonConf
            | None       -> {buttonConf with Label = Some "Submit"}
        WithSubmitFormlet formlet (fun res ->
            match res with
            | Success _ -> true
            | _         -> false
            |> InputButton buttonConf
            |> Formlet.Map ignore
        )

    /// Creates a formlet enhanced with a submit button.
    [<JavaScript>]
    let WithSubmitButton formlet =
        WithCustomSubmitButton FormButtonConfiguration.Default formlet

    /// Creates a formlet outputting a list error messages when
    /// entering a failing state.
    [<JavaScript>]
    let WithErrorSummary (label : string) (formlet : Formlet<'T>) : Formlet<'T> =

        let errrFormlet fs =
            Formlet.OfElement (fun () ->
                FieldSet [
                    Legend [Text label]
                    (fs |> List.map (fun f -> LI [Text f]) |> UL)
                ]
            )

        Formlet.Do {
            let! res = Formlet.LiftResult formlet
            return!
                match res with
                | Success s ->
                    Formlet.Return res
                | Failure fs ->
                    errrFormlet fs
                    |> Formlet.Map (fun _ -> res)
        }
        |> Formlet.MapResult Result.Join

    /// Creates a formlet wrapped inside a validation frame reflecting the state of the
    /// formlet using the default validation frame configuration settings.
    [<JavaScript>]
    let WithValidationFrame (formlet: Formlet<'T>)  : Formlet<'T> =
        WithCustomValidationFrame ValidationFrameConfiguration.Default formlet


    /// Enhances a formlet with a custom formlet for displaying error messages.
    [<JavaScript>]
    let WithErrorFormlet (f: list<string> -> Formlet<'U>) (formlet : Formlet<'T>) : Formlet<'T> =
        Formlet.Do {
            let! res = Formlet.LiftResult formlet
            return!
                match res with
                | Success s ->
                    Formlet.Return res
                | Failure msgs ->
                    Formlet.Do {
                        let! _ = f msgs
                        return res
                    }
        }
        |> Formlet.MapResult Result.Join

    /// Enhances a formlet with a label component.
    [<JavaScript>]
    let WithLabel (labelGen: unit -> Element)  (formlet: Formlet<'T>) : Formlet<'T> =
        Formlet.WithLabel (Some labelGen) formlet

    /// Creates a formlet enhanced with a label specified by the given label configuration.
    [<JavaScript>]
    let WithLabelConfiguration (lc: Layout.LabelConfiguration) (formlet: Formlet<'T>) : Formlet<'T> =
        formlet
        |> Formlet.ApplyLayout
        |> Formlet.WithLayout (Layout.LabelLayout lc)

    /// Creates a formlet enhanced with a label with the given text and an info icon containing
    /// additional information as a tooltip.
    /// This function merges the internal form body components.
    [<JavaScript>]
    let WithLabelAndInfo (label: string) (info: string) (formlet: Formlet<'T>) : Formlet<'T> =
        let lblTbl () =
            Utils.InTable [
                [
                    Label [Text label]
                    Span [
                        Attr.Title info
                        Attr.Class CssConstants.InfoIconClass
                    ]
                ]
            ]
        WithLabel lblTbl formlet

    /// Create a formlet enhanced with a text label.
    [<JavaScript>]
    let WithTextLabel (label: string) (formlet: Formlet<'T>) : Formlet<'T> =
        WithLabel (fun () -> Label [Text label]) formlet

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

    /// Represents a constant visual part of a web form.
    /// Either text or an element produced by the given
    /// element generator function.
    type FormPart =
        | Text of string
        | Element of (unit -> Element)

    /// Represents form container configuration settings.
    type FormContainerConfiguration =
        {
            // Header
            Header: option<FormPart>

            // Spacing
            Padding: Padding

            // Description
            Description: option<FormPart>

            // Color
            BackgroundColor: option<string>
            BorderColor: option<string>

            // Styling
            CssClass: option<string>
            Style: option<string>
        }
    with
        /// Default form container configuration.
        [<JavaScript>]
        static member Default =
            {
                Header = None
                Description = None
                Padding = Padding.Default
                BackgroundColor = None
                BorderColor = None
                CssClass = None
                Style = None
            }

    /// Creates formlets displaying labels above the components.
    [<JavaScript>]
    let WithLabelAbove (formlet: Formlet<'T>) : Formlet<'T> =
        formlet
        |> Formlet.MapBody (fun body ->
            let el =
                let label =
                    match body.Label with
                    | Some l    -> l()
                    | None      -> Span []
                Table [
                    TBody [
                        TR [TD [label]]
                        TR [TD [body.Element]]
                    ]
                ]
            {
                Label = None
                Element = el
            }
        )

    /// Creates a formlet displaying labels on the left
    /// hand side of the components.
    [<JavaScript>]
    let WithLabelLeft (formlet: Formlet<'T>) : Formlet<'T> =
        formlet
        |> Formlet.MapBody (fun body ->
            let el =
                let label =
                    match body.Label with
                    | Some l    -> l()
                    | None      -> Span []
                Table [
                    TBody [
                        TR [TD [body.Element]; TD [label]]
                    ]
                ]
            {
                Label = None
                Element = el
            }
        )

    /// Creates a formlet wrapped inside a form container specified by the given
    /// configuration object.
    [<Require(typeof<Resources.SkinResource>)>]
    [<JavaScript>]
    let WithCustomFormContainer (fc : FormContainerConfiguration) (formlet: Formlet<'T>) : Formlet<'T> =
        formlet
        |> Formlet.ApplyLayout
        |> Formlet.MapElement (fun formEl ->
            let description =
                fc.Description |> Utils.Maybe [] (fun descr ->
                    match descr with
                    | FormPart.Text text -> [P [Tags.Text text]]
                    | FormPart.Element genEl -> [genEl ()]
                )
            let tb =
                fc.Header
                |> Utils.Maybe (Utils.InTable [[Div [Attr.Class CssConstants.HeaderPanelClass] -< description]; [formEl]]) (fun formElem ->
                    let header =
                        let hdr =
                            match formElem with
                            | FormPart.Text text ->
                                H1 [Tags.Text text]
                            | FormPart.Element genElem ->
                                genElem ()
                        Div [Attr.Class CssConstants.HeaderPanelClass] -< (hdr :: description)
                    Utils.InTable [
                        [header]
                        [formEl]
                    ]
                )

            let cell = TD [Attr.Class CssConstants.FormletClass] -< [tb]
            fc.BorderColor
            |> Utils.Maybe () (fun color ->
                cell.SetStyle <| "border-color: " + color
            )
            [
                "background-color" , fc.BackgroundColor |> Utils.MapOption (fun color -> color)
                "padding-left" , fc.Padding.Left |> Utils.MapOption (fun v -> string v + "px")
                "padding-right" , fc.Padding.Right |> Utils.MapOption (fun v -> string v + "px")
                "padding-top" , fc.Padding.Top |> Utils.MapOption (fun v -> string v + "px")
                "padding-bottom" , fc.Padding.Bottom |> Utils.MapOption (fun v -> string v + "px")
            ]
            |> List.iter (fun (name, value) ->
                match value with
                | Some v ->
                    cell.SetCss(name, v)
                | None -> ()
            )
            match fc.Style with
            | Some style -> cell.SetStyle style
            | None -> ()
            match fc.CssClass with
            | Some cls -> cell.AddClass cls
            | None -> ()
            Table [
                TBody [
                    TR [ cell ]
                ]
            ]
        )

    /// Creates a formlet wrapped inside a form container.
    [<Require(typeof<Resources.SkinResource>)>]
    [<JavaScript>]
    let WithFormContainer (formlet: Formlet<'T>) : Formlet<'T> =
        WithCustomFormContainer FormContainerConfiguration.Default formlet

    // Enhances a formlet element with a CSS class.
    // This function merges the internal form body components.
    [<JavaScript>]
    let WithCssClass css formlet =
        formlet
        |> Formlet.MapElement (fun el -> el.AddClass css; el)

    /// Creates a formlet wrapped inside a legend element with the
    /// given label. This function merges the internal form body components.
    [<JavaScript>]
    let WithLegend (label: string) (formlet : Formlet<'T>) : Formlet<'T> =
        formlet
        |> Formlet.MapBody (fun body ->
            let element =
                FieldSet [
                    Legend [Tags.Text label]
                    (
                        match body.Label with
                        | Some label ->
                            Utils.InTable [
                                [label (); body.Element]
                            ]
                        | None ->
                            body.Element
                    )
                ]
            {Element = element ; Label = None}
        )

    /// Creates a formlet enhanced with the given form row configuration.
    [<JavaScript>]
    let WithRowConfiguration (rc: Layout.FormRowConfiguration) (formlet: Formlet<'T>) : Formlet<'T> =
        formlet
        |> Formlet.ApplyLayout
        |> Formlet.WithLayout (Layout.RowLayout rc)


    [<JavaScript>]
    let private Cancel (formlet: Formlet<'T>) (isCancel: 'T -> bool) : Formlet<'T> =
        Formlet.Replace formlet (fun value ->
            if isCancel value then
                Formlet.Empty<'T> ()
            else
                Formlet.Return value
        )

    [<JavaScript>]
    let private Replace (formlet: Formlet<'T1>) f =
        let x =
            formlet
            |> Formlet.MapResult (fun res ->
                match res with
                | Success s ->
                    Success (f s)
                | Failure fs ->
                    Success <|  Formlet.FailWith fs
            )
        Formlet.Switch  x

    [<JavaScript>]
    let private Deletable (formlet: Formlet<option<'T>>) : Formlet<option<'T>> =
        Replace formlet (fun value ->
            match value with
            | None ->
                Formlet.ReturnEmpty None
            | Some value ->
                Formlet.Return (Some value)
        )

    // TODO: Re-factor
    [<JavaScript>]
    let private Many_(add: Formlet<'T>) (f: 'T -> Formlet<option<'U>>) : Formlet<seq<'U>> =
        add
        |> Formlet.Map (fun v ->
            f v
        )
        |> Formlet.SelectMany
        |> Formlet.FlipBody
        |> Formlet.Map (Seq.choose id)

    type ManyConfiguration =
        {
            AddIconClass : string
            RemoveIconClass : string
        }
        with
            [<JavaScript>]
            static member Default =
                {
                    AddIconClass = CssConstants.AddIconClass
                    RemoveIconClass = CssConstants.RemoveIconClass
                }

    /// Creates a formlet for inputting a multiple values using the
    /// given formlet. The first argument specifies the css classes for
    /// remove and add icons.
    [<JavaScript>]
    let CustomMany (config: ManyConfiguration) (formlet: Formlet<'T>) : Formlet<list<'T>> =
        let addButton =
            Controls.ElementButton (fun () ->
                Div [Attr.Class config.AddIconClass] -< [Div []]
            )
            |> Formlet.InitWith 1

        let delF =
            Controls.ElementButton (fun () ->
                Div [Attr.Class config.RemoveIconClass] -< [ Div []]
            )
            |> Formlet.Map ignore
            |> Formlet.WithCancelation formlet
            |> Formlet.WithLayout Layout.Horizontal
            |> Deletable

        let manyF () =
            Many_ addButton (fun _ -> delF)
            |> Formlet.Map List.ofSeq
            |> Formlet.WithLayoutOrDefault
            |> Formlet.ApplyLayout

        let resetS = HotStream<_>.New(Success())
        let resetF =
            (BaseFormlet()).FromState resetS
            |> OfIFormlet
        let reset (_:obj) =
            resetS.Trigger (Success ())

        Formlet.Do {
            let! _ = resetF
            return! manyF ()
        }
        |> Formlet.WithNotification reset

    /// Creates a formlet for inputting a multiple values using the
    /// given formlet.
    [<JavaScript>]
    let Many (formlet: Formlet<'T>) : Formlet<list<'T>> =
        CustomMany ManyConfiguration.Default formlet

    /// Represents configuration settings for
    /// posting form data as JSON.
    type JsonPostConfiguration =
        {
            PostUrl : option<string>
            ParameterName : string
            EncodingType : option<string>
        }


    /// Wrap in form container with a post submit to the given URL
    [<JavaScript>]
    let WithJsonPost (conf: JsonPostConfiguration) formlet =
        let postUrl =
            match conf.PostUrl with
            | Some url  -> [Attr.Action url]
            | None      -> []

        let enc =
            match conf.EncodingType with
            | Some enc  ->
                [Attr.EncType enc]
            | None      ->
                []
        // Hidden field for storing the parameter
        let hiddenField =
            Tags.Input [
                Attr.Type "hidden"
                Attr.Name conf.ParameterName
            ]
        let submitButton =
            Tags.Input [Attr.Type "submit"; Attr.Value "Submit"]

        // Invisible form
        let form =
            let formAttrs =
                Method "POST" :: Attr.Style "display:none" :: postUrl @ enc
            Form formAttrs -< [
                hiddenField
                submitButton
            ]
            |>! OnAfterRender (fun form ->
                // I.E. expects encoding instead of enctype.
                match conf.EncodingType with
                | Some enc ->
                    if enc = "multipart/form-data" then
                        JQuery.JQuery.Of(form.Body).Attr(
                            "encoding", "multipart/form-data"
                        )
                        |> ignore
                | None ->
                    ()
            )

        let formlet =
            formlet
            |> Formlet.Map (fun value ->
                // Get the data
                let data = WebSharper.Json.Stringify value
                // Set the data to the hidden field
                JQuery.JQuery.Of(hiddenField.Body).Val(data).Ignore
                // Submit form
                JQuery.JQuery.Of(submitButton.Body).Click()
            )
        Div [
            form :> Pagelet
            formlet :> _
        ]

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
namespace IntelliFactory.WebSharper.Formlet

open IntelliFactory.WebSharper

/// Defines formlets and their operations.
module Controls =
    open System
    open IntelliFactory.Formlet.Base
    open IntelliFactory.WebSharper.Formlet
    open IntelliFactory.WebSharper.Html
    open IntelliFactory.WebSharper.Html.Events
    open IntelliFactory.Reactive

    [<JavaScript>]
    let private SelectControl<'T> (readOnly: bool) (def: int) (vls: list<string * 'T>) : Formlet<'T> =
        MkFormlet <| fun () ->

            // Index based value dictionary.
            let aVls : 'T [] =
                vls |> List.map snd |> Array.ofList

            // Find the index of the default value
            let sIx =
                if def >= 0 && def < vls.Length then
                    def
                else
                    0
            // Selection box
            let body =
                let select =
                    vls
                    |> List.mapi (fun i (nm, _) ->
                        Tags.Option [Text nm; Attr.Value (string i)]
                    )
                    |> Select
                if readOnly then
                    select -< [Attr.Disabled "disabled"]
                else
                    select
            // Initial value
            let sValue = Success aVls.[sIx]
            let state = HotStream<_>.New(sValue)

            // Set start value
            let reset () =
                body.SetProperty("value" , string sIx)
                state.Trigger sValue
            reset()
            body
            |> Events.OnChange (fun _  ->
                if not readOnly then
                    let value = body.Value
                    aVls.[int value]
                    |> Success
                    |> state.Trigger
            )

            reset()
            body, reset, state

    /// Given an optional value for the default selected index and a list of name and value pairs,
    /// creates a select box control. The formlet produces a value corresponding to
    /// the selected radio button.
    [<JavaScript>]
    let Select<'T> (def: int) (vls: list<string * 'T>) : Formlet<'T> =
        SelectControl false def vls

    /// Given an optional value for the default selected index and a list of name and value pairs,
    /// creates a select box control. The formlet produces a value corresponding to
    /// the selected radio button.
    [<JavaScript>]
    let ReadOnlySelect<'T> (def: int) (vls: list<string * 'T>) : Formlet<'T> =
        SelectControl true def vls


    [<JavaScript>]
    let private InputControl (value: string)
                             (f: HotStream<Result<string>> -> Html.Element)
                             : Formlet<string> =
        MkFormlet <| fun () ->
            let state = HotStream<_>.New(Success value)
            let body = f state
            body.Value <- value
            let reset () =
                body.Value <- value
                state.Trigger (Success value)
            body, reset, state



    [<JavaScript>]
    let private TextAreaControl (readOnly) (value: string) : Formlet<string> =
        InputControl value (fun state ->
            let input =
                if readOnly then
                    [Attr.ReadOnly "readonly"]
                else
                    []
                |> TextArea
            input
            |>! Events.OnKeyUp (fun _ _ ->
                if not readOnly then
                    state.Trigger (Success input.Value)
            )
        )

    /// Given an initial value creates a text area formlet control.
    [<JavaScript>]
    let TextArea (value: string) : Formlet<string> =
        TextAreaControl false value

    /// Read only text area.
    [<JavaScript>]
    let ReadOnlyTextArea(value: string) : Formlet<string> =
        TextAreaControl true value

    [<JavaScript>]
    let private InputField (readOnly: bool) (typ: string) (cls: string) (value: string) : Formlet<string> =
        InputControl value (fun state ->
            let input =
                let ro =
                    if readOnly then
                        [Attr.ReadOnly "readonly"]
                    else
                        []
                Input <| [Attr.Type typ; Attr.Class cls] @ ro
            input |>! Events.OnKeyUp (fun _ _ ->
                if not readOnly then
                    state.Trigger (Success input.Value)
            )
        )


    [<JavaScript>]
    let private CheckboxControl (readOnly : bool) (def: bool) : Formlet<bool> =
        MkFormlet <| fun () ->
            let state = HotStream<_>.New (Success def)
            let body =
                let readOnlyAtts =
                    if readOnly then
                        [Attr.Disabled "disabled"]
                    else
                        []
                Input [Attr.Type "checkbox"; Attr.Class CssConstants.InputCheckboxClass] -< readOnlyAtts
                |>! Events.OnClick (fun cb _ ->
                    if not readOnly then
                        JQuery.JQuery.Of(cb.Body).Prop<bool>("checked")
                        |> Success
                        |> state.Trigger
                )
            if def then
                body.SetAttribute("defaultChecked", "true")
            else
                body.RemoveAttribute "checked"
            let reset ()  =
                if def then
                    body.SetProperty("checked", true)
                else
                    body.RemoveAttribute "checked"
                    body.SetProperty("checked", false)
                state.Trigger (Success def)
            reset ()
            body, reset, state


    /// Creates a check box formlet with the initial state
    /// given by the provided value.
    [<JavaScript>]
    let Checkbox (def: bool) : Formlet<bool> =
        CheckboxControl false def


    /// Creates a read only check box formlet with the state
    /// given by the provided value.
    [<JavaScript>]
    let ReadOnlyCheckbox (def: bool) : Formlet<bool> =
        CheckboxControl true def

    [<JavaScript>]
    let private CheckboxGroupControl (readOnly : bool) (values: List<string * 'T * bool>)  : Formlet<List<'T>> =
        values
        |> List.map (fun (l, v, b) ->
            CheckboxControl readOnly b
            |> Formlet.WithLabel (Some <| fun () -> Label [Text l])
            |> Formlet.Map (fun b -> (b, v))
        )
        |> Formlet.Sequence
        |> Formlet.Map (List.choose (fun (b, v) ->
                if b then Some v else None
            )
        )

    /// Given a list of labels, values and bools indicating if the default value,
    /// constructs a checkbox group control formlet. The formlet produces a
    /// a list of values corresponding to the checked components.
    [<JavaScript>]
    let CheckboxGroup (values: List<string * 'T * bool>)  : Formlet<List<'T>> =
        CheckboxGroupControl false values


    [<JavaScript>]
    let private RadioButtonGroupControl (readOnly: bool) (def: option<int>) (values: list<string * 'T>) : Formlet<'T> =
        Formlet.New <| fun () ->
            let groupId = NewId()
            let state =
                match def with
                | Some defIx    ->
                    values
                    |> List.mapi (fun ix (_,value) -> ix, value)
                    |> List.tryPick (fun (ix,value) ->
                        match def with
                        | Some defIx    ->
                            if defIx = ix then Some (Success value) else None
                        | None          ->
                            None
                    )
                | None          -> None
                |> Utils.Maybe (HotStream<_>.New(Failure [])) HotStream<_>.New

                

            let rbLbVls =
                values
                |> List.map (fun (label, value) ->
                    let inp =
                        Input [
                            Attr.Class CssConstants.InputRadioClass
                            Attr.Type "radio"
                            Attr.Name groupId
                        ] -< (
                            if readOnly then
                                [Attr.Disabled "disabled"]
                            else
                                []
                        )
                    inp , label, value
                )
            let resetRB (rb: Element) (value: 'T) (ix: int)  =
                match def with
                | Some defIx ->
                    if defIx = ix then
                        rb.SetProperty("checked", true)
                        state.Trigger(Success value)
                    else
                        rb.SetProperty("checked", false)
                | None ->
                    rb.RemoveAttribute "checked"
                    state.Trigger (Failure [])
            let reset () =
                rbLbVls
                |> List.iteri (fun ix (rb, _ , value) ->
                    resetRB rb value ix
                )
            let body =
                rbLbVls
                |> List.mapi (fun ix (rb, label, value) ->
                    // Check the button if default value is given
                    resetRB rb value ix
                    rb
                    |> Events.OnClick (fun _ _ ->
                        if not readOnly then
                            state.Trigger(Success value)
                    )
                    ({Label = Some (fun () -> Label [Text label]); Element = rb} : Body)
                )
                |> Tree.FromSequence
                |> Tree.Edit.Replace
                |> RX.Return
            {
                Body    = body
                Dispose = ignore
                Notify  = fun _ -> reset ()
                State   = state
            }

    /// Given an optional value for the default selected index and a list of name and value pairs,
    /// creates a radio button group control. The formlet produces a value corresponding to
    /// the selected radio button.
    [<JavaScript>]
    let RadioButtonGroup (def: option<int>) (values: list<string * 'T>) : Formlet<'T> =
        RadioButtonGroupControl false def values

    /// Given an optional value for the default selected index and a list of name and value pairs,
    /// creates a radio button group control. The formlet produces a value corresponding to
    /// the selected radio button.
    [<JavaScript>]
    let ReadOnlyRadioButtonGroup (def: option<int>) (values: list<string * 'T>) : Formlet<'T> =
        RadioButtonGroupControl true def values


    /// Creates a password control formlet with a default value.
    [<JavaScript>]
    let Password (value: string) : Formlet<string> =
        InputField false "password" CssConstants.InputPasswordClass value

    /// Creates an input text control formlet with a default value.
    [<JavaScript>]
    let Input (value: string) : Formlet<string> =
        InputField false "text" CssConstants.InputTextClass value

    /// Creates an input text control formlet with a default value.
    [<JavaScript>]
    let ReadOnlyInput (value: string) : Formlet<string> =
        InputField true "text" CssConstants.InputTextClass value

    /// Creates a button formlet with the given label.
    /// The first time the button is clicked the formlet will yield
    /// the value "0". For every additional click the value is
    /// incremented by one.
    [<JavaScript>]
    let ElementButton (genElem: unit -> Html.Element) : Formlet<int> =
        MkFormlet <| fun () ->
            let state = HotStream<_>.New(Result.Failure [])
            let count = ref 0
            let body =
                genElem ()
                |>! Events.OnClick (fun _ _ ->
                    state.Trigger (Success count.Value)
                    incr count
                )
            let reset () =
                count := 0
                state.Trigger (Failure [])
            body, reset, state

    /// Creates a button formlet with the given label.
    /// The first time the button is clicked the formlet will yield
    /// the value "0". For every additional click the value is
    /// incremented by one.
    [<JavaScript>]
    let Button (label: string) : Formlet<int> =
        ElementButton <| fun () -> Button [Text label]

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

namespace WebSharper.Formlets.Tests

open WebSharper
open WebSharper.Html.Client
open WebSharper.Formlets
open WebSharper.Formlets.Layout

module FormletTest =

    [<JavaScript>]
    let TestControl (txt:string) (fls:List<Formlet<unit>>) =
        fls
        |> Formlet.Sequence
        |> Formlet.Map ignore
        |> Formlet.MapElement (fun el->
            Div [
                H1 [Text txt]
                Div [Attr.Style "padding:10px; border:1px solid gray"] -< [
                    el
                ]
            ]
        )

    [<JavaScript>]
    let CheckState title (show: 'T -> string) (f: Formlet<'T>) : Formlet<unit> =
        let fcc =
            {
                Enhance.FormContainerConfiguration.Default with
                    Description = Some (Enhance.FormPart.Text "Description text")
                    Header = Some (Enhance.FormPart.Text title)
                    Padding = {Left = Some 30; Right = Some 30; Top = Some 20; Bottom = Some 20}
            }
        Formlet.Do {
            let! v = f
            let! x  =
                Controls.Input (show v)
                |> Formlet.MapElement (fun el -> Div [Attr.Style "background-color:green; margin-top:5px"] -< [el])
            let! y =
                if x = "" then Formlet.Return v else Formlet.Return v
            return y
        }
        |> Enhance.WithCustomFormContainer fcc
        |> Formlet.Map ignore

    (************************************************************************
    * Test Controls
    ************************************************************************)
    [<JavaScript>]
    let TestInput =
        let f1 = Controls.Input ""
        let f2 = Controls.Input "With Default Value"
        (Formlet.Yield (fun v1 v2 -> v1 + v2) <*> f1 <*> f2)
        |> CheckState "Controls.Input" id

    [<JavaScript>]
    let TestCheckbox =
        let f1 = Controls.Checkbox true
        let f2 = Controls.Checkbox false
        (Formlet.Yield (fun v1 v2 -> (v1,v2)) <*> f1 <*> f2)
        |> CheckState "Controls.Checkbox" string

    [<JavaScript>]
    let TestCheckboxGroup1 =
        Controls.CheckboxGroup ["A", 1 , false; "B" , 2, true; "C", 3, true]
        |> CheckState "Controls.CheckboxGroup" (fun xs ->
            xs |> List.map (fun x -> string x + ", ") |> List.fold (+) ""
        )
    [<JavaScript>]
    let TestCheckboxGroup2 =
        Controls.CheckboxGroup ["A", 1 , true; "B" , 2, false; "C", 3, false]
        |> CheckState "Controls.CheckboxGroup" (fun xs ->
            xs |> List.map (fun x -> string x + ", ") |> List.fold (+) ""
        )

    [<JavaScript>]
    let TestRadiobuttonGroup1 =
            Controls.RadioButtonGroup None ["A", 1 ; "B" , 2; "C", 3]
            |> CheckState "Controls.RadioButtonGroup" string

    [<JavaScript>]
    let TestRadiobuttonGroup2 =
            Controls.RadioButtonGroup (Some 2) ["A", 1 ; "B" , 2; "C", 3]
            |> CheckState "Controls.RadioButtonGroup (def value)" string

    [<JavaScript>]
    let TestSelect1  =
        Controls.Select -1 ["A", 1 ; "B" , 2; "C", 3]
        |> CheckState "Controls.Select" string

    [<JavaScript>]
    let TestSelect2  =
        Controls.Select 1 ["A", 1 ; "B" , 2; "C", 3]
        |> CheckState "Controls.Select (def value)" string

    [<JavaScript>]
    let TestTextArea1  =
        Controls.TextArea ""
        |> CheckState "Controls.TextArea" id

    [<JavaScript>]
    let TestTextArea2  =
        Controls.TextArea "Default Value"
        |> CheckState "Controls.TextArea (def value)" id

    [<JavaScript>]
    let TestPassword1 =
        Controls.Password ""
        |> CheckState "Controls.Password" id

    [<JavaScript>]
    let TestPassword2 =
        Controls.Password "password" |> CheckState "Controls.Password (default value)" id

    [<JavaScript>]
    let TestControls =
        [
            TestCheckbox
            TestCheckboxGroup1
            TestInput
            TestPassword1
            TestPassword2
            TestRadiobuttonGroup1
            TestRadiobuttonGroup2
            TestSelect1
            TestSelect2
            TestTextArea1
            TestTextArea2
        ]
        |>
        TestControl "Test Controls"

    (************************************************************************
    * Test Validation
    ************************************************************************)
    [<JavaScript>]
    let TestValidationIsInt =
        Controls.Input "2"
        |> Validator.IsInt "IsInt"
        |> CheckState "Is integer" id


    [<JavaScript>]
    let TestValidationIsFloat =
        Controls.Input ""
        |> Validator.IsFloat "IsFloat"
        |> CheckState "Is float" id

    [<JavaScript>]
    let TestValidationIsEmail =
        Controls.Input ""
        |> Validator.IsEmail "IsEmail"
        |> Enhance.WithValidationIcon
        |> CheckState "Is Email" id

    [<JavaScript>]
    let TestValidationIsEqual =
        Controls.Input ""
        |> Validator.IsEqual "apa" "Is eq apa"
        |> Enhance.WithValidationIcon
        |> CheckState "Is eq apa" id

    [<JavaScript>]
    let TestValidationIsGreaterThan =
        Controls.Input ""
        |> Validator.IsInt "IsInt"
        |> Formlet.Map int
        |> Validator.IsGreaterThan 10 "IsGT 10"
        |> Enhance.WithValidationIcon
        |> CheckState "> 10" string

    [<JavaScript>]
    let TestValidationIsLessThan =
        Controls.Input ""
        |> Validator.IsInt "IsInt"
        |> Formlet.Map int
        |> Validator.IsLessThan 10 "IsLessThan 10"
        |> Enhance.WithValidationIcon
        |> CheckState "IsLessThan 10" string

    [<JavaScript>]
    let TestValidationIsNotEqual =
        Controls.Input ""
        |> Validator.IsInt "IsInt"
        |> Formlet.Map int
        |> Validator.IsNotEqual 10 "IsNot Eq 10"
        |> Enhance.WithValidationIcon
        |> CheckState "IsNot Eq 10" string

    [<JavaScript>]
    let TestValidationWithValidation =
        Controls.Input ""
        |> Validator.IsInt "IsInt"
        |> Formlet.Map int
        |> Validator.Is  (fun s -> s > 10 && s < 15) "10 < x < 15"
        |> Enhance.WithValidationIcon
        |> CheckState "10 < x < 15" string

    [<JavaScript>]
    let TestValidation =
        [
            TestValidationIsEmail
            TestValidationIsFloat
            TestValidationIsEqual
            TestValidationIsGreaterThan
            TestValidationIsInt
            TestValidationIsLessThan
            TestValidationIsNotEqual
            TestValidationWithValidation
        ]
        |> TestControl ""

    (************************************************************************
    * Test Enhance
    ************************************************************************)
    [<JavaScript>]
    let TestEnhanceCustomValidationIcon =
        let vic : Enhance.ValidationIconConfiguration = 
            {
                ErrorIconClass = "MyErrorIcon"; 
                ValidIconClass = "MyValidIcon"
            }
        Formlet.Yield (fun x y -> x) 
        <*> Controls.Input "22" 
        <*> Controls.Input ""
        |> Validator.IsInt "Is Int"
        |> Enhance.WithCustomValidationIcon vic
        |> CheckState "Enhance.WithCustomValidationIcon" (fun v -> string v)

    [<JavaScript>]
    let TestEnhanceValidationIcon =
        Controls.Input "22"
        |> Validator.IsInt "Is Int"
        |> Enhance.WithValidationIcon
        |> CheckState "Enhance.WithValidationIcon" (fun _ -> "()")

    [<JavaScript>]
    let TestEnhanceValidationFrame =
        Controls.Input "22"
        |> Validator.IsInt "Is Int"
        |> Enhance.WithValidationFrame
        |> CheckState "Enhance.WithValidationFrame" (fun _ -> "()")

    [<JavaScript>]
    let TestEnhanceCustomValidationFrame =
        let vfc = {
            Enhance.ValidationFrameConfiguration.Default with
                ErrorClass = Some "MyErrorFrame"
                ValidClass = Some "MyValidFrame"
            }
        Controls.Input "22"
        |> Validator.IsInt "Is Int"
        |> Enhance.WithCustomValidationFrame vfc
        |> CheckState "Enhance.WithValidationFrame" (fun _ -> "()")

    [<JavaScript>]
    let TestEnhanceWithErrorSummary =
        Controls.Input ""
        |> Validator.IsInt "Int"
        |> Enhance.WithValidationIcon
        |> Enhance.WithErrorSummary "Validation validation"
        |> CheckState "TestEnhanceWithErrorSummary" id

    [<JavaScript>]
    let TestEnhanceWithInValidationPanel =
        Controls.Input ""
        |> Validator.IsInt "Int"
        |> Enhance.WithValidationFrame
        |> CheckState "TestEnhanceWithErrorSummary" id

    [<JavaScript>]
    let LabelConfigurations =
        let lc = LabelConfiguration.Default
        [
            Placement.Right, "Layout.Placement.Right"
            Placement.Top, "Layout.Placement.Above"
            Placement.Bottom , "Layout.Placement.Under"
            Placement.Left , "Layout.Placement.Left"
        ]
        |> List.map (fun (p,n) ->
            [
                n + " x Layout.Align.Left",  {lc with Align = Layout.Align.Left; Placement = p}
                n + " x Layout.Align.Right", {lc with Align = Layout.Align.Right; Placement = p}
            ]
        )
        |> List.concat

    [<JavaScript>]
    let TestEnhanceWithLabel =
        let f l =
            Controls.Input ""
            |> Validator.IsInt "Int"
            |> Enhance.WithTextLabel l
        let lf (title,lc) =
            (
                Formlet.Yield (fun _ _ _ _  -> ())
                <*> f "Label 1"
                <*> f "Shorter"
                <*> Controls.Input "No Label"
                <*> f "Longer Label"
            )
            |> Enhance.WithLabelConfiguration lc
            |> Enhance.WithLegend title
        LabelConfigurations
        |> List.map lf
        |> Formlet.Sequence
        |> CheckState "Label configuration" (fun _ -> "")

    [<JavaScript>]
    let TestEnhanceWithLabelVerticalAlign =
        let f va =
            Controls.TextArea ""
            |> Enhance.WithTextLabel "Label"
            |> Enhance.WithLabelConfiguration {LabelConfiguration.Default with VerticalAlign = va}
        let lf  =
            Formlet.Yield (fun a b  c -> a + b + c)
            <*> f VerticalAlign.Top
            <*> f VerticalAlign.Middle
            <*> f VerticalAlign.Bottom
        lf
        |> CheckState "Vertical align of labels" id

    [<JavaScript>]
    let TestEnhanceWithRowConfiguration =
        let padding : Padding =
            {
                Left = Some 10
                Right = Some 10
                Top = Some 20
                Bottom = Some 20
            }
        let rc =
            {
                Layout.FormRowConfiguration.Default with
                    Padding = Some padding
                    Color = None
                    Style = Some (fun _ -> "color:green")
            }
        let rec f label =
            Formlet.Do {
                let! label =
                    Controls.Input ""
                    |> Validator.IsInt ""
                    |> Enhance.WithValidationFrame
                    |> Enhance.WithTextLabel label
                let! _ = f label
                return ()
            }
        f "L1"
        |> Enhance.WithRowConfiguration rc
        |> CheckState "Row Configuration" string

    [<JavaScript>]
    let TestEnhanceWithSubmit=
        let bc = {Enhance.FormButtonConfiguration.Default with Label = Some "Submit"; Style = Some "color:red"}
        Controls.Input ""
        |> Validator.IsInt ""
        |> Enhance.WithValidationFrame
        |> Enhance.WithSubmitButton
        |> CheckState "TestEnhanceWithSubmit" string

    [<JavaScript>]
    let TestEnhanceWithReset=
        let bc = {Enhance.FormButtonConfiguration.Default with Label = Some "Submit"; Style = Some "color:red"}
        Controls.Input ""
        |> Enhance.WithResetButton
        |> CheckState "TestEnhanceWithSubmit" string

    [<JavaScript>]
    let TestEnhanceWithLabelAndInfo =
        Controls.Input "Gurka"
        // |> Validator.IsInt "Int"
        // |> Enhance.WithLabelAndInfo "Label" "Info"
        |> Enhance.WithTextLabel "Label"
        |> Formlet.Map ignore

        // |> Enhance.WithFormContainer
        // |> CheckState "Enhance.WithLabelAndInfo" string

    [<JavaScript>]
    let TestEnhanceWithLegend =
        Controls.Input ""
        |> Validator.IsInt "Int"
        |> Enhance.WithLegend "Legend"
        |> CheckState "Enhance.WithLegend " string

    [<JavaScript>]
    let TestEnhance =
        [
            TestEnhanceValidationIcon
            TestEnhanceCustomValidationIcon
            TestEnhanceValidationFrame
            TestEnhanceCustomValidationFrame
            TestEnhanceWithErrorSummary
            TestEnhanceWithLabel
            TestEnhanceWithLabelAndInfo
            TestEnhanceWithLegend
            TestEnhanceWithLabel
            TestEnhanceWithLabelVerticalAlign
            TestEnhanceWithRowConfiguration
            TestEnhanceWithSubmit
            TestEnhanceWithReset
            TestEnhanceWithInValidationPanel
        ]
        |> TestControl "Test Enhance"

    (************************************************************************
    * Test Combinators
    ************************************************************************)
    [<JavaScript>]
    let IntF = Controls.Input "" |> Validator.IsInt ""

    [<JavaScript>]
    let TestApply =
        (
            Formlet.Yield (fun x y z u -> (x , y , z, u))
            <*> (IntF |> Enhance.WithValidationIcon)
            <*> (IntF |> Enhance.WithValidationIcon)
            <*> (IntF |> Enhance.WithValidationIcon)
            <*> (IntF |> Enhance.WithValidationIcon)
        )
        |> CheckState "Checkbox" (fun (x , y , z, u) ->
            [x,y,z,u]
            |> List.fold (fun x y -> x + " " + (string y)) ""
        )


    [<JavaScript>]
    let TestSequence =
        List.replicate 3 IntF
        |> Formlet.Sequence
        |> CheckState "Test Sequence" (fun xs ->
            xs
            |> List.map (fun x -> string x + ", ")
            |> List.fold (+) ""
        )


    [<JavaScript>]
    let TestMany =
        Controls.Input ""
        |> Enhance.WithTextLabel "Label"
        |> Validator.IsInt ""
        |> Enhance.WithValidationIcon
        |> Enhance.Many
        |> CheckState "Test Many" (fun xs ->
            Seq.fold (fun x y -> x + " " + y) "" xs
        )

    [<JavaScript>]
    let TestBindWith =
        Formlet.BindWith (fun h1 h2 ->
                {Label = None; Element = Div [h2.Element ; h1.Element]}
            ) IntF (fun i ->
            Formlet.OfElement (fun () -> H1 [Text (string i)]))
        |> Enhance.WithLegend "Test Bind With"

    [<JavaScript>]
    let TestDependent =

        let f1 s =
            Formlet.Yield (fun x1 x2 -> x1 + x2)
            <*> (Controls.Input "" |> Enhance.WithTextLabel s)
            <*> (Controls.Input "" |> Enhance.WithTextLabel "L")
        let f2 =
            Formlet.Do {
                let! v1 =
                    f1 ""
                    |> Validator.IsNotEmpty ""
                let! v2 = f1 v1
                return (v1 + v2)
            }
        (
            Formlet.Yield (fun x y ->  x + " " + y)
            <*> f1 "A" <*> f2
        )
        |> CheckState "Test Dependent" id

    [<JavaScript>]
    let TestCombinators =
        [
            TestDependent
            TestSequence
            TestMany
            TestBindWith
            TestApply
        ]
        |> TestControl "Test Combinators"
        |> Enhance.WithFormContainer


    [<JavaScript>]
    let TestReadOnly =
        [
            Controls.ReadOnlyCheckbox true
            |> CheckState "Checkbox" string
            
            Controls.ReadOnlyCheckbox false
            |> CheckState "Checkbox" string

            Controls.ReadOnlyRadioButtonGroup None ["A", 1; "B" , 2]
            |> CheckState "Radio Button" string

            Controls.ReadOnlyRadioButtonGroup (Some 1) ["A", 1; "B" , 2]
            |> CheckState "Radio Button" string

            Controls.ReadOnlyInput "ReadOnly"
            |> CheckState "Input" id

            Controls.ReadOnlySelect 2 ["A", 0; "B", 1; "C", 2]
            |> CheckState "Select" string

            Controls.ReadOnlyTextArea "Default"
            |> CheckState "Text Area" id
        ]
        |> TestControl "ReadOnly" 

    (************************************************************************
    * Test Flowlets
    ************************************************************************)
    [<JavaScript>]
    let TestFlowlet : Formlet<string> =
        Controls.TextArea "a"


    (************************************************************************
    * Test rendering events
    ************************************************************************)
    [<JavaScript>]
    let TestAfterRender =
        let formlet = 
            Formlet.OfElement (fun _ -> 
                Div [Text "KOJ"]
            )
        let f2 = formlet |> Formlet.Map id
        let f1 = formlet 
        f2
    
    [<JavaScript>]
    let TestRenderEvents =
//        [
//            TestAfterRender
//        ]
//        |>
//        TestControl "Test Render Events"
//        |> ignore
        TestAfterRender

[<JavaScript>]
module ByCategory =

    let TestRenderEvents () =
        Div [FormletTest.TestRenderEvents]

    let TestReadOnly () =
        FormletTest.TestReadOnly
        |> Formlet.Render

    let TestControls () =
        FormletTest.TestControls
        |> Formlet.Render

    let TestEnhance () =
        FormletTest.TestEnhance
        |> Formlet.Render

    let TestCombinators () =
        FormletTest.TestCombinators
        |> Formlet.Render

    let TestValidation () =
        let f1 =
            Controls.Input ""
            |> Validator.IsInt "IsInt"
            |> Enhance.WithValidationIcon
            |> Enhance.WithFormContainer
        let f2 =
            Controls.Input ""
            |> Validator.IsFloat "IsFloat"
            |> Enhance.WithValidationIcon
            |> Enhance.WithFormContainer
        Formlet.Sequence [f1; f2]
        |> Formlet.Map ignore
        |> Formlet.Render

    let TestRenderConfig () =
        let fc = {
            Enhance.FormContainerConfiguration.Default with
                Header = Some (Enhance.FormPart.Text "Test render configuration")
                BackgroundColor = None // Some Css.Colors.Pink
                BorderColor = None // Some Css.Colors.Yellow
        }
        let submitConf = {
            Enhance.FormButtonConfiguration.Default with
                Label = Some "Submit"
                Style = Some "color:red;margin-top:10px;margin-right:10px;"
        }
        let resetConf = {
            Enhance.FormButtonConfiguration.Default with
                Label = None
                Style = None
        }
        FormletTest.TestControls
        |> Enhance.WithCustomSubmitAndResetButtons submitConf resetConf
        |> Enhance.WithCustomFormContainer fc
        |> Formlet.Render

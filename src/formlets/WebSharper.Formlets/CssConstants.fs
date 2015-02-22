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

[<Require(typeof<Resources.SkinResource>)>]
module CssConstants =

    [<Inline>]
    [<JavaScript>]
    let FormletClass = "formlet"

    [<Inline>]
    [<JavaScript>]
    let LabelContainerClass = "labelContainer"

    [<Inline>]
    [<JavaScript>]
    let ErrorFormletClass = "errorFormlet"

    [<Inline>]
    [<JavaScript>]
    let SuccessFormletClass = "successFormlet"

    [<Inline>]
    [<JavaScript>]
    let ErrorMessageClass = "errorMessage"

    [<Inline>]
    [<JavaScript>]
    let ValidIconClass = "validIcon"

    [<Inline>]
    [<JavaScript>]
    let AddIconClass = "addIcon"

    [<Inline>]
    [<JavaScript>]
    let RemoveIconClass = "removeIcon"

    [<Inline>]
    [<JavaScript>]
    let InfoIconClass = "infoIcon"

    [<Inline>]
    [<JavaScript>]
    let SuccessIconClass ="successIcon"

    [<Inline>]
    [<JavaScript>]
    let ErrorIconClass ="errorIcon"

    [<Inline>]
    [<JavaScript>]
    let ErrorPanelClass = "errorPanel"

    [<Inline>]
    [<JavaScript>]
    let HeaderPanelClass = "headerPanel"

    [<Inline>]
    [<JavaScript>]
    let RowClass = "row"

    [<JavaScript>]
    let InputTextClass = "inputText"

    [<Inline>]
    [<JavaScript>]
    let InputCheckboxClass = "inputCheckbox"

    [<Inline>]
    [<JavaScript>]
    let InputRadioClass = "inputRadio"

    [<Inline>]
    [<JavaScript>]
    let InputPasswordClass = "inputPassword"

    [<Inline>]
    [<JavaScript>]
    let SubmitButtonClass = "submitButton"

    [<Inline>]
    [<JavaScript>]
    let ResetButtonClass = "resetButton"

    [<Inline>]
    [<JavaScript>]
    let DisabledButtonClass = "disabledButton"

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

/// Provides a small vocabulary for dealing with Internet media types,
/// as far as WebSharper is concerned.
module WebSharper.Core.ContentTypes

open System
open System.IO

type ContentType =
    private {
        Raw : string
    }

    override ct.ToString() = ct.Raw
    member ct.Text = ct.Raw

let private Define n = { Raw = n }

module Application =
    let JavaScript = Define "application/javascript"

module Text =
    let JavaScript = Define "text/javascript"
    let Css = Define "text/css"
    let Plain = Define "text/plain"

let private (|EndsWith|_|) (suf: string) (x: string) =
    if x.ToLower().EndsWith(suf.ToLower()) then Some () else None

let TryGuessByFileName fn =
    match fn with
    | EndsWith ".js" -> Some Application.JavaScript
    | EndsWith ".css" -> Some Text.Css
    | _ -> None

let (|JavaScript|_|) ct =
    let id = ct.Raw.ToLower()
    if id = Application.JavaScript.Text || id = Text.JavaScript.Text then Some () else None

let (|Css|_|) ct =
    let id = ct.Raw.ToLower()
    if id = Text.Css.Text then Some () else None

let Parse (ct: string) =
    { Raw = ct.ToLower() }

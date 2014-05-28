// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

/// Provides a small vocabulary for dealing with Internet media types,
/// as far as WebSharper is concerned.
module IntelliFactory.WebSharper.Core.ContentTypes

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

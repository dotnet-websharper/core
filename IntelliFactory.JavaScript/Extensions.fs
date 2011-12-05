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

[<AutoOpen>]
module internal IntelliFactory.JavaScript.Extensions

open System
open System.Globalization

type Int32 with
    static member FromString text =
        let ic = CultureInfo.InvariantCulture
        match Int32.TryParse(text, NumberStyles.Any, ic) with
        | true, x -> Some x
        | _ -> None

type Int64 with
    static member FromString text =
        let ic = CultureInfo.InvariantCulture
        match Int64.TryParse(text, NumberStyles.Any, ic) with
        | true, x -> Some x
        | _ -> None

type Double with
    static member FromString text =
        let ic = CultureInfo.InvariantCulture
        match Double.TryParse(text, NumberStyles.Any, ic) with
        | true, x -> Some x
        | _ -> None

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

module IntelliFactory.WebSharper.Tests.Char

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
module R = IntelliFactory.WebSharper.Testing.Random
type private C = System.Char

[<JavaScript>]
let Tests =
    Section "Char"

    Test "Char.GetNumericValue" {
        C.GetNumericValue '4' =? 4.
        C.GetNumericValue 'k' =? -1.
    }

    Test "Char.IsDigit" {
        C.GetNumericValue '4' =? 4.
        C.GetNumericValue 'k' =? -1.
    }

    Test "Char.IsControl" {
        C.IsControl '\007' =? true
        C.IsControl 'a' =? false
    }

    Test "Char.IsDigit" {
        C.IsDigit '7' =? true
        C.IsDigit 'k' =? false
    }

    Test "Char.IsLetter" {
        C.IsLetter 'F' =? true
        C.IsLetter '9' =? false
    }

    Test "Char.IsLetterOrDigit" {
        C.IsLetterOrDigit 'F' =? true
        C.IsLetterOrDigit '9' =? true
        C.IsLetterOrDigit '_' =? false
    }

    Test "Char.IsLower" {
        C.IsLower 'F' =? false
        C.IsLower 'f' =? true
    }

    Test "Char.IsUpper" {
        C.IsUpper 'F' =? true
        C.IsUpper 'f' =? false
    }

    Test "Char.IsWhiteSpace" {
        C.IsWhiteSpace ' '  =? true
        C.IsWhiteSpace '\t' =? true
        C.IsWhiteSpace '3'  =? false
    }

    Test "Char.Parse" {
        C.Parse "k" =? 'k'
    }



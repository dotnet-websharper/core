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

module WebSharper.Tests.Char

open WebSharper
open WebSharper.Testing
module R = WebSharper.Testing.Random
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

    Test "char" {
        char 'x' =? 'x'
        char "a" =? 'a'
        char 65 =? 'A'
        Assert.Raises (fun () -> char "aa" |> ignore)
    }

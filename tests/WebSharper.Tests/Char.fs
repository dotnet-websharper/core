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
    Section "Char" {

        Test "Char.GetNumericValue" {
            Equal (C.GetNumericValue '4') 4.
            Equal (C.GetNumericValue 'k') -1.
        }

        Test "Char.IsDigit" {
            Equal (C.GetNumericValue '4') 4.
            Equal (C.GetNumericValue 'k') -1.
        }

        Test "Char.IsControl" {
            True (C.IsControl '\007')
            False (C.IsControl 'a')
        }

        Test "Char.IsDigit" {
            True (C.IsDigit '7')
            False (C.IsDigit 'k')
        }

        Test "Char.IsLetter" {
            True (C.IsLetter 'F')
            False (C.IsLetter '9')
        }

        Test "Char.IsLetterOrDigit" {
            True (C.IsLetterOrDigit 'F')
            True (C.IsLetterOrDigit '9')
            False (C.IsLetterOrDigit '_')
        }

        Test "Char.IsLower" {
            False (C.IsLower 'F')
            True (C.IsLower 'f')
        }

        Test "Char.IsUpper" {
            True (C.IsUpper 'F')
            False (C.IsUpper 'f')
        }

        Test "Char.IsWhiteSpace" {
            True (C.IsWhiteSpace ' ')
            True (C.IsWhiteSpace '\t')
            False (C.IsWhiteSpace '3')
        }

        Test "Char.Parse" {
            Equal (C.Parse "k") 'k'
        }

        Test "char" {
            Equal (char 'x') 'x'
            Equal (char "a") 'a'
            Equal (char 65) 'A'
            Raises (char "aa")
        }

    }
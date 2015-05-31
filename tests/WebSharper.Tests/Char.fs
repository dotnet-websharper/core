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
    TestCategory "Char" {

        Test "Char.GetNumericValue" {
            equal (C.GetNumericValue '4') 4.
            equal (C.GetNumericValue 'k') -1.
        }

        Test "Char.IsDigit" {
            equal (C.GetNumericValue '4') 4.
            equal (C.GetNumericValue 'k') -1.
        }

        Test "Char.IsControl" {
            isTrue (C.IsControl '\007')
            isFalse (C.IsControl 'a')
        }

        Test "Char.IsDigit" {
            isTrue (C.IsDigit '7')
            isFalse (C.IsDigit 'k')
        }

        Test "Char.IsLetter" {
            isTrue (C.IsLetter 'F')
            isFalse (C.IsLetter '9')
        }

        Test "Char.IsLetterOrDigit" {
            isTrue (C.IsLetterOrDigit 'F')
            isTrue (C.IsLetterOrDigit '9')
            isFalse (C.IsLetterOrDigit '_')
        }

        Test "Char.IsLower" {
            isFalse (C.IsLower 'F')
            isTrue (C.IsLower 'f')
        }

        Test "Char.IsUpper" {
            isTrue (C.IsUpper 'F')
            isFalse (C.IsUpper 'f')
        }

        Test "Char.IsWhiteSpace" {
            isTrue (C.IsWhiteSpace ' ')
            isTrue (C.IsWhiteSpace '\t')
            isFalse (C.IsWhiteSpace '3')
        }

        Test "Char.Parse" {
            equal (C.Parse "k") 'k'
        }

        Test "char" {
            equal (char 'x') 'x'
            equal (char "a") 'a'
            equal (char 65) 'A'
            raises (char "aa")
        }

    }
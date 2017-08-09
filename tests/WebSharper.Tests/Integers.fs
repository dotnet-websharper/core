// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

module WebSharper.Tests.Integers

open System
open WebSharper
open WebSharper.Testing

[<JavaScript>]
let Tests =
    TestCategory "Integers" {

        Test "Byte.Parse" {
            equalMsg (Byte.Parse "14") 14uy "14"
            raisesMsg (Byte.Parse "-14") "-14"
            raisesMsg (Byte.Parse "14a") "14a"
            raisesMsg (Byte.Parse "14.2") "14.2"
            equalMsg (Byte.Parse "200") 200uy "200"
            raisesMsg (Byte.Parse "300") "300"
        }

        Test "Byte.TryParse" {
            equalMsg (Byte.TryParse "14") (true, 14uy) "14"
            equalMsg (Byte.TryParse("-14", ref 0uy)) false "-14"
            equalMsg (Byte.TryParse("14a", ref 0uy)) false "14a"
            equalMsg (Byte.TryParse("14.2", ref 0uy)) false "14.2"
            equalMsg (Byte.TryParse "200") (true, 200uy) "200"
            equalMsg (Byte.TryParse("300", ref 0uy)) false "300"
        }

        Test "SByte.Parse" {
            equalMsg (SByte.Parse "14") 14y "14"
            equalMsg (SByte.Parse "-14") -14y "-14"
            raisesMsg (SByte.Parse "14a") "14a"
            raisesMsg (SByte.Parse "14.2") "14.2"
            equalMsg (SByte.Parse "100") 100y "100"
            raisesMsg (SByte.Parse "200") "200"
        }

        Test "SByte.TryParse" {
            equalMsg (SByte.TryParse "14") (true, 14y) "14"
            equalMsg (SByte.TryParse "-14") (true, -14y) "-14"
            equalMsg (SByte.TryParse("14a", ref 0y)) false "14a"
            equalMsg (SByte.TryParse("14.2", ref 0y)) false "14.2"
            equalMsg (SByte.TryParse "100") (true, 100y) "100"
            equalMsg (SByte.TryParse("200", ref 0y)) false "200"
        }

        Test "Int16.Parse" {
            equalMsg (Int16.Parse "14") 14s "14"
            equalMsg (Int16.Parse "-14") -14s "-14"
            raisesMsg (Int16.Parse "14a") "14a"
            raisesMsg (Int16.Parse "14.2") "14.2"
            equalMsg (Int16.Parse "30000") 30000s "30000"
            raisesMsg (Int16.Parse "40000") "40000"
        }

        Test "Int16.TryParse" {
            equalMsg (Int16.TryParse "14") (true, 14s) "14"
            equalMsg (Int16.TryParse "-14") (true, -14s) "-14"
            equalMsg (Int16.TryParse("14a", ref 0s)) false "14a"
            equalMsg (Int16.TryParse("14.2", ref 0s)) false "14.2"
            equalMsg (Int16.TryParse "30000") (true, 30000s) "30000"
            equalMsg (Int16.TryParse("40000", ref 0s)) false "40000"
        }

        Test "UInt16.Parse" {
            equalMsg (UInt16.Parse "14") 14us "14"
            raisesMsg (UInt16.Parse "-14") "-14"
            raisesMsg (UInt16.Parse "14a") "14a"
            raisesMsg (UInt16.Parse "14.2") "14.2"
            equalMsg (UInt16.Parse "60000") 60000us "60000"
            raisesMsg (UInt16.Parse "70000") "70000"
        }

        Test "UInt16.TryParse" {
            equalMsg (UInt16.TryParse "14") (true, 14us) "14"
            equalMsg (UInt16.TryParse("-14", ref 0us)) false "-14"
            equalMsg (UInt16.TryParse("14a", ref 0us)) false "14a"
            equalMsg (UInt16.TryParse("14.2", ref 0us)) false "14.2"
            equalMsg (UInt16.TryParse "60000") (true, 60000us) "60000"
            equalMsg (UInt16.TryParse("70000", ref 0us)) false "70000"
        }

        Test "Int32.Parse" {
            equalMsg (Int32.Parse "14") 14 "14"
            equalMsg (Int32.Parse "-14") -14 "-14"
            raisesMsg (Int32.Parse "14a") "14a"
            raisesMsg (Int32.Parse "14.2") "14.2"
            equalMsg (Int32.Parse "2000000000") 2000000000 "2000000000"
            raisesMsg (Int32.Parse "3000000000") "3000000000"
        }

        Test "Int32 literal fields" {
            equalMsg Int32.MaxValue 2147483647 "MaxValue"
            equalMsg Int32.MinValue -2147483648 "MinValue"
        }

        Test "Int32.TryParse" {
            equalMsg (Int32.TryParse "14") (true, 14) "14"
            equalMsg (Int32.TryParse "-14") (true, -14) "-14"
            equalMsg (Int32.TryParse("14a", ref 0)) false "14a"
            equalMsg (Int32.TryParse("14.2", ref 0)) false "14.2"
            equalMsg (Int32.TryParse "2000000000") (true, 2000000000) "2000000000"
            equalMsg (Int32.TryParse("3000000000", ref 0)) false "3000000000"
        }

        Test "UInt32.Parse" {
            equalMsg (UInt32.Parse "14") 14u "14"
            raisesMsg (UInt32.Parse "-14") "-14"
            raisesMsg (UInt32.Parse "14a") "14a"
            raisesMsg (UInt32.Parse "14.2") "14.2"
            equalMsg (UInt32.Parse "4000000000") 4000000000u "4000000000"
            raisesMsg (UInt32.Parse "5000000000") "5000000000"
        }

        Test "UInt32.TryParse" {
            equalMsg (UInt32.TryParse "14") (true, 14u) "14"
            equalMsg (UInt32.TryParse("-14", ref 0u)) false "-14"
            equalMsg (UInt32.TryParse("14a", ref 0u)) false "14a"
            equalMsg (UInt32.TryParse("14.2", ref 0u)) false "14.2"
            equalMsg (UInt32.TryParse "4000000000") (true, 4000000000u) "4000000000"
            equalMsg (UInt32.TryParse("5000000000", ref 0u)) false "5000000000"
        }
    }

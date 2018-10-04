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

module WebSharper.Tests.Conversions

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

type N = WebSharper.CSharp.Tests.MyNumber

[<JavaScript>]
let Tests =

    TestCategory "Conversions" {

        Test "ToByte" {
            strictEqual (byte (As<byte> 1000)) 232uy
            strictEqual (byte -55y) 201uy
            strictEqual (byte 1000us) 232uy
            strictEqual (byte -1000s) 24uy
            strictEqual (byte 1000u) 232uy
            strictEqual (byte -1000) 24uy
            strictEqual (byte 1000UL) 232uy
            strictEqual (byte -1000L) 24uy
            strictEqual (byte -500.5f) 12uy
            strictEqual (byte -500.5) 12uy
            strictEqual (byte "50") 50uy
            strictEqual (byte 'a') 97uy
            strictEqual (byte 'a') 97uy
            strictEqual (byte 'Ϩ') 232uy
        }

        Test "ToSByte" {
            strictEqual (sbyte (As<byte> 1000)) -24y
            strictEqual (sbyte -55y)    -55y
            strictEqual (sbyte 1000us) -24y
            strictEqual (sbyte -1000s) 24y
            strictEqual (sbyte 1000u) -24y
            strictEqual (sbyte -1000) 24y
            strictEqual (sbyte 1000UL) -24y
            strictEqual (sbyte -1000L) 24y
            strictEqual (sbyte -500.5f) 12y
            strictEqual (sbyte -500.5) 12y
            strictEqual (sbyte "50") 50y
            strictEqual (sbyte 'a') 97y
            strictEqual (sbyte 'a') 97y
            strictEqual (sbyte 'Ϩ') -24y   
        }   
        
        Test "ToInt16" {
            strictEqual (int16 100000UL) -31072s
        }

        Test "ToUInt16" {
            strictEqual (uint16 100000UL) 34464us
        }

        Test "ToInt32" {
            strictEqual (int 1000000000000UL) -727379968
        }

        Test "ToUInt32" {
            strictEqual (uint32 1000000000000UL) 3567587328u
        }

        Test "ToChar" {
            strictEqual (char "a") 'a'
            raises (char "")
            raises (char "ab")
            strictEqual (char 97) 'a'
            strictEqual (char 97.9) 'a'
        }     
        
        Test "ToString" {
            strictEqual (string -1000) "-1000"
            strictEqual (string -1000.55) "-1000.55"
            strictEqual (string 'a') "a"
            strictEqual (string 'a') "a"
        }

        Test "Custom" {
            strictEqual (float (N 3.3)) 3.3
            strictEqual (string (N 3.3)) "3.3"
        }
    }

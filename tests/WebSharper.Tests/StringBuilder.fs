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

module WebSharper.Tests.StringBuilder

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
let Tests =

    TestCategory "StringBuilder" {

        Test "Basics" {
            let sb = System.Text.StringBuilder()
            let res = 
                sb
                    .Append("test")
                    .Append("append")
                    .AppendLine("newline")
                    .AppendLine("newline2")
                    .ToString()
            equal (res.Split([|'\n'|])[0]) "testappendnewline"
            equal (res.Split([|'\n'|])[1]) "newline2"
        }

        Test "Init constructor" {
            let sb = System.Text.StringBuilder("Hello")
            let res = 
                sb
                    .Append(" World")
                    .ToString()
            equal res "Hello World"
        }

        Test "Length" {
            let sb = System.Text.StringBuilder("Hello")
            sb.Append(" World") |> ignore
            equal sb.Length 11
        }

        Test "Clear" {
            let sb = System.Text.StringBuilder("Hello")
            sb.Clear().Append(" World") |> ignore
            equal (string sb) " World"
        }

        Test "Chars" {
            let sb = System.Text.StringBuilder("Hello")
            equal (sb.Chars(1)) 'e'
            sb.Chars(1) <- 'u'
            equal (string sb) "Hullo"
        }

        Test "Insert" {
            let sb = System.Text.StringBuilder("HelloWorld")
            let res = sb.Insert(5, " ").ToString()
            equal res "Hello World"

            raises (sb.Insert (-1, " "))
            raises (sb.Insert (12, " "))
        }

        Test "Remove" {
            let sb = System.Text.StringBuilder("Hello!! World")
            let res = sb.Remove(5, 2).ToString()
            equal res "Hello World"

            raises (sb.Remove (-1, 1))
            raises (sb.Remove (12, 1))
        }

        Test "Replace" {
            let sb = System.Text.StringBuilder("Hello Wh")
            sb.Append("at") |> ignore
            let res = sb.Replace("What", "World").ToString()
            equal res "Hello World"

            let res2 = sb.Replace('l', 'L', 0, 3).ToString()
            equal res2 "HeLlo World"
        }
    }

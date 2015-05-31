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

module WebSharper.Tests.Queue

open WebSharper
open WebSharper.Testing
type Queue<'T> = System.Collections.Generic.Queue<'T>

[<JavaScript>]
let Tests =
    TestCategory "Queue" {

        Test "Construction" {
            let s = Queue()
            equal s.Count 0
        }

        Test "Enqueue" {
            let s = Queue()
            s.Enqueue 1
            s.Enqueue 2
            equal (s.ToArray()) [| 1; 2 |]
        }

        Test "Dequeue" {
            let s = Queue()
            s.Enqueue 1
            s.Enqueue 2
            equal (s.Dequeue()) 1
            equal (s.ToArray()) [|2|]
        }

    }
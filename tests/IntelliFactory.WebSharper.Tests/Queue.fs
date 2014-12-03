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

module IntelliFactory.WebSharper.Tests.Queue

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
type Queue<'T> = System.Collections.Generic.Queue<'T>

[<JavaScript>]
let Tests =
    Section "Queue"

    Test "Construction" {
        let s = Queue()
        s.Count =? 0
    }

    Test "Enqueue" {
        let s = Queue()
        s.Enqueue 1
        s.Enqueue 2
        s.ToArray() =? [| 1; 2 |]
    }

    Test "Dequeue" {
        let s = Queue()
        s.Enqueue 1
        s.Enqueue 2
        s.Dequeue() =? 1
        s.ToArray() =? [|2|]
    }

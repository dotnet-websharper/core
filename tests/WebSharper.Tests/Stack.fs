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

module WebSharper.Tests.Stack

open WebSharper
open WebSharper.Testing
type private Stack<'T> = System.Collections.Generic.Stack<'T>

[<JavaScript>]
let Tests =

    Section "Stack"

    Test "new" {
        let s = Stack<int>()
        s.Count =? 0
    }

    Test "Push" {
        let s = Stack<int>()
        s.Push 1
        s.Push 2
        s.ToArray() =? [| 2; 1 |]
    }

    Test "Pop" {
        let s = Stack<int>()
        s.Push 1
        s.Push 2
        s.Pop() =? 2
        s.ToArray() =? [|1|]
    }


// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

module WebSharper.Collections.Tests.LinkedList

open WebSharper
open WebSharper.Testing

type LL<'T> = System.Collections.Generic.LinkedList<'T>

[<JavaScript>]
let Tests =
    TestCategory "LinkedList" {

        Test "Construction" {
            equal (LL<int>().Count) 0
            equal (LL(seq { 1 .. 3 }) |> Array.ofSeq) [| 1; 2; 3 |]
        }

        Test "Adding nodes" {
            let l = LL()
            l.AddFirst(3) |> ignore
            equal (Array.ofSeq l) [| 3 |]
            let n = l.AddLast(5)
            equal (Array.ofSeq l) [| 3; 5 |]
            l.AddBefore(n, 4) |> ignore
            equal (Array.ofSeq l) [| 3; 4; 5 |]
            l.AddAfter(n, 6) |> ignore
            equal (Array.ofSeq l) [| 3; 4; 5; 6 |]
            isTrue (l.Contains 6)
            l.Clear()
            equal (Array.ofSeq l) [||]
        }

        Test "Removing nodes" {
            let l = LL(seq { 1 .. 5 })
            isTrue (l.Remove 3)
            isFalse (l.Remove 3)
            equal (Array.ofSeq l) [| 1; 2; 4; 5 |]
            l.RemoveFirst();
            equal (Array.ofSeq l) [| 2; 4; 5 |]
            l.RemoveLast();
            equal (Array.ofSeq l) [| 2; 4 |]
            l.AddLast(0) |> l.Remove;
            equal (Array.ofSeq l) [| 2; 4 |]
            l.AddFirst(0) |> l.Remove;
            equal (Array.ofSeq l) [| 2; 4 |]
        }

    }

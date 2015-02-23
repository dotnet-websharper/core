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

module WebSharper.Tests.LinkedList

open WebSharper
open WebSharper.Testing

type LL<'T> = System.Collections.Generic.LinkedList<'T>

[<JavaScript>]
let Tests =
    Section "LinkedList"

    Test "Construction" {
        LL<int>().Count =? 0 
        LL(seq { 1 .. 3 }) |> Array.ofSeq =? [| 1; 2; 3 |]
    }

    Test "Adding nodes" {
        let l = LL()
        l.AddFirst(3) |> ignore
        Array.ofSeq l =? [| 3 |]
        let n = l.AddLast(5)
        Array.ofSeq l =? [| 3; 5 |]
        l.AddBefore(n, 4) |> ignore
        Array.ofSeq l =? [| 3; 4; 5 |]
        l.AddAfter(n, 6) |> ignore
        Array.ofSeq l =? [| 3; 4; 5; 6 |]
        l.Contains(6) =? true
        l.Clear()
        Array.ofSeq l =? [||]
    }

    Test "Removing nodes" {
        let l = LL(seq { 1 .. 5 })
        l.Remove(3) =? true;
        l.Remove(3) =? false;
        Array.ofSeq l =? [| 1; 2; 4; 5 |]
        l.RemoveFirst();
        Array.ofSeq l =? [| 2; 4; 5 |]
        l.RemoveLast();
        Array.ofSeq l =? [| 2; 4 |]
        l.AddLast(0) |> l.Remove;
        Array.ofSeq l =? [| 2; 4 |]
        l.AddFirst(0) |> l.Remove;
        Array.ofSeq l =? [| 2; 4 |]
    }

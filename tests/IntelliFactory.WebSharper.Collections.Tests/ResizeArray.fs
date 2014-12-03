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

module IntelliFactory.WebSharper.Collections.Tests.ResizeArray

open System
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

[<JavaScript>]
let Tests =

    Section "ResizeArray"

    Test "ResizeArray.ctor" {
        let a = ResizeArray()
        a.ToArray() =? [||]
        let b = ResizeArray(3)
        b.ToArray() =? [||]
        let c = ResizeArray([1;2;3])
        c.ToArray() =? [| 1; 2; 3 |]
    }

    Test "ResizeArray.Add" {
        let a = ResizeArray([1])
        a.Add(2)
        a.Add(3)
        a.ToArray() =? [| 1; 2; 3 |]
    }

    Test "ResizeArray.AddRange" {
        let a = ResizeArray([1])
        a.AddRange([2;3])
        a.ToArray() =? [| 1; 2; 3 |]
    }

    Test "ResizeArray.seq" {
        let l = [2;3]
        let a = ResizeArray(l)
        Seq.forall2 (=) a l =? true
    }

    Test "ResizeArray.Clear" {
        let a = ResizeArray([1;2;3])
        a.Clear()
        a.Add(1)
        a.ToArray() =? [| 1 |]
    }

    Test "ResizeArray.CopyTo" {
        let a = ResizeArray([1;2;3])
        let x = [| 0; 0; 0; 0; 0 |]
        a.CopyTo(x)
        x =? [| 1; 2; 3; 0; 0 |]
        a.CopyTo(x, 1)
        x =? [| 1; 1; 2; 3; 0 |]
        a.CopyTo(1, x, 3, 2)
        x =? [| 1; 1; 2; 2; 3 |]
    }

    Test "ResizeArray.Count" {
        let a = ResizeArray([1; 2])
        a.Count =? 2
    }

    Test "ResizeArray.GetRange" {
        let a = ResizeArray([1;2;3;4;5])
        a.GetRange(2, 2).ToArray() =? [|3; 4|]
    }

    Test "ResizeArray.Insert" {
        let a = ResizeArray([1;2;3])
        a.Insert(1, -1)
        a.ToArray() =? [| 1; -1; 2; 3 |]
    }

    Test "ResizeArray.InsertRange" {
        let a = ResizeArray([1;2;3])
        a.InsertRange(1, [-1; -2])
        a.ToArray() =? [| 1; -1; -2; 2; 3 |]
    }

    Test "ResizeArray.Item" {
        let a = ResizeArray([1;2;3])
        a.[0] =? 1
        a.[1] =? 2
        a.[2] =? 3
    }

    Test "ResizeArray.set_Item" {
        let a = ResizeArray([1;2;3])
        a.[0] <- 4
        a.[1] <- 5
        a.[2] <- 6
        a.[0] =? 4
        a.[1] =? 5
        a.[2] =? 6
    }

    Test "ResizeArray.RemoveAt" {
        let a = ResizeArray([1;2;3])
        a.RemoveAt(1)
        a.ToArray() =? [|1; 3|]
    }

    Test "ResizeArray.RemoveRange" {
        let a = ResizeArray([1;2;3;4;5])
        a.RemoveRange(2, 2)
        a.ToArray() =? [|1; 2; 5|]
    }

    Test "ResizeArray.Reverse" {
        let a = ResizeArray([1;2;3;4;5])
        a.Reverse()
        a.ToArray() =? [| 5; 4; 3; 2; 1|]
        a.Reverse(2, 2)
        a.ToArray() =? [| 5; 4; 2; 3;  1|]
    }

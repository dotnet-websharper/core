// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module IntelliFactory.WebSharper.Tests.LinkedList

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

type LL<'T> = System.Collections.Generic.LinkedList<'T>

[<JavaScript>]
let Tests =
    Section "LinkedList"

    Test "Construction" {
        LL(seq { 1 .. 3 }) |> JavaScript.Log
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
    }

// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

module IntelliFactory.WebSharper.Tests.Stack

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
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


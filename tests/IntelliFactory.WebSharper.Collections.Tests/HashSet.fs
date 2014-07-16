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

module IntelliFactory.WebSharper.Tests.HashSet

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

type HS<'T> = System.Collections.Generic.HashSet<'T>

[<JavaScript>]
let Tests =
    Section "HashSet"

    Test "Construction" {
        let s = HS [ "a"; "b"; "c"; "a" ]
        s.Count =? 3
        Set.ofSeq s =? Set [ "a"; "b"; "c" ]
    }

    Test "Element operations" {
        let s = HS [ "a"; "b"; "c"; "a" ]
        s.Add("b") =? false
        s.Remove("a") =? true
        s.Count =? 2
        s.Clear()
        s.Count =? 0
    }

    Test "Set operations" {
        let s = HS [ 1 .. 5 ]
        s.ExceptWith(seq { 4 .. 7 })
        Set.ofSeq s =? Set [ 1 .. 3 ]
        s.UnionWith(seq { 4 .. 7 })
        Set.ofSeq s =? Set [ 1 .. 7 ]
        (HS [ 1 .. 5]).IsSubsetOf(HS [ 0 .. 7 ]) =? true
    }

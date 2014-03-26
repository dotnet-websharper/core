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

module IntelliFactory.WebSharper.Tests.Option

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
module R = IntelliFactory.WebSharper.Testing.Random

[<JavaScript>]
let Tests =

    Section "Option"

    Test "Option.bind" {
        let f = (+) 1 >> Some
        Option.bind f (Some 4) =? Some 5
        Option.bind f None =? None
    }

    Test "Option.count" {
        Option.count (Some 5) =? 1
        Option.count None     =? 0
    }

    Test "Option.exists" {
        let predicate = (=) 3
        Option.exists predicate (Some 3) =? true
        Option.exists predicate None =? false
    }

    Test "Option.fold" {
        Option.fold (+) 1 (Some 3) =? 4
        Option.fold (+) 1 None     =? 1
    }

    Test "Option.foldBack" {
        Option.foldBack (+) (Some 3) 1 =? 4
        Option.foldBack (+) None 1     =? 1
    }

    Test "Option.forall" {
        let predicate = (=) 2
        Option.forall predicate (Some 3) =? false
        Option.forall predicate None     =? true
    }

    Test "Option.get" {
        (Some 1).Value =? 1
        Option.get (Some 1) =? 1
    }

    Test "Option.isNone" {
        None.IsNone =? true
        Option.isNone None =? true
        (Some 1).IsNone =? false
        Option.isNone (Some 1) =? false
    }

    Test "Option.isSome" {
        None.IsSome =? false
        Option.isSome None =? false
        (Some 1).IsSome =? true
        Option.isSome (Some 1) =? true
    }

    Test "Option.iter" {
        let cell = ref 0
        let incr x = cell := !cell + x
        Option.iter incr None
        !cell =? 0
        Option.iter incr (Some 3)
        !cell =? 3
    }

    Test "Option.map" {
        Option.map ((+) 1) (Some 3) =? Some 4
        Option.map ((+) 1) None =? None
    }

    Test "Option.toArray" {
        Option.toArray (Some 3) =? [| 3 |]
        Option.toArray None     =? [||]
    }

    Test "Option.toList" {
        Option.toList (Some 3) =? [3]
        Option.toList None     =? []
    }

    Test "Equality" {
        Some 15 =? Some 15
        (None : option<int>) =? None
    }

    Test "Matching" {
        let r =
            match None with
            | Some x -> x
            | None   -> -1
        r =? -1
        Assert.For 100 R.Int (fun i ->
            let o = Some i
            let a =
                match o with
                | Some x -> x
                | None   -> -1
            i =? a)
    }


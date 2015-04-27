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

module WebSharper.Tests.Option

open WebSharper
open WebSharper.Testing
module R = WebSharper.Testing.Random

[<JavaScript>]
let Tests =

    Section "Option" {

        Test "Option.bind" {
            let f = (+) 1 >> Some
            Equal (Option.bind f (Some 4)) (Some 5)
            Equal (Option.bind f None) None
        }

        Test "Option.count" {
            Equal (Option.count (Some 5)) 1
            Equal (Option.count None)     0
        }

        Test "Option.exists" {
            let predicate = (=) 3
            True (Option.exists predicate (Some 3))
            False (Option.exists predicate None)
        }

        Test "Option.fold" {
            Equal (Option.fold (+) 1 (Some 3)) 4
            Equal (Option.fold (+) 1 None)     1
        }

        Test "Option.foldBack" {
            Equal (Option.foldBack (+) (Some 3) 1) 4
            Equal (Option.foldBack (+) None 1)     1
        }

        Test "Option.forall" {
            let predicate = (=) 2
            False (Option.forall predicate (Some 3))
            True (Option.forall predicate None)
        }

        Test "Option.get" {
            Equal (Some 1).Value 1
            Equal (Option.get (Some 1)) 1
        }

        Test "Option.isNone" {
            True None.IsNone
            True (Option.isNone None)
            False (Some 1).IsNone
            False (Option.isNone (Some 1))
        }

        Test "Option.isSome" {
            False None.IsSome
            False (Option.isSome None)
            True (Some 1).IsSome
            True (Option.isSome (Some 1))
        }

        Test "Option.iter" {
            let cell = ref 0
            let incr x = cell := !cell + x
            Option.iter incr None
            Equal !cell 0
            Option.iter incr (Some 3)
            Equal !cell 3
        }

        Test "Option.map" {
            Equal (Option.map ((+) 1) (Some 3)) (Some 4)
            Equal (Option.map ((+) 1) None) None
        }

        Test "Option.toArray" {
            Equal (Option.toArray (Some 3)) [| 3 |]
            Equal (Option.toArray None)     [||]
        }

        Test "Option.toList" {
            Equal (Option.toList (Some 3)) [3]
            Equal (Option.toList None)     []
        }

        Test "Equality" {
            Equal (Some 15) (Some 15)
            Equal (None : option<int>) None
        }

        Test "Matching" {
            let r =
                match None with
                | Some x -> x
                | None   -> -1
            Equal r -1
            ForR 100 R.Int (fun i -> Do {
                let o = Some i
                let a =
                    match o with
                    | Some x -> x
                    | None   -> -1
                Equal i a
            })
        }

    }
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

    TestCategory "Option" {

        Test "Option.bind" {
            let f = (+) 1 >> Some
            equal (Option.bind f (Some 4)) (Some 5)
            equal (Option.bind f None) None
        }

        Test "Option.count" {
            equal (Option.count (Some 5)) 1
            equal (Option.count None)     0
        }

        Test "Option.exists" {
            let predicate = (=) 3
            isTrue (Option.exists predicate (Some 3))
            isFalse (Option.exists predicate None)
        }

        Test "Option.fold" {
            equal (Option.fold (+) 1 (Some 3)) 4
            equal (Option.fold (+) 1 None)     1
        }

        Test "Option.foldBack" {
            equal (Option.foldBack (+) (Some 3) 1) 4
            equal (Option.foldBack (+) None 1)     1
        }

        Test "Option.forall" {
            let predicate = (=) 2
            isFalse (Option.forall predicate (Some 3))
            isTrue (Option.forall predicate None)
        }

        Test "Option.get" {
            equal (Some 1).Value 1
            equal (Option.get (Some 1)) 1
        }

        Test "Option.isNone" {
            isTrue None.IsNone
            isTrue (Option.isNone None)
            isFalse (Some 1).IsNone
            isFalse (Option.isNone (Some 1))
        }

        Test "Option.isSome" {
            isFalse None.IsSome
            isFalse (Option.isSome None)
            isTrue (Some 1).IsSome
            isTrue (Option.isSome (Some 1))
        }

        Test "Option.iter" {
            let cell = ref 0
            let incr x = cell := !cell + x
            Option.iter incr None
            equal !cell 0
            Option.iter incr (Some 3)
            equal !cell 3
        }

        Test "Option.map" {
            equal (Option.map ((+) 1) (Some 3)) (Some 4)
            equal (Option.map ((+) 1) None) None
        }

        Test "Option.toArray" {
            equal (Option.toArray (Some 3)) [| 3 |]
            equal (Option.toArray None)     [||]
        }

        Test "Option.toList" {
            equal (Option.toList (Some 3)) [3]
            equal (Option.toList None)     []
        }

        Test "Equality" {
            equal (Some 15) (Some 15)
            equal (None : option<int>) None
        }

        Test "Matching" {
            let r =
                match None with
                | Some x -> x
                | None   -> -1
            equal r -1
            property (fun i -> Do {
                let o = Some i
                let a =
                    match o with
                    | Some x -> x
                    | None   -> -1
                equal i a
            })
        }

        #if FSHARP40

        Test "Option.ofObj" {
            equal (Option.ofObj null) None
            let o = obj()
            equal (Option.ofObj o) (Some o)
        }

        Test "Option.toObj" {
            equal (Option.toObj None) null
            let o = obj()
            equal (Option.toObj (Some o)) o
        }

        Test "Option.filter" {
            equal (Option.filter (fun _ -> true) None) None
            equal (Option.filter (fun _ -> true) (Some 3)) (Some 3)
            equal (Option.filter (fun _ -> false) (Some 3)) None
        }

        #endif

    }
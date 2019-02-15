// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

module WebSharper.Tests.ValueOption

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
module R = WebSharper.Testing.RandomValues

[<JavaScript>]
let Tests =

    TestCategory "ValueOption" {

        Test "ValueOption.bind" {
            let f = (+) 1 >> ValueSome
            equal (ValueOption.bind f (ValueSome 4)) (ValueSome 5)
            equal (ValueOption.bind f ValueNone) ValueNone
        }

        Test "ValueOption.count" {
            equal (ValueOption.count (ValueSome 5)) 1
            equal (ValueOption.count ValueNone)     0
        }

        Test "ValueOption.exists" {
            let predicate = (=) 3
            isTrue (ValueOption.exists predicate (ValueSome 3))
            isFalse (ValueOption.exists predicate ValueNone)
        }

        Test "ValueOption.fold" {
            equal (ValueOption.fold (+) 1 (ValueSome 3)) 4
            equal (ValueOption.fold (+) 1 ValueNone)     1
        }

        Test "ValueOption.foldBack" {
            equal (ValueOption.foldBack (+) (ValueSome 3) 1) 4
            equal (ValueOption.foldBack (+) ValueNone 1)     1
        }

        Test "ValueOption.forall" {
            let predicate = (=) 2
            isFalse (ValueOption.forall predicate (ValueSome 3))
            isTrue (ValueOption.forall predicate ValueNone)
        }

        Test "ValueOption.get" {
            equal (ValueSome 1).Value 1
            equal (ValueOption.get (ValueSome 1)) 1
        }

        Test "ValueOption.isNone" {
            isTrue ValueNone.IsNone
            isTrue (ValueOption.isNone ValueNone)
            isFalse (ValueSome 1).IsNone
            isFalse (ValueOption.isNone (ValueSome 1))
        }

        Test "ValueOption.isSome" {
            isFalse ValueNone.IsSome
            isFalse (ValueOption.isSome ValueNone)
            isTrue (ValueSome 1).IsSome
            isTrue (ValueOption.isSome (ValueSome 1))
        }

        Test "ValueOption.iter" {
            let cell = ref 0
            let incr x = cell := !cell + x
            ValueOption.iter incr ValueNone
            equal !cell 0
            ValueOption.iter incr (ValueSome 3)
            equal !cell 3
        }

        Test "ValueOption.map" {
            equal (ValueOption.map ((+) 1) (ValueSome 3)) (ValueSome 4)
            equal (ValueOption.map ((+) 1) ValueNone) ValueNone
        }

        Test "ValueOption.map2" {
            equal (ValueOption.map2 (+) (ValueSome 3) (ValueSome 1)) (ValueSome 4)
            equal (ValueOption.map2 (+) ValueNone (ValueSome 1)) ValueNone
            equal (ValueOption.map2 (+) (ValueSome 1) ValueNone) ValueNone
        }

        Test "ValueOption.map3" {
            equal (ValueOption.map3 (fun a b c -> a + b + c) (ValueSome 3) (ValueSome 1) (ValueSome 2)) (ValueSome 6)
            equal (ValueOption.map3 (fun a b c -> a + b + c) ValueNone (ValueSome 1) (ValueSome 2)) ValueNone
            equal (ValueOption.map3 (fun a b c -> a + b + c) (ValueSome 3) ValueNone (ValueSome 2)) ValueNone
            equal (ValueOption.map3 (fun a b c -> a + b + c) (ValueSome 3) (ValueSome 1) ValueNone) ValueNone
        }

        Test "ValueOption.toArray" {
            equal (ValueOption.toArray (ValueSome 3)) [| 3 |]
            equal (ValueOption.toArray ValueNone)     [||]
        }

        Test "ValueOption.toList" {
            equal (ValueOption.toList (ValueSome 3)) [3]
            equal (ValueOption.toList ValueNone)     []
        }

        Test "Equality" {
            equal (ValueSome 15) (ValueSome 15)
            equal (ValueNone : ValueOption<int>) ValueNone
        }

        Test "Matching" {
            let r =
                match ValueNone with
                | ValueSome x -> x
                | ValueNone   -> -1
            equal r -1
            property (fun i -> Do {
                let o = ValueSome i
                let a =
                    match o with
                    | ValueSome x -> x
                    | ValueNone   -> -1
                equal i a
            })
        }

        Test "ValueOption.ofObj" {
            equal (ValueOption.ofObj null) ValueNone
            let o = obj()
            equal (ValueOption.ofObj o) (ValueSome o)
        }

        Test "ValueOption.toObj" {
            jsEqual (ValueOption.toObj ValueNone) null
            let o = obj()
            equal (ValueOption.toObj (ValueSome o)) o
        }

        Test "ValueOption.ofNullable" {
            equal (ValueOption.ofNullable (System.Nullable())) ValueNone
            let o = System.Nullable(3)
            equal (ValueOption.ofNullable o) (ValueSome 3)
        }

        Test "ValueOption.toNullable" {
            jsEqual (ValueOption.toNullable ValueNone) (System.Nullable())
            equal (ValueOption.toNullable (ValueSome 3)) (System.Nullable(3))
        }

        Test "ValueOption.filter" {
            equal (ValueOption.filter (fun _ -> true) ValueNone) ValueNone
            equal (ValueOption.filter (fun _ -> true) (ValueSome 3)) (ValueSome 3)
            equal (ValueOption.filter (fun _ -> false) (ValueSome 3)) ValueNone
        }

        Test "ValueOption.defaultValue" {
            equal (ValueOption.defaultValue 1 ValueNone) 1
            equal (ValueOption.defaultValue 1 (ValueSome 2)) 2
            equal (ValueOption.defaultWith (fun () -> 1) ValueNone) 1
            let called = ref false
            equal (ValueOption.defaultWith (fun () -> called := true; 1) (ValueSome 2)) 2
            isFalse !called
        }

        Test "ValueOption.flatten" {
            equal (ValueOption.flatten ValueNone) ValueNone
            equal (ValueOption.flatten (ValueSome ValueNone)) ValueNone
            equal (ValueOption.flatten (ValueSome (ValueSome 3))) (ValueSome 3)
        }

        Test "ValueOption.orElse" {
            equal (ValueOption.orElse (ValueSome 1) ValueNone) (ValueSome 1)
            equal (ValueOption.orElse (ValueSome 1) (ValueSome 2)) (ValueSome 2)
            equal (ValueOption.orElseWith (fun () -> ValueSome 1) ValueNone) (ValueSome 1)
            let called = ref false
            equal (ValueOption.orElseWith (fun () -> called := true; ValueSome 1) (ValueSome 2)) (ValueSome 2)
            isFalse !called
        }
    }

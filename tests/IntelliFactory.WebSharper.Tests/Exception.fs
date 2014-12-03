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

module IntelliFactory.WebSharper.Tests.Exception

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

exception E0
exception E1 of int
exception E2 of int * string

type E3 [<JavaScript>] (message) =
    inherit exn(message)

[<JavaScript>]
let Tests =

    Section "Exception"

    Test "E0" {
        (try 1 with E0 -> 2) =? 1
        (try (raise E0; 1) with E0 -> 2) =? 2
    }

    Test "E1" {
        (try (raise (E1 3); 0) with E1 x -> x) =? 3
    }

    Test "E2" {
        (try (raise (E2 (1, "K")); "") with E2 (_, x) -> x) =? "K"
    }

    Test "E3" {
        let _ =
            try raise (E3 "OOPS") with
            | :? E3 as x -> x.Message =? "OOPS"
        ()
    }

    Test "Catching" {
        let k =
            try raise (E3 "OOPS") with
            | E0 _ -> 0
            | E1 _ -> 1
            | :? E3 -> 3
            | E2 _ -> 2
        k =? 3
    }

    Test "Reraising" {
        let _ =
            try
                try raise (E3 "OOPS") with
                | E0 _ -> ()
                | E2 _ -> ()
            with e ->
                e.Message =? "OOPS"
        ()
    }


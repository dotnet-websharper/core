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

module WebSharper.Tests.Exception

open WebSharper
open WebSharper.Testing

[<JavaScript>] exception E0
[<JavaScript>] exception E1 of int
[<JavaScript>] exception E2 of int * string

type E3 [<JavaScript>] (message) =
    inherit exn(message)

module Server =
    [<Remote>]
    let error() : Async<string> = 
        async {
            return failwith "Server error in remote method"
        }

[<JavaScript>]
let Tests =

    TestCategory "Exception" {

        Test "Raises" {
            raises (failwith "should fail")
        }
        
        Test "E0" {
            equal (try 1 with E0 -> 2) 1
            equal (try (raise E0; 1) with E0 -> 2) 2
        }

        Test "E1" {
            equal (try (raise (E1 3); 0) with E1 x -> x) 3
        }

        Test "E2" {
            equal (try (raise (E2 (1, "K")); "") with E2 (_, x) -> x) "K"
        }

        Test "E3" {
            equal (
                try
                    raise (E3 "OOPS")
                    "Success"
                with
                | :? E3 as x -> x.Message)
                "OOPS"
        }

        Test "Catching" {
            let k =
                try raise (E3 "OOPS") with
                | E0 _ -> 0
                | E1 _ -> 1
                | :? E3 -> 3
                | E2 _ -> 2
            equal k 3
        }

        Test "Reraising" {
            equal (
                try
                    try
                        raise (E3 "OOPS")
                        "Success"
                    with
                    | E0 _ -> "E0"
                    | E2 _ -> "E2"
                with e ->
                    e.Message)
                "OOPS"
        }

        Test "Exception properties" {
            equal (
                try (raise (exn("OOPS", E0)); "no error", 1) 
                with e -> 
                    e.Message,
                    match e.InnerException with
                    | E0 -> 2
                    | _ -> 0
            ) ("OOPS", 2)            
        }

        Test "Inheritance" {
            isTrue (box (System.ArgumentException()) :? exn)
            isTrue (box E0 :? exn)
            isTrue (box (E1 42) :? exn)
            isTrue (box (E2(10, "a")) :? exn)
            isTrue (box (E3("a")) :? exn)
        }

        Test "Print message #1402" {
            let msg =
                try
                    failwithf "My %s message" "error"
                with
                | exn ->
                    sprintf "%A" exn
            equal msg "Error: My error message"
        }

        Test "Print message from server error #1402" {
            let! msg =
                async {
                    try
                        let! msg = Server.error()    
                        return msg
                    with
                    | exn ->
                        return sprintf "%A" exn
                }
            equal msg "Error: Response status is not 200: 500"
        }
    }

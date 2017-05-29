// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

module WebSharper.Tests.Result

open WebSharper
open WebSharper.Testing

[<JavaScript>]
let Tests =

    TestCategory "Result" {

        Test "Contruct" {
            let r = Ok 3
            let r2 = Error "e"
            equal (match r with Ok x -> x | _ -> 0) 3
            equal (match r2 with Error x -> x | _ -> "") "e"
        }

        Test "Map" {
            let r = Ok 3
            let r2 = Error "e"
            equal (r |> Result.map (fun x -> x + 1)) (Ok 4)
            equal (r2 |> Result.map (fun x -> x + 1)) (Error "e")
        }

        Test "MapError" {
            let r = Ok 3
            let r2 = Error "e"
            equal (r |> Result.mapError (fun x -> x + "r")) (Ok 3)
            equal (r2 |> Result.mapError (fun x -> x + "r")) (Error "er")
        }

        Test "Bind" {
            let r = Ok 3
            let r2 = Error "e"
            equal (r |> Result.bind (fun x -> Ok (x + 1))) (Ok 4)
            equal (r |> Result.bind (fun x -> Error "err")) (Error "err")
            equal (r2 |> Result.bind (fun x -> Ok())) (Error "e")
        }

    }

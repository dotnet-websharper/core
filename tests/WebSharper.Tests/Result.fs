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

module WebSharper.Tests.Result

open WebSharper
open WebSharper.Testing

[<JavaScript>]
let Tests =

    let r = Ok 3
    let r2 = Error "e"

    TestCategory "Result" {
        Test "Contruct" {
            equal (match r with Ok x -> x | _ -> 0) 3
            equal (match r2 with Error x -> x | _ -> "") "e"
        }

        Test "Map" {
            equal (r |> Result.map (fun x -> x + 1)) (Ok 4)
            equal (r2 |> Result.map (fun x -> x + 1)) (Error "e")
        }

        Test "MapError" {
            equal (r |> Result.mapError (fun x -> x + "r")) (Ok 3)
            equal (r2 |> Result.mapError (fun x -> x + "r")) (Error "er")
        }

        Test "Bind" {
            equal (r |> Result.bind (fun x -> Ok (x + 1))) (Ok 4)
            equal (r |> Result.bind (fun x -> Error "err")) (Error "err")
            equal (r2 |> Result.bind (fun x -> Ok())) (Error "e")
        }

        Test "IsOk" {
            isTrue (r |> Result.isOk)
            isFalse (r2 |> Result.isOk)
        }

        Test "IsError" {
            isFalse (r |> Result.isError)
            isTrue (r2 |> Result.isError)
        }

        Test "DefaultValue" {
            equal (r |> Result.defaultValue 4) 3
            equal (r2 |> Result.defaultValue 4) 4
        }

        Test "DefaultWith" {
            equal (r |> Result.defaultWith (fun e -> if e = "e" then 4 else 5)) 3
            equal (r2 |> Result.defaultWith (fun e -> if e = "e" then 4 else 5)) 4
        }

        Test "Count" {
            equal (r |> Result.count) 1
            equal (r2 |> Result.count) 0
        }

        Test "Fold" {
            equal (r |> Result.fold (fun s x -> 10*s + x) 1) 13
            equal (r2 |> Result.fold (fun s x -> 10*s + x) 1) 1
        }

        Test "FoldBack" {
            equal (1 |> Result.foldBack (fun x s -> 10*x + s) r) 31
            equal (1 |> Result.foldBack (fun x s -> 10*x + s) r2) 1
        }

        Test "Exists" {
            isTrue (r |> Result.exists (fun x -> x > 0))
            isFalse (r |> Result.exists (fun x -> x < 0))
            isFalse (r2 |> Result.exists (fun x -> x > 0))
        }

        Test "ForAll" {
            isTrue (r |> Result.forall (fun x -> x > 0))
            isFalse (r |> Result.forall (fun x -> x < 0))
            isTrue (r2 |> Result.forall (fun x -> x > 0))
        }

        Test "Contains" {
            isTrue (r |> Result.contains 3)
            isFalse (r |> Result.contains 4)
            isFalse (r2 |> Result.contains 3)
        }

        Test "Iterate" {
            let s = ref 1
            r |> Result.iter (fun x -> s.Value <- s.Value + x)
            equal s.Value 4
            r2 |> Result.iter (fun x -> s.Value <- s.Value + x)
            equal s.Value 4
        }

        Test "ToArray" {
            equal (r |> Result.toArray) [| 3 |]
            equal (r2 |> Result.toArray) [| |]
        }

        Test "ToList" {
            equal (r |> Result.toList) [ 3 ]
            equal (r2 |> Result.toList) [ ]
        }

        //Test "ToSeq" {
        //    equal (r |> Result.toSeq |> Seq.toArray) [| 3 |]
        //    equal (r2 |> Result.toSeq |> Seq.toArray) [| |]
        //}

        Test "ToOption" {
            equal (r |> Result.toOption) (Some 3)
            equal (r2 |> Result.toOption) None
        }

        Test "ToValueOption" {
            equal (r |> Result.toValueOption) (ValueSome 3)
            equal (r2 |> Result.toValueOption) ValueNone
        }
    }

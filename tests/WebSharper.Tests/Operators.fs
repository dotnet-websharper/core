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

module WebSharper.Tests.Operators

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
module R = WebSharper.Testing.Random

(* TODO: the coverage of the Operators module is far from complete. *)
[<JavaScript>]
let Tests =

    Section "Operators"

    Test "=" {
        null =? null
        null <>? obj ()
        obj () <>? null
    }

    Test "+" {
        1 + 2 =? 3
    }

    Test "max" {
        max 1 3 =? 3
        max "asdf" "asdg" =? "asdg"
        max (1, 2) (2, 1) =? (2, 1)
    }

    Test "min" {
        min 1 3 =? 1
        min "asdf" "asdg" =? "asdf"
        min (1, 2) (2, 1) =? (1, 2)
        Assert.For 100 (R.Tuple2Of (R.Int, R.Int)) (fun (x, y) ->
            max x y >= min x y =? true)
    }

    let add1 = ((+) 1)
    let twice = (( * ) 2)
    let add (x, y) = x + y

    Test ">>" {
        (List.length >> add1 >> twice) [1; 2] =? 6
        (id >> add) (1, 2) =? 3
    }

    Test "<<" {
        (twice << add1 << List.length) [1; 2] =? 6
        (add << id) (1, 2) =? 3
    }

    Test "|>" {
        (1, 2) |> add =? 3
        0 |> add1 |> twice |> add1 =? 3   
    }

    Test "<|" {
        add <| (1, 2)  =? 3
    }

    Test "!" {
        let r = ref 3
        !r =? 3
    }

    Test ":=" {
        let r = ref 3
        r := 4
        !r =? 4
    }

    Test "incr" {
        let r = ref 3
        incr r
        !r =? 4
    }

    Test "decr" {
        let r = ref 3
        decr r
        !r =? 2
    }

    Test "int" {
        int "3" =? 3
        int "3.5" =? 3
        int -3.5 =? - 3
    }

    Test "float" {
        float "3.5" =? 3.5
    }

    Test "|>!" {
        let a =
            ref 1
            |>! incr
        !a =? 2
    }

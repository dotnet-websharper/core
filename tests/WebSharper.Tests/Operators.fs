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
        Equal null null
        NotEqual null (obj ())
        NotEqual (obj ()) null
    }

    Test "+" {
        Equal (1 + 2) 3
    }

    Test "max" {
        Equal (max 1 3) 3
        Equal (max "asdf" "asdg") "asdg"
        Equal (max (1, 2) (2, 1)) (2, 1)
    }

    Test "min" {
        Equal (min 1 3) 1
        Equal (min "asdf" "asdg") "asdf"
        Equal (min (1, 2) (2, 1)) (1, 2)
        ForR 100 (R.Tuple2Of (R.Int, R.Int)) (fun (x, y) -> Do {
            True (max x y >= min x y)
        })
    }

    let add1 = ((+) 1)
    let twice = (( * ) 2)
    let add (x, y) = x + y

    Test ">>" {
        Equal ((List.length >> add1 >> twice) [1; 2]) 6
        Equal ((id >> add) (1, 2)) 3
    }

    Test "<<" {
        Equal ((twice << add1 << List.length) [1; 2]) 6
        Equal ((add << id) (1, 2)) 3
    }

    Test "|>" {
        Equal ((1, 2) |> add) 3
        Equal (0 |> add1 |> twice |> add1) 3   
    }

    Test "<|" {
        Equal (add <| (1, 2)) 3
    }

    Test "!" {
        let r = ref 3
        Equal !r 3
    }

    Test ":=" {
        let r = ref 3
        r := 4
        Equal !r 4
    }

    Test "incr" {
        let r = ref 3
        incr r
        Equal !r 4
    }

    Test "decr" {
        let r = ref 3
        decr r
        Equal !r 2
    }

    Test "int" {
        Equal (int "3") 3
        Equal (int "3.5") 3
        Equal (int -3.5) -3
    }

    Test "float" {
        Equal (float "3.5") 3.5
    }

    Test "|>!" {
        let a =
            ref 1
            |>! incr
        Equal !a 2
    }

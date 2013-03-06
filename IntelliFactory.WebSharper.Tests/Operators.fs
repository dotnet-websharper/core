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

module IntelliFactory.WebSharper.Tests.Operators

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
module R = IntelliFactory.WebSharper.Testing.Random

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

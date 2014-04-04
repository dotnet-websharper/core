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


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

module IntelliFactory.WebSharper.Tests.Async

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

[<Inline "IntelliFactory.WebSharper.Concurrency.scheduler().tick()">]
let tick() = ()

[<Inline "IntelliFactory.WebSharper.Concurrency.scheduler().idle">]
let isIdle() = X<bool>

[<JavaScript>]
let ( @=? ) a b =
    let res = ref None
    async {
        let! r = a
        res := Some r
    } |> Async.Start
    while not (isIdle()) do tick()
    !res |> Option.get =? b

[<JavaScript>]
let Tests =

    Section "Async"

    Test "Bind and Return" {
        async { return 1 } @=? 1 
    }

    Test "For" {
        async {
            let l = ref []
            for i in 1 .. 3 do 
                l := i :: !l
            return  !l
        } @=? [ 3; 2; 1 ]
    }

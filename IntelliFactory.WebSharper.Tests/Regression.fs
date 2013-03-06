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

module IntelliFactory.WebSharper.Tests.Regression

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

[<Inline "new Array(0)">]
let private Empty<'T> : 'T [] = X

[<JavaScript>]
let private bug431_f ((x, y) as t) =
    [| y; x |]

[<JavaScript>]
let private bug431_g () =
    let t = (1, 2)
    bug431_f t

[<JavaScript>]
let private bug35_Foo (t: 'T * 'T) x = x

[<JavaScript>]
let private bug35_Bar () = bug35_Foo ("a","b") "c"

module private Bug61_M =

    type T[<JavaScript>]() =
        [<JavaScript>]
        member this.F = 1

type private Bug61_T2 [<JavaScript>]() =
    inherit Bug61_M.T()

module internal Bug367 =

    [<JavaScript>]
    let x = 1

    module B =
        [<JavaScript>]
        let y = x

    [<JavaScript>]
    let z = B.y

module BugBB80 =

    [<Sealed>]
    type X [<JavaScript>] (k: ref<int>) =
        interface System.IDisposable with
            [<JavaScript>]
            member this.Dispose() = incr k

    [<JavaScript>]
    let test () =
        async {
            let a = ref 0
            do! async {
                    use x = new X(a)
                    return ()
                }
            return Test "Bug BB80" { !a =? 1 }
        }
        |> Async.Start


[<JavaScript>]
let Tests =
    Section "Regression"

    Test "Bug #26" {
        ([||] = Empty<int>) |? "[||] = Empty<int>"
    }

    Test "Bug #35" {
        bug35_Bar () =? "c"
    }

    Test "Bug #61" {
        let x = Bug61_T2()
        x.F =? 1
    }

    Test "Bug #367" {
        Bug367.B.y =? 1
        Bug367.z =? 1
    }

    Test "Bug #476" {
        let q = new System.Collections.Generic.Queue<int>()
        seq {
            q.Enqueue -1
            let c = ref 0
            while !c < 2 do
                q.Enqueue !c
                incr c
            q.Enqueue 2
            while !c < 4 do
                q.Enqueue !c
                incr c
            q.Enqueue 4
        }
        |> Seq.length |> ignore
        q.ToArray() =? [|-1; 0; 1; 2; 2; 3; 4|]
        let t (x: list<int>) = Seq.toArray (Seq.windowed 3 x)
        t [] =? [||]
        t [1] =? [||]
        t [1;2] =? [||]
        t [1;2;3] =? [|[|1;2;3|]|]
        t [1;2;3;4] =? [|[|1;2;3|]; [|2;3;4|]|]
        t [1;2;3;4;5] =? [|[|1;2;3|]; [|2;3;4|]; [|3;4;5|]|]
    }

    Test "Bug #431" {
        bug431_g() =? [| 2; 1 |]
    }

    Test "Bug #484" {
        string 0 =? "0"
    }

    do BugBB80.test()

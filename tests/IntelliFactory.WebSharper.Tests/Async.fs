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

type Message =
    | Increment of int
    | GetCurrent of AsyncReplyChannel<int> 
    | ScanNegative
    | LastScanned of AsyncReplyChannel<int>
    | GetHistory of AsyncReplyChannel<int list>
    | Die

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

    Test "Parallel" {
        Async.Parallel (Array.empty<Async<int>>) @=? [||]

        Async.Parallel [|
            async { return 1 }
            async { return 2 }
        |] @=? [| 1; 2 |] 
    }

    Test "TryWith" {
        let ops = ref ""
        let a = async { ops := !ops + "A" }
        let b = async { ops := !ops + "B" }
        async {
            try
                do! a
                failwith "error"
                do! a
            with _ ->
                do! b
            return !ops
        } @=? "AB"
    }

    Test "TryFinally" {
        let ops = ref ""
        let a = async { ops := !ops + "A" }
        async {
            try
                try
                    do! a
                    failwith "error"
                    do! a
                finally ops := !ops + "F"
            with _ -> ()
            return !ops
        } @=? "AF"
    }

    Test "Cancellation" {
        let ops = ref ""
        let a = async { ops := !ops + "A" }
        async {
            do! a
            Async.CancelDefaultToken()
            do! a
        } @=? JavaScript.Undefined
        !ops =? "A"
    }

    Test "MailboxProcessor" {
        let mb = 
            MailboxProcessor.Start <| fun mb ->
                let v = ref 0
                let n = ref 0
                let h = ref []
                let add i =
                    h := !v :: !h
                    v := !v + i
                let rec loop() = async {
                    let! msg = mb.Receive()
                    match msg with
                    | Increment i ->
                        add i
                    | GetCurrent chan ->
                        chan.Reply !v
                    | ScanNegative ->
                        let! j = 
                            mb.Scan (function
                                | Increment i when i < 0 -> Some (async { n := i; return -1 })
                                | _ -> None
                            )
                        add j
                    | LastScanned chan ->
                        chan.Reply !n
                    | GetHistory chan ->
                        chan.Reply !h
                    | Die -> failwith "error"
                    do! loop()
                }
                loop()
        async {
            mb.Post(Increment 1)
            return! mb.PostAndAsyncReply GetCurrent     
        } @=? 1
        async {
            mb.Post(Increment 5)
            return! mb.PostAndAsyncReply GetCurrent     
        } @=? 6
        async {
            mb.Post(ScanNegative)  
            mb.Post(Increment 3)  
            mb.Post(Increment -2)
            mb.Post(Increment 5)  
            return! mb.PostAndAsyncReply LastScanned
        } @=? -2
        mb.PostAndAsyncReply GetCurrent @=? 13
        mb.PostAndAsyncReply GetHistory @=? [ 8; 5; 6; 1; 0 ] 

        // testing Error event
        let errorCatched = ref false
        mb.Error.Add (fun _ -> errorCatched := true)
        mb.Post(Die)
        do while not (isIdle()) do tick()
        !errorCatched =? true
    }
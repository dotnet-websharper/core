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

module IntelliFactory.WebSharper.Tests.Async

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

[<Inline "IntelliFactory.WebSharper.Concurrency.scheduler().tick()">]
let tick() = ()

[<Inline "IntelliFactory.WebSharper.Concurrency.scheduler().idle">]
let isIdle() = X<bool>

[<JavaScript>]
let forceAsync() =
    while not (isIdle()) do tick()

[<JavaScript>]
let ( @=? ) a b =
    let res = ref None
    async {
        let! r = a
        res := Some r
    } |> Async.Start
    forceAsync()
    !res |> Option.get =? b

//[<JavaScript>]
//let runSyncWithCT a ct =
//    Async.Start (a, ct) 
//    while not (isIdle()) do tick()

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
        let cancelled = ref false
        let cts = new System.Threading.CancellationTokenSource()
        cts.Token.Register(fun () -> cancelled := true) |> ignore
        Async.Start (
            async {
                do! a
                cts.Cancel()
                do! a
            }, cts.Token)
        forceAsync()
        !cancelled =? true
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
        forceAsync()
        !errorCatched =? true
    }

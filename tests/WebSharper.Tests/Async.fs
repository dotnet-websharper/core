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

module WebSharper.Tests.Async

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

type Message =
    | Increment of int
    | GetCurrent of AsyncReplyChannel<int> 
    | ScanNegative
    | LastScanned of AsyncReplyChannel<int>
    | GetHistory of AsyncReplyChannel<int list>
    | Die

[<JavaScript>]
let Tests =

    TestCategory "Async" {

        Test "Bind and Return" {
            let! x = async { return 1 }
            equal x 1 
        }

        Test "For" {
            let! x = async {
                let l = ref []
                for i in 1 .. 3 do 
                    l := i :: !l
                return  !l
            }
            equal x [ 3; 2; 1 ]
        }

        Test "Parallel" {
            let! x = Async.Parallel (Array.empty<Async<int>>)
            equal x [||]

            let! x =
                Async.Parallel [|
                    async { return 1 }
                    async { return 2 }
                |]
            equal x [| 1; 2 |] 
        }

        Test "TryWith" {
            let ops = ref ""
            let a = async { ops := !ops + "A" }
            let b = async { ops := !ops + "B" }
            let! x = async {
                try
                    do! a
                    failwith "error"
                    do! a
                with _ ->
                    do! b
                return !ops
            }
            equal x "AB"
        }

        Test "TryFinally" {
            let ops = ref ""
            let a = async { ops := !ops + "A" }
            let! x = async {
                try
                    try
                        do! a
                        failwith "error"
                        do! a
                    finally ops := !ops + "F"
                with _ -> ()
                return !ops
            }
            equal x "AF"
        }

        Test "Cancellation" {
            let ops = ref ""
            let a = async { ops := !ops + "A" }
            let cancelled = ref false
            let cts = new System.Threading.CancellationTokenSource()
            cts.Token.Register(fun () -> cancelled := true) |> ignore
            Async.Start (
                Async.TryCancelled(
                    async {                    
                        try
                            do! a
                            cts.Cancel()
                            do! a
                        finally 
                            ops := !ops + "B"    
                    },
                    fun _ -> ops := !ops + "C"
                ), cts.Token)
            do! Async.Sleep 500
            equal !cancelled true
            equal !ops "ABC"
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
            let! x = async {
                mb.Post(Increment 1)
                return! mb.PostAndAsyncReply GetCurrent     
            }
            equal x 1
            let! x = async {
                mb.Post(Increment 5)
                return! mb.PostAndAsyncReply GetCurrent     
            }
            equal x 6
            let! x = async {
                mb.Post(ScanNegative)  
                mb.Post(Increment 3)  
                mb.Post(Increment -2)
                mb.Post(Increment 5)  
                return! mb.PostAndAsyncReply LastScanned
            }
            equal x -2
            let! x = mb.PostAndAsyncReply GetCurrent
            equal x 13
            let! x = mb.PostAndAsyncReply GetHistory
            equal x [ 8; 5; 6; 1; 0 ] 

//            // testing Error event
            let errorCatched = ref false
            mb.Error.Add (fun _ -> errorCatched := true)
            mb.Post(Die)
            do! Async.Sleep 500
            equal !errorCatched true
        }

        Test "AwaitEvent" {
            let e = Event<int>()
            let res = ref 0
            async {
                let! r = Async.AwaitEvent e.Publish
                res := r
            } |> Async.StartImmediate
            e.Trigger(3)
            do! Async.Sleep 500
            equal !res 3
        }

        Test "StartImmediate" {
            let x, y =
                let res = ref 0 
                async {
                    incr res 
                }
                |> Async.Start
                let x = !res
                async {
                    incr res 
                }
                |> Async.StartImmediate
                x, !res
            equal x 0
            equal y 1
        }

        Test "StartImmediateAsTask" {
            let x, y, t =
                let res = ref 0 
                async {
                    incr res 
                }
                |> Async.Start
                let x = !res
                let t =
                    async {
                        incr res 
                        return 2
                    }
                    |> Async.StartImmediateAsTask
                x, !res, t
            equal x 0
            equal y 1
            isTrue t.IsCompleted
            equal t.Result 2
        }

        Test "StartWithContinuations" {
            let x =
                let res = ref 0 
                async {
                    incr res 
                }
                |> fun a -> Async.StartWithContinuations (a, (fun _ -> incr res), ignore, ignore)
                !res
            equal x 2
        }

        Test "StartChild" { 
            let! a = Async.StartChild (async.Return 1)
            let! b = Async.StartChild (async.Return 2)
            equalAsync a 1
            equalAsync b 2
        }

        Test "StartChildAsTask" { 
            let! a = Async.StartChildAsTask (async.Return 1)
            let! b = Async.StartChildAsTask (async.Return 2)
            equalAsync (Async.AwaitTask a) 1
            equalAsync (Async.AwaitTask b) 2
        }

        Test "Match!" {
            let! x = 
                async {
                    match! async { return Some 1 } with
                    | Some x -> return x
                    | None -> return 0
                }
            equal x 1 
        }

    }

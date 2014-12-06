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

namespace IntelliFactory.WebSharper.Control

open System.Collections.Generic
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JavaScript
open System.Threading

[<Proxy(typeof<AsyncReplyChannel<_>>)>]
type private ChannelProxy<'T> =
    [<Inline "$this($res)">]
    member this.Reply(res: 'T) = X<unit>

[<Proxy(typeof<System.TimeoutException>)>]
type TimeoutExceptionProxy =
    [<Inline "new Error(\"TimeoutException\")">]
    new () = {}

[<JavaScript>]
[<Proxy(typeof<MailboxProcessor<_>>)>]
[<Name "MailboxProcessor">]
type private MailboxProcessorProxy<'T> (initial, ?token: CancellationToken) =
    let mutable started = false
    let errorEvent = Event<exn>()
    let mailbox = LinkedList<'T>()
    let mutable savedCont = None

    let startAsync a =
        Async.Start(a, ?cancellationToken = token)

    let resume() =
        match savedCont with
        | None -> ()
        | Some c ->
            savedCont <- None
            c |> startAsync

    do  match token with
        | Some ct -> 
            ct.Register(fun () -> resume()) |> ignore
        | None -> ()

    let dequeue() =
        let f = mailbox.First.Value
        mailbox.RemoveFirst()
        f

    [<Inline>]
    member this.Error = errorEvent.Publish
    [<Inline>]
    member this.add_Error handler = this.Error.AddHandler handler
    [<Inline>]
    member this.remove_Error handler = this.Error.RemoveHandler handler

    member val DefaultTimeout = -1 with get, set    

    member this.Start() =
        if started then
            failwith "The MailboxProcessor has already been started."
        else
            started <- true
            async {
                try do! initial (As<MailboxProcessor<'T>> this)
                with err -> errorEvent.Trigger err
            } |> startAsync

    static member Start(initial, ?token: CancellationToken) =
        let mb = new MailboxProcessor<'T>(initial, ?cancellationToken = token)
        mb.Start()
        mb

    [<Inline>]
    member this.Post(msg: 'T) =
        mailbox.AddLast msg |> ignore
        resume()

    member this.TryReceive(?timeout: int) =        
        let timeout = defaultArg timeout this.DefaultTimeout
        Async.FromContinuations <| fun (ok, _, _) ->
            if mailbox.First = null then
                if timeout < 0 then
                    savedCont <- Some <| async { dequeue() |> Some |> ok }
                else  
                    let waiting = ref true
                    let pending = 
                        JS.SetTimeout(fun () ->
                            if !waiting then
                                waiting := false
                                savedCont <- None
                                ok None
                        ) timeout 
                    savedCont <- Some <| async { 
                        if !waiting then do
                            waiting := false  
                            JS.ClearTimeout pending
                            dequeue() |> Some |> ok 
                    }
            else dequeue() |> Some |> ok

    member this.Receive(?timeout: int) =
        async {
            let! x = this.TryReceive(?timeout = timeout)
            return
                match x with
                | None -> raise (System.TimeoutException())
                | Some x -> x
        }
                              
    member this.CurrentQueueLength = mailbox.Count

    member this.PostAndTryAsyncReply(msgf: AsyncReplyChannel<'R> -> 'T, ?timeout: int) : Async<'R option> =
        let timeout = defaultArg timeout this.DefaultTimeout
        Async.FromContinuations <| fun (ok, _, _) ->
            if timeout < 0 then
                As (Some >> ok) |> msgf |> this.Post
            else
                let waiting = ref true
                As (fun res ->
                    if !waiting then
                        waiting := false
                        ok (Some res)
                ) |> msgf |> this.Post
                JS.SetTimeout (fun () ->
                    if !waiting then
                        waiting := false
                        ok None        
                ) timeout |> ignore               

    member this.PostAndAsyncReply(msgf: AsyncReplyChannel<'R> -> 'T, ?timeout: int) : Async<'R> =
        async {
            let! x = this.PostAndTryAsyncReply(msgf, ?timeout = timeout)
            return
                match x with
                | None -> raise (System.TimeoutException())
                | Some x -> x
        }

    member this.TryScan(scanner, ?timeout: int) =
        let timeout = defaultArg timeout this.DefaultTimeout
        async {
            let scanInbox() =
                let mutable m = mailbox.First
                let mutable found = None
                while m <> null do 
                    match scanner m.Value with
                    | None ->
                        m <- m.Next
                    | _ as a ->
                        mailbox.Remove m
                        m <- null
                        found <- a
                found
            match scanInbox() with
            | Some found -> 
                let! res = found
                return Some res
            | _ -> return! Async.FromContinuations <| fun (ok, _, _) -> 
                if timeout < 0 then
                    let rec scanNext() =
                        savedCont <- Some <| async {
                            match scanner mailbox.First.Value with
                            | None -> scanNext()
                            | Some c ->
                                mailbox.RemoveFirst()
                                let! res = c
                                ok (Some res)
                        }
                    scanNext()
                else
                    let waiting = ref true
                    let pending = 
                        JS.SetTimeout(fun () ->
                            if !waiting then
                                waiting := false
                                savedCont <- None
                                ok None
                        ) timeout
                    let rec scanNext() =
                        savedCont <- Some <| async {
                            match scanner mailbox.First.Value with
                            | None -> scanNext()
                            | Some c ->
                                mailbox.RemoveFirst()
                                let! res = c
                                if !waiting then do
                                    waiting := false
                                    JS.ClearTimeout pending
                                    ok (Some res)
                        }
                    scanNext()
        }
        
    member this.Scan(scanner, ?timeout: int) =
        async {
            let! x = this.TryScan(scanner, ?timeout = timeout)
            return
                match x with
                | None -> raise (System.TimeoutException())
                | Some x -> x
        }

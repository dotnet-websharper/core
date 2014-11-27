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

namespace IntelliFactory.WebSharper.Control

open System.Collections.Generic
open IntelliFactory.WebSharper
open System.Threading

[<JavaScript>]                                
[<Proxy(typeof<AsyncReplyChannel<_>>)>]
[<Name "AsyncReplyChannel">]
type private ChannelProxy<'T>(reply: 'T -> unit) =
    member this.Reply(res) = reply res

[<JavaScript>]
[<Proxy(typeof<MailboxProcessor<_>>)>]
[<Name "MailboxProcessor">]
type private MailboxProcessorProxy<'T> (initial, ?token: CancellationToken) =
    let mutable started = false
    let errorEvent = Event<exn>()
    let mailbox = LinkedList<'T>()
    let mutable savedCont = None
    
    let dequeue() =
        let f = mailbox.First.Value
        mailbox.RemoveFirst()
        f

    member this.Error = errorEvent.Publish

    member this.add_Error handler = this.Error.AddHandler handler
    member this.remove_Error handler = this.Error.RemoveHandler handler

    member this.Start() =
        if started then
            failwith "The MailboxProcessor has already been started."
        else
            started <- true
            async {
                try do! initial (As<MailboxProcessor<'T>> this)
                with err -> errorEvent.Trigger err
            } |> Async.Start

    static member Start(initial, ?token: CancellationToken) =
        let mb = new MailboxProcessor<'T>(initial)
        mb.Start()
        mb

    member this.Post(msg: 'T) =
        mailbox.AddLast msg |> ignore   
        match savedCont with
        | None -> ()
        | Some c ->
            savedCont <- None
            c |> Async.Start

    member this.Receive(?timeout: int) =
        Async.FromContinuations <| fun (ok, _, _) ->
            if mailbox.First = null then
                savedCont <- Some <| async { dequeue() |> ok }
            else dequeue() |> ok
              
    member this.CurrentQueueLength = mailbox.Count

    member this.PostAndAsyncReply(msgf: AsyncReplyChannel<'R> -> 'T, ?timeout: int) : Async<'R> =
        Async.FromContinuations <| fun (ok, _, _) ->
            ChannelProxy ok |> As |> msgf |> this.Post

    member this.Scan(scanner, ?timeout: int) =
        let scanInbox() =
            let mutable m = mailbox.First
            let mutable found = None
            while m <> null do 
                match scanner m.Value with
                | None ->
                    m <- m.Next
                | _ as res ->
                    mailbox.Remove m
                    m <- null
                    found <- res
            found

        let rec scanNext ok =
            savedCont <- Some <| async {
                match scanner mailbox.First.Value with
                | None -> scanNext ok
                | Some c ->
                    mailbox.RemoveFirst()
                    let! res = c
                    ok res
            }

        async {
            match scanInbox() with
            | Some res -> return! res
            | _ -> return! Async.FromContinuations <| fun (ok, _, _) -> scanNext ok
        }

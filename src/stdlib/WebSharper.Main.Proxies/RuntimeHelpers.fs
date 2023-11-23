// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

[<WebSharper.Proxy  "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers, FSharp.Core">]
module private WebSharper.RuntimeHelpersProxy

#nowarn "40"

open WebSharper.JavaScript

type IE<'T> = System.Collections.Generic.IEnumerator<'T>

[<Inline>]
let safeDispose (x: System.IDisposable) =
    if x <> null then x.Dispose()

[<Name "enumFinally">]
let EnumerateThenFinally (s: seq<'T>) (f: unit -> unit) : seq<'T> =
    Enumerable.Of <| fun () ->
        let enum = try Enumerator.Get s with e -> f(); raise e
        Enumerator.NewDisposing () (fun _ -> enum.Dispose(); f()) <| fun e ->
            if enum.MoveNext() then
                e.Current <- enum.Current
                true
            else
                false

[<Name "enumUsing">]
let EnumerateUsing<'T1,'T2,'T3 when 'T1 :> System.IDisposable
                                and 'T2 :> seq<'T3>>
        (x: 'T1) (f: 'T1 -> 'T2) : seq<'T3> =

    Enumerable.Of <| fun () ->
        let enum = try Enumerator.Get (f x) with e -> x.Dispose(); raise e
        Enumerator.NewDisposing () (fun _ -> enum.Dispose(); x.Dispose()) <| fun e ->
            if enum.MoveNext() then
                e.Current <- enum.Current
                true
            else
                false

[<Name "enumWhile">]
let EnumerateWhile (f: unit -> bool) (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let rec next (en: Enumerator.T<IE<_>,'T>) =
            match en.State with
            | null ->
                if f () then
                    en.State <- Enumerator.Get s
                    next en
                else
                    false
            | e ->
                if e.MoveNext() then
                    en.Current <- e.Current
                    true
                else
                    e.Dispose()
                    en.State <- null
                    next en
        Enumerator.NewDisposing null (fun en -> safeDispose en.State) next)

[<Name "createEvent">]
let CreateEvent<'D, 'A when 'D : delegate<'A, unit> and 'D :> System.Delegate> 
        (add: 'D -> unit) 
        (remove: 'D -> unit)
        (create: (obj -> 'A -> unit) -> 'D) : IEvent<'D, 'A> =
    { new IEvent<'D, 'A> with
        member this.AddHandler h = add h
        member this.RemoveHandler h = remove h
        member this.Subscribe (r: System.IObserver<'A>) =     
            let h = create (fun _ args -> r.OnNext(args))
            add h
            { new System.IDisposable with member this.Dispose() = remove h }
    }

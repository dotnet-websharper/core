// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

module internal IntelliFactory.WebSharper.Control.Observable

open System
open IntelliFactory.WebSharper

type private Observable<'T> =
    internal {
        Subscribe : IObserver<'T> -> IDisposable
    }

    interface IObservable<'T> with

        [<JavaScript>]
        member this.Subscribe observer =
            this.Subscribe observer

[<JavaScript>]
let New f : IObservable<_> = upcast { Subscribe = f }

[<JavaScript>]
let Of (f: ('T -> unit) -> (unit -> unit)) : IObservable<_> =
    New (fun o -> Disposable.Of (f (fun x -> o.OnNext x)))


[<JavaScript>]
let Return<'T> (x: 'T) : IObservable<'T> =
    let f (o : IObserver<'T>) =
        o.OnNext x
        o.OnCompleted ()
        Disposable.Of ignore
    New f

[<JavaScript>]
let Never<'T> () : IObservable<'T> =
    New(fun _ ->
        Disposable.Of ignore
    )

[<JavaScript>]
let Map  (f: 'T -> 'U) (io: IObservable<'T>) : IObservable<'U> =
    New <| fun o1 ->
        let on v = o1.OnNext (f v)
        io.Subscribe <| Observer.New(on, o1.OnError, o1.OnCompleted)


[<JavaScript>]
let Filter (f: 'T -> bool) (io: IObservable<'T>) : IObservable<'T> =
    New <| fun o1 ->
        let on v = if f v then o1.OnNext v
        io.Subscribe <|  Observer.New(on, o1.OnError, o1.OnCompleted)

[<JavaScript>]
let Choose  (f: 'T -> option<'U>) (io: IObservable<'T>) : IObservable<'U> =
    New <| fun o1 ->
        let on v =
            match f v with
            | Some v    -> o1.OnNext v
            | None      -> ()
        io.Subscribe <| Observer.New(on, o1.OnError, o1.OnCompleted)

[<JavaScript>]
let Drop count (io: IObservable<'T>) : IObservable<'T> =
    New <| fun o1 ->
        let index = ref 0
        let on v =
            incr index
            if index.Value > count then
                o1.OnNext v
        io.Subscribe <| Observer.New(on, o1.OnError, o1.OnCompleted)

[<JavaScript>]
let Merge (io1: IObservable<'T>) (io2: IObservable<'T>) : IObservable<'T> =
    New <| fun o ->
        let completed1 = ref false
        let completed2 = ref false
        let disp1 =
            Observer.New(o.OnNext, ignore, fun () ->
                completed1 := true
                if completed1.Value && completed2.Value then
                    o.OnCompleted ()
            )
            |> io1.Subscribe
        let disp2 =
            Observer.New(o.OnNext, ignore, fun () ->
                completed2 := true
                if completed1.Value && completed2.Value then
                    o.OnCompleted ()
            )
            |> io2.Subscribe
        Disposable.Of (fun () -> disp1.Dispose(); disp2.Dispose())

[<JavaScript>]
let Concat (io1: IObservable<'T>) (io2: IObservable<'T>) =
    New <| fun o ->
        let innerDisp = ref None
        let outerDisp =
            io1.Subscribe(
                Observer.New(o.OnNext, ignore, fun () ->
                    innerDisp := Some <| io2.Subscribe(o)
                )
            )
        Disposable.Of <| fun () ->
            if innerDisp.Value.IsSome then
                innerDisp.Value.Value.Dispose ()
            outerDisp.Dispose()


[<JavaScript>]
let Range (start: int , count: int) : IObservable<int> =
    New <| fun o ->
        for i = start to start + count do
            o.OnNext i
        Disposable.Of ignore


[<JavaScript>]
let CombineLatest   (io1 : IObservable<'T>)
                    (io2: IObservable<'U>)
                    (f: 'T -> 'U -> 'S) : IObservable<'S> =
    New <| fun o ->
        let lv1 = ref None
        let lv2 = ref None
        let update () =
            match !lv1, !lv2 with
            | Some v1, Some v2  ->
                o.OnNext (f v1 v2)
            | _                 ->
                ()
        let o1 =
            let onNext x =
                lv1 := Some x
                update ()
            Observer.New(onNext, ignore, ignore)
        let o2 =
            let onNext y =
                lv2 := Some y
                update ()
            Observer.New(onNext, ignore, ignore)
        let d1 = io1.Subscribe(o1)
        let d2 = io2.Subscribe(o2)
        Disposable.Of (fun () -> d1.Dispose() ; d2.Dispose())

[<JavaScript>]
let Switch (io: IObservable<IObservable<'T>>) : IObservable<'T> =
    New (fun o ->
        let disp =
            let index = ref 0
            let disp : option<IDisposable> ref = ref None

            // Subscribe to outer stream
            io.Subscribe(fun (o1 : IObservable<'T>) ->
                // Update counter
                incr index

                // Dispose old observer
                if disp.Value.IsSome then
                    disp.Value.Value.Dispose ()

                let currentIndex = index.Value
                let d =
                    o1.Subscribe (fun v ->
                        if currentIndex = index.Value then
                            o.OnNext(v)
                    ) |> Some
                // Update dispose
                disp := d
            )
        disp
    )

[<JavaScript>]
let SelectMany (io: IObservable<IObservable<'T>>) : IObservable<'T> =
    New (fun o ->
        let disp = ref ignore
        // Subscribe to outer stream
        let d =
            io.Subscribe(fun (o1 : IObservable<'T>) ->
                // Subscribe to inner stream
                let d =
                    o1.Subscribe (fun v ->
                        o.OnNext v
                    )
                disp := fun () ->
                    disp.Value ()
                    d.Dispose ()
            )
        Disposable.Of (fun () ->
            disp.Value ()
            d.Dispose ()
        )
    )

[<JavaScript>]
let Aggregate (io: IObservable<'T>) (seed: 'S) (acc: 'S -> 'T -> 'S) =
    New (fun o ->
        let state = ref seed
        io.Subscribe(fun value ->
            state := acc state.Value value
            o.OnNext(state.Value)
        )
    )

//    [<JavaScript>]
//    let CollectLatest (outer: IObservable<IObservable<'T>>) : IObservable<seq<'T>> =
//        New (fun o ->
//            let dict = System.Collections.Generic.Dictionary<int, 'T>()
//            let index = ref 0
//
//            // Outer subscription
//            outer.Subscribe(fun (inner: IObservable<'T>) ->
//                incr index
//                let currentIndex = index.Value
//
//                // Inner subscription
//                inner.Subscribe(fun value ->
//                    dict.[currentIndex] <- value
//                    seq {for pair in dict do yield pair.Value}
//                    |> o.OnNext
//                )|> ignore
//            )
//        )

[<JavaScript>]
let Sequence (ios: seq<IObservable<'T>>) : IObservable<List<'T>> =
    let rec sequence (ios: list<IObservable<'T>>) =
        match ios with
        | []        ->
            Return []
        | x::xs  ->
            let rest = sequence xs
            CombineLatest x rest  (fun x y -> x :: y)
    sequence (List.ofSeq ios)


[<Inline>]
[<JavaScript>]
let Heat (io: IObservable<'T>) : IObservable<'T> =
    let formStream = HotStream.HotStream<_>.New()
    let disp =
        io.Subscribe formStream.Trigger
    formStream :> IObservable<'T>

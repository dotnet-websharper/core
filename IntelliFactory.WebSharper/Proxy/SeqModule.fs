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

[<IntelliFactory.WebSharper.Core.Attributes.Name "Seq">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Collections.SeqModule, \
     FSharp.Core, Version=2.0.0.0, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.SeqModuleProxy

module J = IntelliFactory.WebSharper.JavaScript

[<JavaScript>]
let private insufficient () =
    failwith "The input sequence has an insufficient number of elements."

[<JavaScript>]
[<Name "append">]
let Append (s1: seq<'T>) (s2: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let e1 = Enumerator.Get s1
        Enumerator.New e1 (fun x ->
            if x.State.MoveNext() then
                x.Current <- x.State.Current
                true
            elif x.State ===. e1 then
                let e2 = s2.GetEnumerator()
                x.State <- e2
                if e2.MoveNext() then
                    x.Current <- e2.Current
                    true
                else
                    false
            else
                false))

[<JavaScript>]
[<Name "average">]
let Average<'T> (s: seq<'T>) : 'T =
    let (count, sum) =
        Seq.fold
            (fun (n, s) x -> (n + 1, s + As<float> x))
            (0, 0.)
            s
    As<'T> (sum / As<float> count)

[<JavaScript>]
[<Name "averageBy">]
let AverageBy<'T,'U> (f: 'T -> 'U) (s: seq<'T>) : 'U =
    let (count, sum) =
        Seq.fold
            (fun (n, s) x -> (n + 1, s + As<float> (f x)))
            (0, 0.)
            s
    As<'U> (sum / As<float> count)

[<JavaScript>]
[<Name "cache">]
let Cache<'T> (s: seq<'T>) : seq<'T> =
    let cache = new System.Collections.Generic.Queue<'T>()
    let enum  = Enumerator.Get s
    Enumerable.Of <| fun () ->
        let next (e: Enumerator.T<_,_>) =
            if e.State + 1 < cache.Count then
                e.State   <- e.State + 1
                e.Current <- (?) cache (As e.State)
                true
            else
                if enum.MoveNext() then
                    e.State   <- e.State + 1
                    e.Current <- enum.Current
                    cache.Enqueue e.Current
                    true
                else
                    false
        Enumerator.New 0 next

/// IEnumerable is not supported.
[<Inline "$i">]
[<Name "cast">]
let Cast<'T> (i: System.Collections.IEnumerable) = X<seq<'T>>

[<JavaScript>]
[<Name "choose">]
let Choose (f: 'T -> option<'U>) (s: seq<'T>) : seq<'U> =
    s
    |> Seq.collect (fun x ->
        match f x with
        | Some v -> [v]
        | None   -> [])

[<JavaScript>]
[<Name "collect">]
let Collect f s = Seq.concat (Seq.map f s)

[<JavaScript>]
[<Name "compareWith">]
let CompareWith  (f: 'T -> 'T -> int) (s1: seq<'T>) (s2: seq<'T>) : int =
    use e1 = Enumerator.Get s1
    use e2 = Enumerator.Get s2
    let mutable r = 0
    let mutable loop = true
    while loop && r = 0 do
        match e1.MoveNext(), e2.MoveNext() with
        | true, false ->
            r <- 1
        | false, true ->
            r <- -1
        | false, false ->
            loop <- false
        | true, true ->
            r <- f e1.Current e2.Current
    r

[<JavaScript>]
[<Name "concat">]
let Concat ss = Seq.fold Seq.append Seq.empty ss

[<JavaScript>]
[<Name "countBy">]
let CountBy (f: 'T -> 'K) (s: seq<'T>) : seq<'K * int> =
    Seq.delay <| fun () ->
        let d = new obj()
        use e = Enumerator.Get s
        let keys = System.Collections.Generic.Queue<_>()
        while e.MoveNext() do
            let k = f e.Current
            let h = As<string> (Unchecked.hash k)
            if J.HasOwnProperty d (As h) then
                (?<-) d h ((?) d h + 1)
            else
                keys.Enqueue k
                (?<-) d h 1
        keys.ToArray()
        |> Array.map (fun k -> (k, (?) d (As (hash k))))
        |> As<_>

[<JavaScript>]
[<Name "delay">]
let Delay<'T> (f: unit -> seq<'T>) : seq<'T> =
    Enumerable.Of (fun () -> Enumerator.Get(f()))

[<JavaScript>]
[<Name "distinct">]
let Distinct<'T when 'T : equality> (s: seq<'T>) : seq<'T> =
    Seq.distinctBy id s

[<JavaScript>]
[<Name "distinctBy">]
let DistinctBy<'T,'K when 'K : equality>
        (f: 'T -> 'K) (s: seq<'T>) : seq<'T> =
    Enumerable.Of <| fun () ->
        let enum        = Enumerator.Get s
        let seen        = obj ()
        Enumerator.New () <| fun e ->
            if enum.MoveNext() then
                let mutable cur = enum.Current
                let h c         = As<string> (hash (f c))
                let check c     = J.HasOwnProperty seen (h c)
                let mutable has = check cur
                while has && enum.MoveNext() do
                    cur <- enum.Current
                    has <- check cur
                if has then
                    false
                else
                    (?<-) seen (h cur) ()
                    e.Current <- cur
                    true
            else
                false

[<JavaScript>]
[<Name "empty">]
let Empty<'T> : seq<'T> = [||] :> _

[<JavaScript>]
[<Name "exists">]
let Exists p (s: seq<_>) =
    use e = Enumerator.Get s
    let mutable r = false
    while not r && e.MoveNext() do
        r <- p e.Current
    r

[<JavaScript>]
[<Name "exists2">]
let Exists2 p (s1: seq<_>) (s2: seq<_>) =
    use e1 = Enumerator.Get s1
    use e2 = Enumerator.Get s2
    let mutable r = false
    while not r && e1.MoveNext() && e2.MoveNext() do
        r <- p e1.Current e2.Current
    r

[<JavaScript>]
[<Name "filter">]
let Filter (f: 'T -> bool) (s: seq<'T>) =
    Enumerable.Of <| fun () ->
        let enum = Enumerator.Get s
        Enumerator.New () <| fun e ->
            let mutable loop = enum.MoveNext()
            let mutable c    = enum.Current
            let mutable res  = false
            while loop do
                if f c then
                    e.Current <- c
                    res       <- true
                    loop      <- false
                else
                    if enum.MoveNext() then
                        c <- enum.Current
                    else
                        loop <- false
            res

[<JavaScript>]
[<Name "find">]
let Find p (s: seq<_>) =
    match Seq.tryFind p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<JavaScript>]
[<Name "findIndex">]
let FindIndex p (s: seq<_>) =
    match Seq.tryFindIndex p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<JavaScript>]
[<Name "fold">]
let Fold<'T,'S> (f: 'S -> 'T -> 'S) (x: 'S) (s: seq<'T>) : 'S =
    let mutable r = x
    use e = Enumerator.Get s
    while e.MoveNext() do
        r <- f r e.Current
    r

[<JavaScript>]
[<Name "forall">]
let ForAll p s =
    not (Seq.exists (fun x -> not (p x)) s)

[<JavaScript>]
[<Name "forall2">]
let ForAll2 p s1 s2 =
    not (Seq.exists2 (fun x y -> not (p x y)) s1 s2)

[<Inline "$x.push($y)">]
let push (x: obj) (y: obj) = ()

[<JavaScript>]
[<Name "groupBy">]
let GroupBy (f: 'T -> 'K when 'K : equality)
            (s: seq<'T>) : seq<'K * seq<'T>> =
    Seq.delay (fun () ->
        let d  = obj ()
        let d1 = obj ()
        let keys : obj [] = [||]
        use e = Enumerator.Get s
        while e.MoveNext() do
            let c = e.Current
            let k = f c
            let h = As<string> (hash k)
            if not (J.HasOwnProperty d h) then
                push keys k
            (?<-) d1 h k
            if J.HasOwnProperty d h then
                push ((?) d h) c
            else
                (?<-) d h [| c |]
        As<_> (Array.map (fun k -> (k, (?) d (As (hash k)))) keys))

[<JavaScript>]
[<Name "head">]
let Head (s: seq<'T>) : 'T =
    use e = Enumerator.Get s
    if e.MoveNext() then e.Current else insufficient()

[<JavaScript>]
[<Name "init">]
let Initialize (n: int) (f: int -> 'T) : seq<'T> =
    Seq.take n (Seq.initInfinite f)

[<JavaScript>]
[<Name "initInfinite">]
let InitializeInfinite (f: int -> 'T) : seq<'T> =
    Enumerable.Of <| fun () ->
        Enumerator.New 0 <| fun e ->
            e.Current <- f e.State
            e.State   <- e.State + 1
            true

[<JavaScript>]
[<Name "isEmpty">]
let IsEmpty (s: seq<'T>) : bool =
    use e = Enumerator.Get s
    not (e.MoveNext())

[<JavaScript>]
[<Name "iter">]
let Iterate p (s: seq<_>) =
    Seq.iteri (fun _ x -> p x) s

[<JavaScript>]
[<Name "iter2">]
let Iterate2 p (s1: seq<_>) (s2: seq<_>) =
    use e1 = Enumerator.Get s1
    use e2 = Enumerator.Get s2
    while e1.MoveNext() && e2.MoveNext() do
        p e1.Current e2.Current

[<JavaScript>]
[<Name "iteri">]
let IterateIndexed p (s: seq<_>) =
    let mutable i = 0
    use e = Enumerator.Get s
    while e.MoveNext() do
        p i e.Current
        i <- i + 1

[<JavaScript>]
[<Name "length">]
let Length (s: seq<_>) =
    let mutable i = 0
    use e = Enumerator.Get s
    while e.MoveNext() do
        i <- i + 1
    i

[<JavaScript>]
[<Name "map">]
let Map (f: 'T -> 'U) (s: seq<'T>) : seq<'U> =
    Enumerable.Of <| fun () ->
        let en = Enumerator.Get s
        Enumerator.New () <| fun e ->
            if en.MoveNext() then
                e.Current <- f en.Current
                true
            else
                false

[<JavaScript>]
[<Name "mapi">]
let MapIndexed (f: int -> 'T -> 'U) (s: seq<'T>) : seq<'U> =
    Seq.map2 f (Seq.initInfinite id) s

[<JavaScript>]
[<Name "mapi2">]
let Map2 (f: 'T -> 'U -> 'V) (s1: seq<'T>) (s2: seq<'U>) : seq<'V> =
    Enumerable.Of <| fun () ->
        let e1 = Enumerator.Get s1
        let e2 = Enumerator.Get s2
        Enumerator.New () <| fun e ->
            if e1.MoveNext() && e2.MoveNext() then
                e.Current <- f e1.Current e2.Current
                true
            else
                false

[<JavaScript>]
[<Name "maxBy">]
let MaxBy (f: 'T -> 'U) (s: seq<'T>) : 'T =
    Seq.reduce (fun x y -> if f x >= f y then x else y) s

[<JavaScript>]
[<Name "minBy">]
let MinBy (f: 'T -> 'U) (s: seq<'T>) : 'T =
    Seq.reduce (fun x y -> if f x <= f y then x else y) s

[<JavaScript>]
[<Name "max">]
let Max (s: seq<'T>) : 'T =
    Seq.reduce (fun x y -> if x >= y then x else y) s

[<JavaScript>]
[<Name "min">]
let Min (s: seq<'T>) : 'T =
    Seq.reduce (fun x y -> if x <= y then x else y) s

[<JavaScript>]
[<Name "nth">]
let Get index (s: seq<'T>) =
    if index < 0 then
        failwith "negative index requested"
    let mutable pos = -1
    use e = Enumerator.Get s
    while pos < index do
        if not (e.MoveNext()) then
            insufficient ()
        pos <- pos + 1
    e.Current

[<Inline "$a">]
[<Name "ofArray">]
let OfArray (a: 'T[]) = X<seq<'T>>

[<Inline "$l">]
[<Name "ofList">]
let OfList (l: list<'T>) = X<seq<'T>>

[<JavaScript>]
[<Name "pairwise">]
let Pairwise (s: seq<'T>) : seq<'T * 'T> =
    Seq.windowed 2 s
    |> Seq.map (fun x -> (x.[0], x.[1]))

[<JavaScript>]
[<Name "pick">]
let Pick p (s: seq<_>) =
    match Seq.tryPick p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<JavaScript>]
[<Name "readOnly">]
let ReadOnly (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () -> Enumerator.Get s)

[<JavaScript>]
[<Name "reduce">]
let Reduce (f: 'T -> 'T -> 'T) (source: seq<'T>) : 'T =
    use e = Enumerator.Get source
    if not (e.MoveNext()) then
        failwith "The input sequence was empty"
    let mutable r = e.Current
    while e.MoveNext() do
        r <- f r e.Current
    r

[<JavaScript>]
[<Name "scan">]
let Scan<'T,'S> (f: 'S -> 'T -> 'S) (x: 'S) (s: seq<'T>) : seq<'S> =
    Enumerable.Of <| fun () ->
        let en = Enumerator.Get s
        Enumerator.New false <| fun e ->
            if e.State then
                if en.MoveNext() then
                    e.Current <- f e.Current en.Current
                    true
                else
                    false
            else
                e.Current <- x
                e.State <- true
                true

[<Inline "[$x]">]
[<Name "singleton">]
let Singleton<'T> (x: 'T) = X<seq<'T>>

[<JavaScript>]
[<Name "skip">]
let Skip (n: int) (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let e = Enumerator.Get s
        for i = 1 to n do
            if not (e.MoveNext()) then
                insufficient ()
        e)

[<JavaScript>]
[<Name "skipWhile">]
let SkipWhile (f: 'T -> bool) (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let e = Enumerator.Get s
        let mutable empty = true
        while e.MoveNext() && f e.Current do
            empty <- false
        if empty then
            Seq.empty.GetEnumerator()
        else
            Enumerator.New true (fun x ->
                if x.State then
                    x.State <- false
                    x.Current <- e.Current
                    true
                else
                    let r = e.MoveNext()
                    x.Current <- e.Current
                    r))

[<JavaScript>]
[<Name "sort">]
let Sort<'T when 'T : comparison> (s: seq<'T>) =
    Seq.sortBy id s

[<JavaScript>]
[<Name "sortBy">]
let SortBy<'T, 'U when 'U: comparison>
        (f: 'T -> 'U) (s: seq<'T>) : seq<'T> =
    Seq.delay (fun () ->
        let array = Array.ofSeq s
        Array.sortInPlaceBy f array
        array :> _)

[<JavaScript>]
[<Name "sum">]
let Sum<'T> (s: seq<'T>) : 'T =
    box (Seq.fold (fun s x -> s + (box x :?> _)) 0. s) :?> _

[<JavaScript>]
[<Name "sumBy">]
let SumBy<'T,'U> (f: 'T -> 'U) (s: seq<'T>) : 'U =
    box (Seq.fold (fun s x -> s + (box (f x) :?> _)) 0. s) :?> _

[<JavaScript>]
[<Name "take">]
let Take (n: int) (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let e = Enumerator.Get s
        Enumerator.New 0 (fun enum ->
            if enum.State >= n then
                false
            else
                if e.MoveNext() then
                    enum.State <- enum.State + 1
                    enum.Current <- e.Current
                    true
                else
                    e.Dispose()
                    enum.State <- n
                    false
        )
    )

[<JavaScript>]
[<Name "takeWhile">]
let TakeWhile (f: 'T -> bool) (s: seq<'T>) : seq<'T> =
    seq {
        use e = Enumerator.Get s
        while e.MoveNext() && f e.Current do
            yield e.Current
    }

[<JavaScript>]
[<Name "toArray">]
let ToArray (s: seq<'T>) =
    let q = new System.Collections.Generic.Queue<'T>()
    for e in s do q.Enqueue e
    q.ToArray()

[<JavaScript>]
[<Name "toList">]
let ToList (s: seq<'T>) = List.ofSeq s

[<JavaScript>]
[<Name "truncate">]
let Truncate (n: int) (s: seq<'T>) : seq<'T> =
    seq {
        use e = Enumerator.Get s
        let i = ref 0
        while e.MoveNext() && !i < n do
            incr i
            yield e.Current
    }

[<JavaScript>]
[<Name "tryFind">]
let TryFind ok (s: seq<_>) =
    use e = Enumerator.Get s
    let mutable r = None
    while r.IsNone && e.MoveNext() do
        let x = e.Current
        if ok x then
            r <- Some x
    r

[<JavaScript>]
[<Name "tryFindIndex">]
let TryFindIndex ok (s: seq<_>) =
    use e = Enumerator.Get s
    let mutable loop = true
    let mutable i = 0
    while loop && e.MoveNext() do
        let x = e.Current
        if ok x then
            loop <- false
        else
            i <- i + 1
    if loop then None else Some i

[<JavaScript>]
[<Name "tryPick">]
let TryPick f (s: seq<_>) =
    use e = Enumerator.Get s
    let mutable r = None
    while r = None && e.MoveNext() do
        r <- f e.Current
    r

[<JavaScript>]
[<Name "unfold">]
let Unfold (f: 'S -> option<'T * 'S>) (s: 'S) : seq<'T> =
    Enumerable.Of <| fun () ->
        Enumerator.New s <| fun e ->
            match f e.State with
            | Some (t, s) ->
                e.Current <- t
                e.State  <- s
                true
            | None ->
                false

[<JavaScript>]
[<Name "windowed">]
let Windowed (windowSize: int) (s: seq<'T>) : seq<'T []> =
    if windowSize <= 0 then
        failwith "The input must be non-negative."
    seq {
        use e = Enumerator.Get s
        let q = new System.Collections.Generic.Queue<'T>()
        while q.Count < windowSize && e.MoveNext() do
            q.Enqueue e.Current
        if q.Count = windowSize then
            yield q.ToArray()
            while e.MoveNext() do
                ignore (q.Dequeue())
                q.Enqueue e.Current
                yield q.ToArray()
    }

[<JavaScript>]
[<Name "zip">]
let Zip (s1: seq<'T>) (s2: seq<'U>) =
    Seq.map2 (fun x y -> x, y) s1 s2

[<JavaScript>]
[<Name "zip3">]
let Zip3 (s1: seq<'T>) (s2: seq<'U>) (s3: seq<'V>) : seq<'T * 'U * 'V> =
    Seq.map2 (fun x (y, z) -> (x, y, z)) s1 (Seq.zip s2 s3)

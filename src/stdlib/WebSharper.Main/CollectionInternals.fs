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

/// Provides seq/list/array proxies
[<JavaScript>]
module internal WebSharper.CollectionInternals

open WebSharper.JavaScript

[<Name "WebSharper.Arrays.splitInto">]
let ArraySplitInto count (arr: 'T[]) =
    if count <= 0 then failwith "Count must be positive"
    let len = arr.Length
    if len = 0 then
        [| |]
    else
        let count = min count len
        let res = Array.zeroCreate count : 'T[][]
        let minChunkSize = len / count
        let mutable startIndex = 0
        for i = 0 to len % count - 1 do
            res.[i] <- Array.sub arr startIndex (minChunkSize + 1)
            startIndex <- startIndex + minChunkSize + 1
        for i = len % count to count - 1 do
            res.[i] <-  Array.sub arr startIndex minChunkSize
            startIndex <- startIndex + minChunkSize
        res

[<Name "WebSharper.Arrays.contains">]
let ArrayContains (item: 'T) (arr: 'T[])  =
    let mutable c = true
    let mutable i = 0
    let l = arr.Length
    while c && i < l do
        if arr.[i] = item then
            c <- false
        else
            i <- i + 1
    not c

[<Name "WebSharper.Arrays.tryFindBack">]
let ArrayTryFindBack f (arr: _ []) =
    let mutable res = None
    let mutable i = Array.length arr - 1
    while i > 0 && Option.isNone res do
        if f arr.[i] then res <- Some arr.[i]
        i <- i - 1
    res

[<Name "WebSharper.Arrays.tryFindIndexBack">]
let ArrayTryFindIndexBack f (arr: _ []) =
    let mutable res = None
    let mutable i = Array.length arr - 1
    while i > 0 && Option.isNone res do
        if f arr.[i] then res <- Some i
        i <- i - 1
    res

[<Name "WebSharper.Arrays.mapFold">]
let ArrayMapFold<'T, 'S, 'R> (f: 'S -> 'T -> 'R * 'S) (zero: 'S) (arr: 'T[]) : 'R[] * 'S =
    let r = JavaScript.Array(Array.length arr)
    let mutable acc = zero
    for i = 0 to Array.length arr - 1 do
        let a, b = f acc arr.[i]
        r.[i] <- a
        acc <- b 
    r.Self, acc

[<Name "WebSharper.Arrays.mapFoldBack">]
let ArrayMapFoldBack<'T,'S,'R> (f: 'T -> 'S -> 'R * 'S) (arr: 'T[]) (zero: 'S) : 'R[] * 'S =
    let r = JavaScript.Array<'R>(Array.length arr)
    let mutable acc = zero
    let len = Array.length arr
    for j = 1 to len do
        let i = len - j
        let a, b = f arr.[i] acc
        r.[i] <- a
        acc <- b 
    r.Self, acc

[<Name "WebSharper.Arrays.mapInPlace">]
let mapInPlace (f: 'T1 -> 'T2) (arr: 'T1 []) =
    for i = 0 to Array.length arr - 1 do
        arr.JS.[i] <- As (f arr.JS.[i])

[<Name "WebSharper.Arrays.mapiInPlace">]
let mapiInPlace (f: int -> 'T1 -> 'T2) (arr: 'T1 []) : 'T2[] =
    for i = 0 to Array.length arr - 1 do
        arr.JS.[i] <- As (f i arr.JS.[i])
    As arr

[<Name "WebSharper.Arrays.sortInPlaceByDescending">]
let ArraySortInPlaceByDescending<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) =
    (mapiInPlace (fun i x -> x, (f x, i)) arr).JS.Sort(fun (x, y) -> - compare (snd x) (snd y)) |> mapInPlace fst 

[<Name "WebSharper.Seq.tryHead">]
let SeqTryHead (s: seq<'T>) =
    use e = Enumerator.Get s
    if e.MoveNext() then Some e.Current else None

[<Name "WebSharper.Seq.tryItem">]
let SeqTryItem i (s: seq<'T>) =
    if i < 0 then None else
    let mutable j = 0
    use e = Enumerator.Get s
    let mutable go = true
    while go && j <= i do
        if e.MoveNext() then
            j <- j + 1
        else
            go <- false
    if go then Some e.Current else None

[<Name "WebSharper.Seq.tryLast">]
let SeqTryLast (s: seq<'T>) =
    use e = Enumerator.Get s
    if e.MoveNext() then 
        while e.MoveNext() do ()
        Some e.Current 
    else None

[<Name "WebSharper.Seq.chunkBySize">]
let SeqChunkBySize (size: int) (s: seq<'T>) =
    if size <= 0 then failwith "Chunk size must be positive"
    Enumerable.Of <| fun () ->
        let o = Enumerator.Get s
        Enumerator.NewDisposing () (fun _ -> o.Dispose()) <| fun e ->
            if o.MoveNext() then
                let res = [|o.Current|]
                while res.Length < size && o.MoveNext() do 
                    res.JS.Push o.Current |> ignore
                e.Current <- res
                true
            else false

[<Name "WebSharper.Seq.compareWith">]
let SeqCompareWith  (f: 'T -> 'T -> int) (s1: seq<'T>) (s2: seq<'T>) : int =
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

[<Name "WebSharper.Seq.countBy">]
let SeqCountBy (f: 'T -> 'K) (s: seq<'T>) : seq<'K * int> =
    Seq.delay <| fun () ->
        let d = new obj()
        use e = Enumerator.Get s
        let keys = System.Collections.Generic.Queue<_>()
        while e.MoveNext() do
            let k = f e.Current
            let h = As<string> (Unchecked.hash k)
            if JS.HasOwnProperty d (As h) then
                (?<-) d h ((?) d h + 1)
            else
                keys.Enqueue k
                (?<-) d h 1
        keys.ToArray()
        |> Array.map (fun k -> (k, (?) d (As (hash k))))
        |> As<_>

[<Name "WebSharper.Seq.distinct">]
let SeqDistinct<'T when 'T : equality> (s: seq<'T>) : seq<'T> =
    Seq.distinctBy id s

[<Name "WebSharper.Seq.distinctBy">]
let SeqDistinctBy<'T,'K when 'K : equality>
        (f: 'T -> 'K) (s: seq<'T>) : seq<'T> =
    Enumerable.Of <| fun () ->
        let o        = Enumerator.Get s
        let seen        = Array<Array<'K>>()
        let add c =
            let k = f c
            let h = hash k
            let cont = seen.[h]
            if cont = JS.Undefined then
                seen.[h] <- [|k|].JS
                true
            else
                if ArrayContains k (As cont) then
                    false
                else
                    cont.Push(k) |> ignore
                    true         
        Enumerator.NewDisposing () (fun _ -> o.Dispose()) <| fun e ->
            if o.MoveNext() then
                let mutable cur = o.Current
                let mutable has = add cur
                while not has && o.MoveNext() do
                    cur <- o.Current
                    has <- add cur
                if has then
                    e.Current <- cur
                    true
                else
                    false
            else
                false

[<Name "WebSharper.Seq.except">]
let SeqExcept (itemsToExclude: seq<'T>) (s: seq<'T>) =
    Enumerable.Of <| fun () ->
        let o        = Enumerator.Get s
        let seen        = Array<Array<'T>>()
        let add c =
            let h = hash c
            let cont = seen.[h]
            if cont = JS.Undefined then
                seen.[h] <- [|c|].JS
                true
            else
                if ArrayContains c (As cont) then
                    false
                else
                    cont.Push(c) |> ignore
                    true         
        for i in itemsToExclude do
            add i |> ignore
        Enumerator.NewDisposing () (fun _ -> o.Dispose()) <| fun e ->
            if o.MoveNext() then
                let mutable cur = o.Current
                let mutable has = add cur
                while not has && o.MoveNext() do
                    cur <- o.Current
                    has <- add cur
                if has then
                    e.Current <- cur
                    true
                else
                    false
            else
                false

[<Name "WebSharper.List.skip">]
let ListSkip i (l : list<'T>) =
    let mutable res = l
    for j = 1 to i do
        match res with 
        | _ :: t ->
            res <- t
        | [] -> failwith "Input list too short."
    res

[<Inline "$x.push($y)">]
let arrayPush (x: obj) (y: obj) = ()

[<Name "WebSharper.Seq.groupBy">]
let SeqGroupBy (f: 'T -> 'K when 'K : equality)
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
            if not (JS.HasOwnProperty d h) then
                arrayPush keys k
            (?<-) d1 h k
            if JS.HasOwnProperty d h then
                arrayPush ((?) d h) c
            else
                (?<-) d h [| c |]
        As<_> (Array.map (fun k -> (k, (?) d (As (hash k)))) keys))

[<Name "WebSharper.Seq.insufficient">]
let InsufficientElements() =
    failwith "The input sequence has an insufficient number of elements."

[<Name "WebSharper.Seq.last">]
let SeqLast (s: seq<_>) =
    use e = Enumerator.Get s
    if not <| e.MoveNext() then InsufficientElements()
    else 
        while e.MoveNext() do ()
        e.Current

[<Name "WebSharper.List.map3">]
let ListMap3 f (l1: list<_>) (l2: list<_>) (l3: list<_>) =
    Array.map2
        ( <| )
        (Array.map2 f (Array.ofList l1) (Array.ofList l2))
        (Array.ofList l3)
    |> List.ofArray

[<Name "WebSharper.Seq.contains">]
let SeqContains (el: 'T) (s: seq<'T>) =
    use e = Enumerator.Get s
    let mutable r = false
    while not r && e.MoveNext() do
        r <- e.Current = el
    r

[<Name "WebSharper.Seq.pairwise">]
let SeqPairwise (s: seq<'T>) : seq<'T * 'T> =
    Seq.windowed 2 s
    |> Seq.map (fun x -> (x.[0], x.[1]))

[<Name "WebSharper.List.skipWhile">]
let rec ListSkipWhile<'T> (predicate : 'T -> bool) (list : list<'T>) : list<'T> =
    let mutable rest = list
    while not (List.isEmpty rest) && predicate (List.head rest) do
        rest <- List.tail rest 
    rest

[<Name "WebSharper.Seq.unfold">]
let SeqUnfold<'T, 'S> (f: 'S -> option<'T * 'S>) (s: 'S) : seq<'T> =
    Enumerable.Of <| fun () ->
        Enumerator.New s <| fun e ->
            match f e.State with
            | Some (t, s) ->
                e.Current <- t
                e.State  <- s
                true
            | None ->
                false

[<Name "WebSharper.Seq.truncate">]
let SeqTruncate (n: int) (s: seq<'T>) : seq<'T> =
    seq {
        use e = Enumerator.Get s
        let i = ref 0
        while e.MoveNext() && !i < n do
            incr i
            yield e.Current
    }

[<Name "WebSharper.Seq.nonNegative">]
let InputMustBeNonNegative() =
    failwith "The input must be non-negative."

[<Name "WebSharper.Seq.windowed">]
let SeqWindowed (windowSize: int) (s: seq<'T>) : seq<'T []> =
    if windowSize <= 0 then
        failwith "The input must be positive."
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

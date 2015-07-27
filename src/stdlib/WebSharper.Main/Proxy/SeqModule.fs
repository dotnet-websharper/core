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

[<WebSharper.Core.Attributes.Name "Seq">]
[<WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Collections.SeqModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.SeqModuleProxy

open WebSharper.JavaScript
open WebSharper.CollectionInternals

[<JavaScript>]
[<Inline>]
let safeDispose (x: System.IDisposable) =
    if x <> null then x.Dispose()

[<JavaScript>]
[<Name "append">]
let Append (s1: seq<'T>) (s2: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let e1 = Enumerator.Get s1
        let first = ref true
        Enumerator.NewDisposing e1 (fun x -> safeDispose x.State) (fun x ->
            if x.State.MoveNext() then
                x.Current <- x.State.Current
                true
            else 
                safeDispose x.State
                x.State <- null
                if !first then
                    first := false
                    x.State <- Enumerator.Get s2
                    if x.State.MoveNext() then
                        x.Current <- x.State.Current
                        true
                    else
                        x.State.Dispose()
                        x.State <- null
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
    let enum  = ref (Enumerator.Get s)
    Enumerable.Of <| fun () ->
        let next (e: Enumerator.T<_,_>) =
            if e.State + 1 < cache.Count then
                e.State   <- e.State + 1
                e.Current <- (?) cache (As e.State)
                true
            else
                let en = !enum
                if en = null then false
                elif en.MoveNext() then
                    e.State   <- e.State + 1
                    e.Current <- en.Current
                    cache.Enqueue e.Current
                    true
                else
                    en.Dispose()
                    enum := null
                    false
        Enumerator.New 0 next

/// IEnumerable is not supported.
[<Inline "$i">]
let Cast<'T> (i: System.Collections.IEnumerable) = X<seq<'T>>

[<JavaScript>]
[<Inline>]
let Contains (el: 'T) (s: seq<'T>) =
    SeqContains el s

[<JavaScript>]
[<Name "choose">]
let Choose (f: 'T -> option<'U>) (s: seq<'T>) : seq<'U> =
    s
    |> Seq.collect (fun x ->
        match f x with
        | Some v -> [v]
        | None   -> [])

[<JavaScript>]
[<Inline>]
let ChunkBySize (size: int) (s: seq<'T>) = SeqChunkBySize size s

[<JavaScript>]
[<Name "collect">]
let Collect f s = Seq.concat (Seq.map f s)

[<JavaScript>]
[<Inline>]
let CompareWith  (f: 'T -> 'T -> int) (s1: seq<'T>) (s2: seq<'T>) : int =
    SeqCompareWith f s1 s2

[<JavaScript>]
[<Name "concat">]
let Concat (ss: seq<#seq<'T>>) : seq<'T> =
    Enumerable.Of (fun () ->
        let outerE = Enumerator.Get ss
        let rec next (st: Enumerator.T<Enumerator.IE<'T>,'T>) =
            match st.State with
            | null ->
                if outerE.MoveNext() then
                    st.State <- Enumerator.Get outerE.Current
                    next st
                else
                    outerE.Dispose()
                    false
            | innerE ->
                if innerE.MoveNext() then
                    st.Current <- innerE.Current
                    true
                else
                    st.Dispose()
                    st.State <- null
                    next st
        Enumerator.NewDisposing null (fun st -> 
            safeDispose st.State 
            safeDispose outerE) 
            next)

[<JavaScript>]
[<Inline>]
let CountBy (f: 'T -> 'K) (s: seq<'T>) : seq<'K * int> =
    SeqCountBy f s

[<JavaScript>]
[<Name "delay">]
let Delay<'T> (f: unit -> seq<'T>) : seq<'T> =
    Enumerable.Of (fun () -> Enumerator.Get(f()))

[<JavaScript>]
[<Inline>]
let Distinct<'T when 'T : equality> (s: seq<'T>) : seq<'T> =
    SeqDistinct s

[<JavaScript>]
[<Inline>]
let DistinctBy<'T,'K when 'K : equality>
        (f: 'T -> 'K) (s: seq<'T>) : seq<'T> =
    SeqDistinctBy f s

[<JavaScript>]
[<Name "splitInto">]
let SplitInto count (s: seq<'T>) =
    if count <= 0 then failwith "Count must be positive"
    Seq.delay (fun () -> ArraySplitInto count (Array.ofSeq s) |> Seq.ofArray)   

[<JavaScript>]
[<Name "empty">]
let Empty<'T> : seq<'T> = [||] :> _

[<JavaScript>]
[<Inline>]
let Except (itemsToExclude: seq<'T>) (s: seq<'T>) =
    SeqExcept itemsToExclude s

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
        Enumerator.NewDisposing () (fun _ -> enum.Dispose()) <| fun e ->
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

[<JavaScript>]
[<Inline>]
let GroupBy (f: 'T -> 'K when 'K : equality)
            (s: seq<'T>) : seq<'K * seq<'T>> =
    SeqGroupBy f s

[<JavaScript>]
[<Name "head">]
let Head (s: seq<'T>) : 'T =
    use e = Enumerator.Get s
    if e.MoveNext() then e.Current else InsufficientElements()

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
[<Inline>]
let Last (s: seq<_>) =
    SeqLast s

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
        Enumerator.NewDisposing () (fun _ -> en.Dispose()) <| fun e ->
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
[<Name "map2">]
let Map2 (f: 'T -> 'U -> 'V) (s1: seq<'T>) (s2: seq<'U>) : seq<'V> =
    Enumerable.Of <| fun () ->
        let e1 = Enumerator.Get s1
        let e2 = Enumerator.Get s2
        Enumerator.NewDisposing () (fun _ -> e1.Dispose(); e2.Dispose()) <| fun e ->
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
            InsufficientElements()
        pos <- pos + 1
    e.Current

[<JavaScript>]
[<Inline>]
let Item index (s: seq<'T>) = Get index s

[<Inline "$a">]
[<Name "ofArray">]
let OfArray (a: 'T[]) = X<seq<'T>>

[<Inline "$l">]
[<Name "ofList">]
let OfList (l: list<'T>) = X<seq<'T>>

[<JavaScript>]
[<Inline>]
let Pairwise (s: seq<'T>) : seq<'T * 'T> =
    SeqPairwise s

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
        Enumerator.NewDisposing false (fun _ -> en.Dispose()) <| fun e ->
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
        let enum = Enumerator.Get s
        Enumerator.NewDisposing true (fun _ -> enum.Dispose()) (fun e ->
            if e.State then
                for i = 1 to n do
                    if not (enum.MoveNext()) then
                        InsufficientElements()
                e.State <- false
            if enum.MoveNext() then
                e.Current <- enum.Current
                true
            else
                false))

[<JavaScript>]
[<Name "skipWhile">]
let SkipWhile (f: 'T -> bool) (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let enum = Enumerator.Get s
        Enumerator.NewDisposing true (fun _ -> enum.Dispose()) (fun e ->
            if e.State then
                let mutable go = true
                let mutable empty = false
                while go do
                    if enum.MoveNext() then
                        if not (f enum.Current) then go <- false 
                    else 
                        go <-false
                        empty <- true
                e.State <- false
                if empty then 
                    false 
                else
                    e.Current <- enum.Current
                    true
            else
                if enum.MoveNext() then
                    e.Current <- enum.Current
                    true
                else
                    false))

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
[<Name "sortByDescending">]
let SortByDescending<'T, 'U when 'U: comparison>
        (f: 'T -> 'U) (s: seq<'T>) : seq<'T> =
    Seq.delay (fun () ->
        let array = Array.ofSeq s
        ArraySortInPlaceByDescending f array
        array :> _)

[<JavaScript>]
[<Name "sortDescending">]
let SortDescending<'T when 'T : comparison> (s: seq<'T>) =
    SortByDescending id s

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
    if n < 0 then
        InputMustBeNonNegative()
    Enumerable.Of (fun () ->
        let e = ref (Enumerator.Get s)
        Enumerator.NewDisposing 0 (fun _ -> safeDispose !e) (fun enum ->
            enum.State <- enum.State + 1
            if enum.State > n then false else
            let en = !e
            if en = null then InsufficientElements()
            elif en.MoveNext() then
                enum.Current <- en.Current
                if enum.State = n then
                    en.Dispose()
                    e := null
                true
            else
                en.Dispose()
                e := null
                InsufficientElements()
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
[<Inline>]
let Truncate (n: int) (s: seq<'T>) : seq<'T> =
    SeqTruncate n s

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
[<Inline>]
let TryFindBack ok (s: seq<_>) =
    ArrayTryFindBack ok (Array.ofSeq s) 

[<JavaScript>]
[<Inline>]
let TryHead (s: seq<'T>) = SeqTryHead s

[<JavaScript>]
[<Inline>]
let TryItem i (s: seq<'T>) = SeqTryItem i s

[<JavaScript>]
[<Inline>]
let TryLast (s: seq<'T>) =  SeqTryLast s

[<JavaScript>]
[<Name "findBack">]
let FindBack p (s: seq<_>) =
    match TryFindBack p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

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
[<Inline>]
let TryFindIndexBack ok (s: seq<_>) =
    ArrayTryFindIndexBack ok (Array.ofSeq s) 

[<JavaScript>]
[<Name "findIndexBack">]
let FindIndexBack p (s: seq<_>) =
    match TryFindIndexBack p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<JavaScript>]
[<Name "tryPick">]
let TryPick f (s: seq<_>) =
    use e = Enumerator.Get s
    let mutable r = None
    while r = None && e.MoveNext() do
        r <- f e.Current
    r

[<JavaScript>]
[<Inline>]
let Unfold (f: 'S -> option<'T * 'S>) (s: 'S) : seq<'T> =
    SeqUnfold f s

[<JavaScript>]
[<Inline>]
let Windowed (windowSize: int) (s: seq<'T>) : seq<'T []> =
    SeqWindowed windowSize s

[<JavaScript>]
[<Name "zip">]
let Zip (s1: seq<'T>) (s2: seq<'U>) =
    Seq.map2 (fun x y -> x, y) s1 s2

[<JavaScript>]
[<Name "zip3">]
let Zip3 (s1: seq<'T>) (s2: seq<'U>) (s3: seq<'V>) : seq<'T * 'U * 'V> =
    Seq.map2 (fun x (y, z) -> (x, y, z)) s1 (Seq.zip s2 s3)

[<JavaScript>]
[<Name "fold2">]
let Fold2<'T1,'T2,'S> (f: 'S -> 'T1 -> 'T2 -> 'S)
                        (s: 'S)
                        (s1: seq<'T1>)
                        (s2: seq<'T2>) : 'S =
    Array.fold2 f s (Array.ofSeq s1) (Array.ofSeq s2)

[<JavaScript>]
[<Name "foldBack">]
let FoldBack f (s: seq<_>) state =
    Array.foldBack f (Array.ofSeq s) state

[<JavaScript>]
[<Name "foldBack2">]
let FoldBack2 f (s1: seq<_>) (s2: seq<_>) s =
    Array.foldBack2 f (Array.ofSeq s1) (Array.ofSeq s2) s

[<JavaScript>]
[<Name "iteri2">]
let IterateIndexed2 f (s1: seq<_>) (s2: seq<_>) =
    Array.iteri2 f (Array.ofSeq s1) (Array.ofSeq s2)

[<JavaScript>]
[<Name "map3">]
let Map3 f (s1: seq<_>) (s2: seq<_>) (s3: seq<_>) =
    Enumerable.Of <| fun () ->
        let e1 = Enumerator.Get s1
        let e2 = Enumerator.Get s2
        let e3 = Enumerator.Get s3
        Enumerator.NewDisposing () (fun _ -> e1.Dispose(); e2.Dispose(); e3.Dispose()) <| fun e ->
            if e1.MoveNext() && e2.MoveNext() && e3.MoveNext() then
                e.Current <- f e1.Current e2.Current e3.Current
                true
            else
                false

[<JavaScript>]
[<Name "mapi2">]
let MapIndexed2 f (s1: seq<_>) (s2: seq<_>) =
    Map3 f (Seq.initInfinite id) s1 s2

[<JavaScript>]
[<Name "mapFold">]
let MapFold f zero s =
    ArrayMapFold f zero (Seq.toArray s)
    |> (fun (x, y) ->
        (Array.toSeq x, y)
    )

[<JavaScript>]
[<Name "mapFoldBack">]
let MapFoldBack f s zero =
    ArrayMapFoldBack f (Seq.toArray s) zero
    |> (fun (x, y) ->
        (Array.toSeq x, y)
    )

[<JavaScript>]
[<Name "permute">]
let Permute f (s: seq<_>) =
    Seq.delay (fun () -> Seq.ofArray (Array.permute f (Array.ofSeq s)))

[<JavaScript>]
[<Name "reduceBack">]
let ReduceBack f (s: seq<_>) =
    Array.reduceBack f (Array.ofSeq s)

[<JavaScript>]
[<Name "replicate">]
let Replicate size value =
    if size < 0 then InputMustBeNonNegative()
    seq { for i in 0 .. size - 1 -> value }

[<JavaScript>]
[<Name "rev">]
let Reverse (s: seq<'T>) =
    Seq.delay (fun () -> Array.rev (Seq.toArray s) |> Array.toSeq)
    
[<JavaScript>]
[<Name "scanBack">]
let ScanBack f (l: seq<_>) s =
    Seq.delay (fun () -> Seq.ofArray (Array.scanBack f (Array.ofSeq l) s))

[<JavaScript>]
[<Name "indexed">]
let Indexed (s : seq<'T>) : seq<int * 'T> =
    Seq.mapi (fun a b -> (a, b)) s

[<JavaScript>]
[<Name "sortWith">]
let SortWith f (s: seq<_>) =
    Seq.delay (fun () -> 
        let a = Array.ofSeq s
        Array.sortInPlaceWith f a
        Seq.ofArray a)

[<JavaScript>]
[<Name "tail">]
let Tail<'T> (s : seq<'T>) : seq<'T> =
    Seq.skip 1 s

[<JavaScript>]
[<Inline>]
let Where (predicate : 'T -> bool) (s : seq<'T>) : seq<'T> =
    Filter predicate s

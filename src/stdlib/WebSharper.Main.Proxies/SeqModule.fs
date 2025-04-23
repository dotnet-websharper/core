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

[<WebSharper.Name "Seq">]
[<WebSharper.Proxy
    "Microsoft.FSharp.Collections.SeqModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.SeqModuleProxy

#nowarn "77" // op_Addition warnings

open WebSharper.JavaScript
open WebSharper.CollectionInternals

module M = WebSharper.Core.Macros

[<Inline>]
let safeDispose (x: System.IDisposable) =
    if x <> null then x.Dispose()

let seqEmpty() =
    failwith "The input sequence was empty."

[<Name "allPairs">]
let AllPairs (source1: seq<_>) (source2: seq<_>) =
    let cached = Seq.cache source2
    source1 |> Seq.collect (fun x -> cached |> Seq.map (fun y -> x,y))

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

[<Name "cache">]
let Cache<'T> (s: seq<'T>) : seq<'T> =
    let cache = JavaScript.Array<'T>()
    let o  = ref (Enumerator.Get s)
    Enumerable.Of <| fun () ->
        let next (e: Enumerator.T<_,_>) =
            if e.State < cache.Length then
                e.Current <- cache.[e.State]
                e.State   <- e.State + 1
                true
            else
                let en = !o
                if en = null then false
                elif en.MoveNext() then
                    e.State   <- e.State + 1
                    e.Current <- en.Current
                    cache.Push(e.Current) |> ignore
                    true
                else
                    en.Dispose()
                    o := null
                    false
        Enumerator.New 0 next

/// IEnumerable is not supported.
[<Inline "$i">]
let Cast<'T> (i: System.Collections.IEnumerable) = X<seq<'T>>

[<Inline>]
let Contains (el: 'T) (s: seq<'T>) =
    SeqContains el s

[<Name "choose">]
let Choose (f: 'T -> option<'U>) (s: seq<'T>) : seq<'U> =
    s
    |> Seq.collect (fun x ->
        match f x with
        | Some v -> [v]
        | None   -> [])

[<Inline>]
let ChunkBySize (size: int) (s: seq<'T>) = SeqChunkBySize size s

[<Name "collect">]
let Collect f s = Seq.concat (Seq.map f s)

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
                    (st :> System.IDisposable).Dispose()
                    st.State <- null
                    next st
        Enumerator.NewDisposing null (fun st -> 
            safeDispose st.State 
            safeDispose outerE) 
            next)

[<Name "countBy">]
let CountBy (f: 'T -> 'K) (s: seq<'T>) : seq<'K * int> =
    Seq.delay <| fun () ->
        ArrayCountBy f (Array.ofSeq s) |> Seq.ofArray

[<Name "delay">]
let Delay<'T> (f: unit -> seq<'T>) : seq<'T> =
    Enumerable.Of (fun () -> Enumerator.Get(f()))

[<Name "distinct">]
let Distinct<'T when 'T : equality> (s: seq<'T>) : seq<'T> =
    Seq.distinctBy id s

[<Name "distinctBy">]
let DistinctBy<'T,'K when 'K : equality>
        (f: 'T -> 'K) (s: seq<'T>) : seq<'T> =
    Enumerable.Of <| fun () ->
        let o  = Enumerator.Get s
        let seen = System.Collections.Generic.HashSet<'K>()
        Enumerator.NewDisposing () (fun _ -> o.Dispose()) <| fun e ->
            if o.MoveNext() then
                let mutable cur = o.Current
                let mutable has = seen.Add(f cur)
                while not has && o.MoveNext() do
                    cur <- o.Current
                    has <- seen.Add(f cur)
                if has then
                    e.Current <- cur
                    true
                else
                    false
            else
                false

[<Name "splitInto">]
let SplitInto count (s: seq<'T>) =
    if count <= 0 then failwith "Count must be positive"
    Seq.delay (fun () -> ArraySplitInto count (Array.ofSeq s) |> Seq.ofArray)   

[<Inline>]
let Empty<'T> : seq<'T> = As [||]

[<Name "exactlyOne">]
let ExactlyOne<'T> (s: seq<'T>) =
    use e = Enumerator.Get s
    if e.MoveNext() then
        let x = e.Current
        if e.MoveNext() then
            invalidOp "Sequence contains more than one element"
        else x
    else invalidOp "Sequence contains no elements"

[<Name "tryExactlyOne">]
let TryExactlyOne<'T> (s: seq<'T>) =
    use e = Enumerator.Get s
    if e.MoveNext() then
        let x = e.Current
        if e.MoveNext() then
            None
        else Some x
    else None

[<Inline>]
let Except (itemsToExclude: seq<'T>) (s: seq<'T>) =
    SeqExcept itemsToExclude s

[<Name "exists">]
let Exists p (s: seq<_>) =
    use e = Enumerator.Get s
    let mutable r = false
    while not r && e.MoveNext() do
        r <- p e.Current
    r

[<Name "exists2">]
let Exists2 p (s1: seq<_>) (s2: seq<_>) =
    use e1 = Enumerator.Get s1
    use e2 = Enumerator.Get s2
    let mutable r = false
    while not r && e1.MoveNext() && e2.MoveNext() do
        r <- p e1.Current e2.Current
    r

[<Name "filter">]
let Filter (f: 'T -> bool) (s: seq<'T>) =
    Enumerable.Of <| fun () ->
        let o = Enumerator.Get s
        Enumerator.NewDisposing () (fun _ -> o.Dispose()) <| fun e ->
            let mutable loop = o.MoveNext()
            let mutable c    = JS.Undefined
            let mutable res  = false
            while loop do
                c <- o.Current
                if f c then
                    e.Current <- c
                    res       <- true
                    loop      <- false
                elif not (o.MoveNext()) then
                    loop <- false
            res

[<Name "find">]
let Find p (s: seq<_>) =
    match Seq.tryFind p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Name "findIndex">]
let FindIndex p (s: seq<_>) =
    match Seq.tryFindIndex p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Name "fold">]
let Fold<'T,'S> (f: 'S -> 'T -> 'S) (x: 'S) (s: seq<'T>) : 'S =
    let mutable r = x
    use e = Enumerator.Get s
    while e.MoveNext() do
        r <- f r e.Current
    r

[<Name "forall">]
let ForAll p s =
    not (Seq.exists (fun x -> not (p x)) s)

[<Name "forall2">]
let ForAll2 p s1 s2 =
    not (Seq.exists2 (fun x y -> not (p x y)) s1 s2)

[<Name "groupBy">]
let GroupBy (f: 'T -> 'K when 'K : equality) (s: seq<'T>) : seq<'K * seq<'T>> =
    Seq.delay <| fun () ->
        ArrayGroupBy f (Array.ofSeq s) |> As

[<Name "head">]
let Head (s: seq<'T>) : 'T =
    use e = Enumerator.Get s
    if e.MoveNext() then e.Current else InsufficientElements()

[<Name "init">]
let Initialize (n: int) (f: int -> 'T) : seq<'T> =
    Seq.take n (Seq.initInfinite f)

[<Name "initInfinite">]
let InitializeInfinite (f: int -> 'T) : seq<'T> =
    Enumerable.Of <| fun () ->
        Enumerator.New 0 <| fun e ->
            e.Current <- f e.State
            e.State   <- e.State + 1
            true

[<Name "isEmpty">]
let IsEmpty (s: seq<'T>) : bool =
    use e = Enumerator.Get s
    not (e.MoveNext())

[<Name "iter">]
let Iterate p (s: seq<_>) =
    use e = Enumerator.Get s
    while e.MoveNext() do
        p e.Current

[<Name "iter2">]
let Iterate2 p (s1: seq<_>) (s2: seq<_>) =
    use e1 = Enumerator.Get s1
    use e2 = Enumerator.Get s2
    while e1.MoveNext() && e2.MoveNext() do
        p e1.Current e2.Current

[<Name "iteri">]
let IterateIndexed p (s: seq<_>) =
    let mutable i = 0
    use e = Enumerator.Get s
    while e.MoveNext() do
        p i e.Current
        i <- i + 1

[<Inline>]
let Last (s: seq<_>) =
    SeqLast s

[<Name "length">]
let Length (s: seq<_>) =
    let mutable i = 0
    use e = Enumerator.Get s
    while e.MoveNext() do
        i <- i + 1
    i

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

[<Name "mapi">]
let MapIndexed (f: int -> 'T -> 'U) (s: seq<'T>) : seq<'U> =
    Seq.map2 f (Seq.initInfinite id) s

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

[<Name "maxBy">]
let MaxBy (f: 'T -> 'U) (s: seq<'T>) : 'T =
    use e = Enumerator.Get s
    if not (e.MoveNext()) then
        seqEmpty()
    let mutable m = e.Current
    let mutable fm = f m
    while e.MoveNext() do
        let x = e.Current
        let fx = f x
        if fx > fm then
            m <- x
            fm <- fx
    m

[<Name "minBy">]
let MinBy (f: 'T -> 'U) (s: seq<'T>) : 'T =
    use e = Enumerator.Get s
    if not (e.MoveNext()) then
        seqEmpty()
    let mutable m = e.Current
    let mutable fm = f m
    while e.MoveNext() do
        let x = e.Current
        let fx = f x
        if fx < fm then
            m <- x
            fm <- fx
    m

[<Name "max">]
let Max (s: seq<'T>) : 'T =
    use e = Enumerator.Get s
    if not (e.MoveNext()) then
        seqEmpty()
    let mutable m = e.Current
    while e.MoveNext() do
        let x = e.Current
        if x > m then
            m <- x
    m

[<Name "min">]
let Min (s: seq<'T>) : 'T =
    use e = Enumerator.Get s
    if not (e.MoveNext()) then
        seqEmpty()
    let mutable m = e.Current
    while e.MoveNext() do
        let x = e.Current
        if x < m then
            m <- x
    m

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

[<Inline>]
let Item index (s: seq<'T>) = Get index s

[<Inline "$a">]
[<Name "ofArray">]
let OfArray (a: 'T[]) = X<seq<'T>>

[<Inline "$l">]
[<Name "ofList">]
let OfList (l: list<'T>) = X<seq<'T>>

[<Name "pairwise">]
let Pairwise (s: seq<'T>) : seq<'T * 'T> =
    Seq.windowed 2 s
    |> Seq.map (fun x -> (x.[0], x.[1]))

[<Name "pick">]
let Pick p (s: seq<_>) =
    match Seq.tryPick p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Name "readOnly">]
let ReadOnly (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () -> Enumerator.Get s)

[<Name "reduce">]
let Reduce (f: 'T -> 'T -> 'T) (source: seq<'T>) : 'T =
    use e = Enumerator.Get source
    if not (e.MoveNext()) then
        seqEmpty()
    let mutable r = e.Current
    while e.MoveNext() do
        r <- f r e.Current
    r

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

[<Name "skip">]
let Skip (n: int) (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let o = Enumerator.Get s
        Enumerator.NewDisposing true (fun _ -> o.Dispose()) (fun e ->
            if e.State then
                for i = 1 to n do
                    if not (o.MoveNext()) then
                        InsufficientElements()
                e.State <- false
            if o.MoveNext() then
                e.Current <- o.Current
                true
            else
                false))

[<Name "skipWhile">]
let SkipWhile (f: 'T -> bool) (s: seq<'T>) : seq<'T> =
    Enumerable.Of (fun () ->
        let o = Enumerator.Get s
        Enumerator.NewDisposing true (fun _ -> o.Dispose()) (fun e ->
            if e.State then
                let mutable go = true
                let mutable empty = false
                while go do
                    if o.MoveNext() then
                        if not (f o.Current) then go <- false 
                    else 
                        go <-false
                        empty <- true
                e.State <- false
                if empty then 
                    false 
                else
                    e.Current <- o.Current
                    true
            else
                if o.MoveNext() then
                    e.Current <- o.Current
                    true
                else
                    false))

[<Name "sort">]
let Sort<'T when 'T : comparison> (s: seq<'T>) =
    Seq.sortBy id s

[<Name "sortBy">]
let SortBy<'T, 'U when 'U: comparison>
        (f: 'T -> 'U) (s: seq<'T>) : seq<'T> =
    Seq.delay (fun () ->
        let array = Array.ofSeq s
        Array.sortInPlaceBy f array
        array :> _)

[<Name "sortByDescending">]
let SortByDescending<'T, 'U when 'U: comparison>
        (f: 'T -> 'U) (s: seq<'T>) : seq<'T> =
    Seq.delay (fun () ->
        let array = Array.ofSeq s
        ArraySortInPlaceByDescending f array
        array :> _)

[<Name "sortDescending">]
let SortDescending<'T when 'T : comparison> (s: seq<'T>) =
    SortByDescending id s

[<Inline>]
let inline private SumGeneric< ^T when ^T : (static member (+) : ^T * ^T -> ^T) and ^T : (static member Zero : ^T)> (s: seq< ^T>) : ^T =
    let mutable res = LanguagePrimitives.GenericZero< ^T>
    for x in s do
        res <- (^T: (static member (+) : ^T * ^T -> ^T) (res, x))
    res

[<Name "sum">]
[<Macro(typeof<M.SumOrAverageMacro>)>]
[<JavaScript>]
let Sum<'T> (s: seq<'T> ) : 'T =
    As<'T>(SumGeneric<int> (As<seq<int>> s))

[<Inline>]
let inline private SumByGeneric< ^T, ^U when ^U : (static member (+) : ^U * ^U -> ^U) and ^U : (static member Zero : ^U)> (f: ^T -> ^U) (s: seq< ^T>) : ^U =
    let mutable res = LanguagePrimitives.GenericZero< ^U>
    for x in s do
        res <- (^U: (static member (+) : ^U * ^U -> ^U) (res, f x))
    res

[<Name "sumBy">]
[<Macro(typeof<M.SumOrAverageMacro>)>]
[<JavaScript>]
let SumBy<'T,'U> (f: 'T -> 'U) (s: seq<'T>) : 'U =
    As<'U>(SumByGeneric<int, int> (As<int -> int> f) (As<seq<int>> s))

[<Inline>]
let inline AverageGeneric< ^T when ^T : (static member ( + ) : ^T * ^T -> ^T) 
                and ^T : (static member DivideByInt : ^T * int -> ^T)
                and ^T : (static member Zero : ^T)> (s: seq< ^T>) : ^T =
    let mutable res = LanguagePrimitives.GenericZero< ^T>
    let mutable count = 0
    for x in s do
        res <- (^T: (static member (+) : ^T * ^T -> ^T) (res, x))
        count <- count + 1
    if count = 0 then
        seqEmpty()
    else
        (^T: (static member DivideByInt : ^T * int -> ^T) (res, count))

[<Name "average">]
[<Macro(typeof<M.SumOrAverageMacro>)>]
[<JavaScript>]
let Average<'T> (s: seq<'T>) : 'T =
    As<'T>(AverageGeneric<float> (As<seq<float>> s))

[<Inline>]
let inline AverageByGeneric< ^T, ^U when ^U : (static member ( + ) : ^U * ^U -> ^U) 
                and ^U : (static member DivideByInt : ^U * int -> ^U)
                and ^U : (static member Zero : ^U)> (f: ^T -> ^U) (s: seq< ^T>) : ^U =
    let mutable res = LanguagePrimitives.GenericZero< ^U>
    let mutable count = 0
    for x in s do
        res <- (^U: (static member (+) : ^U * ^U -> ^U) (res, f x))
        count <- count + 1
    if count = 0 then
        seqEmpty()
    else
        (^U: (static member DivideByInt : ^U * int -> ^U) (res, count))

[<Name "averageBy">]
[<Macro(typeof<M.SumOrAverageMacro>)>]
[<JavaScript>]
let AverageBy<'T,'U> (f: 'T -> 'U) (s: seq<'T>) : 'U =
    As<'U>(AverageByGeneric<float, float> (As<float -> float> f) (As<seq<float>> s))

[<Name "take">]
let Take (n: int) (s: seq<'T>) : seq<'T> =
    if n < 0 then
        InputMustBeNonNegative()
    Enumerable.Of (fun () ->
        let e = ref (Enumerator.Get s)
        Enumerator.NewDisposing 0 (fun _ -> safeDispose !e) (fun o ->
            o.State <- o.State + 1
            if o.State > n then false else
            let en = !e
            if en = null then InsufficientElements()
            elif en.MoveNext() then
                o.Current <- en.Current
                if o.State = n then
                    en.Dispose()
                    e := null
                true
            else
                en.Dispose()
                e := null
                InsufficientElements()
        )
    )

[<Name "takeWhile">]
let TakeWhile (f: 'T -> bool) (s: seq<'T>) : seq<'T> =
    seq {
        use e = Enumerator.Get s
        while e.MoveNext() && f e.Current do
            yield e.Current
    }

[<Inline>]
let ToArray (s: seq<'T>) =
    Array.ofSeq s

[<Inline>]
let ToList (s: seq<'T>) = List.ofSeq s

[<Name "transpose">]
let Transpose (x: seq<#seq<'T>>) : seq<seq<'T>> =
    Seq.delay (fun () ->
        ArrayTranspose (Array.ofSeq (x |> Seq.map Array.ofSeq)) |> As
    )

[<Name "truncate">]
let Truncate (n: int) (s: seq<'T>) : seq<'T> =
    seq {
        use e = Enumerator.Get s
        let i = ref 0
        while e.MoveNext() && !i < n do
            incr i
            yield e.Current
    }

[<Name "tryFind">]
let TryFind ok (s: seq<_>) =
    use e = Enumerator.Get s
    let mutable r = None
    while r.IsNone && e.MoveNext() do
        let x = e.Current
        if ok x then
            r <- Some x
    r

[<Inline>]
let TryFindBack ok (s: seq<_>) =
    ArrayTryFindBack ok (Array.ofSeq s) 

[<Inline>]
let TryHead (s: seq<'T>) = SeqTryHead s

[<Inline>]
let TryItem i (s: seq<'T>) = SeqTryItem i s

[<Inline>]
let TryLast (s: seq<'T>) =  SeqTryLast s

[<Name "findBack">]
let FindBack p (s: seq<_>) =
    match TryFindBack p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

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

[<Inline>]
let TryFindIndexBack ok (s: seq<_>) =
    ArrayTryFindIndexBack ok (Array.ofSeq s) 

[<Name "findIndexBack">]
let FindIndexBack p (s: seq<_>) =
    match TryFindIndexBack p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Name "tryPick">]
let TryPick f (s: seq<_>) =
    use e = Enumerator.Get s
    let mutable r = None
    while r = None && e.MoveNext() do
        r <- f e.Current
    r

[<Name "unfold">]
let Unfold<'S, 'T> (f: 'S -> option<'T * 'S>) (s: 'S) : seq<'T> =
    Enumerable.Of <| fun () ->
        Enumerator.New s <| fun e ->
            match f e.State with
            | Some (t, s) ->
                e.Current <- t
                e.State  <- s
                true
            | None ->
                false

[<Name "windowed">]
let Windowed (windowSize: int) (s: seq<'T>) : seq<'T []> =
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

[<Name "zip">]
let Zip (s1: seq<'T>) (s2: seq<'U>) =
    Seq.map2 (fun x y -> x, y) s1 s2

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

[<Name "zip3">]
let Zip3 (s1: seq<'T>) (s2: seq<'U>) (s3: seq<'V>) : seq<'T * 'U * 'V> =
    Map3 (fun x y z -> x, y, z) s1 s2 s3

[<Name "fold2">]
let Fold2<'T1,'T2,'S> (f: 'S -> 'T1 -> 'T2 -> 'S)
                        (s: 'S)
                        (s1: seq<'T1>)
                        (s2: seq<'T2>) : 'S =
    Array.fold2 f s (Array.ofSeq s1) (Array.ofSeq s2)

[<Name "foldBack">]
let FoldBack f (s: seq<_>) state =
    Array.foldBack f (Array.ofSeq s) state

[<Name "foldBack2">]
let FoldBack2 f (s1: seq<_>) (s2: seq<_>) s =
    Array.foldBack2 f (Array.ofSeq s1) (Array.ofSeq s2) s

[<Name "iteri2">]
let IterateIndexed2 f (s1: seq<_>) (s2: seq<_>) =
    let mutable i = 0
    use e1 = Enumerator.Get s1
    use e2 = Enumerator.Get s2
    while e1.MoveNext() && e2.MoveNext() do
        f i e1.Current e2.Current
        i <- i + 1

[<Name "mapi2">]
let MapIndexed2 f (s1: seq<_>) (s2: seq<_>) =
    Map3 f (Seq.initInfinite id) s1 s2

[<Name "mapFold">]
let MapFold<'T,'S,'R> f zero s =
    ArrayMapFold<'T,'S,'R> f zero (Seq.toArray s)
    |> As<seq<'R> * 'S>

[<Name "mapFoldBack">]
let MapFoldBack<'T,'S,'R> f s zero =
    ArrayMapFoldBack<'T,'S,'R> f (Seq.toArray s) zero
    |> As<seq<'R> * 'S>

[<Name "permute">]
let Permute f (s: seq<_>) =
    Seq.delay (fun () -> Seq.ofArray (Array.permute f (Array.ofSeq s)))

[<Name "reduceBack">]
let ReduceBack f (s: seq<_>) =
    Array.reduceBack f (Array.ofSeq s)

[<Name "replicate">]
let Replicate size value =
    if size < 0 then InputMustBeNonNegative()
    seq { for i in 0 .. size - 1 -> value }

[<Name "rev">]
let Reverse (s: seq<'T>) =
    Seq.delay (fun () -> Array.rev (Seq.toArray s) |> Array.toSeq)
    
[<Name "scanBack">]
let ScanBack f (l: seq<_>) s =
    Seq.delay (fun () -> Seq.ofArray (Array.scanBack f (Array.ofSeq l) s))

[<Name "indexed">]
let Indexed (s : seq<'T>) : seq<int * 'T> =
    Seq.mapi (fun a b -> (a, b)) s

[<Name "sortWith">]
let SortWith f (s: seq<_>) =
    Seq.delay (fun () -> 
        let a = Array.ofSeq s
        Array.sortInPlaceWith f a
        Seq.ofArray a)

[<Name "tail">]
let Tail<'T> (s : seq<'T>) : seq<'T> =
    Seq.skip 1 s

[<Inline>]
let Where (predicate : 'T -> bool) (s : seq<'T>) : seq<'T> =
    Filter predicate s

[<Name "insertAt">]
let InsertAt (index: int) (item: 'T) (arr: 'T seq): 'T seq =
    if index >= 0 then
        if index = 0 then
            Seq.append [item] arr
        else
            Enumerable.Of <| fun () ->
                let en = Enumerator.Get arr
                let mutable ind = 0
                Enumerator.NewDisposing () (fun _ -> en.Dispose())
                    (fun e ->
                        if ind = index then
                            e.Current <- item
                            ind <- ind + 1
                            true
                        else
                            if en.MoveNext() then
                                e.Current <- en.Current
                                ind <- ind + 1
                                true
                            else
                                if ind < index then
                                    OutOfBounds()
                                else 
                                    false
                    )
    else
        OutOfBounds()

[<Name "insertManyAt">]
let InsertManyAt (index: int) (items: System.Collections.Generic.IEnumerable<'T>) (arr: 'T seq): 'T seq =
    if index >= 0 then
        if index = 0 then
            Seq.append items arr
        else
            Enumerable.Of <| fun () ->
                let en = Enumerator.Get arr
                let mutable newItems = null       
                let mutable ind = 0
                Enumerator.NewDisposing () (fun _ -> en.Dispose(); safeDispose newItems)
                    (fun e ->
                        if ind = index then
                            if isNull newItems then
                                newItems <- Enumerator.Get items   
                            if newItems.MoveNext() then
                                e.Current <- newItems.Current
                                true
                            else
                                ind <- ind + 1
                                if en.MoveNext() then
                                    e.Current <- en.Current
                                    true
                                else
                                    false
                        else
                            if en.MoveNext() then
                                e.Current <- en.Current
                                ind <- ind + 1
                                true
                            else
                                if ind < index then
                                    OutOfBounds()
                                else 
                                    false
                    )
    else
        OutOfBounds()

[<Name "removeAt">]
let RemoveAt (index: int) (arr: 'T seq): 'T seq =
    if index >= 0 then
        if index = 0 then
            Seq.tail arr
        else
            Enumerable.Of <| fun () ->
                let en = Enumerator.Get arr                     
                let mutable ind = 0
                Enumerator.NewDisposing () (fun _ -> en.Dispose())
                    (fun e ->
                        if ind = index then
                            if en.MoveNext() then
                                if en.MoveNext() then
                                    e.Current <- en.Current
                                    ind <- ind + 2
                                    true
                                else
                                    false
                            else
                                OutOfBounds()
                        else
                            if en.MoveNext() then
                                e.Current <- en.Current
                                ind <- ind + 1
                                true
                            else
                                if ind < index then
                                    OutOfBounds()
                                else 
                                    false
                    )
    else
        OutOfBounds()

[<Name "removeManyAt">]
let RemoveManyAt (index: int) (number: int) (arr: 'T seq): 'T seq =
    if index >= 0 then
        if index = 0 then
            Seq.skip number arr
        else
            Enumerable.Of <| fun () ->
                let en = Enumerator.Get arr                     
                let mutable ind = 0
                Enumerator.NewDisposing () (fun _ -> en.Dispose())
                    (fun e ->
                        if ind = index then
                            if en.MoveNext() then
                                let mutable current = number
                                while current > 0 && (
                                    if en.MoveNext() then 
                                        true
                                    else
                                        current <- -1
                                        false
                                ) do
                                    current <- current - 1
                                if current = 0 then
                                    e.Current <- en.Current
                                    ind <- ind + number + 1
                                    true
                                else
                                    false
                            else
                                OutOfBounds()
                        else
                            if en.MoveNext() then
                                e.Current <- en.Current
                                ind <- ind + 1
                                true
                            else
                                if ind < index then
                                    OutOfBounds()
                                else 
                                    false
                    )
    else
        OutOfBounds()

[<Name "updateAt">]
let UpdateAt (index: int) (item: 'T) (arr: 'T seq): 'T seq =
    if index >= 0 then
        if index = 0 then
            Seq.append [item] (Seq.tail arr)
        else
            Enumerable.Of <| fun () ->
                let en = Enumerator.Get arr
                let mutable ind = 0
                Enumerator.NewDisposing () (fun _ -> en.Dispose())
                    (fun e ->
                        if ind = index then
                            if en.MoveNext() then
                                e.Current <- item
                                ind <- ind + 1
                                true
                            else
                                OutOfBounds()
                        else
                            if en.MoveNext() then
                                e.Current <- en.Current
                                ind <- ind + 1
                                true
                            else
                                if ind < index then
                                    OutOfBounds()
                                else 
                                    false
                    )
    else
        OutOfBounds()

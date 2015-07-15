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

/// Provides seq/list/array proxies
module internal WebSharper.CollectionInternals

open WebSharper.JavaScript

[<JavaScript>]
[<Name "Arrays.splitInto">]
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

[<JavaScript>]
[<Name "Arrays.contains">]
let ArrayContains (item: 'T) (arr: Array<'T>)  =
    let mutable c = true
    let mutable i = 0
    let l = arr.Length
    while c && i < l do
        if arr.[i] = item then
            c <- false
        else
            i <- i + 1
    not c

[<JavaScript>]
[<Name "Arrays.tryFindBack">]
let ArrayTryFindBack f (arr: _ []) =
    let mutable res = None
    let mutable i = Array.length arr - 1
    while i > 0 && Option.isNone res do
        if f arr.[i] then res <- Some arr.[i]
        i <- i - 1
    res

[<JavaScript>]
[<Name "Arrays.tryFindIndexBack">]
let ArrayTryFindIndexBack f (arr: _ []) =
    let mutable res = None
    let mutable i = Array.length arr - 1
    while i > 0 && Option.isNone res do
        if f arr.[i] then res <- Some i
        i <- i - 1
    res

[<JavaScript>]
[<Name "Arrays.mapFold">]
let ArrayMapFold (f: 'S -> 'T -> 'R * 'S) (zero: 'S) (arr: 'T[]) : 'R[] * 'S =
    let r = Array.zeroCreate<'R>(Array.length arr)
    let mutable acc = zero
    for i = 0 to Array.length arr - 1 do
        let a, b = f acc arr.[i]
        r.[i] <- a
        acc <- b 
    r, acc

[<JavaScript>]
[<Name "Arrays.mapFoldBack">]
let ArrayMapFoldBack (f: 'T -> 'S -> 'R * 'S) (arr: 'T[]) (zero: 'S) : 'R[] * 'S =
    let r = Array.zeroCreate<'R>(Array.length arr)
    let mutable acc = zero
    let len = Array.length arr
    for j = 1 to len do
        let i = len - j
        let a, b = f arr.[i] acc
        r.[i] <- a
        acc <- b 
    r, acc

[<JavaScript>]
[<Name "Arrays.sortInPlaceByDescending">]
let ArraySortInPlaceByDescending<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) =
    As<unit> (arr.JS.Sort(fun (x, y) -> - compare (f x) (f y)))

[<JavaScript>]
[<Name "Seq.tryHead">]
let SeqTryHead (s: seq<'T>) =
    use e = Enumerator.Get s
    if e.MoveNext() then Some e.Current else None

[<JavaScript>]
[<Name "Seq.tryItem">]
let SeqTryItem i (s: seq<'T>) =
    if i < 0 then None else
    let mutable j = 0
    use e = Enumerator.Get s
    let mutable go = true
    while go && j < i do
        if e.MoveNext() then
            j <- j + 1
        else
            go <- false
    if go then Some e.Current else None

[<JavaScript>]
[<Name "Seq.tryLast">]
let SeqTryLast (s: seq<'T>) =
    use e = Enumerator.Get s
    if e.MoveNext() then 
        while e.MoveNext() do ()
        Some e.Current 
    else None

[<JavaScript>]
[<Name "Seq.chunkBySize">]
let SeqChunkBySize (size: int) (s: seq<'T>) =
    if size <= 0 then failwith "Chunk size must be positive"
    Enumerable.Of <| fun () ->
        let enum = Enumerator.Get s
        Enumerator.NewDisposing () (fun _ -> enum.Dispose()) <| fun e ->
            if enum.MoveNext() then
                let res = [|enum.Current|]
                while res.Length < size && e.MoveNext() do 
                    res.JS.Push enum.Current |> ignore
                e.Current <- res
                true
            else false

[<JavaScript>]
[<Name "Seq.compareWith">]
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

[<JavaScript>]
[<Name "Seq.countBy">]
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

[<JavaScript>]
[<Name "Seq.distinct">]
let SeqDistinct<'T when 'T : equality> (s: seq<'T>) : seq<'T> =
    Seq.distinctBy id s

[<JavaScript>]
[<Name "Seq.distinctBy">]
let SeqDistinctBy<'T,'K when 'K : equality>
        (f: 'T -> 'K) (s: seq<'T>) : seq<'T> =
    Enumerable.Of <| fun () ->
        let enum        = Enumerator.Get s
        let seen        = Array<Array<'K>>()
        let add c =
            let k = f c
            let h = hash k
            let cont = seen.[h]
            if cont = JS.Undefined then
                seen.[h] <- [|k|].JS
                true
            else
                if cont |> ArrayContains k then
                    false
                else
                    cont.Push(k) |> ignore
                    true         
        Enumerator.NewDisposing () (fun _ -> enum.Dispose()) <| fun e ->
            if enum.MoveNext() then
                let mutable cur = enum.Current
                let mutable has = add cur
                while not has && enum.MoveNext() do
                    cur <- enum.Current
                    has <- add cur
                if has then
                    e.Current <- cur
                    true
                else
                    false
            else
                false

[<JavaScript>]
[<Name "Seq.except">]
let SeqExcept (itemsToExclude: seq<'T>) (s: seq<'T>) =
    Enumerable.Of <| fun () ->
        let enum        = Enumerator.Get s
        let seen        = Array<Array<'T>>()
        let add c =
            let h = hash c
            let cont = seen.[h]
            if cont = JS.Undefined then
                seen.[h] <- [|c|].JS
                true
            else
                if cont |> ArrayContains c then
                    false
                else
                    cont.Push(c) |> ignore
                    true         
        for i in itemsToExclude do
            add i |> ignore
        Enumerator.NewDisposing () (fun _ -> enum.Dispose()) <| fun e ->
            if enum.MoveNext() then
                let mutable cur = enum.Current
                let mutable has = add cur
                while not has && enum.MoveNext() do
                    cur <- enum.Current
                    has <- add cur
                if has then
                    e.Current <- cur
                    true
                else
                    false
            else
                false

[<JavaScript>]
[<Name "List.skip">]
let ListSkip i (l : list<'T>) =
    let mutable res = l
    for j = 1 to i do
        match res with 
        | _ :: t ->
            res <- t
        | [] -> failwith "Input list too short."
    res
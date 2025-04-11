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

/// Provides seq/list/array proxies
[<JavaScript>]
module internal WebSharper.CollectionInternals

open WebSharper.JavaScript

[<Name "splitInto">]
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
            res.JS.[i] <- Array.sub arr startIndex (minChunkSize + 1)
            startIndex <- startIndex + minChunkSize + 1
        for i = len % count to count - 1 do
            res.JS.[i] <-  Array.sub arr startIndex minChunkSize
            startIndex <- startIndex + minChunkSize
        res

[<Name "arrContains">]
let ArrayContains (item: 'T) (arr: 'T[])  =
    let mutable c = true
    let mutable i = 0
    let l = arr.Length
    while c && i < l do
        if arr.JS.[i] = item then
            c <- false
        else
            i <- i + 1
    not c

[<Name "tryFindBack">]
let ArrayTryFindBack f (arr: _ []) =
    let mutable res = None
    let mutable i = Array.length arr - 1
    while i >= 0 && Option.isNone res do
        let r = arr.JS.[i]
        if f r then res <- Some r
        i <- i - 1
    res

[<Name "tryFindIndexBack">]
let ArrayTryFindIndexBack f (arr: _ []) =
    let mutable res = None
    let mutable i = Array.length arr - 1
    while i >= 0 && Option.isNone res do
        if f arr.[i] then res <- Some i
        i <- i - 1
    res

[<Name "mapFold">]
let ArrayMapFold<'T, 'S, 'R> (f: 'S -> 'T -> 'R * 'S) (zero: 'S) (arr: 'T[]) : 'R[] * 'S =
    let r = JavaScript.Array(Array.length arr)
    let mutable acc = zero
    for i = 0 to Array.length arr - 1 do
        let a, b = f acc arr.JS.[i]
        r.[i] <- a
        acc <- b 
    r.Self, acc

[<Name "mapFoldBack">]
let ArrayMapFoldBack<'T,'S,'R> (f: 'T -> 'S -> 'R * 'S) (arr: 'T[]) (zero: 'S) : 'R[] * 'S =
    let r = JavaScript.Array<'R>(Array.length arr)
    let mutable acc = zero
    let len = Array.length arr
    for j = 1 to len do
        let i = len - j
        let a, b = f arr.JS.[i] acc
        r.[i] <- a
        acc <- b 
    r.Self, acc

[<Name "mapInPlace">]
let mapInPlace (f: 'T1 -> 'T2) (arr: 'T1 []) =
    for i = 0 to Array.length arr - 1 do
        arr.JS.[i] <- As (f arr.JS.[i])

[<Name "mapiInPlace">]
let mapiInPlace (f: int -> 'T1 -> 'T2) (arr: 'T1 []) : 'T2[] =
    for i = 0 to Array.length arr - 1 do
        arr.JS.[i] <- As (f i arr.JS.[i])
    As arr

[<Name "sortInPlaceByDescending">]
let ArraySortInPlaceByDescending<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) =
    (mapiInPlace (fun i x -> x, (f x, i)) arr).JS.Sort(fun (x, y) -> - compare (snd x) (snd y)) |> mapInPlace fst 

[<Name "tryHead">]
let SeqTryHead (s: seq<'T>) =
    use e = Enumerator.Get s
    if e.MoveNext() then Some e.Current else None

[<Name "tryItem">]
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

[<Name "tryLast">]
let SeqTryLast (s: seq<'T>) =
    use e = Enumerator.Get s
    if e.MoveNext() then 
        let mutable c = e.Current
        while e.MoveNext() do
            c <- e.Current
        Some c 
    else None

[<Name "chunkBySize">]
let SeqChunkBySize (size: int) (s: seq<'T>) =
    if size <= 0 then failwith "Chunk size must be positive"
    Enumerable.Of <| fun () ->
        let o = Enumerator.Get s
        Enumerator.NewDisposing true (fun _ -> o.Dispose()) <| fun e ->
            if e.State && o.MoveNext() then
                let res = [|o.Current|]
                while e.State && res.Length < size do
                    if o.MoveNext() then
                        res.JS.Push o.Current |> ignore
                    else 
                        e.State <- false
                e.Current <- res
                true
            else false

[<Name "countBy">]
let ArrayCountBy (f: 'T -> 'K) (a: 'T[]) : ('K * int)[] =
    let d = System.Collections.Generic.Dictionary<'K, int>()
    let keys = JavaScript.Array()
    for i = 0 to a.Length - 1 do
        let c = a.JS.[i]
        let k = f c
        if d.ContainsKey(k) then
            d.[k] <- d.[k] + 1 
        else
            keys.Push(k) |> ignore
            d.Add(k, 1)
    As<'K[]> keys |> mapInPlace (fun k -> (k, d.[k]))
    As keys

[<Name "except">]
let SeqExcept (itemsToExclude: seq<'T>) (s: seq<'T>) =
    Enumerable.Of <| fun () ->
        let o  = Enumerator.Get s
        let seen = System.Collections.Generic.HashSet(itemsToExclude)
        Enumerator.NewDisposing () (fun _ -> o.Dispose()) <| fun e ->
            if o.MoveNext() then
                let mutable cur = o.Current
                let mutable has = seen.Add(cur)
                while not has && o.MoveNext() do
                    cur <- o.Current
                    has <- seen.Add(cur)
                if has then
                    e.Current <- cur
                    true
                else
                    false
            else
                false

[<Name "skip">]
let ListSkip i (l : list<'T>) =
    let mutable res = l
    for j = 1 to i do
        match res with 
        | _ :: t ->
            res <- t
        | [] -> failwith "Input list too short."
    res

[<Name "groupBy">]
let ArrayGroupBy (f: 'T -> 'K when 'K : equality) (a: 'T[]) : ('K * 'T[])[] =
    let d = System.Collections.Generic.Dictionary<'K, 'T[]>()
    let keys = JavaScript.Array()
    for i = 0 to a.Length - 1 do
        let c = a.JS.[i]
        let k = f c
        if d.ContainsKey(k) then
            d.[k].JS.Push(c) |> ignore
        else
            keys.Push(k) |> ignore
            d.Add(k, [| c |])
    As<'K[]> keys |> mapInPlace (fun k -> (k, d.[k]))
    As keys

[<Name "insufficient">]
let InsufficientElements() =
    failwith "The input sequence has an insufficient number of elements."

[<Name "last">]
let SeqLast (s: seq<_>) =
    use e = Enumerator.Get s
    if not <| e.MoveNext() then InsufficientElements()
    else 
        let mutable res = e.Current
        while e.MoveNext() do
            res <- e.Current
        res

[<Name "seqContains">]
let SeqContains (el: 'T) (s: seq<'T>) =
    use e = Enumerator.Get s
    let mutable r = false
    while not r && e.MoveNext() do
        r <- e.Current = el
    r

[<Name "skipWhile">]
let rec ListSkipWhile<'T> (predicate : 'T -> bool) (list : list<'T>) : list<'T> =
    let mutable rest = list
    while not (List.isEmpty rest) && predicate (List.head rest) do
        rest <- List.tail rest 
    rest

[<Name "nonNegative">]
let InputMustBeNonNegative() =
    failwith "The input must be non-negative."

[<Name "transposeArray">]
let ArrayTranspose (array:'T[][]) : 'T[][] =
    let len = array.Length
    if len = 0 then [||] else
    let lenInner = array.[0].Length

    for j in 1..len-1 do
        if lenInner <> array.[j].Length then
            failwith "The arrays have different lengths."

    let result = Array lenInner
    for i in 0..lenInner-1 do
        result.[i] <- Array len
        for j in 0..len-1 do
            result.[i].[j] <- array.[j].[i]
    As result

[<Inline "Math.random()">]
let MathRandom() = System.Random().NextDouble()

[<Name "next">]
let RandomizerNext (randomizer: unit -> float) (maxValue: int) = 
    let value = randomizer()
    if value < 0.0 || value >= 1.0 then
        invalidArg "randomizer" ("Randomizer returned " + value.ToString() + ", should be in range [0.0, 1.0).")
    int (value * float maxValue)

[<Name "randomChoiceBy">]
let ArrayRandomChoiceBy (randomizer: unit -> float) (source: 'T[]) = 
    source[RandomizerNext randomizer source.Length]

[<Name "randomChoice">]
let ArrayRandomChoice (source: 'T[]) = 
    ArrayRandomChoiceBy MathRandom source      

[<Name "randomSampleBy">]
let ArrayRandomSampleBy (randomizer: unit -> float) (count: int) (source: 'T[]) = 
    let inputLength = source.Length
    
    if inputLength = 0 then
        invalidArg "source" "Empty source."

    if count > inputLength then
        invalidArg "count" "Not enough elements."

    let pool = Array.copy source

    let result = Array.create count JS.Undefined

    for i = 0 to count - 1 do
        let j = RandomizerNext randomizer (inputLength - i)
        result[i] <- pool[j]
        pool[j] <- pool[inputLength - i - 1]

    result

[<Name "randomSample">]
let ArrayRandomSample  (count: int) (source: 'T[]) = 
    ArrayRandomSampleBy MathRandom count source      

[<Name "randomShuffleInPlaceBy">]
let ArrayRandomShuffleInPlaceBy (randomizer: unit -> float) (source: 'T[]) =     
    for i = source.Length - 1 downto 0 do
        let j = RandomizerNext randomizer i
        let mutable si = source[i] // TODO why inlined without mutable, possible bug/regression
        source[i] <- source[j]
        source[j] <- si

[<Name "randomShuffleInPlace">]
let ArrayRandomShuffleInPlace (source: 'T[]) =     
    ArrayRandomShuffleInPlaceBy MathRandom source


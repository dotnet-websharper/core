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

[<WebSharper.Name "List">]
[<WebSharper.Proxy
    "Microsoft.FSharp.Collections.ListModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.ListModuleProxy

open WebSharper.JavaScript
open WebSharper.CollectionInternals

let badLengths() =
    failwith "The lists have different lengths."

let listEmpty() =
    failwith "The input list was empty."

[<Inline "$l.$0">]
let unsafeHead (l: list<'T>) = X<'T> 

[<Inline "$l.$1">]
let unsafeTail (l: list<'T>) = X<list<'T>> 

let setValue (l: list<'T>) (v: 'T) =
    JS.Set l "$" 1
    JS.Set l "$0" v

let setTail (l: list<'T>) (t: list<'T>) =
    JS.Set l "$1" t
    t

[<Inline "$l.$ == 1">]
let notEmpty (l: list<_>) = X<bool>

[<Name "append">]
let Append (x: list<'T>) (y: list<'T>) = 
    if List.isEmpty x then y else
    let res = [] : 'T list
    let mutable r = res
    let mutable l = x
    let mutable go = true
    while go do
        setValue r (unsafeHead l)
        l <- unsafeTail l
        if List.isEmpty l then
            go <- false
        else
            r <- setTail r [] 
    setTail r y |> ignore
    res

[<Inline>]
let inline Average (l: list<_>) = Seq.average l

[<Inline>]
let inline AverageBy f (l: list<_>) = Seq.averageBy f l

[<Name "choose">]
let Choose f (l: list<_>) = List.ofSeq (Seq.choose f l)

[<Name "collect">]
let Collect (f: _ -> list<_>) (l: list<_>) = List.ofSeq (Seq.collect f l)

[<Name "concat">]
let Concat (s: seq<list<_>>) = List.ofSeq (Seq.concat s)

[<Inline>]
let Empty<'T> : list<'T> = []

[<Inline>]
let Exists<'T> (p: 'T -> bool) (l: list<'T>) = Seq.exists p l

[<Name "exists2">]
let Exists2<'T1,'T2> (p : 'T1 -> 'T2 -> bool)
                        (l1: list<'T1>)
                        (l2: list<'T2>) =
    Seq.exists2 p (Seq.ofList l1) (Seq.ofList l2)

[<Name "filter">]
let Filter<'T> (p: 'T -> bool) (l: list<'T>) =
    List.ofSeq (Seq.filter p l)

[<Inline>]
let Find p (l: list<_>) = Seq.find p l

[<Inline>]
let FindIndex p (l: list<_>) = Seq.findIndex p l

[<Inline>]
let Fold<'T,'S> (f: 'S -> 'T -> 'S) (s: 'S) (l: list<'T>) : 'S =
    Seq.fold f s l

[<Name "fold2">]
let Fold2<'T1,'T2,'S> (f: 'S -> 'T1 -> 'T2 -> 'S)
                        (s: 'S)
                        (l1: list<'T1>)
                        (l2: list<'T2>) : 'S =
    Array.fold2 f s (Array.ofList l1) (Array.ofList l2)

[<Name "foldBack">]
let FoldBack f (l: list<_>) s =
    Array.foldBack f (Array.ofList l) s

[<Name "foldBack2">]
let FoldBack2 f (l1: list<_>) (l2: list<_>) s =
    Array.foldBack2 f (Array.ofList l1) (Array.ofList l2) s

[<Inline>]
let ForAll p (l: list<_>) = Seq.forall p l

[<Name "forall2">]
let ForAll2 p (l1: list<_>) (l2: list<_>) =
    Array.forall2 p (Array.ofList l1) (Array.ofList l2)

[<Name "head">]
let Head (l: list<'T>) =
    match l with 
    | h :: _ -> h
    | _ -> listEmpty()

[<Name "init">]
let Initialize s f = List.ofArray (Array.init s f)

[<Inline "$l.$ == 0">]
let IsEmpty (l: list<_>) = X<bool>

[<Name "iter">]
let Iterate f (l: list<_>) =
    let mutable r = l
    while notEmpty r do
        f r.Head
        r <- r.Tail

[<Name "iter2">]
let Iterate2 f (l1: list<_>) (l2: list<_>) =
    let mutable r1 = l1
    let mutable r2 = l2
    while notEmpty r1 do
        if List.isEmpty r2 then
            badLengths()
        f r1.Head r2.Head
        r1 <- r1.Tail
        r2 <- r2.Tail
    if notEmpty r2 then
        badLengths()

[<Name "iteri">]
let IterateIndexed f (l: list<_>) =
    let mutable r = l
    let mutable i = 0
    while notEmpty r do
        f i r.Head
        r <- r.Tail
        i <- i + 1

[<Name "iteri2">]
let IterateIndexed2 f (l1: list<_>) (l2: list<_>) =
    let mutable r1 = l1
    let mutable r2 = l2
    let mutable i = 0
    while notEmpty r1 do
        if List.isEmpty r2 then
            badLengths()
        f i r1.Head r2.Head
        r1 <- r1.Tail
        r2 <- r2.Tail
        i <- i + 1
    if notEmpty r2 then
        badLengths()

[<Inline>]
let Length (l: list<_>) =
    let mutable r = l
    let mutable i = 0
    while notEmpty r do
        r <- r.Tail
        i <- i + 1
    i

[<Name "map">]
let Map (f: 'T1 -> 'T2) (x: list<'T1>) = 
    let res = [] : list<'T2>
    let mutable r = res
    let mutable l = x
    while notEmpty l do
        setValue r (f (unsafeHead l))
        r <- setTail r []
        l <- unsafeTail l
    res

[<Name "map2">]
let Map2 (f: 'T1 -> 'T2 -> 'T3) (x1: list<'T1>) (x2: list<'T2>) =
    let res = [] : list<'T3>
    let mutable r = res
    let mutable l1 = x1
    let mutable l2 = x2
    while notEmpty l1 && notEmpty l2 do
        setValue r (f (unsafeHead l1) (unsafeHead l2))
        r <- setTail r []
        l1 <- unsafeTail l1
        l2 <- unsafeTail l2
    if notEmpty l1 || notEmpty l2 then
        badLengths()
    res

[<Inline>]
let Map3 (f: 'T1 -> 'T2 -> 'T3 -> 'T4) (x1: list<'T1>) (x2: list<'T2>) (x3: list<'T3>) =
    let res = [] : list<'T4> 
    let mutable r = res
    let mutable l1 = x1
    let mutable l2 = x2
    let mutable l3 = x3
    while notEmpty l1 && notEmpty l2 && notEmpty l3 do
        setValue r (f (unsafeHead l1) (unsafeHead l2) (unsafeHead l3))
        r <- setTail r []
        l1 <- unsafeTail l1
        l2 <- unsafeTail l2
        l3 <- unsafeTail l3
    if notEmpty l1 || notEmpty l2 || notEmpty l3 then
        badLengths()
    res

[<Name "mapi">]
let MapIndexed (f: int -> 'T1 -> 'T2) (x: list<'T1>) =
    let res = [] : list<'T2>
    let mutable r = res
    let mutable l = x
    let mutable i = 0
    while notEmpty l do
        setValue r (f i (unsafeHead l))
        r <- setTail r []
        l <- unsafeTail l
        i <- i + 1
    res

[<Name "mapi2">]
let MapIndexed2 (f: int -> 'T1 -> 'T2 -> 'T3) (x1: list<'T1>) (x2: list<'T2>) =
    let res = [] : list<'T3>
    let mutable r = res
    let mutable l1 = x1
    let mutable l2 = x2
    let mutable i = 0
    while notEmpty l1 && notEmpty l2 do
        setValue r (f i (unsafeHead l1) (unsafeHead l2))
        r <- setTail r []
        l1 <- unsafeTail l1
        l2 <- unsafeTail l2
        i <- i + 1
    if notEmpty l1 || notEmpty l2 then
        badLengths()
    res

[<Name "max">]
let Max (l: list<_>) = Seq.reduce max l

[<Name "maxBy">]
let MaxBy f (l: list<_>) =
    Seq.reduce (fun x y -> if f x > f y then x else y) l

[<Name "min">]
let Min (l: list<_>) = Seq.reduce min l

[<Name "minBy">]
let MinBy f (l: list<_>) =
    Seq.reduce (fun x y -> if f x < f y then x else y) l

[<Inline>]
let Get (l: list<_>) ix = Seq.nth ix l

[<Inline>]
let Item ix (l: list<_>) = Seq.nth ix l

[<Name "ofArray">]
let OfArray<'T> (arr: 'T []) =
    let mutable r = []
    for i = arr.Length - 1 downto 0 do
        r <- arr.[i] :: r
    r

[<Name "ofSeq">]
let OfSeq (s: seq<'T>) =
    if s :? _ list then
        As<'T list> s
    elif s :? System.Array then
        List.ofArray (As<'T[]> s)
    else
        let res = [] : list<'T>
        let mutable last = res
        use e = Enumerator.Get s
        while e.MoveNext() do
            setValue last e.Current
            last <- setTail last []
        JS.Set last "$" 0
        res

[<Name "partition">]
let Partition p (l: list<_>) =
    let (a, b) = Array.partition p (Array.ofList l)
    (List.ofArray a, List.ofArray b)

[<Name "permute">]
let Permute f (l: list<_>) =
    List.ofArray (Array.permute f (Array.ofList l))

[<Inline>]
let Pick f (l: list<_>) = Seq.pick f l

[<Inline>]
let Reduce (f: 'T -> 'T -> 'T) (list: list<'T>) : 'T =
    Seq.reduce f list

[<Name "reduceBack">]
let ReduceBack f (l: list<_>) =
    Array.reduceBack f (Array.ofList l)

[<Name "replicate">]
let Replicate size value =
    List.ofArray (Array.create size value)

[<Name "rev">]
let Reverse (l: list<'T>) =
    let mutable res = []
    let mutable r = l
    while notEmpty r do
        res <- unsafeHead r :: res
        r <- unsafeTail r
    res

[<Name "scan">]
let Scan<'T,'S> (f: 'S -> 'T -> 'S) (s: 'S) (l: list<'T>) : list<'S> =
    List.ofSeq (Seq.scan f s l)

[<Name "scanBack">]
let ScanBack f (l: list<_>) s =
    List.ofArray (Array.scanBack f (Array.ofList l) s)

[<Name "sort">]
let Sort (l: list<_>) =
    let a = Array.ofList l
    Array.sortInPlace a
    List.ofArray a

[<Name "sortBy">]
let SortBy f (l: list<_>) =
    List.sortWith (fun x y -> compare (f x) (f y)) l

[<Name "sortByDescending">]
let SortByDescending f (l: list<_>) =
    List.sortWith (fun x y -> - compare (f x) (f y)) l

[<Name "sortDescending">]
let SortDescending (l: list<_>) =
    let a = Array.ofList l
    ArraySortInPlaceByDescending id a
    List.ofArray a

[<Name "sortWith">]
let SortWith f (l: list<_>) =
    let a = Array.ofList l
    Array.sortInPlaceWith f a
    List.ofArray a

[<Inline>]
let inline Sum (l: list<'T>) : 'T = Seq.sum l

[<Inline>]
let inline SumBy (f: 'T -> 'U) (l: list<'T>) : 'U = Seq.sumBy f l

[<Name "tail">]
let Tail (l: list<'T>) = 
    match l with 
    | _ :: t -> t
    | _ -> listEmpty()

[<Inline>]
let ToArray (l: list<_>) = Array.ofList l

[<Inline "$x">]
let ToSeq<'T> (x: list<'T>) : seq<'T> = x :> _

[<Inline>]
let TryFind p (l: list<_>) = Seq.tryFind p l

[<Inline>]
let TryFindIndex p (l: list<_>) = Seq.tryFindIndex p l

[<Inline>]
let TryPick p (l: list<_>) = Seq.tryPick p l

[<Name "unzip">]
let Unzip (l: list<_>) =
    let x = System.Collections.Generic.Queue<_>()
    let y = System.Collections.Generic.Queue<_>()
    for (a, b) in l do
        x.Enqueue a
        y.Enqueue b
    (List.ofArray (x.ToArray()), List.ofArray (y.ToArray()))

[<Name "unzip3">]
let Unzip3 (l: list<_>) =
    let x = System.Collections.Generic.Queue<_>()
    let y = System.Collections.Generic.Queue<_>()
    let z = System.Collections.Generic.Queue<_>()
    for (a, b, c) in l do
        x.Enqueue a
        y.Enqueue b
        z.Enqueue c
    (
        List.ofArray (x.ToArray()),
        List.ofArray (y.ToArray()),
        List.ofArray (z.ToArray())
    )

[<Name "zip">]
let Zip (l1: list<_>) (l2: list<_>) =
    List.ofArray (Array.zip (Array.ofList l1) (Array.ofList l2))

[<Name "zip3">]
let Zip3 (l1: list<_>) (l2: list<_>) (l3: list<_>) =
    List.ofArray (Array.zip3 (Array.ofList l1)
        (Array.ofList l2) (Array.ofList l3))

[<Name "chunkBySize">]
let ChunkBySize size list =
    SeqChunkBySize size (List.toSeq list)
    |> Seq.toList
    |> List.map Array.toList

[<Name "compareWith">]
let CompareWith  (f: 'T -> 'T -> int) (l1: list<'T>) (l2: list<'T>) : int =
    SeqCompareWith f (List.toSeq l1) (List.toSeq l2)

[<Name "countBy">]
let CountBy (f: 'T -> 'K) (l: list<'T>) : list<'K * int> =
    SeqCountBy f (List.toSeq l)
    |> Seq.toList

[<Name "distinct">]
let Distinct<'T when 'T : equality> (l: list<'T>) : list<'T> =
    SeqDistinct (List.toSeq l)
    |> Seq.toList

[<Name "distinctBy">]
let DistinctBy<'T,'K when 'K : equality>
        (f: 'T -> 'K) (l: list<'T>) : list<'T> =
    SeqDistinctBy f (List.toSeq l)
    |> Seq.toList

[<Name "splitInto">]
let SplitInto count (list: list<'T>) =
    ArraySplitInto count (List.toArray list)
    |> Array.toList
    |> List.map Array.toList

[<Name "except">]
let Except (itemsToExclude: seq<'T>) (l: list<'T>) =
    SeqExcept itemsToExclude (List.toSeq l)
    |> Seq.toList

[<Name "tryFindBack">]
let TryFindBack ok (l: list<_>) =
    ArrayTryFindBack ok (Array.ofList l)

[<Name "findBack">]
let FindBack p (s: list<_>) =
    match TryFindBack p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Inline>]
let TryFindIndexBack ok (l: list<_>) =
    ArrayTryFindIndexBack ok (Array.ofList l) 

[<Name "findIndexBack">]
let FindIndexBack p (s: list<_>) =
    match TryFindIndexBack p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Name "groupBy">]
let GroupBy (f: 'T -> 'K when 'K : equality)
            (l: list<'T>) : list<'K * list<'T>> =
    SeqGroupBy f (List.toSeq l)
    |> Seq.toList
    |> List.map (fun (k, s) ->
        (k, Seq.toList s)
    )

[<Name "last">]
let Last (list : list<'T>) : 'T =
    if List.isEmpty list then
        listEmpty()
    let mutable r = list
    let mutable t = unsafeTail r
    while notEmpty t do
        r <- t
        t <- unsafeTail r
    unsafeHead r

[<Name "contains">]
let Contains (el: 'T) (l: list<'T>) =
    SeqContains el (List.toSeq l)

[<Name "mapFold">]
let MapFold<'T, 'S, 'R> (f: 'S -> 'T -> 'R * 'S) zero list =
    ArrayMapFold f zero (List.toArray list)
    |> (fun (x, y) ->
        (Array.toList x, y)
    )

[<Name "mapFoldBack">]
let MapFoldBack<'T, 'S, 'R> f list zero =
    ArrayMapFoldBack<'T, 'S, 'R> f (List.toArray list) zero
    |> (fun (x, y) ->
        (Array.toList x, y)
    )

[<Name "pairwise">]
let Pairwise (l: list<'T>) : list<'T * 'T> =
    SeqPairwise (List.toSeq l)
    |> Seq.toList

[<Name "indexed">]
let Indexed (list : list<'T>) : list<int * 'T> =
    List.mapi (fun a b -> (a, b)) list

[<Name "singleton">]
let Singleton<'T> (x: 'T) =
    [x]

[<Inline>]
let Skip<'T> i (l : list<'T>) = ListSkip i l

[<Inline>]
let SkipWhile<'T> (predicate : 'T -> bool) (list : list<'T>) : list<'T> =
    ListSkipWhile predicate list

[<Inline>]
let Take<'T> n (list: list<'T>) =
    Seq.take n list |> List.ofSeq

[<Inline>]
let TakeWhile<'T> (predicate : 'T -> bool) (list: list<'T>) =
    Seq.takeWhile predicate list |> List.ofSeq

[<Inline>]
let Truncate<'T> n (list: list<'T>) =
    Seq.truncate n list |> List.ofSeq

[<Name "tryHead">]
let TryHead<'T> (list: list<'T>) =
    match list with
    | head :: _ ->
        Some head
    | [] ->
        None

[<Name "tryItem">]
let rec TryItem<'T> n (list: list<'T>) =
    SeqTryItem n list 

[<Name "tryLast">]
let TryLast<'T> (list: list<'T>) =
    SeqTryLast list

[<Name "exactlyOne">]
let ExactlyOne (list : 'T list) =
    match list with
    | head :: [] ->
        head
    | _ ->
        failwith "The input does not have precisely one element."

[<Name "unfold">]
let Unfold<'T, 'S> (f: 'S -> option<'T * 'S>) (s: 'S) : list<'T> =
    SeqUnfold f s
    |> Seq.toList

[<Inline>]
let Where (predicate : 'T -> bool) (s : 'T list) : 'T list =
    Filter predicate s

[<Name "windowed">]
let Windowed (windowSize: int) (s: 'T list) : list<list<'T>> =
    SeqWindowed windowSize (List.toSeq s)
    |> Seq.map List.ofArray |> Seq.toList

[<Name "splitAt">]
let SplitAt (n: int) (list: 'T list) =
    (Take n list, Skip n list)

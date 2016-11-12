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

[<Inline>]
let setValue (l: list<'T>) (v: 'T) =
    JS.Set l "$0" v

[<Inline>]
let setTail (l: list<'T>) (t: list<'T>) =
    JS.Set l "$1" t

[<Inline "new WebSharper.List.T({$: 1})"; Pure>]
let freshEmptyList() = X<list<'T>>

[<Inline>]
let freshTail (l: list<'T>) =
    let t = freshEmptyList()
    setTail l t
    t

[<Inline "$l.$ == 1">]
let notEmpty (l: list<_>) = X<bool>

[<Name "append">]
let Append (x: list<'T>) (y: list<'T>) = 
    if List.isEmpty x then y
    elif List.isEmpty y then x else
    let res = freshEmptyList()
    let mutable r = res
    let mutable l = x
    let mutable go = true
    while go do
        setValue r (unsafeHead l)
        l <- unsafeTail l
        if List.isEmpty l then
            go <- false
        else
            r <- freshTail r
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

[<Name "exists">]
let Exists<'T> (p: 'T -> bool) (x: list<'T>) =
    let mutable e = false
    let mutable l = x
    while not e && notEmpty l do
        e <- p (unsafeHead l)
        l <- unsafeTail l
    e

[<Name "exists2">]
let Exists2<'T1,'T2> (p : 'T1 -> 'T2 -> bool)
                        (x1: list<'T1>)
                        (x2: list<'T2>) =
    let mutable e = false
    let mutable l1 = x1
    let mutable l2 = x2
    while not e && notEmpty l1 && notEmpty l2 do
        e <- p (unsafeHead l1) (unsafeHead l2)
        l1 <- unsafeTail l1
        l2 <- unsafeTail l2
    if not e && (notEmpty l1 || notEmpty l2) then
        badLengths()
    e

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

[<Name "forAll">]
let ForAll p (x: list<_>) =
    let mutable a = true
    let mutable l = x
    while a && notEmpty l do
        a <- p (unsafeHead l)
        l <- unsafeTail l
    a

[<Name "forall2">]
let ForAll2 p (x1: list<_>) (x2: list<_>) =
    let mutable a = true
    let mutable l1 = x1
    let mutable l2 = x2
    while a && notEmpty l1 && notEmpty l2 do
        a <- p (unsafeHead l1) (unsafeHead l2)
        l1 <- unsafeTail l1
        l2 <- unsafeTail l2
    if a && (notEmpty l1 || notEmpty l2) then
        badLengths()
    a

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

[<Name "length">]
let Length (l: list<_>) =
    let mutable r = l
    let mutable i = 0
    while notEmpty r do
        r <- r.Tail
        i <- i + 1
    i

[<Name "map">]
let Map (f: 'T1 -> 'T2) (x: list<'T1>) = 
    if List.isEmpty x then As x else
    let res = freshEmptyList()
    let mutable r = res
    let mutable l = x
    let mutable go = true
    while go do
        setValue r (f (unsafeHead l))
        l <- unsafeTail l
        if List.isEmpty l then
            go <- false
        else
            r <- freshTail r
    setTail r []
    res

[<Name "map2">]
let Map2 (f: 'T1 -> 'T2 -> 'T3) (x1: list<'T1>) (x2: list<'T2>) =
    let mutable go = notEmpty x1 && notEmpty x2
    if not go then 
        if notEmpty x1 || notEmpty x2 then
            badLengths()
        else As x1
    else
    let res = freshEmptyList()
    let mutable r = res
    let mutable l1 = x1
    let mutable l2 = x2
    while go do
        setValue r (f (unsafeHead l1) (unsafeHead l2))
        l1 <- unsafeTail l1
        l2 <- unsafeTail l2
        if notEmpty l1 && notEmpty l2 then
            r <- freshTail r
        else 
            go <- false
    if notEmpty l1 || notEmpty l2 then
        badLengths()
    setTail r []
    res

[<Name "map3">]
let Map3 (f: 'T1 -> 'T2 -> 'T3 -> 'T4) (x1: list<'T1>) (x2: list<'T2>) (x3: list<'T3>) =
    let mutable go = notEmpty x1 && notEmpty x2 && notEmpty x3
    if not go then 
        if notEmpty x1 || notEmpty x2 || notEmpty x3 then
            badLengths()
        else As x1
    else
    let res = freshEmptyList()
    let mutable r = res
    let mutable l1 = x1
    let mutable l2 = x2
    let mutable l3 = x3
    while go do
        setValue r (f (unsafeHead l1) (unsafeHead l2) (unsafeHead l3))
        l1 <- unsafeTail l1
        l2 <- unsafeTail l2
        l3 <- unsafeTail l3
        if notEmpty l1 && notEmpty l2 && notEmpty l3 then
            r <- freshTail r
        else 
            go <- false
    if notEmpty l1 || notEmpty l2 || notEmpty l3 then
        badLengths()
    setTail r []
    res

[<Name "mapi">]
let MapIndexed (f: int -> 'T1 -> 'T2) (x: list<'T1>) =
    if List.isEmpty x then As x else
    let res = freshEmptyList()
    let mutable r = res
    let mutable l = x
    let mutable i = 0
    let mutable go = true
    while go do
        setValue r (f i (unsafeHead l))
        l <- unsafeTail l
        if List.isEmpty l then 
            go <- false
        else
            r <- freshTail r
            i <- i + 1
    setTail r []
    res

[<Name "mapi2">]
let MapIndexed2 (f: int -> 'T1 -> 'T2 -> 'T3) (x1: list<'T1>) (x2: list<'T2>) =
    let mutable go = notEmpty x1 && notEmpty x2
    if not go then 
        if notEmpty x1 || notEmpty x2 then
            badLengths()
        else As x1
    else
    let res = freshEmptyList()
    let mutable r = res
    let mutable l1 = x1
    let mutable l2 = x2
    let mutable i = 0
    while go do
        setValue r (f i (unsafeHead l1) (unsafeHead l2))
        l1 <- unsafeTail l1
        l2 <- unsafeTail l2
        if notEmpty l1 && notEmpty l2 then
            r <- freshTail r
            i <- i + 1
        else 
            go <- false
    if notEmpty l1 || notEmpty l2 then
        badLengths()
    setTail r []
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
        use e = Enumerator.Get s
        let mutable go = e.MoveNext()
        if not go then [] else
        let res = freshEmptyList()
        let mutable r = res
        while go do
            setValue r e.Current
            if e.MoveNext() then
                r <- freshTail r
            else    
                go <- false
        setTail r []
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
    List.map2 (fun x y -> x, y) l1 l2

[<Name "zip3">]
let Zip3 (l1: list<_>) (l2: list<_>) (l3: list<_>) =
    Map3 (fun x y z -> (x, y, z)) l1 l2 l3

[<Name "chunkBySize">]
let ChunkBySize size list =
    SeqChunkBySize size (List.toSeq list)
    |> Seq.toList
    |> List.map Array.toList

[<Name "compareWith">]
let CompareWith  (f: 'T -> 'T -> int) (l1: list<'T>) (l2: list<'T>) : int =
    Seq.compareWith f (List.toSeq l1) (List.toSeq l2)

[<Name "countBy">]
let CountBy (f: 'T -> 'K) (l: list<'T>) : list<'K * int> =
    ArrayCountBy f (List.toArray l)
    |> Array.toList

[<Name "distinct">]
let Distinct<'T when 'T : equality> (l: list<'T>) : list<'T> =
    Seq.distinct (List.toSeq l)
    |> Seq.toList

[<Name "distinctBy">]
let DistinctBy<'T,'K when 'K : equality>
        (f: 'T -> 'K) (l: list<'T>) : list<'T> =
    Seq.distinctBy f (List.toSeq l)
    |> Seq.toList

[<Name "splitInto">]
let SplitInto count (list: list<'T>) =
    ArraySplitInto count (List.toArray list)
    |> Array.toList
    |> List.map Array.toList

[<Name "except">]
let Except (itemsToExclude: seq<'T>) (l: list<'T>) =
    SeqExcept itemsToExclude l
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
    let arr = ArrayGroupBy f (List.toArray l)
    arr |> mapInPlace (fun (k, s) -> (k, Array.toList s))
    Array.toList (As arr)

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
let Contains (el: 'T) (x: list<'T>) =
    let mutable c = false
    let mutable l = x
    while not c && notEmpty l do
        c <- el = unsafeHead l
        l <- unsafeTail l
    c

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
    Seq.pairwise (List.toSeq l)
    |> Seq.toList

[<Name "indexed">]
let Indexed (list : list<'T>) : list<int * 'T> =
    List.mapi (fun a b -> (a, b)) list

[<Inline>]
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

[<Inline>]
let TryItem<'T> n (list: list<'T>) =
    SeqTryItem n list 

[<Inline>]
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
    Seq.unfold f s
    |> Seq.toList

[<Inline>]
let Where (predicate : 'T -> bool) (s : 'T list) : 'T list =
    Filter predicate s

[<Name "windowed">]
let Windowed (windowSize: int) (s: 'T list) : list<list<'T>> =
    Seq.windowed windowSize (List.toSeq s)
    |> Seq.map List.ofArray |> Seq.toList

[<Name "splitAt">]
let SplitAt (n: int) (list: 'T list) =
    (Take n list, Skip n list)

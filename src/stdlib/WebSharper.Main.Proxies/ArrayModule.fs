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

[<WebSharper.Name "Arrays">]
[<WebSharper.Proxy
    "Microsoft.FSharp.Collections.ArrayModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.ArrayModuleProxy

open WebSharper.JavaScript
open WebSharper.CollectionInternals

module F = WebSharper.IntrinsicFunctionProxy

let checkLength (arr1: 'T1[]) (arr2: 'T2[]) =
    if Array.length arr1 <> Array.length arr2 then
        failwith "The arrays have different lengths."

[<Inline "$x.push($y)">]
let push (x: obj) (y: obj) = ()

[<Inline "$arr1.concat($arr2)">]
let Append<'T> (arr1: 'T []) (arr2: 'T []) : 'T [] = arr1

[<Name "allPairs">]
let AllPairs (array1: 'T1 []) (array2: 'T2 []) =
    let len1 = Array.length array1
    let len2 = Array.length array2
    let res = JavaScript.Array (len1 * len2)
    for i = 0 to len1-1 do
        for j = 0 to len2-1 do
            res.[i * len2 + j] <- (array1.JS.[i],array2.JS.[j])
    res |> As<('T1 * 'T2) []>

[<Name "average">]
let inline Average (arr: 'T []): 'T = As (float (Array.sum arr) / float (Array.length arr))

[<Name "averageBy">]
let inline AverageBy (f: 'T -> 'U) (arr: 'T []) : 'U = As (float (Array.sumBy f arr) / float (Array.length arr))

[<Name "blit">]
let CopyTo<'T> (arr1: 'T [], start1, arr2: 'T [], start2, length) =
    F.checkRange arr1 start1 length
    F.checkRange arr2 start2 length
    for i = 0 to length - 1 do
        arr2.JS.[start2 + i] <- arr1.JS.[start1 + i]

[<Name "choose">]
let Choose<'T,'U> (f: 'T -> option<'U>) (arr: 'T []) : 'U [] =
    let q : 'U [] = [||]
    for i = 0 to Array.length arr - 1 do
        match f arr.JS.[i] with
        | Some x -> push q x
        | None   -> ()
    q

[<Inline "Array.prototype.concat.apply([], $x)"; Pure>]
let concatArray (x: 'T[][]) = X<'T[]>

[<Name "collect">]
let Collect<'T,'U> (f: 'T -> 'U[]) (x: 'T []) : 'U[] =
    concatArray (Array.map f x)

[<Name "concat">]
let Concat<'T> (xs: seq<'T []>) : 'T [] =
    concatArray (Array.ofSeq xs)

[<Inline>]
let SplitInto count (arr: 'T[]) = ArraySplitInto count arr

[<Inline "$x.slice()">]
let Copy (x: 'T []) = X<'T []>

[<Name "create">]
let Create (size: int) value =
    let r = JavaScript.Array(size)
    for i = 0 to size - 1 do
        r.[i] <- value
    r.Self

[<Inline "[]">]
let Empty () = X<'T []>

[<Name "exists">]
let Exists<'T> (f: 'T -> bool) (x: 'T []) =
    let mutable e = false
    let mutable i = 0
    let l = x.Length
    while not e && i < l do
        if f x.JS.[i] then
            e <- true
        else
            i <- i + 1
    e

[<Name "exists2">]
let Exists2 f (x1: _ []) (x2: _ []) =
    checkLength x1 x2
    let mutable e = false
    let mutable i = 0
    let l = x1.Length
    while not e && i < l do
        if f x1.JS.[i] x2.JS.[i] then
            e <- true
        else
            i <- i + 1

    e

[<Name "fill">]
let Fill<'T> (arr: 'T []) (start: int) (length: int) (value: 'T) =
    F.checkRange arr start length
    for i = start to start + length - 1 do
        arr.JS.[i] <- value

[<Name "filter">]
let Filter<'T> f (arr: 'T []) : 'T [] =
    let r : 'T [] = [||]
    for i = 0 to Array.length arr - 1 do
        if f arr.JS.[i] then
            push r arr.JS.[i]
    r

[<Name "find">]
let Find f (arr: _ []) =
    match Array.tryFind f arr with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Name "findIndex">]
let FindIndex f (arr: _ []) =
    match Array.tryFindIndex f arr with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Name "fold">]
let Fold<'T,'S> (f: 'S -> 'T -> 'S) (zero: 'S) (arr: 'T []) : 'S =
    let mutable acc = zero
    for i = 0 to Array.length arr - 1 do
        acc <- f acc arr.JS.[i]
    acc

[<Name "fold2">]
let Fold2<'T1,'T2,'S> f (zero: 'S) (arr1: 'T1 []) (arr2: 'T2 []) : 'S =
    checkLength arr1 arr2
    let mutable accum = zero
    for i in 0 .. Array.length arr1 - 1 do
        accum <- f accum arr1.JS.[i] arr2.JS.[i]
    accum

[<Name "foldBack">]
let FoldBack f (arr: _ []) zero =
    let mutable acc = zero
    let len = Array.length arr
    for i = 1 to len do
        acc <- f arr.JS.[len - i] acc
    acc

[<Name "foldBack2">]
let FoldBack2 f (arr1: _ []) (arr2: _ []) zero =
    checkLength arr1 arr2
    let len = Array.length arr1
    let mutable accum = zero
    for i in 1 .. len do
        accum <- f arr1.JS.[len - i] arr2.JS.[len - i] accum
    accum

[<Name "forall">]
let ForAll f (x: _ []) =
    let mutable a = true
    let mutable i = 0
    let l = x.Length
    while a && i < l do
        if f x.JS.[i] then
            i <- i + 1
        else
            a <- false
    a

[<Name "forall2">]
let ForAll2 f (x1: _ []) (x2: _ []) =
    checkLength x1 x2
    let mutable a = true
    let mutable i = 0
    let l = x1.Length
    while a && i < l do
        if f x1.JS.[i] x2.JS.[i] then
            i <- i + 1
        else
            a <- false
    a

[<Inline>]
let Get (arr: _ []) index =
    F.GetArray arr index

[<Inline>]
let Item index (arr: _ []) =
    F.GetArray arr index

[<Name "init">]
let Initialize (size: int) f =
    if size < 0 then
        failwith "Negative size given."
    let r = JavaScript.Array(size)
    for i = 0 to size - 1 do
        r.[i] <- f i
    r.Self

[<Inline "$arr.length == 0">]
let IsEmpty (arr: _ []) = X<bool>

[<Name "iter">]
let Iterate f (arr: 'T []) =
    for i = 0 to Array.length arr - 1 do
        f arr.JS.[i]

[<Name "iter2">]
let Iterate2 f (arr1: _ []) (arr2: _ []) =
    checkLength arr1 arr2
    for i = 0 to Array.length arr1 - 1 do
        f arr1.JS.[i] arr2.JS.[i]

[<Name "iteri">]
let IterateIndexed f (arr: 'T []) =
    for i = 0 to Array.length arr - 1 do
        f i arr.JS.[i]

[<Name "iteri2">]
let IterateIndexed2 f (arr1: _ []) (arr2: _ []) =
    checkLength arr1 arr2
    for i = 0 to Array.length arr1 - 1 do
        f i arr1.JS.[i] arr2.JS.[i]

[<Inline "$arr.length">]
let Length<'T> (arr: 'T []) = X<int>

[<Name "map">]
let Map<'T1,'T2> (f: 'T1 -> 'T2) (arr: 'T1 []) : 'T2 [] =
    let r = JavaScript.Array<'T2>(Array.length arr)
    for i = 0 to Array.length arr - 1 do
        r.[i] <- f arr.JS.[i]
    r.Self

[<Name "map2">]
let Map2 (f: 'T1 -> 'T2 -> 'T3) (arr1: 'T1 []) (arr2: 'T2 []) : 'T3 [] =
    checkLength arr1 arr2
    let r = JavaScript.Array<'T3>(Array.length arr2)
    for i = 0 to Array.length arr2 - 1 do
        r.[i] <- f arr1.JS.[i] arr2.JS.[i]
    r.Self

[<Name "mapi">]
let MapIndexed f (arr: _ []) =
    let y = JavaScript.Array(Array.length arr)
    for i = 0 to Array.length arr - 1 do
        y.[i] <- f i arr.JS.[i]
    y.Self

[<Name "mapi2">]
let MapIndexed2 f (arr1: 'T1 []) (arr2: 'T2 []): 'U[] =
    checkLength arr1 arr2
    let res = JavaScript.Array(Array.length arr1)
    for i = 0 to Array.length arr1 - 1 do
        res.[i] <- f i arr1.JS.[i] arr2.JS.[i]
    res.Self

[<Inline>]
let MapFold<'T,'S,'R> f zero arr = ArrayMapFold<'T, 'S, 'R> f zero arr

[<Inline>]
let MapFoldBack f arr zero = ArrayMapFoldBack f arr zero

let private nonEmpty (arr: _ []) =
    if Array.length arr = 0 then
        failwith "The input array was empty."

[<Name "max">]
let Max arr =
    nonEmpty arr
    let mutable m = arr.JS.[0]
    for i = 1 to Array.length arr - 1 do
        let x = arr.JS.[i]
        if x > m then
            m <- x
    m

[<Name "maxBy">]
let MaxBy f arr =
    nonEmpty arr
    let mutable m = arr.JS.[0]
    let mutable fm = f m
    for i = 1 to Array.length arr - 1 do
        let x = arr.JS.[i]
        let fx = f x
        if fx > fm then
            m <- x
            fm <- fx
    m

[<Name "min">]
let Min arr =
    nonEmpty arr
    let mutable m = arr.JS.[0]
    for i = 1 to Array.length arr - 1 do
        let x = arr.JS.[i]
        if x < m then
            m <- x
    m


[<Name "minBy">]
let MinBy f arr =
    nonEmpty arr
    let mutable m = arr.JS.[0]
    let mutable fm = f m
    for i = 1 to Array.length arr - 1 do
        let x = arr.JS.[i]
        let fx = f x
        if fx < fm then
            m <- x
            fm <- fx
    m

[<Name "ofList">]
let OfList<'T> (xs: list<'T>) =
    let q : 'T [] = [||]
    let mutable l = xs
    while not (List.isEmpty l) do
        push q l.Head
        l <- l.Tail
    q

[<Name "ofSeq">]
let OfSeq<'T> (xs: seq<'T>) : 'T [] =
    if xs :? System.Array then
        Array.copy (As<'T[]> xs)
    elif xs :? _ list then
        Array.ofList (As<'T list> xs)
    else
        let q : 'T [] = [||]
        use o = Enumerator.Get xs
        while o.MoveNext() do
            push q o.Current
        q

[<Name "partition">]
let Partition f (arr: 'T []) : 'T [] * 'T [] =
    let ret1 : 'T [] = [||]
    let ret2 : 'T [] = [||]
    for i = 0 to Array.length arr - 1 do
        if f arr.JS.[i] then
            push ret1 arr.JS.[i]
        else
            push ret2 arr.JS.[i]
    (ret1, ret2)

[<Name "permute">]
let Permute f (arr: 'T []) =
    let ret = JavaScript.Array(Array.length arr)
    for i = 0 to Array.length arr - 1 do
        ret.[f i] <- arr.JS.[i]
    ret.Self

[<Name "pick">]
let Pick f (arr: _ []) =
    match Array.tryPick f arr with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Name "reduce">]
let Reduce f (arr: _ []) =
    nonEmpty arr
    let mutable acc = arr.JS.[0]
    for i = 1 to Array.length arr - 1 do
        acc <- f acc arr.JS.[i]
    acc

[<Name "reduceBack">]
let ReduceBack f (arr: _ []) =
    nonEmpty arr
    let len = Array.length arr
    let mutable acc = arr.JS.[len - 1]
    for i = 2 to len do
        acc <- f arr.JS.[len - i] acc
    acc

[<Inline "$x.slice().reverse()">]
let Reverse (x: 'T []) = X<'T []>

[<Name "scan">]
let Scan<'T,'S> (f: 'S -> 'T -> 'S) (zero: 'S) (arr: 'T []) : 'S [] =
    let ret = JavaScript.Array(1 + Array.length arr)
    ret.[0] <- zero
    for i = 0 to Array.length arr - 1 do
        ret.[i + 1] <- f ret.[i] arr.JS.[i]
    ret.Self

[<Name "scanBack">]
let ScanBack (f: 'T -> 'S -> 'S) (arr: 'T []) (zero: 'S) : 'S [] =
    let len = Array.length arr
    let ret = JavaScript.Array(1 + len)
    ret.[len] <- zero
    for i = 0 to len - 1 do
        ret.[len - i - 1] <- f arr.JS.[len - i - 1] ret.[len - i]
    ret.Self

[<Inline>]
let Set (arr: _ []) i v =
    F.SetArray arr i v

[<Name "sort">]
let Sort<'T when 'T: comparison> (arr: 'T []) : 'T [] =
    (Array.mapi (fun i x -> x, i) arr).JS.Sort(fun (x, y) -> compare x y) |> Array.map fst

[<Name "sortBy">]
let SortBy<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) : 'T [] =
    (Array.mapi (fun i x -> x, (f x, i)) arr).JS.Sort(fun (x, y) -> compare (snd x) (snd y)) |> Array.map fst

[<Name "sortInPlace">]
let SortInPlace<'T when 'T: comparison> (arr: 'T []) =
    (mapiInPlace (fun i x -> x, i) arr).JS.Sort(fun (x, y) -> compare x y) |> mapInPlace fst

[<Name "sortInPlaceBy">]
let SortInPlaceBy<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) =
    (mapiInPlace (fun i x -> x, (f x, i)) arr).JS.Sort(fun (x, y) -> compare (snd x) (snd y)) |> mapInPlace fst 

[<Name "sortInPlaceWith">]
let SortInPlaceWith<'T> (comparer: 'T -> 'T -> int) (arr: 'T []) =
    arr.JS.Sort(fun (x, y) -> comparer x y) |> ignore

[<Name "sortWith">]
let SortWith<'T> (comparer: 'T -> 'T -> int) (arr: 'T []) : 'T [] =
    (Array.copy arr).JS.Sort(fun (x, y) -> comparer x y)

[<Name "sortByDescending">]
let SortByDescending<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) : 'T [] =
    (Array.mapi (fun i x -> x, (f x, i)) arr).JS.Sort(fun (x, y) -> - compare (snd x) (snd y)) |> Array.map fst

[<Name "sortDescending">]
let SortDescending<'T when 'T: comparison> (arr: 'T []) : 'T [] =
    (Array.mapi (fun i x -> x, i) arr).JS.Sort(fun (x, y) -> - compare x y) |> Array.map fst

[<Inline "$x.slice($start,$start+$length)">]
let private subArray (x: 'T) start length = X<'T>

[<Inline>]
let GetSubArray (arr: 'T []) (start: int) (length: int) : 'T []=
    F.GetArraySub arr start length

[<Direct "var sum = 0; for (var i = 0; i < $arr.length; i++) sum += $arr[i]; return sum">]
[<Name "sum">]
let Sum (arr: 'T []) : 'T = X<'T>

[<Direct "var sum = 0; for (var i = 0; i < $arr.length; i++) sum += $f($arr[i]); return sum">]
[<Name "sumBy">]
let SumBy (f: 'T -> 'U) (arr: 'T []) : 'U =  X<'U>

[<Name "transpose">]
let Transpose (x: 'T[] seq) : 'T[][] =
    match x with
    | :? System.Array -> ArrayTranspose (As<'T[][]> x)
    | _ -> ArrayTranspose (Array.ofSeq x)

[<Inline>]
let ToList arr = List.ofArray arr

[<Inline "$arr">]
let ToSeq (arr: _ []) = arr :> seq<_>

[<Name "tryFind">]
let TryFind f (arr: _ []) =
    let mutable res = None
    let mutable i = 0
    while i < Array.length arr && Option.isNone res do
        if f arr.JS.[i] then res <- Some arr.JS.[i]
        i <- i + 1
    res

[<Inline>]
let TryFindBack f (arr: _ []) = ArrayTryFindBack f arr

[<Name "tryFindIndex">]
let TryFindIndex f (arr: _ []) =
    let mutable res = None
    let mutable i = 0
    while i < Array.length arr && Option.isNone res do
        if f arr.JS.[i] then res <- Some i
        i <- i + 1
    res

[<Inline>]
let TryFindIndexBack f (arr: _ []) = ArrayTryFindIndexBack f arr

[<Name "tryHead">]
let TryHead (arr: 'T[]) =
    if Array.length arr = 0 then None else Some arr.JS.[0]

[<Name "tryItem">]
let TryItem i (arr: 'T[]) =
    if Array.length arr <= i || i < 0 then None else Some arr.JS.[i]

[<Name "tryLast">]
let TryLast (arr: 'T[]) =
    let len = Array.length arr
    if len = 0 then None else Some arr.JS.[len - 1]

[<Name "tryPick">]
let TryPick f (arr: _ []) =
    let mutable res = None
    let mutable i = 0
    while i < Array.length arr && Option.isNone res do
        match f arr.JS.[i] with
        | Some _ as r -> res <- r
        | _ -> ()
        i <- i + 1
    res

[<Name "unzip">]
let Unzip<'T1,'T2> (arr: ('T1 * 'T2) []) : 'T1 [] * 'T2 [] =
    let x : 'T1 [] = [||]
    let y : 'T2 [] = [||]
    for i = 0 to Array.length arr - 1 do
        let (a, b) = arr.JS.[i]
        push x a
        push y b
    (x, y)

[<Name "unzip3">]
let Unzip3<'T1,'T2,'T3> (arr: ('T1 * 'T2 * 'T3) []) =
    let x : 'T1 [] = [||]
    let y : 'T2 [] = [||]
    let z : 'T3 [] = [||]
    for i = 0 to Array.length arr - 1 do
        match arr.JS.[i] with
        | (a, b, c) ->
            push x a
            push y b
            push z c
    (x, y, z)

[<Inline>]
[<Name "zeroCreate">]
let ZeroCreate<'T> (size: int) =
    Create size Unchecked.defaultof<'T>

[<Name "zip">]
let Zip (arr1: 'T1 []) (arr2: 'T2 []) =
    checkLength arr1 arr2
    let res = Array.zeroCreate (Array.length arr1)
    for i = 0 to Array.length arr1 - 1 do
        res.JS.[i] <- (arr1.JS.[i], arr2.JS.[i])
    res

[<Name "zip3">]
let Zip3 (arr1: _ [], arr2: _ [], arr3: _ []) =
    checkLength arr1 arr2
    checkLength arr2 arr3
    let res = Array.zeroCreate (Array.length arr1)
    for i = 0 to Array.length arr1 - 1 do
        res.JS.[i] <- (arr1.JS.[i], arr2.JS.[i], arr3.JS.[i])
    res

[<Name "chunkBySize">]
let ChunkBySize size array =
    SeqChunkBySize size (Array.toSeq array)
    |> Seq.toArray

[<Name "compareWith">]
let CompareWith  (f: 'T -> 'T -> int) (a1: 'T []) (a2: 'T []) : int =
    Seq.compareWith f (Array.toSeq a1) (Array.toSeq a2)

[<Inline>]
let CountBy (f: 'T -> 'K) (a: 'T []) : ('K * int) [] =
    ArrayCountBy f a

[<Name "distinct">]
let Distinct<'T when 'T : equality> (l: 'T []) : 'T [] =
    Seq.distinct (Array.toSeq l)
    |> Seq.toArray

[<Name "distinctBy">]
let DistinctBy<'T,'K when 'K : equality>
        (f: 'T -> 'K) (a: 'T []) : 'T [] =
    Seq.distinctBy f (Array.toSeq a)
    |> Seq.toArray

[<Name "except">]
let Except (itemsToExclude: seq<'T>) (a: 'T []) =
    SeqExcept itemsToExclude (Array.toSeq a)
    |> Seq.toArray

[<Name "findBack">]
let FindBack p (s: _ []) =
    match TryFindBack p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Name "findIndexBack">]
let FindIndexBack p (s: _ []) =
    match TryFindIndexBack p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<Inline>]
let GroupBy (f: 'T -> 'K when 'K : equality)
            (a: 'T []) : ('K * 'T []) [] =
    ArrayGroupBy f a

[<Name "head">]
let Head (arr : 'T []) : 'T =
    nonEmpty arr
    arr.JS.[0]

[<Name "last">]
let Last (arr : 'T []) : 'T =
    nonEmpty arr
    arr.JS.[Array.length arr - 1]

[<Name "map3">]
let Map3 (f: 'T1 -> 'T2 -> 'T3 -> 'T4) (arr1: 'T1 []) (arr2: 'T2 []) (arr3: 'T3 []) : 'T4 [] =
    checkLength arr1 arr2
    checkLength arr1 arr3
    let r = JavaScript.Array<'T4>(Array.length arr3)
    for i = 0 to Array.length arr3 - 1 do
        r.[i] <- f arr1.JS.[i] arr2.JS.[i] arr3.JS.[i]
    r.Self

[<Inline>]
let Contains (el: 'T) (a: 'T []) =
    ArrayContains el a

[<Name "pairwise">]
let Pairwise (a: 'T []) : ('T * 'T) [] =
    Seq.pairwise (Array.toSeq a)
    |> Seq.toArray

[<Name "replicate">]
let Replicate size value =
    Array.create size value

[<Name "indexed">]
let Indexed (ar : 'T []) : (int * 'T) [] =
    Array.mapi (fun a b -> (a, b)) ar

[<Inline>]
let Singleton<'T> (x: 'T) =
    [| x |]

[<Name "skip">]
let Skip<'T> i (ar : 'T []) =
    if i < 0 then InputMustBeNonNegative() else
    if i > Array.length ar then InsufficientElements() else
    ar.JS.Slice(i)

[<Name "skipWhile">]
let SkipWhile<'T> (predicate : 'T -> bool) (ar : 'T []) : 'T [] =
    let len = Array.length ar
    let mutable i = 0
    while i < len && predicate ar.JS.[i] do
        i <- i + 1
    ar.JS.Slice(i)

[<Name "tail">]
let Tail<'T> (ar : 'T []) : 'T [] =
    Skip 1 ar

[<Name "take">]
let Take<'T> n (ar: 'T []) =
    if n < 0 then InputMustBeNonNegative() else
    if n > Array.length ar then InsufficientElements() else
    ar.JS.Slice(0, n)

[<Name "takeWhile">]
let TakeWhile<'T> (predicate : 'T -> bool) (ar: 'T []) =
    let len = Array.length ar
    let mutable i = 0
    while i < len && predicate ar.JS.[i] do
        i <- i + 1
    ar.JS.Slice(0, i)

[<Inline>]
let Truncate<'T> n (ar: 'T []) =
    ar.JS.Slice(0, n)

[<Name "exactlyOne">]
let ExactlyOne (ar : 'T []) =
    if Array.length ar = 1 then
        ar.JS.[0]
    else
        failwith "The input does not have precisely one element."

[<Name "tryExactlyOne">]
let TryExactlyOne (ar : 'T []) =
    if Array.length ar = 1 then
        Some ar.JS.[0]
    else
        None

[<Name "unfold">]
let Unfold<'T, 'S> (f: 'S -> option<'T * 'S>) (s: 'S) : 'T [] =
    Seq.unfold f s
    |> Seq.toArray

[<Inline>]
let Where (predicate : 'T -> bool) (s : 'T []) : 'T [] =
    Filter predicate s

[<Name "windowed">]
let Windowed (windowSize: int) (s: 'T []) : array<'T []> =
    Seq.windowed windowSize (Array.toSeq s)
    |> Seq.toArray

[<Name "splitAt">]
let SplitAt (n: int) (ar: 'T []) =
    Take n ar, Skip n ar

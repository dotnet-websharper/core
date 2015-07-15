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

[<WebSharper.Core.Attributes.Name "Arrays">]
[<WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Collections.ArrayModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.ArrayModuleProxy

open WebSharper.JavaScript
open WebSharper.CollectionInternals

module F = WebSharper.IntrinsicFunctionProxy

[<JavaScript>]
let checkLength (arr1: 'T1[]) (arr2: 'T2[]) =
    if Array.length arr1 <> Array.length arr2 then
        failwith "Arrays differ in length."

[<Inline "$x.push($y)">]
let push (x: obj) (y: obj) = ()

[<Inline "$arr1.concat($arr2)">]
let Append<'T> (arr1: 'T []) (arr2: 'T []) : 'T [] = arr1

[<JavaScript>]
[<Name "average">]
let inline Average (arr: 'T []): 'T = As (float (Array.sum arr) / float (Array.length arr))

[<JavaScript>]
[<Name "averageBy">]
let inline AverageBy (f: 'T -> 'U) (arr: 'T []) : 'U = As (float (Array.sumBy f arr) / float (Array.length arr))

[<JavaScript>]
[<Name "blit">]
let CopyTo<'T> (arr1: 'T [], start1, arr2: 'T [], start2, length) =
    F.checkRange arr1 start1 length
    F.checkRange arr2 start2 length
    for i = 0 to length - 1 do
        arr2.[start2 + i] <- arr1.[start1 + i]

[<JavaScript>]
[<Name "choose">]
let Choose<'T,'U> (f: 'T -> option<'U>) (arr: 'T []) : 'U [] =
    let q : 'U [] = [||]
    for i = 0 to Array.length arr - 1 do
        match f arr.[i] with
        | Some x -> push q x
        | None   -> ()
    q

[<Inline "Array.prototype.concat.apply([], $x)">]
let concatArray (x: 'T[][]) = X<'T[]>

[<JavaScript>]
[<Name "collect">]
let Collect<'T,'U> (f: 'T -> 'U[]) (x: 'T []) : 'U[] =
    concatArray (Array.map f x)

[<JavaScript>]
[<Name "concat">]
let Concat<'T> (xs: seq<'T []>) : 'T [] =
    concatArray (Array.ofSeq xs)

[<JavaScript>]
[<Inline>]
let SplitInto count (arr: 'T[]) = ArraySplitInto count arr

[<Inline "$x.slice()">]
let Copy (x: 'T []) = X<'T []>

[<JavaScript>]
[<Name "create">]
let Create size value =
    let r = Array.zeroCreate size
    for i = 0 to size - 1 do
        r.[i] <- value
    r

[<Inline "[]">]
let Empty () = X<'T []>

[<Inline>]
[<JavaScript>]
let Exists<'T> (f: 'T -> bool) (arr: 'T []) = Seq.exists f arr

[<JavaScript>]
[<Name "exists2">]
let Exists2 f (arr1: _ []) (arr2: _ []) =
    checkLength arr1 arr2
    Seq.exists2 f arr1 arr2

[<JavaScript>]
[<Name "fill">]
let Fill<'T> (arr: 'T []) (start: int) (length: int) (value: 'T) =
    F.checkRange arr start length
    for i = start to start + length - 1 do
        arr.[i] <- value

[<JavaScript>]
[<Name "filter">]
let Filter<'T> f (arr: 'T []) : 'T [] =
    let r : 'T [] = [||]
    for i = 0 to Array.length arr - 1 do
        if f arr.[i] then
            push r arr.[i]
    r

[<JavaScript>]
[<Name "find">]
let Find f (arr: _ []) =
    match Array.tryFind f arr with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<JavaScript>]
[<Name "findINdex">]
let FindIndex f (arr: _ []) =
    match Array.tryFindIndex f arr with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<JavaScript>]
[<Name "fold">]
let Fold<'T,'S> (f: 'S -> 'T -> 'S) (zero: 'S) (arr: 'T []) : 'S =
    let mutable acc = zero
    for i = 0 to Array.length arr - 1 do
        acc <- f acc arr.[i]
    acc

[<JavaScript>]
[<Name "fold2">]
let Fold2<'T1,'T2,'S> f (zero: 'S) (arr1: 'T1 []) (arr2: 'T2 []) : 'S =
    checkLength arr1 arr2
    let mutable accum = zero
    for i in 0 .. Array.length arr1 - 1 do
        accum <- f accum arr1.[i] arr2.[i]
    accum

[<JavaScript>]
[<Name "foldBack">]
let FoldBack f (arr: _ []) zero =
    let mutable acc = zero
    let len = Array.length arr
    for i = 1 to len do
        acc <- f arr.[len - i] acc
    acc

[<JavaScript>]
[<Name "foldBack2">]
let FoldBack2 f (arr1: _ []) (arr2: _ []) zero =
    checkLength arr1 arr2
    let len = Array.length arr1
    let mutable accum = zero
    for i in 1 .. len do
        accum <- f arr1.[len - i] arr2.[len - i] accum
    accum

[<Inline>]
[<JavaScript>]
let ForAll f (arr: _ []) = Seq.forall f arr

[<JavaScript>]
[<Name "forall2">]
let ForAll2 f (arr1: _ []) (arr2: _ []) =
    checkLength arr1 arr2
    Seq.forall2 f arr1 arr2

[<Inline>]
[<JavaScript>]
let Get (arr: _ []) index =
    F.GetArray arr index

[<Inline>]
[<JavaScript>]
let Item index (arr: _ []) =
    F.GetArray arr index

[<JavaScript>]
[<Name "init">]
let Initialize size f =
    if size < 0 then
        failwith "Negative size given."
    let r = Array.zeroCreate size
    for i = 0 to size - 1 do
        r.[i] <- f i
    r

[<Inline "$arr.length == 0">]
let IsEmpty (arr: _ []) = X<bool>

[<JavaScript>]
[<Name "iter">]
let Iterate f (arr: 'T []) =
    for i = 0 to Array.length arr - 1 do
        f arr.[i]

[<JavaScript>]
[<Name "iter2">]
let Iterate2 f (arr1: _ []) (arr2: _ []) =
    checkLength arr1 arr2
    for i = 0 to Array.length arr1 - 1 do
        f arr1.[i] arr2.[i]

[<JavaScript>]
[<Name "iteri">]
let IterateIndexed f (arr: 'T []) =
    for i = 0 to Array.length arr - 1 do
        f i arr.[i]

[<JavaScript>]
[<Name "iteri2">]
let IterateIndexed2 f (arr1: _ []) (arr2: _ []) =
    checkLength arr1 arr2
    for i = 0 to Array.length arr1 - 1 do
        f i arr1.[i] arr2.[i]

[<Inline "$arr.length">]
let Length<'T> (arr: 'T []) = X<int>

[<JavaScript>]
[<Name "map">]
let Map<'T1,'T2> (f: 'T1 -> 'T2) (arr: 'T1 []) : 'T2 [] =
    let r = Array.zeroCreate<'T2>(Array.length arr)
    for i = 0 to Array.length arr - 1 do
        r.[i] <- f arr.[i]
    r

[<JavaScript>]
[<Name "map2">]
let Map2 (f: 'T1 -> 'T2 -> 'T3) (arr1: 'T1 []) (arr2: 'T2 []) : 'T3 [] =
    checkLength arr1 arr2
    let r = Array.zeroCreate<'T3>(Array.length arr2)
    for i = 0 to Array.length arr2 - 1 do
        r.[i] <- f arr1.[i] arr2.[i]
    r

[<JavaScript>]
[<Name "mapi">]
let MapIndexed f (arr: _ []) =
    let y = Array.zeroCreate(Array.length arr)
    for i = 0 to Array.length arr - 1 do
        y.[i] <- f i arr.[i]
    y

[<JavaScript>]
[<Name "mapi2">]
let MapIndexed2 f (arr1: 'T1 []) (arr2: 'T2 []): 'U[] =
    checkLength arr1 arr2
    let res = Array.zeroCreate(Array.length arr1)
    for i = 0 to Array.length arr1 - 1 do
        res.[i] <- f i arr1.[i] arr2.[i]
    res

[<JavaScript>]
[<Inline>]
let MapFold f zero arr = ArrayMapFold f zero arr

[<JavaScript>]
[<Inline>]
let MapFoldBack f arr zero = ArrayMapFoldBack f arr zero

[<JavaScript>]
[<Name "max">]
let Max x = Array.reduce max x

[<JavaScript>]
[<Name "maxBy">]
let MaxBy (f, arr) =
    Array.reduce (fun x y -> if f x > f y then x else y) arr

[<JavaScript>]
[<Name "min">]
let Min x = Array.reduce min x

[<JavaScript>]
[<Name "minBy">]
let MinBy f arr =
    Array.reduce (fun x y -> if f x < f y then x else y) arr

[<Inline>]
[<JavaScript>]
let OfList<'T> (xs: list<'T>) = Array.ofSeq xs

[<JavaScript>]
[<Name "ofSeq">]
let OfSeq<'T> (xs: seq<'T>) : 'T [] =
    let q : 'T [] = [||]
    use enum = Enumerator.Get xs
    while enum.MoveNext() do
        push q enum.Current
    q

[<JavaScript>]
[<Name "partition">]
let Partition f (arr: 'T []) : 'T [] * 'T [] =
    let ret1 : 'T [] = [||]
    let ret2 : 'T [] = [||]
    for i = 0 to Array.length arr - 1 do
        if f arr.[i] then
            push ret1 arr.[i]
        else
            push ret2 arr.[i]
    (ret1, ret2)

[<JavaScript>]
[<Name "permute">]
let Permute f (arr: 'T []) =
    let ret = Array.zeroCreate (Array.length arr)
    for i = 0 to Array.length arr - 1 do
        ret.[f i] <- arr.[i]
    ret

[<JavaScript>]
[<Name "pick">]
let Pick f (arr: _ []) =
    match Array.tryPick f arr with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<JavaScript>]
let private nonEmpty (arr: _ []) =
    if Array.length arr = 0 then
        failwith "The input array was empty."

[<JavaScript>]
[<Name "reduce">]
let Reduce f (arr: _ []) =
    nonEmpty arr
    let mutable acc = arr.[0]
    for i = 1 to Array.length arr - 1 do
        acc <- f acc arr.[i]
    acc

[<JavaScript>]
[<Name "reduceBack">]
let ReduceBack f (arr: _ []) =
    nonEmpty arr
    let len = Array.length arr
    let mutable acc = arr.[len - 1]
    for i = 2 to len do
        acc <- f arr.[len - i] acc
    acc

[<Inline "$x.slice().reverse()">]
[<Name "rev">]
let Reverse (x: 'T []) = X<'T []>

[<JavaScript>]
[<Name "scan">]
let Scan<'T,'S> (f: 'S -> 'T -> 'S) (zero: 'S) (arr: 'T []) : 'S [] =
    let ret = Array.zeroCreate (1 + Array.length arr)
    ret.[0] <- zero
    for i = 0 to Array.length arr - 1 do
        ret.[i + 1] <- f ret.[i] arr.[i]
    ret

[<JavaScript>]
[<Name "scanBack">]
let ScanBack (f: 'T -> 'S -> 'S) (arr: 'T []) (zero: 'S) : 'S [] =
    let len = Array.length arr
    let ret = Array.zeroCreate (1 + len)
    ret.[len] <- zero
    for i = 0 to len - 1 do
        ret.[len - i - 1] <- f arr.[len - i - 1] ret.[len - i]
    ret

[<Inline>]
[<JavaScript>]
let Set (arr: _ []) i v =
    F.SetArray arr i v

[<Inline "$x.sort($wsruntime.CreateFuncWithArgs($f))">]
let private sortArray (x: 'T[]) (f: 'T * 'T -> int) = X<'T[]>

[<JavaScript>]
[<Name "sort">]
let Sort<'T when 'T: comparison> (arr: 'T []) : 'T [] =
    Array.sortBy id arr

[<JavaScript>]
[<Name "sortBy">]
let SortBy<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) : 'T [] =
    sortArray (Array.copy arr) (fun (x, y) -> compare (f x) (f y))

[<JavaScript>]
[<Name "sortInPlace">]
let SortInPlace<'T when 'T: comparison> (arr: 'T []) =
    Array.sortInPlaceBy id arr

[<JavaScript>]
[<Name "sortInPlaceBy">]
let SortInPlaceBy<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) =
    As<unit> (sortArray arr (fun (x, y) -> compare (f x) (f y)))

[<JavaScript>]
[<Name "sortInPlaceWith">]
let SortInPlaceWith<'T> (comparer: 'T -> 'T -> int) (arr: 'T []) =
    As<unit> (sortArray arr (fun (x, y) -> comparer x y))

[<JavaScript>]
[<Name "sortWith">]
let SortWith<'T> (comparer: 'T -> 'T -> int) (arr: 'T []) : 'T [] =
    sortArray (Array.copy arr) (fun (x, y) -> comparer x y)

[<JavaScript>]
[<Name "sortByDescending">]
let SortByDescending<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) : 'T [] =
    sortArray (Array.copy arr) (fun (x, y) -> - compare (f x) (f y))

[<JavaScript>]
[<Name "sortDescending">]
let SortDescending<'T when 'T: comparison> (arr: 'T []) : 'T [] =
    SortByDescending id arr

[<Inline "$x.slice($start,$start+$length)">]
let private subArray (x: 'T) start length = X<'T>

[<Inline>]
[<JavaScript>]
let GetSubArray (arr: 'T []) (start: int) (length: int) : 'T []=
    F.GetArraySub arr start length

[<Direct "var sum = 0; for (var i = 0; i < $arr.length; i++) sum += $arr[i]; return sum">]
[<Name "sum">]
let Sum (arr: 'T []) : 'T = X<'T>

[<Direct "var sum = 0; for (var i = 0; i < $arr.length; i++) sum += $f($arr[i]); return sum">]
[<Name "sumBy">]
let SumBy (f: 'T -> 'U) (arr: 'T []) : 'U =  X<'U>

[<JavaScript>]
[<Inline>]
let ToList arr = List.ofArray arr

[<Inline "$arr">]
let ToSeq (arr: _ []) = arr :> seq<_>

[<JavaScript>]
[<Name "tryFind">]
let TryFind f (arr: _ []) =
    let mutable res = None
    let mutable i = 0
    while i < Array.length arr && Option.isNone res do
        if f arr.[i] then res <- Some arr.[i]
        i <- i + 1
    res

[<JavaScript>]
[<Inline>]
let TryFindBack f (arr: _ []) = ArrayTryFindBack f arr

[<JavaScript>]
[<Name "tryFindIndex">]
let TryFindIndex f (arr: _ []) =
    let mutable res = None
    let mutable i = 0
    while i < Array.length arr && Option.isNone res do
        if f arr.[i] then res <- Some i
        i <- i + 1
    res

[<JavaScript>]
[<Inline>]
let TryFindIndexBack f (arr: _ []) = ArrayTryFindIndexBack f arr

[<JavaScript>]
[<Name "tryHead">]
let TryHead (arr: 'T[]) =
    if arr.Length = 0 then None else Some arr.JS.[0]

[<JavaScript>]
[<Name "tryItem">]
let TryItem i (arr: 'T[]) =
    if arr.Length <= i || i < 0 then None else Some arr.JS.[i]

[<JavaScript>]
[<Name "tryLast">]
let TryLast (arr: 'T[]) =
    let len = arr.Length
    if len = 0 then None else Some arr.JS.[len - 1]

[<JavaScript>]
[<Name "tryPick">]
let TryPick f (arr: _ []) =
    let mutable res = None
    let mutable i = 0
    while i < Array.length arr && Option.isNone res do
        match f arr.[i] with
        | Some _ as r -> res <- r
        | _ -> ()
        i <- i + 1
    res

[<JavaScript>]
[<Name "unzip">]
let Unzip<'T1,'T2> (arr: ('T1 * 'T2) []) : 'T1 [] * 'T2 [] =
    let x : 'T1 [] = [||]
    let y : 'T2 [] = [||]
    for i = 0 to Array.length arr - 1 do
        let (a, b) = arr.[i]
        push x a
        push y b
    (x, y)

[<JavaScript>]
[<Name "unzip3">]
let Unzip3<'T1,'T2,'T3> (arr: ('T1 * 'T2 * 'T3) []) =
    let x : 'T1 [] = [||]
    let y : 'T2 [] = [||]
    let z : 'T3 [] = [||]
    for i = 0 to Array.length arr - 1 do
        match arr.[i] with
        | (a, b, c) ->
            push x a
            push y b
            push z c
    (x, y, z)

[<Inline "Array($size)">]
[<Name "zeroCreate">]
let ZeroCreate (size: int) = X<'T []>

[<JavaScript>]
[<Name "zip">]
let Zip (arr1: 'T1 []) (arr2: 'T2 []) =
    checkLength arr1 arr2
    let res = Array.zeroCreate (Array.length arr1)
    for i = 0 to Array.length arr1 - 1 do
        res.[i] <- (arr1.[i], arr2.[i])
    res

[<JavaScript>]
[<Name "zip3">]
let Zip3 (arr1: _ [], arr2: _ [], arr3: _ []) =
    checkLength arr1 arr2
    checkLength arr2 arr3
    let res = Array.zeroCreate (Array.length arr1)
    for i = 0 to Array.length arr1 - 1 do
        res.[i] <- (arr1.[i], arr2.[i], arr3.[i])
    res
        
[<JavaScript>]
[<Name "chunkBySize">]
let ChunkBySize size array =
    SeqChunkBySize size (Array.toSeq array)
    |> Seq.toArray
    
[<JavaScript>]
[<Name "compareWith">]
let CompareWith  (f: 'T -> 'T -> int) (a1: 'T []) (a2: 'T []) : int =
    SeqCompareWith f (Array.toSeq a1) (Array.toSeq a2)
        
[<JavaScript>]
[<Name "countBy">]
let CountBy (f: 'T -> 'K) (a: 'T []) : ('K * int) [] =
    SeqCountBy f (Array.toSeq a)
    |> Seq.toArray

[<JavaScript>]
[<Name "distinct">]
let Distinct<'T when 'T : equality> (l: 'T []) : 'T [] =
    SeqDistinct (Array.toSeq l)
    |> Seq.toArray

[<JavaScript>]
[<Name "distinctBy">]
let DistinctBy<'T,'K when 'K : equality>
        (f: 'T -> 'K) (a: 'T []) : 'T [] =
    SeqDistinctBy f (Array.toSeq a)
    |> Seq.toArray

[<JavaScript>]
[<Name "except">]
let Except (itemsToExclude: seq<'T>) (a: 'T []) =
    SeqExcept itemsToExclude (Array.toSeq a)
    |> Seq.toArray

[<JavaScript>]
[<Name "findBack">]
let FindBack p (s: _ []) =
    match TryFindBack p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<JavaScript>]
[<Name "findIndexBack">]
let FindIndexBack p (s: _ []) =
    match TryFindIndexBack p s with
    | Some x -> x
    | None   -> failwith "KeyNotFoundException"

[<JavaScript>]
[<Name "groupBy">]
let GroupBy (f: 'T -> 'K when 'K : equality)
            (a: 'T []) : ('K * 'T []) [] =
    SeqGroupBy f (Array.toSeq a)
    |> Seq.toArray
    |> Array.map (fun (k, s) ->
        (k, Seq.toArray s)
    )

[<JavaScript>]
[<Name "head">]
let Head (ar : 'T []) : 'T =
    List.head (Array.toList ar)

[<JavaScript>]
[<Name "last">]
let Last (ar : 'T []) : 'T =
    SeqLast (Array.toSeq ar)

[<JavaScript>]
[<Name "map3">]
let Map3 f (l1: _ []) (l2: _ []) (l3: _ []) =
    ListMap3 f (Array.toList l1) (Array.toList l2) (Array.toList l3)
    |> List.toArray

[<JavaScript>]
[<Name "contains">]
let Contains (el: 'T) (a: 'T []) =
    SeqContains el (Array.toSeq a)

[<JavaScript>]
[<Name "pairwise">]
let Pairwise (a: 'T []) : ('T * 'T) [] =
    SeqPairwise (Array.toSeq a)
    |> Seq.toArray

[<JavaScript>]
[<Name "replicate">]
let Replicate size value =
    Array.create size value

[<JavaScript>]
[<Name "indexed">]
let Indexed (ar : 'T []) : (int * 'T) [] =
    Array.mapi (fun a b -> (a, b)) ar

[<JavaScript>]
[<Inline>]
let Singleton<'T> (x: 'T) =
    [| x |]

[<JavaScript>]
[<Name "skip">]
let Skip<'T> i (ar : 'T []) =
    ListSkip i (Array.toList ar)
    |> List.toArray

[<JavaScript>]
[<Name "skipWhile">]
let SkipWhile<'T> (predicate : 'T -> bool) (ar : 'T []) : 'T [] =
    ListSkipWhile predicate (Array.toList ar)
    |> List.toArray

[<JavaScript>]
[<Name "tail">]
let Tail<'T> (ar : 'T []) : 'T [] =
    List.tail (Array.toList ar)
    |> List.toArray

[<JavaScript>]
[<Name "take">]
let Take<'T> n (ar: 'T []) =
    ListTake n (Array.toList ar)
    |> List.toArray

[<JavaScript>]
[<Name "takeWhile">]
let TakeWhile<'T> (predicate : 'T -> bool) (ar: 'T []) =
    ListTakeWhile predicate (Array.toList ar)
    |> List.toArray

[<JavaScript>]
[<Name "truncate">]
let Truncate<'T> n (ar: 'T []) =
    ListTruncate n (Array.toList ar)
    |> List.toArray

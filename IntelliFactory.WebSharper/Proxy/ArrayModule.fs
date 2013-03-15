// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

[<IntelliFactory.WebSharper.Core.Attributes.Name "Arrays">]
[<IntelliFactory.WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Collections.ArrayModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private IntelliFactory.WebSharper.ArrayModuleProxy

[<JavaScript>]
let checkRange (arr: 'T []) (start: int) (size: int) : unit =
    if (size < 0) || (start < 0) || (arr.Length < start + size) then
        failwith "Index was outside the bounds of the array."

[<JavaScript>]
let checkLength (arr1: 'T1[]) (arr2: 'T2[]) =
    if arr1.Length <> arr2.Length then
        failwith "Arrays differ in length."

[<Inline "$x.push($y)">]
let push (x: obj) (y: obj) = ()

[<Inline "$arr1.concat($arr2)">]
let Append<'T> (arr1: 'T []) (arr2: 'T []) : 'T [] = arr1

[<Inline>]
[<JavaScript>]
let inline Average (arr: 'T []) : 'T = Seq.average arr

[<Inline>]
[<JavaScript>]
[<Name "averageBy">]
let inline AverageBy (f: 'T -> 'U) (arr: 'T []) : 'U = Seq.averageBy f arr

[<JavaScript>]
[<Name "blit">]
let CopyTo<'T> (arr1: 'T [], start1, arr2: 'T [], start2, length) =
    checkRange arr1 start1 length
    checkRange arr2 start2 length
    for i = 0 to length - 1 do
        arr2.[start2 + i] <- arr1.[start1 + i]

[<JavaScript>]
[<Name "choose">]
let Choose<'T,'U> (f: 'T -> option<'U>) (arr: 'T []) : 'U [] =
    let q : 'U [] = [||]
    for i in 0 .. arr.Length - 1 do
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

[<Inline "$x.slice(0)">]
let Copy (x: 'T []) = X<'T []>

[<JavaScript>]
[<Name "create">]
let Create size value =
    let r = Array.zeroCreate size
    for i in 0 .. size - 1 do
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
    checkRange arr start length
    for i = start to start + length - 1 do
        arr.[i] <- value

[<JavaScript>]
[<Name "filter">]
let Filter<'T> f (arr: 'T []) : 'T [] =
    let r : 'T [] = [||]
    for i = 0 to arr.Length - 1 do
        if f arr.[i] then
            push r arr.[i]
    r

[<Inline>]
[<JavaScript>]
let Find f (arr: _ []) = Seq.find f arr

[<Inline>]
[<JavaScript>]
let FindIndex f (arr: _ []) = Seq.findIndex f arr

[<JavaScript>]
[<Name "fold">]
let Fold<'T,'S> (f: 'S -> 'T -> 'S) (zero: 'S) (arr: 'T []) : 'S =
    let mutable acc = zero
    for i = 0 to arr.Length - 1 do
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
    let len = arr.Length
    for i = 1 to len do
        acc <- f arr.[len - i] acc
    acc

[<JavaScript>]
[<Name "foldBack2">]
let FoldBack2 f (arr1: _ []) (arr2: _ []) zero =
    checkLength arr1 arr2
    let len = arr1.Length
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

[<Inline "$arr[$index]">]
[<Name "get">]
let Get (arr: _ []) index = arr.[index]

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
let IsEmpty (arr: _ []) = arr.Length = 0

[<JavaScript>]
[<Name "iter">]
let Iterate f (arr: 'T []) =
    for i = 0 to arr.Length - 1 do
        f arr.[i]

[<JavaScript>]
[<Name "iter2">]
let Iterate2 f (arr1: _ []) (arr2: _ []) =
    checkLength arr1 arr2
    for i = 0 to arr1.Length - 1 do
        f arr1.[i] arr2.[i]

[<JavaScript>]
[<Name "iteri">]
let IterateIndexed f (arr: 'T []) =
    for i = 0 to arr.Length - 1 do
        f i arr.[i]

[<JavaScript>]
[<Name "iteri2">]
let IterateIndexed2 f (arr1: _ []) (arr2: _ []) =
    checkLength arr1 arr2
    for i = 0 to arr1.Length - 1 do
        f i arr1.[i] arr2.[i]

[<Inline "$arr.length">]
let Length<'T> (arr: 'T []) = arr.Length

[<JavaScript>]
[<Name "map">]
let Map<'T1,'T2> (f: 'T1 -> 'T2) (arr: 'T1 []) : 'T2 [] =
    let r = Array.zeroCreate<'T2> arr.Length
    for i = 0 to arr.Length - 1 do
        r.[i] <- f arr.[i]
    r

[<JavaScript>]
[<Name "map2">]
let Map2 (f: 'T1 -> 'T2 -> 'T3) (arr1: 'T1 []) (arr2: 'T2 []) : 'T3 [] =
    checkLength arr1 arr2
    let r = Array.zeroCreate<'T3> arr2.Length
    for i = 0 to arr2.Length - 1 do
        r.[i] <- f arr1.[i] arr2.[i]
    r

[<JavaScript>]
[<Name "mapi">]
let MapIndexed f (arr: _ []) =
    let y = Array.zeroCreate arr.Length
    for i = 0 to arr.Length - 1 do
        y.[i] <- f i arr.[i]
    y

[<JavaScript>]
[<Name "mapi2">]
let MapIndexed2 f (arr1: _ []) (arr2: _ []) =
    checkLength arr1 arr2
    let res = Array.zeroCreate arr1.Length
    for i = 0 to arr1.Length - 1 do
        res.[i] <- f i arr1.[i] arr2.[i]
    res

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
    for i = 0 to arr.Length - 1 do
        if f arr.[i] then
            push ret1 arr.[i]
        else
            push ret2 arr.[i]
    (ret1, ret2)

[<JavaScript>]
[<Name "permute">]
let Permute f (arr: _ []) =
    let ret = Array.copy arr
    for i = 0 to arr.Length - 1 do
        ret.[f i] <- arr.[i]
    ret

[<Inline>]
[<JavaScript>]
let Pick (f, arr: _ []) = Seq.pick f arr

[<JavaScript>]
let private nonEmpty (arr: _ []) =
    if arr.Length = 0 then
        failwith "The input array was empty."

[<JavaScript>]
[<Name "reduce">]
let Reduce f (arr: _ []) =
    nonEmpty arr
    let mutable acc = arr.[0]
    for i = 1 to arr.Length - 1 do
        acc <- f acc arr.[i]
    acc

[<JavaScript>]
[<Name "reduceBack">]
let ReduceBack f (arr: _ []) =
    nonEmpty arr
    let len = arr.Length
    let mutable acc = arr.[len - 1]
    for i = 2 to len do
        acc <- f arr.[len - i] acc
    acc

[<Inline "$x.slice(0,$x.length).reverse()">]
[<Name "rev">]
let Reverse (x: 'T []) = X<'T []>

[<JavaScript>]
[<Name "scan">]
let Scan<'T,'S> (f: 'S -> 'T -> 'S) (zero: 'S) (arr: 'T []) : 'S [] =
    let ret = Array.zeroCreate (1 + arr.Length)
    ret.[0] <- zero
    for i = 0 to arr.Length - 1 do
        ret.[i + 1] <- f ret.[i] arr.[i]
    ret

[<JavaScript>]
[<Name "scanBack">]
let ScanBack (f: 'T -> 'S -> 'S) (arr: 'T []) (zero: 'S) : 'S [] =
    let len = arr.Length
    let ret = Array.zeroCreate (1 + len)
    ret.[len] <- zero
    for i = 0 to len - 1 do
        ret.[len - i - 1] <- f arr.[len - i - 1] ret.[len - i]
    ret

[<Inline "void ($arr[$i] = $v)">]
let Set (arr: _ [], i, v) = arr.[i] <- v

[<Inline "$x.sort($f)">]
let private sortArray (x: 'T) f = X<'T>

[<JavaScript>]
[<Name "sort">]
let Sort<'T when 'T: comparison> (arr: 'T []) : 'T [] =
    Array.sortBy id arr

[<JavaScript>]
[<Name "sortBy">]
let SortBy<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) : 'T [] =
    let f (x, y) = compare (f x) (f y)
    sortArray (Array.copy arr) f

[<JavaScript>]
[<Name "sortInPlace">]
let SortInPlace<'T when 'T: comparison> (arr: 'T []) =
    Array.sortInPlaceBy id arr

[<JavaScript>]
[<Name "sortInPlaceBy">]
let SortInPlaceBy<'T,'U when 'U: comparison> (f: 'T -> 'U) (arr: 'T []) =
    let f (x, y) = compare (f x) (f y)
    As<unit> (sortArray arr f)

[<JavaScript>]
[<Name "sortInPlaceWith">]
let SortInPlaceWith<'T> (comparer: 'T -> 'T -> int) (arr: 'T []) =
    let f (x, y) = comparer x y
    As<unit> (sortArray arr f)

[<JavaScript>]
[<Name "sortWith">]
let SortWith<'T> (comparer: 'T -> 'T -> int) (arr: 'T []) : 'T [] =
    let f (x, y) = comparer x y
    sortArray (Array.copy arr) f

[<Inline "$x.slice($start,$start+$length)">]
let private subArray (x: 'T) start length = X<'T>

[<JavaScript>]
[<Name "sub">]
let GetSubArray (arr: 'T []) (start: int) (length: int) : 'T []=
    checkRange arr start length
    subArray arr start length

[<Inline>]
[<JavaScript>]
let inline Sum (arr: 'T []) : 'T = Seq.sum arr

[<Inline>]
[<JavaScript>]
let inline SumBy (f: 'T -> 'U) (arr: 'T []) : 'U = Seq.sumBy f arr

[<JavaScript>]
[<Inline>]
let ToList arr = List.ofArray arr

[<Inline "$arr">]
let ToSeq (arr: _ []) = arr :> seq<_>

[<Inline>]
[<JavaScript>]
let TryFind f (arr: _ []) = Seq.tryFind f arr

[<Inline>]
[<JavaScript>]
let TryFindIndex f (arr: _ []) = Seq.tryFindIndex f arr

[<Inline>]
[<JavaScript>]
let TryPick f (arr: _ []) = Seq.tryPick f arr

[<JavaScript>]
[<Name "unzip">]
let Unzip<'T1,'T2> (arr: ('T1 * 'T2) []) : 'T1 [] * 'T2 [] =
    let x : 'T1 [] = [||]
    let y : 'T2 [] = [||]
    for i = 0 to arr.Length - 1 do
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
    for i = 0 to arr.Length - 1 do
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
    for i = 0 to arr1.Length - 1 do
        res.[i] <- (arr1.[i], arr2.[i])
    res

[<JavaScript>]
[<Name "zip3">]
let Zip3 (arr1: _ [], arr2: _ [], arr3: _ []) =
    checkLength arr1 arr2
    checkLength arr2 arr3
    let res = Array.zeroCreate (Array.length arr1)
    for i = 0 to arr1.Length - 1 do
        res.[i] <- (arr1.[i], arr2.[i], arr3.[i])
    res

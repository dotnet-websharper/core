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

[<WebSharper.Core.Attributes.Name "List">]
[<WebSharper.Core.Attributes.Proxy
    "Microsoft.FSharp.Collections.ListModule, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.ListModuleProxy

open WebSharper.JavaScript
open WebSharper.CollectionInternals

[<JavaScript>]
[<Name "append">]
let Append (x: list<_>) (y: list<_>) = List.ofSeq (Seq.append x y)

[<Inline>]
[<JavaScript>]
let inline Average (l: list<_>) = Seq.average l

[<Inline>]
[<JavaScript>]
let inline AverageBy f (l: list<_>) = Seq.averageBy f l

[<JavaScript>]
[<Name "choose">]
let Choose f (l: list<_>) = List.ofSeq (Seq.choose f l)

[<JavaScript>]
[<Name "collect">]
let Collect (f: _ -> list<_>) (l: list<_>) = List.ofSeq (Seq.collect f l)

[<JavaScript>]
[<Name "concat">]
let Concat (s: seq<list<_>>) = List.ofSeq (Seq.concat s)

[<Inline>]
[<JavaScript>]
let Empty<'T> : list<'T> = []

[<Inline>]
[<JavaScript>]
let Exists<'T> (p: 'T -> bool) (l: list<'T>) = Seq.exists p l

[<JavaScript>]
[<Name "exists2">]
let Exists2<'T1,'T2> (p : 'T1 -> 'T2 -> bool)
                        (l1: list<'T1>)
                        (l2: list<'T2>) =
    Array.exists2 p (Array.ofSeq l1) (Array.ofSeq l2)

[<JavaScript>]
[<Name "filter">]
let Filter<'T> (p: 'T -> bool) (l: list<'T>) =
    List.ofSeq (Seq.filter p l)

[<Inline>]
[<JavaScript>]
let Find p (l: list<_>) = Seq.find p l

[<Inline>]
[<JavaScript>]
let FindIndex p (l: list<_>) = Seq.findIndex p l

[<Inline>]
[<JavaScript>]
let Fold<'T,'S> (f: 'S -> 'T -> 'S) (s: 'S) (l: list<'T>) : 'S =
    Seq.fold f s l

[<JavaScript>]
[<Name "fold2">]
let Fold2<'T1,'T2,'S> (f: 'S -> 'T1 -> 'T2 -> 'S)
                        (s: 'S)
                        (l1: list<'T1>)
                        (l2: list<'T2>) : 'S =
    Array.fold2 f s (Array.ofSeq l1) (Array.ofSeq l2)

[<JavaScript>]
[<Name "foldBack">]
let FoldBack f (l: list<_>) s =
    Array.foldBack f (Array.ofSeq l) s

[<JavaScript>]
[<Name "foldBack2">]
let FoldBack2 f (l1: list<_>) (l2: list<_>) s =
    Array.foldBack2 f (Array.ofSeq l1) (Array.ofSeq l2) s

[<Inline>]
[<JavaScript>]
let ForAll p (l: list<_>) = Seq.forall p l

[<JavaScript>]
[<Name "forall2">]
let ForAll2 p (l1: list<_>) (l2: list<_>) =
    Array.forall2 p (Array.ofSeq l1) (Array.ofSeq l2)

[<JavaScript>]
[<Name "head">]
let Head (l: list<'T>) =
    match l with 
    | h :: _ -> h
    | _ -> failwith "The input list was empty."

[<JavaScript>]
[<Name "init">]
let Initialize s f = List.ofArray (Array.init s f)

[<Inline "$l.$ == 0">]
let IsEmpty (l: list<_>) = X<bool>

[<Inline>]
[<JavaScript>]
let Iterate f (l: list<_>) = Seq.iter f l

[<JavaScript>]
[<Name "iter2">]
let Iterate2 f (l1: list<_>) (l2: list<_>) =
    Array.iter2 f (Array.ofSeq l1) (Array.ofSeq l2)

[<Inline>]
[<JavaScript>]
let IterateIndexed f (l: list<_>) = Seq.iteri f l

[<JavaScript>]
[<Name "iteri2">]
let IterateIndexed2 f (l1: list<_>) (l2: list<_>) =
    Array.iteri2 f (Array.ofSeq l1) (Array.ofSeq l2)

[<Inline>]
[<JavaScript>]
let Length (l: list<_>) = Seq.length l

[<JavaScript>]
[<Name "map">]
let Map f (l: list<_>) = List.ofSeq (Seq.map f l)

[<JavaScript>]
[<Name "map2">]
let Map2 f (l1: list<_>) (l2: list<_>) =
    List.ofArray (Array.map2 f (Array.ofSeq l1) (Array.ofSeq l2))

[<JavaScript>]
[<Name "map3">]
let Map3 f (l1: list<_>) (l2: list<_>) (l3: list<_>) =
    Array.map2
        ( <| )
        (Array.map2 f (Array.ofSeq l1) (Array.ofSeq l2))
        (Array.ofSeq l3)
    |> List.ofArray

[<JavaScript>]
[<Name "mapi">]
let MapIndexed f (l: list<_>) = List.ofSeq (Seq.mapi f l)

[<JavaScript>]
[<Name "mapi2">]
let MapIndexed2 f (l1: list<_>) (l2: list<_>) =
    List.ofArray (Array.mapi2 f (Array.ofSeq l1) (Array.ofSeq l2))

[<JavaScript>]
[<Name "max">]
let Max (l: list<_>) = Seq.reduce max l

[<JavaScript>]
[<Name "maxBy">]
let MaxBy f (l: list<_>) =
    Seq.reduce (fun x y -> if f x > f y then x else y) l

[<JavaScript>]
[<Name "min">]
let Min (l: list<_>) = Seq.reduce min l

[<JavaScript>]
[<Name "minBy">]
let MinBy f (l: list<_>) =
    Seq.reduce (fun x y -> if f x < f y then x else y) l

[<Inline>]
[<JavaScript>]
let Get (l: list<_>) ix = Seq.nth ix l

[<JavaScript>]
[<Name "ofArray">]
let OfArray<'T> (arr: 'T []) =
    let mutable r = []
    for i = 0 to arr.Length - 1 do
        r <- arr.[arr.Length - i - 1] :: r
    r

[<JavaScript>]
[<Name "ofSeq">]
let OfSeq (s: seq<'T>) =
    let r = System.Collections.Generic.Stack<_>()
    use e = Enumerator.Get s
    while e.MoveNext() do
        r.Push e.Current
    let x = r.ToArray()
    System.Array.Reverse x
    List.ofArray x

[<JavaScript>]
[<Name "partition">]
let Partition p (l: list<_>) =
    let (a, b) = Array.partition p (Array.ofSeq l)
    (List.ofArray a, List.ofArray b)

[<JavaScript>]
[<Name "permute">]
let Permute f (l: list<_>) =
    List.ofArray (Array.permute f (Array.ofSeq l))

[<Inline>]
[<JavaScript>]
let Pick f (l: list<_>) = Seq.pick f l

[<Inline>]
[<JavaScript>]
let Reduce (f: 'T -> 'T -> 'T) (list: list<'T>) : 'T =
    Seq.reduce f list

[<JavaScript>]
[<Name "reduceBack">]
let ReduceBack f (l: list<_>) =
    Array.reduceBack f (Array.ofSeq l)

[<JavaScript>]
[<Name "replicate">]
let Replicate size value =
    List.ofArray (Array.create size value)

[<JavaScript>]
[<Name "rev">]
let Reverse (l: list<'T>) =
    let a = Array.ofSeq l
    System.Array.Reverse a
    List.ofArray a

[<JavaScript>]
[<Name "scan">]
let Scan<'T,'S> (f: 'S -> 'T -> 'S) (s: 'S) (l: list<'T>) : list<'S> =
    List.ofSeq (Seq.scan f s l)

[<JavaScript>]
[<Name "scanBack">]
let ScanBack f (l: list<_>) s =
    List.ofArray (Array.scanBack f (Array.ofSeq l) s)

[<JavaScript>]
[<Name "sort">]
let Sort (l: list<_>) =
    let a = Array.ofSeq l
    Array.sortInPlace a
    List.ofArray a

[<JavaScript>]
[<Name "sortBy">]
let SortBy f (l: list<_>) =
    List.sortWith (fun x y -> compare (f x) (f y)) l

[<JavaScript>]
[<Name "sortByDescending">]
let SortByDescending f (l: list<_>) =
    List.sortWith (fun x y -> - compare (f x) (f y)) l

[<JavaScript>]
[<Name "sortDescending">]
let SortDescending (l: list<_>) =
    let a = Array.ofSeq l
    ArraySortInPlaceByDescending id a
    List.ofArray a

[<JavaScript>]
[<Name "sortWith">]
let SortWith f (l: list<_>) =
    let a = Array.ofSeq l
    Array.sortInPlaceWith f a
    List.ofArray a

[<Inline>]
[<JavaScript>]
let inline Sum (l: list<'T>) : 'T = Seq.sum l

[<Inline>]
[<JavaScript>]
let inline SumBy (f: 'T -> 'U) (l: list<'T>) : 'U = Seq.sumBy f l

[<JavaScript>]
[<Name "tail">]
let Tail (l: list<'T>) = 
    match l with 
    | _ :: t -> t
    | _ -> failwith "The input list was empty."

[<Inline>]
[<JavaScript>]
let ToArray (l: list<_>) = Array.ofSeq l

[<Inline "$x">]
let ToSeq<'T> (x: list<'T>) : seq<'T> = x :> _

[<Inline>]
[<JavaScript>]
let TryFind p (l: list<_>) = Seq.tryFind p l

[<Inline>]
[<JavaScript>]
let TryFindIndex p (l: list<_>) = Seq.tryFindIndex p l

[<Inline>]
[<JavaScript>]
let TryPick p (l: list<_>) = Seq.tryPick p l

[<JavaScript>]
[<Name "unzip">]
let Unzip (l: list<_>) =
    let x = System.Collections.Generic.Queue<_>()
    let y = System.Collections.Generic.Queue<_>()
    for (a, b) in l do
        x.Enqueue a
        y.Enqueue b
    (List.ofArray (x.ToArray()), List.ofArray (y.ToArray()))

[<JavaScript>]
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

[<JavaScript>]
[<Name "zip">]
let Zip (l1: list<_>) (l2: list<_>) =
    List.ofArray (Array.zip (Array.ofSeq l1) (Array.ofSeq l2))

[<JavaScript>]
[<Name "zip3">]
let Zip3 (l1: list<_>) (l2: list<_>) (l3: list<_>) =
    List.ofArray (Array.zip3 (Array.ofSeq l1)
        (Array.ofSeq l2) (Array.ofSeq l3))

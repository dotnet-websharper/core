// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
namespace Conformance

open System
open WebSharper
open Conformance.H

[<JavaScript>]
module Lists =
    let private l () = [ 3; 1; 4; 1; 5; 9; 2; 6 ]
    let private e () : int list = []

    let Cases () = [
        C "list.basics" (fun () -> string (List.length (l ())) + "," + string (List.head (l ())) + "," + Ji (List.tail (l ())))
        C "list.head.empty" (fun () -> string (List.head (e ())))
        C "list.cons.append" (fun () -> Ji (0 :: [ 1 ] @ [ 2; 3 ]))
        C "list.map.filter" (fun () -> Ji (l () |> List.filter (fun x -> x % 2 = 1) |> List.map (fun x -> x * 10)))
        C "list.sort" (fun () -> Ji (List.sort (l ())) + Ji (List.sortDescending (l ())))
        C "list.sort.stable" (fun () -> J ([ "bb"; "a"; "cc"; "d" ] |> List.sortBy String.length))
        C "list.sortwith" (fun () -> Ji (List.sortWith (fun a b -> compare b a) (l ())))
        C "list.fold" (fun () -> string (List.fold (fun acc x -> acc * 10 + x) 0 [ 1; 2; 3 ]) + "," + string (List.foldBack (fun x acc -> acc * 10 + x) [ 1; 2; 3 ] 0))
        C "list.reduce" (fun () -> string (List.reduce (-) [ 10; 2; 3 ]) + "," + string (List.reduceBack (-) [ 10; 2; 3 ]))
        C "list.reduce.empty" (fun () -> string (List.reduce (+) (e ())))
        C "list.scan" (fun () -> Ji (List.scan (+) 0 [ 1; 2; 3 ]))
        C "list.sum.average" (fun () -> string (List.sum (l ())) + "," + string (List.average [ 1.0; 2.0 ]) + "," + string (List.sumBy float [ 1; 2 ]))
        C "list.average.empty" (fun () -> string (List.average ([]: float list)))
        C "list.minmax" (fun () -> string (List.max (l ())) + "," + string (List.min (l ())) + "," + string (List.maxBy (fun x -> -x) (l ())))
        C "list.distinct" (fun () -> Ji (List.distinct (l ())) + Ji (List.distinctBy (fun x -> x % 3) (l ())))
        C "list.groupby" (fun () -> J (l () |> List.groupBy (fun x -> x % 3) |> List.map (fun (k, xs) -> string k + ":" + Ji xs)))
        C "list.countby" (fun () -> J (l () |> List.countBy (fun x -> x > 3) |> List.map (fun (k, c) -> string k + ":" + string c)))
        C "list.partition" (fun () -> let a, b = List.partition (fun x -> x > 3) (l ()) in Ji a + Ji b)
        C "list.chunk" (fun () -> J (List.chunkBySize 3 (l ()) |> List.map Ji))
        C "list.windowed" (fun () -> J (List.windowed 3 [ 1; 2; 3; 4 ] |> List.map Ji))
        C "list.pairwise" (fun () -> J (List.pairwise [ 1; 2; 3 ] |> List.map (fun (a, b) -> string a + string b)))
        C "list.zip" (fun () -> J (List.zip [ 1; 2 ] [ "a"; "b" ] |> List.map (fun (a, b) -> string a + b)))
        C "list.zip.mismatch" (fun () -> string (List.length (List.zip [ 1; 2 ] [ "a" ])))
        C "list.unzip" (fun () -> let a, b = List.unzip [ 1, "a"; 2, "b" ] in Ji a + J b)
        C "list.collect" (fun () -> Ji (List.collect (fun x -> [ x; x ]) [ 1; 2 ]))
        C "list.choose" (fun () -> Ji (List.choose (fun x -> if x > 3 then Some (x * 2) else None) (l ())))
        C "list.find" (fun () -> string (List.find (fun x -> x > 4) (l ())) + "," + string (List.findIndex (fun x -> x > 4) (l ())) + "," + string (List.findBack (fun x -> x < 3) (l ())))
        C "list.find.missing" (fun () -> string (List.find (fun x -> x > 100) (l ())))
        C "list.tryfind" (fun () -> string (List.tryFind (fun x -> x > 100) (l ())) + "," + string (List.tryFindIndex (fun x -> x = 5) (l ())))
        C "list.pick" (fun () -> string (List.pick (fun x -> if x > 4 then Some (x * 100) else None) (l ())))
        C "list.exists" (fun () -> B (List.exists ((=) 9) (l ())) + "," + B (List.forall (fun x -> x > 0) (l ())) + "," + B (List.contains 7 (l ())))
        C "list.take.skip" (fun () -> Ji (List.take 2 (l ())) + Ji (List.skip 6 (l ())) + Ji (List.truncate 20 [ 1 ]))
        C "list.take.too.many" (fun () -> Ji (List.take 20 (l ())))
        C "list.takewhile" (fun () -> Ji (List.takeWhile (fun x -> x > 2) (l ())) + Ji (List.skipWhile (fun x -> x > 2) (l ())))
        C "list.item" (fun () -> string (List.item 2 (l ())) + "," + string (l ()).[3])
        C "list.item.out.of.range" (fun () -> string (List.item 20 (l ())))
        C "list.last" (fun () -> string (List.last (l ())) + "," + string (List.tryLast (e ())))
        C "list.rev" (fun () -> Ji (List.rev [ 1; 2; 3 ]))
        C "list.init.replicate" (fun () -> Ji (List.init 3 (fun i -> i * i)) + Ji (List.replicate 2 7))
        C "list.mapi.indexed" (fun () -> J (List.mapi (fun i x -> string i + "=" + string x) [ 5; 6 ]) + J (List.indexed [ 'a' ] |> List.map string))
        C "list.map2.map3" (fun () -> Ji (List.map2 (+) [ 1; 2 ] [ 10; 20 ]) + Ji (List.map3 (fun a b c -> a + b + c) [ 1 ] [ 2 ] [ 3 ]))
        C "list.iter" (fun () -> let mutable s = "" in List.iteri (fun i x -> s <- s + string i + string x) [ 7; 8 ]; s)
        C "list.except" (fun () -> Ji (List.except [ 1; 5 ] (l ())))
        C "list.splitat" (fun () -> let a, b = List.splitAt 3 (l ()) in Ji a + Ji b)
        C "list.allpairs" (fun () -> J (List.allPairs [ 1; 2 ] [ 'a'; 'b' ] |> List.map (fun (a, b) -> string a + string b)))
        C "list.transpose" (fun () -> J (List.transpose [ [ 1; 2 ]; [ 3; 4 ] ] |> List.map Ji))
        C "list.unfold" (fun () -> Ji (List.unfold (fun x -> if x > 20 then None else Some (x, x * 2)) 1))
        C "list.mapfold" (fun () -> let r, s = List.mapFold (fun acc x -> x * 2, acc + x) 0 [ 1; 2; 3 ] in Ji r + string s)
        C "list.equality" (fun () -> B ([ 1; 2 ] = [ 1; 2 ]) + "," + B ([ 1; 2 ] < [ 1; 3 ]) + "," + string (compare [ 1; 2 ] [ 1 ]))
        C "list.comprehension" (fun () -> Ji [ for x in 1 .. 4 do if x % 2 = 0 then yield x * x ])
        C "list.comprehension.nested" (fun () -> J [ for a in 1 .. 2 do for b in [ "x"; "y" ] -> string a + b ])
        C "list.updateat" (fun () -> Ji (List.updateAt 1 9 [ 1; 2; 3 ]) + Ji (List.insertAt 0 0 [ 1 ]) + Ji (List.removeAt 0 [ 1; 2 ]))
        C "list.exactlyone" (fun () -> string (List.exactlyOne [ 4 ]) + "," + string (List.tryExactlyOne [ 1; 2 ]))
        C "list.randomless.permute" (fun () -> Ji (List.permute (fun i -> (i + 1) % 3) [ 1; 2; 3 ]))
    ]

[<JavaScript>]
module Arrays =
    let private a () = [| 3; 1; 4; 1; 5 |]

    let Cases () = [
        C "array.basics" (fun () -> let x = a () in x.[0] <- 9; Ji x + string x.Length)
        C "array.index.out.of.range" (fun () -> string (a ()).[10])
        C "array.set.out.of.range" (fun () -> let x = a () in x.[10] <- 1; "set")
        C "array.slice" (fun () -> Ji (a ()).[1..2] + Ji (a ()).[3..] + Ji (a ()).[..0])
        C "array.slice.empty" (fun () -> Ji (a ()).[3..1])
        C "array.create.zero" (fun () -> Ji (Array.zeroCreate 3) + J (Array.create 2 "x"))
        C "array.zerocreate.string" (fun () -> let x : string[] = Array.zeroCreate 1 in B (isNull x.[0]))
        C "array.sort.inplace" (fun () -> let x = a () in Array.sortInPlace x; Ji x)
        C "array.sort.copy" (fun () -> let x = a () in let y = Array.sort x in Ji x + Ji y)
        C "array.map.filter" (fun () -> Ji (a () |> Array.filter (fun x -> x > 1) |> Array.map (fun x -> x + 1)))
        C "array.fold" (fun () -> string (Array.fold (+) 0 (a ())) + "," + string (Array.sum (a ())))
        C "array.blit" (fun () -> let x = a () in Array.blit [| 7; 8 |] 0 x 1 2; Ji x)
        C "array.blit.bad" (fun () -> let x = a () in Array.blit [| 7; 8 |] 0 x 4 2; Ji x)
        C "array.fill" (fun () -> let x = a () in Array.fill x 1 2 0; Ji x)
        C "array.append.concat" (fun () -> Ji (Array.append [| 1 |] [| 2 |]) + Ji (Array.concat [ [| 1 |]; [| 2; 3 |] ]))
        C "array.copy.independent" (fun () -> let x = a () in let y = Array.copy x in y.[0] <- 0; string x.[0])
        C "array.rev" (fun () -> Ji (Array.rev (a ())))
        C "array.equality" (fun () -> B ([| 1; 2 |] = [| 1; 2 |]) + "," + B (obj.ReferenceEquals([| 1 |], [| 1 |])))
        C "array.compare" (fun () -> string (compare [| 1; 2 |] [| 1; 3 |]) + "," + string (compare [| 1; 2; 3 |] [| 2 |]))
        C "array.hash.equal" (fun () -> B (hash [| 1; 2 |] = hash [| 1; 2 |]))
        C "array.contains" (fun () -> B (Array.contains 4 (a ())) + "," + string (Array.IndexOf(a (), 1)))
        C "array.init" (fun () -> Ji (Array.init 4 (fun i -> i * 2)))
        C "array.binarysearch" (fun () -> string (Array.BinarySearch([| 1; 3; 5; 7 |], 5)))
        C "array.resize" (fun () -> let mutable x = [| 1; 2 |] in Array.Resize(&x, 4); Ji x)
        C "array.reverse.inplace" (fun () -> let x = a () in Array.Reverse x; Ji x)
        C "array.sortby.inplace" (fun () -> let x = [| "bb"; "a"; "ccc" |] in Array.sortInPlaceBy String.length x; J x)
        C "array.choose.pick" (fun () -> Ji (Array.choose (fun x -> if x > 2 then Some x else None) (a ())))
        C "array.partition" (fun () -> let p, q = Array.partition (fun x -> x > 2) (a ()) in Ji p + Ji q)
        C "array.sub" (fun () -> Ji (Array.sub (a ()) 1 3))
        C "array.sub.bad" (fun () -> Ji (Array.sub (a ()) 3 5))
        C "array.max.empty" (fun () -> string (Array.max ([||]: int[])))
        C "array.2d" (fun () -> let m = Array2D.init 2 3 (fun i j -> i * 10 + j) in string m.[1, 2] + "," + string (Array2D.length1 m) + "," + string (Array2D.length2 m))
        C "array.2d.set" (fun () -> let m = Array2D.zeroCreate 2 2 in m.[1, 1] <- 5; string (m.[1, 1] + m.[0, 0]))
        C "array.jagged" (fun () -> let m = [| [| 1 |]; [| 2; 3 |] |] in string m.[1].[1])
        C "array.comprehension" (fun () -> Ji [| for i in 0 .. 3 -> i * i |])
        C "array.for.in" (fun () -> let mutable s = 0 in (for x in a () do s <- s + x); string s)
    ]

[<JavaScript>]
module Seqs =
    let Cases () = [
        C "seq.lazy" (fun () -> let mutable n = 0 in let s = Seq.map (fun x -> n <- n + 1; x) [ 1; 2; 3 ] in let _ = Seq.head s in string n)
        C "seq.reevaluates" (fun () -> let mutable n = 0 in let s = seq { n <- n + 1; yield n } in string (Seq.head s) + string (Seq.head s))
        C "seq.cache" (fun () -> let mutable n = 0 in let s = Seq.cache (seq { n <- n + 1; yield n }) in string (Seq.head s) + string (Seq.head s))
        C "seq.infinite" (fun () -> Ji (Seq.initInfinite (fun i -> i * 3) |> Seq.take 4))
        C "seq.expression" (fun () -> Ji (seq { yield 1; yield! [ 2; 3 ]; for i in 4 .. 5 do yield i }))
        C "seq.while" (fun () -> Ji (seq { let mutable i = 0 in while i < 3 do yield i; i <- i + 1 }))
        C "seq.tryfinally" (fun () -> let mutable log = "" in (for x in seq { try yield 1; yield 2 finally log <- log + "F" } do log <- log + string x); log)
        C "seq.use.dispose" (fun () -> let mutable log = "" in let d = { new IDisposable with member _.Dispose() = log <- log + "D" } in (for x in seq { use _ = d in yield 1 } do log <- log + string x); log)
        C "seq.take.too.many" (fun () -> Ji (Seq.take 5 [ 1; 2 ]))
        C "seq.truncate" (fun () -> Ji (Seq.truncate 5 [ 1; 2 ]))
        C "seq.skip.too.many" (fun () -> Ji (Seq.skip 5 [ 1; 2 ]))
        C "seq.head.empty" (fun () -> string (Seq.head Seq.empty<int>))
        C "seq.zip" (fun () -> J (Seq.zip [ 1; 2; 3 ] [ "a"; "b" ] |> Seq.map (fun (a, b) -> string a + b)))
        C "seq.sortby" (fun () -> Ji (Seq.sortBy (fun x -> -x) [ 1; 3; 2 ]))
        C "seq.groupby" (fun () -> J (Seq.groupBy (fun x -> x % 2) [ 1; 2; 3; 4 ] |> Seq.map (fun (k, xs) -> string k + ":" + Ji xs)))
        C "seq.distinct" (fun () -> Ji (Seq.distinct [ 3; 1; 3; 2; 1 ]))
        C "seq.compare" (fun () -> string (Seq.compareWith compare [ 1; 2 ] [ 1; 3 ]))
        C "seq.length" (fun () -> string (Seq.length (seq { 1 .. 10 })))
        C "seq.sum.empty" (fun () -> string (Seq.sum Seq.empty<int>))
        C "seq.average.empty" (fun () -> string (Seq.average Seq.empty<float>))
        C "seq.item" (fun () -> string (Seq.item 2 [ 5; 6; 7 ]))
        C "seq.item.missing" (fun () -> string (Seq.item 5 [ 5; 6; 7 ]))
        C "seq.chunk" (fun () -> J (Seq.chunkBySize 2 [ 1; 2; 3 ] |> Seq.map Ji))
        C "seq.windowed" (fun () -> J (Seq.windowed 2 [ 1; 2; 3 ] |> Seq.map Ji))
        C "seq.pairwise" (fun () -> J (Seq.pairwise [ 1; 2; 3 ] |> Seq.map (fun (a, b) -> string (a * b))))
        C "seq.append" (fun () -> Ji (Seq.append [ 1 ] [ 2 ] |> Seq.append [ 0 ]))
        C "seq.concat" (fun () -> Ji (Seq.concat [ [ 1 ]; []; [ 2; 3 ] ]))
        C "seq.unfold" (fun () -> Ji (Seq.unfold (fun (a, b) -> if a > 20 then None else Some (a, (b, a + b))) (0, 1)))
        C "seq.scan" (fun () -> Ji (Seq.scan (+) 0 [ 1; 2; 3 ]))
        C "seq.exists" (fun () -> B (Seq.exists (fun x -> x > 2) [ 1; 3 ]) + "," + B (Seq.forall2 (=) [ 1 ] [ 1 ]))
        C "seq.tolist.toarray" (fun () -> string (List.length (Seq.toList (seq { 1 .. 3 }))) + string (Array.length (Seq.toArray [ 1 ])))
        C "seq.range.empty" (fun () -> Ji (seq { 5 .. 1 }))
        C "seq.enumerator" (fun () -> let e = (Seq.ofList [ 1; 2 ]).GetEnumerator() in let a = e.MoveNext() in B a + string e.Current)
        C "seq.max.by" (fun () -> string (Seq.maxBy String.length [ "a"; "ccc"; "bb" ]))
        C "seq.readonly" (fun () -> Ji (Seq.readonly [| 1; 2 |]))
        C "seq.delay" (fun () -> let mutable n = 0 in let s = Seq.delay (fun () -> n <- n + 1; Seq.singleton n) in string n + "," + Ji s)
    ]

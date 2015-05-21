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

module WebSharper.Tests.Seq

open WebSharper
open WebSharper.Testing
module R = WebSharper.Testing.Random

[<JavaScript>]
let Tests =

    Section "Seq" {

        Test "Seq.append" {
            Equal (Seq.append (seq { 1 .. 5 }) (seq { 6 .. 10 }) |> Seq.toArray)
                [| 1 .. 10 |]
            ForR 100 (R.ArrayOf R.Int) (fun x -> Do {
                Equal (Seq.toArray (Seq.append x [])) x
                Equal (Seq.toArray (Seq.append [] x)) x
            })
            ForR 100 (R.Tuple2Of (R.ArrayOf R.Int, R.ArrayOf R.Int))
                (fun (x, y) -> Do {
                    let s1 = Array.toSeq x
                    let s2 = Array.toSeq y
                    Equal (Seq.length (Seq.append s1 s2))
                        (Seq.length s1 + Seq.length s2)
                })
            ForR 100 (R.Tuple2Of (R.ArrayOf R.Int, R.ArrayOf R.Int))
                (fun (x, y) -> Do {
                    let s1 = Array.toSeq x
                    let s2 = Array.toSeq y
                    Equal (Seq.toArray (Seq.append s1 s2))
                        (Array.append (Array.ofSeq s1) (Array.ofSeq s2))
                })
        }

        Test "Seq.average" {
            Equal (seq { 0. .. 100. } |> Seq.average) 50.
        }

        Test "Seq.averageBy" {
            Equal (Seq.averageBy float (seq { 0 .. 100 })) 50.
        }

        Test "Seq.cache" {
            ForR 100 (R.ArrayOf R.Int) (fun x -> Do {
                let s = Seq.toArray (Seq.cache x)
                Equal (Seq.toArray s) x
            })
        }

        Test "Seq.cast" {
            Equal (Seq.cast [| 1; 2; 3 |] |> Seq.toArray) [| 1; 2; 3 |]
        }

        Test "Seq.choose" {
            let oneToSix = seq { 1 .. 6 }
            let sixToString n =
                match n with
                | 6 -> Some "six"
                | _ -> None
            Equal (Seq.choose sixToString oneToSix |> Seq.toArray) [| "six" |]
        }

        Test "Seq.collect" {
            let f (x : int) = Seq.singleton x
            Equal (Seq.collect f (seq { 0 .. 100 }) |> Seq.toArray)
                [| 0 .. 100 |]
        }

        Test "Seq.compareWith" {
            let s1 = seq { 1 .. 10 }
            let s2 = seq { 1 .. 10 }
            Equal (Seq.compareWith compare s2 s1) 0
            Equal (Seq.compareWith compare Seq.empty<int> Seq.empty<int>) 0
            Equal (Seq.compareWith compare [1] []) 1
            Equal (Seq.compareWith compare [] [1]) -1
            Equal (Seq.compareWith compare [1; 2; 5] [1; 3; 2]) -1
            Equal (Seq.compareWith compare [1; 3; 2] [1; 2; 5; 9; 10]) 1
        }

        Test "Seq.concat" {
            let seqOfSeqs = [| seq { 1 .. 5 }
                               Seq.empty
                               Seq.singleton 6
                               seq { 7 .. 10 }
                            |]
            let array = seqOfSeqs |> Seq.concat |> Seq.toArray
            Equal array [| 1 .. 10 |]
        }

        Test "Seq.countBy" {
            let r = ref 0
            let s = Seq.countBy id (seq {for i = 1 to 10 do r := !r + 1; yield 0})
            Equal !r 0
            Equal (s |> Seq.toArray) [|0, 10 |]
            Equal !r 10
            Equal (Seq.countBy (fun _ -> 1) [| 1 .. 100 |] |> Seq.toArray) [| 1, 100 |]
            Equal (Seq.countBy id [1; 2; 1; 3; 3; 4; 1; 1] |> Seq.toArray)
                [| (1, 4); (2, 1); (3, 2); (4, 1) |]
            Equal (Seq.countBy id [2; 1; 2] |> Seq.toArray)
                [| (2, 2) ; (1, 1) |]
            let xs : list<int> = []
            Equal (Seq.countBy id xs |> Seq.toArray) [||]
        }

        Test "Seq.delay" {
            let r = ref 0
            let s = Seq.delay (fun () -> incr r; [| 1 |] :> _)
            Equal !r 0
            Equal (Seq.toArray s) [| 1 |]
            Equal !r 1
        }

        Test "Seq.distinct" {
            let xs = seq [0; 1; 2; 3; 0; 3; 2; 3; 0; 0; 0]
            Equal (Seq.distinct xs |> Seq.toArray) [| 0 .. 3 |]
        }

        Test "Seq.distinctBy" {
            let xs = seq [0; 1 ; 2; 3; 0; 3; 2; 3; 0; 0; 0]
            Equal (Seq.distinctBy ((+) 1) xs |> Seq.toArray)
                [| 0 .. 3 |]
        }

        Test "Seq.empty" {
            Equal (Seq.empty |> Seq.toArray) [||]
        }

        Test "Seq.exists" {
            let xs = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5]
            True (Seq.exists ((=) 9) xs)
            True (Seq.exists ((=) 5) xs)
            False (Seq.exists ((=) -6) xs)
        }

        Test "Seq.exists2" {
            let xs1 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5; 6]
            let xs2 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5]
            True (Seq.exists2 (fun x y -> (x = y) && (x = 0)) xs1 xs2)
            True (Seq.exists2 (fun x y -> (x = y) && (x = 5)) xs1 xs2)
            False (Seq.exists2 (fun x y -> (x = y) && (x = -6)) xs1 xs2)
        }

        Test "Seq.filter" {
            let xs = seq { 1 .. 10 }
            let ffalse (x : int) = false
            let ftrue (x : int) = true
            let f x = x <= 5
            Equal (Seq.filter f xs |> Seq.toArray) [| 1 .. 5 |]
            Equal (Seq.filter ffalse xs |> Seq.length) 0
            Equal (Seq.filter ftrue xs |> Seq.length) (Seq.length xs)
        }

        Test "Seq.find" {
            let xs = seq [7; 1 ; 2; 9; 0; 3; 2; 3; 0; 9; 5]
            Equal (Seq.find (fun x -> x % 3 = 0) xs) 9
            let s0 = seq [None; None; Some 4; None]
            Equal (Seq.find Option.isSome s0) (Some 4)
            Raises (Seq.find ((=) (Some 5)) s0)
        }

        Test "Seq.findIndex" {
            let xs = seq [7; 1 ; 2; 9; 0; 3; 2; 3; 0; 9; 5]
            Equal (Seq.findIndex (fun x -> x % 3 = 0) xs) 3
            let s0 = seq [None; None; Some 4; None]
            Equal (Seq.findIndex (Option.isSome) s0) 2
            Raises (Seq.findIndex ((=) (Some 5)) s0)
        }

        Test "Seq.fold" {
            Equal (Seq.fold (+) 0 <| seq { 1 .. 5 }) 15
            let s = Seq.init 10 (fun n -> seq { 1 .. 2 })
            Equal (Seq.fold (fun x y -> x + Seq.length y) 0 <| s) 20
            ForR 100 (R.ListOf R.Int) (fun x -> Do {
                let a = Array.fold (+) 0 (Array.ofList x)
                Equal (Seq.fold (+) 0 <| List.toSeq x) a
            })
        }

        Test "Seq.forall" {
            let xs1 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5; 21]
            let xs2 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5]
            let a = Seq.forall ((<>) 21)
            True (a xs2)
            False (a xs1)
            ForR 100 (R.ArrayOf R.Int) (fun s -> Do {
                let f a = a % 2 = 0
                Equal (Seq.forall f s) (not (Seq.exists (not << f) s))
            })
        }

        Test "Seq.forall2" {
            False (Seq.forall2 (fun x y -> x = y) [1;2;3] [1;3;3])
            ForR 100 (R.ArrayOf R.Int) (fun xs -> Do {
                True (Seq.forall2 (=) xs xs)
            })
        }

        Test "Seq.groupBy" {
            let gs =
                [| 0 .. 9 |]
                |> Seq.groupBy (fun n -> if n < 5 then 0 else 1)
                |> Seq.map (fun (k,xs) -> k, Seq.toArray xs)
                |> Seq.toArray
            Equal gs [| (0, [| 0 .. 4 |]); (1, [| 5 .. 9 |]) |]
            let gs =
                [2; 1; 2]
                |> Seq.groupBy id
                |> Seq.map (fun (k,xs) -> k, Seq.toArray xs)
                |> Seq.toArray
            Equal gs [| (2, [| 2; 2 |]); (1, [| 1 |]) |]
        }

        Test "Seq.head" {
            let s = seq ["apa"]
            Equal (Seq.head s) "apa"
            Equal (Seq.head [1; 2; 3]) 1
            Raises (Seq.head Seq.empty)
        }

        Test "Seq.init" {
            let s = Seq.init 10 (fun n -> n)
            Raises (Seq.init -1 ignore)
            Equal (Seq.toArray s) [|0..9|]
        }

        Test "Seq.initInfinite" {
            let n = Seq.initInfinite (fun x -> x)
            Equal (Seq.take 5 n |> Seq.toArray) [| 0 .. 4 |]
            Equal (Seq.take 10 n |> Seq.toArray) [| 0 .. 9 |]
        }

        Test "Seq.isEmpty" {
            True (Seq.isEmpty Seq.empty)
            False (Seq.isEmpty [1])
        }

        Test "Seq.iter" {
            let r = ref 0
            Seq.iter (fun x -> r := !r + x) [1..10]
            Equal !r 55
        }

        Test "Seq.iter2" {
            let n = ref 0
            Seq.iter2 (fun a b -> n := !n * a + b) [1 .. 10] [1 .. 3]
            Equal !n 15
        }

        Test "Seq.iteri" {
            let r = ref 0
            Seq.iteri (fun i x -> r := !r * i + x) [1..10]
            Equal !r 1972819
        }

        Test "Seq.length" {
            Equal (Seq.length [1; 2; 3]) 3
            Equal (Seq.length []) 0
        }

        Test "Seq.map" {
            let oneToTen = seq { 1 .. 10 }
            let s = Seq.map (fun x -> x % 2) oneToTen
            Equal (Seq.toArray s) [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; |]
        }

        Test "Seq.map2" {
            let oneToTen = seq { 1 .. 10 }
            let tenToTwentytwo = seq { 10 .. 22 }
            let s = Seq.map2 (fun x y -> x % 2) oneToTen tenToTwentytwo
            Equal (Seq.toArray s) [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; |]
        }

        Test "Seq.mapi" {
            let oneToTen = seq { 1 .. 10 }
            let s = Seq.mapi (fun x y -> x % 2) oneToTen
            Equal (Seq.toArray s) [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; |]
        }

        Test "Seq.max" {
            Equal (Seq.max [1; 2; 3; 2; 1]) 3
        }

        Test "Seq.maxBy" {
            Equal (Seq.maxBy (fun x -> x + 1) [1; 2; 3; 2; 1]) 3
        }

        Test "Seq.min" {
            Equal (Seq.min [4; 5; 3; 5; 4]) 3
        }

        Test "Seq.minBy" {
            Equal (Seq.minBy (fun x -> x + 1) [4; 5; 3; 5; 4]) 3
        }

        Test "Seq.nth" {
            Equal (Seq.nth 2 [1; 2; 3]) 3
        }

        Test "Seq.ofArray" {
            Equal (Seq.ofArray [| 1..3 |] |> Seq.toArray) [| 1..3 |]
        }

        Test "Seq.ofList" {
            Equal (Seq.ofList [1..3] |> Seq.toList) [1..3]
        }

        Test "Seq.pairwise" {
            Equal (seq {1..4} |> Seq.pairwise |> Seq.toArray)
                [| (1, 2); (2, 3); (3, 4) |]
        }

        Test "Seq.pick" {
            let finder x = if x = 4 then Some x else None
            Equal (Seq.pick finder [4]) 4
        }

        Test "Seq.readonly" {
            let a = [| 1 .. 3 |]
            let b = Seq.readonly a
            Equal (Seq.toArray b) [| 1 .. 3 |]
        }

        Test "Seq.reduce" {
            Equal (Seq.reduce (+) [1;1;1;1;1]) 5
            Equal (Seq.reduce (+) [3]) 3
        }

        Test "Seq.scan" {
            let l3 = seq [seq [2;1]; seq [1;2;3]]
            Equal (Seq.scan (fun x y -> x + Seq.length y) 0 l3 |> Seq.toArray)
                [|0;2;5|]
            ForR 100 (R.ArrayOf R.Int) (fun x -> Do {
                let bySeq = Seq.scan (+) 0 x
                let byArray = Array.scan (+) 0 x
                Equal (Seq.toArray bySeq) byArray
            })
        }

        Test "Seq.singleton" {
            let six = Seq.singleton 6
            Equal (Seq.toArray six) [| 6 |]
        }

        Test "Seq.skip" {
            let s = seq { 1 .. 10 }
            Equal (Seq.toArray (Seq.skip 5 s)) [| 6; 7; 8; 9; 10 |]
        }

        Test "Seq.skipWhile" {
            let s = seq {1..10}
            Equal (Seq.toArray (Seq.skipWhile ((>) 6) s)) [|6;7;8;9;10|]
            Equal (Seq.toArray (Seq.skipWhile ((>) 6) [])) [||]
        }

        Test "Seq.sort" {
            Equal (Seq.sort [| 1; 8; -5; 2 |] |> Seq.toArray)
                [| -5; 1; 2; 8 |]
        }

        Test "Seq.sortBy" {
            let s = Seq.sortBy (fun x -> 10 - x) [6;7;8;9;10]
            Equal (Seq.toArray s) [|10;9;8;7;6|]
        }

        Test "Seq.sum" {
            let s1 = seq {1..4}
            let s2 = seq [1.; 2.; 3.; 4.]
            Equal (Seq.sum s1) 10
            Equal (Seq.sum s2) 10.
            Equal (Seq.sum<int> []) 0
        }

        Test "Seq.sumBy" {
            Equal (Seq.sumBy (fun x -> x * x) [|1 .. 4|]) 30
            Equal (Seq.sumBy (fun x -> x * x) [||]) 0
        }

        Test "Seq.take" {
            let s = seq { 1 .. 10 }
            Equal (Seq.toArray (Seq.take 5 s)) [| 1; 2; 3; 4; 5 |]
            Equal (Seq.toArray (Seq.take 10 s)) [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
            Raises (Seq.toArray (Seq.take -1 s))
            Raises (Seq.toArray (Seq.take 11 s))
        }

        Test "Seq.takeWhile" {
            let s = seq {1..10}
            Equal (Seq.toArray (Seq.takeWhile ((>) 6) s))
                [|1;2;3;4;5|]
        }

        Test "Seq.toArray" {
            Equal (Seq.toArray [1..3]) [| 1..3 |]
        }

        Test "Seq.toList" {
            Equal (Seq.toList [|1..3|]) [1..3]
        }

        Test "Seq.truncate" {
            Equal (Seq.truncate 3 [1..10] |> Seq.toArray) [|1; 2; 3|]
            Equal (Seq.truncate 3 [1]     |> Seq.toArray) [|1|]
        }

        Test "Seq.tryFind" {
            Equal (Seq.tryFind (fun x -> fst x = 3)
                    [(1, "A"); (2, "B"); (3, "C")]) (Some (3, "C"))
            Equal (Seq.tryFind (fun x -> fst x = 3)
                    [(1, "A"); (2, "B")]) None
        }

        Test "Seq.tryFindIndex" {
            Equal (Seq.tryFindIndex (fun x -> fst x = 3)
                    [(1, "A"); (2, "B"); (3, "C")]) (Some 2)
            Equal (Seq.tryFindIndex (fun x -> fst x = 3)
                    [(1, "A"); (2, "B")]) None
        }

        Test "Seq.tryPick" {
            let finder x = if x = 4 then Some x else None
            Equal (Seq.tryPick finder [4]) (Some 4)
            Equal (Seq.tryPick finder [3]) None
        }

        Test "Seq.unfold" {
            Equal (Seq.unfold (fun s ->
                    if s < 3 then
                        Some (s, s + 1)
                    else
                        None)
                0
                |> Seq.toArray) [| 0; 1; 2 |]
        }

        Test "Seq.windowed" {
            Equal (Seq.windowed 3 [1..5] |> Seq.toArray)
                [| [| 1; 2; 3 |]
                   [| 2; 3; 4 |]
                   [| 3; 4; 5 |] |]
        }

        Test "Seq.zip" {
            let (s1:int seq), s2 = seq [], seq [1]
            let s1,s2 = seq ["a"],seq [1]
            let a1 = Seq.zip s1 s2
            Equal (Seq.toArray a1) [| ("a", 1) |]
        }

        Test "Seq.zip3" {
            let (arr1: int seq), arr2, arr3 = seq [], seq [1], seq [1]
            let arr1, arr2, arr3 = seq ["a"], seq [1], seq [(1, 2)]
            let a112 = Seq.zip3 arr1 arr2 arr3
            Equal (Seq.toArray a112) [| ("a", 1, (1, 2)) |]
        }

        Test "Comprehensions" {
            Equal (seq {for i = 1 to 10 do yield i} |> Seq.toArray)
                [| 1 .. 10 |]
            Equal (seq {for x in 1..5 -> x * x} |> Seq.toArray)
                [| 1; 4; 9; 16; 25 |]
            let vec1 = seq [1; 2; 3]
            let vec2 = seq [4; 5; 6]
            let l2 =
                seq {
                    for x in vec1 do
                        for y in vec2 do
                            yield x * y
                }
            Equal (Seq.toArray l2) [|4; 5; 6; 8; 10; 12; 12; 15; 18|]
            let l3 n =
                seq {
                    for row in 1 .. n do
                        for col in 1 .. n do
                            if (row + col) % 2 = 0 then
                                yield (row, col)
                }
            let sol = [| (1, 1); (1, 3); (1, 5); (2, 2); (2, 4);
                         (3, 1); (3, 3); (3, 5); (4, 2); (4, 4);
                         (5, 1); (5, 3); (5, 5) |]
            Equal (Seq.toArray (l3 5)) sol
            let fibonacci =
                let rec fibonacciInner a b =
                    seq {
                        let c = a + b
                        if c < 10 then
                            yield c
                            yield! fibonacciInner b c
                        else
                            yield c
                    }
                fibonacciInner 1 1
            Equal (Seq.toArray fibonacci) [| 2; 3; 5; 8; 13 |]
            let r = ref 0
            Raises (
                seq {
                    try
                        for i in 1 .. 10 do
                            if i = 10 then
                                failwith "10"
                            else
                                yield i
                    finally
                        incr r
                }
                |> Seq.toArray)
            Equal !r 1
            True (seq { 1 .. -1 } |> Seq.isEmpty)
            True (seq { 1 .. -1 .. 3 } |> Seq.isEmpty)
        }

    }
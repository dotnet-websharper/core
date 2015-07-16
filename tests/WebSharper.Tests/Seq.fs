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
type CustomHash(a: int, b: int) =    
    member this.A = a
    member this.B = b
    override this.GetHashCode() = a
    override this.Equals other =
        match other with
        | :? CustomHash as o ->
            this.A = o.A && this.B = o.B
        | _ -> false

[<JavaScript>]
let Tests =

    TestCategory "Seq" {

        Test "Seq.append" {
            equal (Seq.append (seq { 1 .. 5 }) (seq { 6 .. 10 }) |> Seq.toArray)
                [| 1 .. 10 |]
            property (fun x -> Do {
                equal (Seq.toArray (Seq.append x [])) x
                equal (Seq.toArray (Seq.append [] x)) x
            })
            property (fun (x, y) -> Do {
                    let s1 = Array.toSeq x
                    let s2 = Array.toSeq y
                    equal (Seq.length (Seq.append s1 s2))
                        (Seq.length s1 + Seq.length s2)
                })
            property (fun (x, y) -> Do {
                    let s1 = Array.toSeq x
                    let s2 = Array.toSeq y
                    equal (Seq.toArray (Seq.append s1 s2))
                        (Array.append (Array.ofSeq s1) (Array.ofSeq s2))
                })
        }

        Test "Seq.average" {
            equal (seq { 0. .. 100. } |> Seq.average) 50.
        }

        Test "Seq.averageBy" {
            equal (Seq.averageBy float (seq { 0 .. 100 })) 50.
        }

        Property "Seq.cache" (fun x -> Do {
            let s = Seq.toArray (Seq.cache (Array.toSeq x))
            equal (Seq.toArray s) x
        })

        Test "Seq.cast" {
            equal (Seq.cast [| 1; 2; 3 |] |> Seq.toArray) [| 1; 2; 3 |]
        }

        Test "Seq.choose" {
            let oneToSix = seq { 1 .. 6 }
            let sixToString n =
                match n with
                | 6 -> Some "six"
                | _ -> None
            equal (Seq.choose sixToString oneToSix |> Seq.toArray) [| "six" |]
        }

        Test "Seq.collect" {
            let f (x : int) = Seq.singleton x
            equal (Seq.collect f (seq { 0 .. 100 }) |> Seq.toArray)
                [| 0 .. 100 |]
        }

        Test "Seq.compareWith" {
            let s1 = seq { 1 .. 10 }
            let s2 = seq { 1 .. 10 }
            equal (Seq.compareWith compare s2 s1) 0
            equal (Seq.compareWith compare Seq.empty<int> Seq.empty<int>) 0
            equal (Seq.compareWith compare [1] []) 1
            equal (Seq.compareWith compare [] [1]) -1
            equal (Seq.compareWith compare [1; 2; 5] [1; 3; 2]) -1
            equal (Seq.compareWith compare [1; 3; 2] [1; 2; 5; 9; 10]) 1
        }

        Test "Seq.concat" {
            let seqOfSeqs = [| seq { 1 .. 5 }
                               Seq.empty
                               Seq.singleton 6
                               seq { 7 .. 10 }
                            |]
            let array = seqOfSeqs |> Seq.concat |> Seq.toArray
            equal array [| 1 .. 10 |]
        }

        Test "Seq.countBy" {
            let r = ref 0
            let s = Seq.countBy id (seq {for i = 1 to 10 do r := !r + 1; yield 0})
            equal !r 0
            equal (s |> Seq.toArray) [|0, 10 |]
            equal !r 10
            equal (Seq.countBy (fun _ -> 1) [| 1 .. 100 |] |> Seq.toArray) [| 1, 100 |]
            equal (Seq.countBy id [1; 2; 1; 3; 3; 4; 1; 1] |> Seq.toArray)
                [| (1, 4); (2, 1); (3, 2); (4, 1) |]
            equal (Seq.countBy id [2; 1; 2] |> Seq.toArray)
                [| (2, 2) ; (1, 1) |]
            let xs : list<int> = []
            equal (Seq.countBy id xs |> Seq.toArray) [||]
        }

        Test "Seq.delay" {
            let r = ref 0
            let s = Seq.delay (fun () -> incr r; [| 1 |] :> _)
            equal !r 0
            equal (Seq.toArray s) [| 1 |]
            equal !r 1
        }

        Test "Seq.distinct" {
            let xs = seq [0; 1; 2; 3; 0; 3; 2; 3; 0; 0; 0]
            equal (Seq.distinct xs |> Seq.toArray) [| 0 .. 3 |]

            let ys = [| CustomHash(0, 0); CustomHash(0, 1) |]
            equal (Seq.distinct ys |> Seq.toArray) ys
        }

        Test "Seq.distinctBy" {
            let xs = seq [0; 1 ; 2; 3; 0; 3; 2; 3; 0; 0; 0]
            equal (Seq.distinctBy ((+) 1) xs |> Seq.toArray)
                [| 0 .. 3 |]
        }

        Test "Seq.empty" {
            equal (Seq.empty |> Seq.toArray) [||]
        }

        Test "Seq.exists" {
            let xs = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5]
            isTrue (Seq.exists ((=) 9) xs)
            isTrue (Seq.exists ((=) 5) xs)
            isFalse (Seq.exists ((=) -6) xs)
        }

        Test "Seq.exists2" {
            let xs1 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5; 6]
            let xs2 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5]
            isTrue (Seq.exists2 (fun x y -> (x = y) && (x = 0)) xs1 xs2)
            isTrue (Seq.exists2 (fun x y -> (x = y) && (x = 5)) xs1 xs2)
            isFalse (Seq.exists2 (fun x y -> (x = y) && (x = -6)) xs1 xs2)
        }

        Test "Seq.filter" {
            let xs = seq { 1 .. 10 }
            let ffalse (x : int) = false
            let ftrue (x : int) = true
            let f x = x <= 5
            equal (Seq.filter f xs |> Seq.toArray) [| 1 .. 5 |]
            equal (Seq.filter ffalse xs |> Seq.length) 0
            equal (Seq.filter ftrue xs |> Seq.length) (Seq.length xs)
        }

        Test "Seq.find" {
            let xs = seq [7; 1 ; 2; 9; 0; 3; 2; 3; 0; 9; 5]
            equal (Seq.find (fun x -> x % 3 = 0) xs) 9
            let s0 = seq [None; None; Some 4; None]
            equal (Seq.find Option.isSome s0) (Some 4)
            raises (Seq.find ((=) (Some 5)) s0)
        }

        Test "Seq.findIndex" {
            let xs = seq [7; 1 ; 2; 9; 0; 3; 2; 3; 0; 9; 5]
            equal (Seq.findIndex (fun x -> x % 3 = 0) xs) 3
            let s0 = seq [None; None; Some 4; None]
            equal (Seq.findIndex (Option.isSome) s0) 2
            raises (Seq.findIndex ((=) (Some 5)) s0)
        }

        Test "Seq.fold" {
            equal (Seq.fold (+) 0 <| seq { 1 .. 5 }) 15
            let s = Seq.init 10 (fun n -> seq { 1 .. 2 })
            equal (Seq.fold (fun x y -> x + Seq.length y) 0 <| s) 20
            property (fun x -> Do {
                let a = Array.fold (+) 0 x
                equal (Seq.fold (+) 0 <| Array.toSeq x) a
            })
        }

        Test "Seq.forall" {
            let xs1 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5; 21]
            let xs2 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5]
            let a = Seq.forall ((<>) 21)
            isTrue (a xs2)
            isFalse (a xs1)
            property (fun s -> Do {
                let f a = a % 2 = 0
                equal (Seq.forall f s) (not (Seq.exists (not << f) s))
            })
        }

        Test "Seq.forall2" {
            isFalse (Seq.forall2 (fun x y -> x = y) [1;2;3] [1;3;3])
            property (fun xs -> Do {
                isTrue (Seq.forall2 (=) xs xs)
            })
        }

        Test "Seq.groupBy" {
            let gs =
                [| 0 .. 9 |]
                |> Seq.groupBy (fun n -> if n < 5 then 0 else 1)
                |> Seq.map (fun (k,xs) -> k, Seq.toArray xs)
                |> Seq.toArray
            equal gs [| (0, [| 0 .. 4 |]); (1, [| 5 .. 9 |]) |]
            let gs =
                [2; 1; 2]
                |> Seq.groupBy id
                |> Seq.map (fun (k,xs) -> k, Seq.toArray xs)
                |> Seq.toArray
            equal gs [| (2, [| 2; 2 |]); (1, [| 1 |]) |]
        }

        Test "Seq.head" {
            let s = seq ["apa"]
            equal (Seq.head s) "apa"
            equal (Seq.head [1; 2; 3]) 1
            raises (Seq.head Seq.empty)
        }

        Test "Seq.init" {
            let s = Seq.init 10 (fun n -> n)
            raises (Seq.init -1 ignore)
            equal (Seq.toArray s) [|0..9|]
        }

        Test "Seq.initInfinite" {
            let n = Seq.initInfinite (fun x -> x)
            equal (Seq.take 5 n |> Seq.toArray) [| 0 .. 4 |]
            equal (Seq.take 10 n |> Seq.toArray) [| 0 .. 9 |]
        }

        Test "Seq.isEmpty" {
            isTrue (Seq.isEmpty Seq.empty)
            isFalse (Seq.isEmpty [1])
        }

        Test "Seq.iter" {
            let r = ref 0
            Seq.iter (fun x -> r := !r + x) [1..10]
            equal !r 55
        }

        Test "Seq.iter2" {
            let n = ref 0
            Seq.iter2 (fun a b -> n := !n * a + b) [1 .. 10] [1 .. 3]
            equal !n 15
        }

        Test "Seq.iteri" {
            let r = ref 0
            Seq.iteri (fun i x -> r := !r * i + x) [1..10]
            equal !r 1972819
        }

        Test "Seq.last" {
            let s = seq ["apa"]
            equal (Seq.last s) "apa"
            equal (Seq.last [1; 2; 3]) 3
            raises (Seq.last Seq.empty)
        }

        Test "Seq.length" {
            equal (Seq.length [1; 2; 3]) 3
            equal (Seq.length []) 0
        }

        Test "Seq.map" {
            let oneToTen = seq { 1 .. 10 }
            let s = Seq.map (fun x -> x % 2) oneToTen
            equal (Seq.toArray s) [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; |]
        }

        Test "Seq.map2" {
            let oneToTen = seq { 1 .. 10 }
            let tenToTwentytwo = seq { 10 .. 22 }
            let s = Seq.map2 (fun x y -> x % 2) oneToTen tenToTwentytwo
            equal (Seq.toArray s) [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; |]
        }

        Test "Seq.mapi" {
            let oneToTen = seq { 1 .. 10 }
            let s = Seq.mapi (fun x y -> x % 2) oneToTen
            equal (Seq.toArray s) [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; |]
        }

        Test "Seq.max" {
            equal (Seq.max [1; 2; 3; 2; 1]) 3
        }

        Test "Seq.maxBy" {
            equal (Seq.maxBy (fun x -> x + 1) [1; 2; 3; 2; 1]) 3
        }

        Test "Seq.min" {
            equal (Seq.min [4; 5; 3; 5; 4]) 3
        }

        Test "Seq.minBy" {
            equal (Seq.minBy (fun x -> x + 1) [4; 5; 3; 5; 4]) 3
        }

        Test "Seq.nth" {
            equal (Seq.nth 2 [1; 2; 3]) 3
        }

        Test "Seq.ofArray" {
            equal (Seq.ofArray [| 1..3 |] |> Seq.toArray) [| 1..3 |]
        }

        Test "Seq.ofList" {
            equal (Seq.ofList [1..3] |> Seq.toList) [1..3]
        }

        Test "Seq.pairwise" {
            equal (seq {1..4} |> Seq.pairwise |> Seq.toArray)
                [| (1, 2); (2, 3); (3, 4) |]
        }

        Test "Seq.pick" {
            let finder x = if x = 4 then Some x else None
            equal (Seq.pick finder [4]) 4
        }

        Test "Seq.readonly" {
            let a = [| 1 .. 3 |]
            let b = Seq.readonly a
            equal (Seq.toArray b) [| 1 .. 3 |]
        }

        Test "Seq.reduce" {
            equal (Seq.reduce (+) [1;1;1;1;1]) 5
            equal (Seq.reduce (+) [3]) 3
        }

        Test "Seq.scan" {
            let l3 = seq [seq [2;1]; seq [1;2;3]]
            equal (Seq.scan (fun x y -> x + Seq.length y) 0 l3 |> Seq.toArray)
                [|0;2;5|]
            property (fun x -> Do {
                let bySeq = Seq.scan (+) 0 x
                let byArray = Array.scan (+) 0 x
                equal (Seq.toArray bySeq) byArray
            })
        }

        Test "Seq.singleton" {
            let six = Seq.singleton 6
            equal (Seq.toArray six) [| 6 |]
        }

        Test "Seq.skip" {
            let s = seq { 1 .. 10 }
            equal (Seq.toArray (Seq.skip 5 s)) [| 6; 7; 8; 9; 10 |]
        }

        Test "Seq.skipWhile" {
            let s = seq {1..10}
            equal (Seq.toArray (Seq.skipWhile ((>) 6) s)) [|6;7;8;9;10|]
            equal (Seq.toArray (Seq.skipWhile ((>) 6) [])) [||]
        }

        Test "Seq.sort" {
            equal (Seq.sort [| 1; 8; -5; 2 |] |> Seq.toArray)
                [| -5; 1; 2; 8 |]
        }

        Test "Seq.sortBy" {
            let s = Seq.sortBy (fun x -> 10 - x) [6;7;8;9;10]
            equal (Seq.toArray s) [|10;9;8;7;6|]
        }

        Test "Seq.sum" {
            let s1 = seq {1..4}
            let s2 = seq [1.; 2.; 3.; 4.]
            equal (Seq.sum s1) 10
            equal (Seq.sum s2) 10.
            equal (Seq.sum<int> []) 0
        }

        Test "Seq.sumBy" {
            equal (Seq.sumBy (fun x -> x * x) [|1 .. 4|]) 30
            equal (Seq.sumBy (fun x -> x * x) [||]) 0
        }

        Test "Seq.take" {
            let s = seq { 1 .. 10 }
            equal (Seq.toArray (Seq.take 5 s)) [| 1; 2; 3; 4; 5 |]
            equal (Seq.toArray (Seq.take 10 s)) [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
            raises (Seq.toArray (Seq.take -1 s))
            raises (Seq.toArray (Seq.take 11 s))
        }

        Test "Seq.takeWhile" {
            let s = seq {1..10}
            equal (Seq.toArray (Seq.takeWhile ((>) 6) s))
                [|1;2;3;4;5|]
        }

        Test "Seq.toArray" {
            equal (Seq.toArray [1..3]) [| 1..3 |]
        }

        Test "Seq.toList" {
            equal (Seq.toList [|1..3|]) [1..3]
        }

        Test "Seq.truncate" {
            equal (Seq.truncate 3 [1..10] |> Seq.toArray) [|1; 2; 3|]
            equal (Seq.truncate 3 [1]     |> Seq.toArray) [|1|]
        }

        Test "Seq.tryFind" {
            equal (Seq.tryFind (fun x -> fst x = 3)
                    [(1, "A"); (2, "B"); (3, "C")]) (Some (3, "C"))
            equal (Seq.tryFind (fun x -> fst x = 3)
                    [(1, "A"); (2, "B")]) None
        }

        Test "Seq.tryFindIndex" {
            equal (Seq.tryFindIndex (fun x -> fst x = 3)
                    [(1, "A"); (2, "B"); (3, "C")]) (Some 2)
            equal (Seq.tryFindIndex (fun x -> fst x = 3)
                    [(1, "A"); (2, "B")]) None
        }

        Test "Seq.tryPick" {
            let finder x = if x = 4 then Some x else None
            equal (Seq.tryPick finder [4]) (Some 4)
            equal (Seq.tryPick finder [3]) None
        }

        Test "Seq.unfold" {
            equal (Seq.unfold (fun s ->
                    if s < 3 then
                        Some (s, s + 1)
                    else
                        None)
                0
                |> Seq.toArray) [| 0; 1; 2 |]
        }

        Test "Seq.windowed" {
            equal (Seq.windowed 3 [1..5] |> Seq.toArray)
                [| [| 1; 2; 3 |]
                   [| 2; 3; 4 |]
                   [| 3; 4; 5 |] |]
        }

        Test "Seq.zip" {
            let (s1:int seq), s2 = seq [], seq [1]
            let s1,s2 = seq ["a"],seq [1]
            let a1 = Seq.zip s1 s2
            equal (Seq.toArray a1) [| ("a", 1) |]
        }

        Test "Seq.zip3" {
            let (arr1: int seq), arr2, arr3 = seq [], seq [1], seq [1]
            let arr1, arr2, arr3 = seq ["a"], seq [1], seq [(1, 2)]
            let a112 = Seq.zip3 arr1 arr2 arr3
            equal (Seq.toArray a112) [| ("a", 1, (1, 2)) |]
        }

        Test "Comprehensions" {
            equal (seq {for i = 1 to 10 do yield i} |> Seq.toArray)
                [| 1 .. 10 |]
            equal (seq {for x in 1..5 -> x * x} |> Seq.toArray)
                [| 1; 4; 9; 16; 25 |]
            let vec1 = seq [1; 2; 3]
            let vec2 = seq [4; 5; 6]
            let l2 =
                seq {
                    for x in vec1 do
                        for y in vec2 do
                            yield x * y
                }
            equal (Seq.toArray l2) [|4; 5; 6; 8; 10; 12; 12; 15; 18|]
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
            equal (Seq.toArray (l3 5)) sol
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
            equal (Seq.toArray fibonacci) [| 2; 3; 5; 8; 13 |]
            let r = ref 0
            raises (
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
            equal !r 1
            isTrue (seq { 1 .. -1 } |> Seq.isEmpty)
            isTrue (seq { 1 .. -1 .. 3 } |> Seq.isEmpty)
        }

    #if FSHARP40

        Test "Seq.contains" {
            isTrue (Seq.contains 0 seq { 0 .. 4 })
        }

        Test "Seq.chunkBySize" {
            equal (seq { yield [| 1 .. 4 |]; yield [| 5 .. 8 |] }) (Seq.chunkBySize 4 seq { 1 .. 8 })
            equal (seq { yield [| 1 .. 4 |]; yield [| 5 .. 8 |]; yield [| 9; 10 |] }) (Seq.chunkBySize 4 seq { 1 .. 10 })
            raises (Seq.chunkBySize 0)
        }

        Test "Seq.splitInto" {
            equal (Seq.splitInto 2 Seq.empty) Seq.empty
            raises ((Seq.splitInto 0) Seq.empty)
        }

        Test "Seq.except" {
            equal (Seq.except Seq.empty seq { 0; 1 }) (seq { 0 .. 1 })
            equal (Seq.except seq { 0 } seq { 0; 1 }) (seq { yield 1 })
        }

        Test "Seq.findBack" {
            raises (Seq.findBack (fun _ -> true) Seq.empty)
            equal (Seq.findBack (fun x -> x % 5 = 0) seq { 1 .. 10 }) 10
        }

        Test "Seq.findIndexBack" {
            raises (Seq.findIndexBack (fun _ -> true) Seq.empty)
            equal (Seq.findIndexBack (fun x -> x % 5 = 0) seq { 1 .. 10 }) 9
        }

        Test "Seq.indexed" {
            equal (Seq.indexed seq { 0 .. 4 }) (Seq.zip (seq { 0 .. 4 }) (seq { 0 .. 4 }))
            equal (Seq.indexed Seq.empty) Seq.empty
        }

        Test "Seq.item" {
            property (fun x -> Do {
                let list = seq { yield x }

                equal (Seq.item 0 list) x
            })
        }

        Test "Seq.mapFold" {
            equal (Seq.mapFold (fun s x -> (x + 1, s + x)) 0 seq { 0 .. 4 }) (Seq.map ((+) 1) (seq { 0 .. 4 }), Seq.sum (seq { 0 .. 4 }))
        }

        Test "Seq.mapFoldBack" {
            equal (Seq.mapFoldBack (fun x s -> (x + 1, s + x)) seq { 0 .. 4 } 0) (Seq.map ((+) 1) (seq { 0 .. 4 }), Seq.sum (seq { 0 .. 4 }))
        }

        Test "Seq.sortDescending" {
            equal (Seq.sortDescending seq { 0 .. 4 }) (seq { 4 .. -1 .. 0 })
        }

        Test "Seq.sortByDescending" {
            equal (Seq.sortByDescending (fun (x : string) -> x.Length) [ ".."; "."; "....."; "..."; "...."; ] |> Seq.ofList) ([ "....."; "...."; "..."; ".."; "." ] |> Seq.ofList)
        }

        Test "Seq.tryFindBack" {
            equal (Seq.tryFindBack (fun _ -> true) Seq.empty) None
            equal (Seq.tryFindBack (fun x -> x % 5 = 0) seq { 1 .. 10 }) (Some 10)
        }

        Test "Seq.tryFindIndexBack" {
            equal (Seq.tryFindIndexBack (fun _ -> true) Seq.empty) None
            equal (Seq.tryFindIndexBack (fun x -> x % 5 = 0) seq { 1 .. 10 }) (Some 9)
        }

        Test "Seq.tryHead" {
            equal (Seq.tryHead Seq.empty) None
            property (fun x -> Do {
                equal (Seq.tryHead seq { x; 0 }) (Some x)
            })
        }

        Test "Seq.tryItem" {
            equal (Seq.tryItem 0 Seq.empty) None
            property (fun x -> Do {
                equal (Seq.tryItem 0 seq { yield x; yield 1 }) (Some x)
            })
        }

        Test "Seq.tryLast" {
            equal (Seq.tryLast Seq.empty) None
            property (fun x -> Do {
                equal (Seq.tryLast seq { yield 0; yield x }) (Some x)
            })
        }

        Test "Seq.where" {
            equal (Seq.where (fun x -> x % 2 = 0) (seq { 0 .. 4 })) ([ 0; 2; 4 ] |> Seq.ofList)
            equal (Seq.where (fun _ -> true) Seq.empty) Seq.empty
        }

        Test "Seq.map3" {
            raises (Seq.map3 id seq { 0; 1 } seq { 0 .. 2 } seq { 0 .. 2 })
            equal (Seq.map3 id Seq.empty Seq.empty Seq.empty) Seq.empty
            equal (Seq.map3 (fun x y z -> x + y + z) seq { yield 0; yield 1 } seq { 2 .. 3 } seq { 4 .. 5 }) (seq { 6 .. 9 })
        }

        Test "Seq.replicate" {
            raises (Seq.replicate -1 Seq.empty)
            equal (Seq.replicate 100 0) (seq { for _ in [ 1 .. 100 ] do yield 0 })
        }

        Test "Seq.tail" {
            raises (Seq.tail Seq.empty)
            property (fun x -> Do {
                isTrue (Seq.length (Seq.tail (Seq.replicate x 0)) = x - 1)
            })
        }

        Test "Seq.fold2" {
            equal (Seq.fold2 (fun x y z -> x + y + z) 1 Seq.empty Seq.empty) 1
        }

        Test "Seq.foldBack" {
            equal (Seq.foldBack (+) Seq.empty 1) 1
        }

        Test "Seq.foldBack2" {
            equal (Seq.foldBack2 (fun x y z -> x + y + z) Seq.empty Seq.empty 1) 1
        }

        Test "Seq.iteri2" {
            let cache = ref 0

            let action x y z =
                cache := x + y + z

            Seq.iteri2 action (seq { 1 .. 3 }) (seq { 1 .. 3 })

            equal !cache 15
        }

        Test "Seq.mapi2" {
            equal (Seq.mapi2 (fun _ -> failwith "Never happens") Seq.empty Seq.empty) Seq.empty
        }

        Test "Seq.permute" {
            equal (Seq.permute (fun x -> (x + 1) % 4) (seq { 1 .. 4 })) ([ 4; 1; 2; 3 ] |> Seq.ofList)
        }

        Test "Seq.reduceBack" {
            equal (Seq.reduceBack (-) (seq { 1 .. 4 })) (1 - (2 - (3 - 4)))
        }

        Test "Seq.rev" {
            equal (Seq.rev (seq { 0 .. 4 })) (seq { 4 .. -1 .. 0 })
        }

        Test "Seq.scanBack" {
            equal (Seq.scanBack (+) { 1 .. 10 } 9) (seq [ 64; 63; 61; 58; 54; 49; 43; 36; 28; 19; 9 ])
        }

        Test "Seq.sortWith" {
            equal (Seq.sortWith (fun a b -> compare (a % 3) (b % 3)) (seq { 0 .. 10 })) (seq [ 0; 3; 6; 9; 1; 4; 7; 10; 2; 5; 8 ])
        }

        #endif
    
    }

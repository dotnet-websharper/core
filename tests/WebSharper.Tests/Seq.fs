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

    Section "Seq"

    Test "Seq.append" {
        Seq.append (seq { 1 .. 5 }) (seq { 6 .. 10 })
        |> Seq.toArray =? [| 1 .. 10 |]
        Assert.For 100 (R.ArrayOf R.Int) (fun x ->
            Seq.toArray (Seq.append x []) =? x
            Seq.toArray (Seq.append [] x) =? x)
        Assert.For 100 (R.Tuple2Of (R.ArrayOf R.Int, R.ArrayOf R.Int))
            (fun (x, y) ->
                let s1 = Array.toSeq x
                let s2 = Array.toSeq y
                Seq.length (Seq.append s1 s2) =? Seq.length s1 + Seq.length s2)
        Assert.For 100 (R.Tuple2Of (R.ArrayOf R.Int, R.ArrayOf R.Int))
            (fun (x, y) ->
                let s1 = Array.toSeq x
                let s2 = Array.toSeq y
                Seq.toArray (Seq.append s1 s2) =?
                    Array.append (Array.ofSeq s1) (Array.ofSeq s2))
    }

    Test "Seq.average" {
        seq { 0. .. 100. }
        |> Seq.average =? 50.
    }

    Test "Seq.averageBy" {
        Seq.averageBy float (seq { 0 .. 100 }) =? 50.
    }

    Test "Seq.cache" {
        Assert.For 100 (R.ArrayOf R.Int) (fun x ->
            let s = Seq.toArray (Seq.cache x)
            Seq.toArray s =? x)
    }

    Test "Seq.cast" {
        Seq.cast [| 1; 2; 3 |] |> Seq.toArray =? [| 1; 2; 3 |]
    }

    Test "Seq.choose" {
        let oneToSix = seq { 1 .. 6 }
        let sixToString n =
            match n with
            | 6 -> Some "six"
            | _ -> None
        Seq.choose sixToString oneToSix
        |> Seq.toArray =? [| "six" |]
    }

    Test "Seq.collect" {
        let f (x : int) = Seq.singleton x
        Seq.collect f (seq { 0 .. 100 }) |> Seq.toArray
            =? [| 0 .. 100 |]
    }

    Test "Seq.compareWith" {
        let s1 = seq { 1 .. 10 }
        let s2 = seq { 1 .. 10 }
        Seq.compareWith compare s2 s1 =? 0
        Seq.compareWith compare Seq.empty<int> Seq.empty<int> =? 0
        Seq.compareWith compare [1] [] =? 1
        Seq.compareWith compare [] [1] =? -1
        Seq.compareWith compare [1; 2; 5] [1; 3; 2] =? -1
        Seq.compareWith compare [1; 3; 2] [1; 2; 5; 9; 10] =? 1
    }

    Test "Seq.concat" {
        let seqOfSeqs = [| seq { 1 .. 5 }
                           Seq.empty
                           Seq.singleton 6
                           seq { 7 .. 10 }
                        |]
        let array = seqOfSeqs |> Seq.concat |> Seq.toArray
        array =? [| 1 .. 10 |]
    }

    Test "Seq.countBy" {
        let r = ref 0
        let s = Seq.countBy id (seq {for i = 1 to 10 do r := !r + 1; yield 0})
        !r =? 0
        s |> Seq.toArray =? [|0, 10 |]
        !r =? 10
        Seq.countBy (fun _ -> 1) [| 1 .. 100 |] |> Seq.toArray =? [| 1, 100 |]
        Seq.countBy id [1; 2; 1; 3; 3; 4; 1; 1] |> Seq.toArray
            =? [| (1, 4); (2, 1); (3, 2); (4, 1) |]
        Seq.countBy id [2; 1; 2] |> Seq.toArray
            =? [| (2, 2) ; (1, 1) |]
        let xs : list<int> = []
        Seq.countBy id xs |> Seq.toArray =? [||]
    }

    Test "Seq.delay" {
        let r = ref 0
        let s = Seq.delay (fun () -> incr r; [| 1 |] :> _)
        !r =? 0
        Seq.toArray s =? [| 1 |]
        !r =? 1
    }

    Test "Seq.distinct" {
        let xs = seq [0; 1; 2; 3; 0; 3; 2; 3; 0; 0; 0]
        Seq.distinct xs |> Seq.toArray =? [| 0 .. 3 |]
    }

    Test "Seq.distinctBy" {
        let xs = seq [0; 1 ; 2; 3; 0; 3; 2; 3; 0; 0; 0]
        Seq.distinctBy ((+) 1) xs |> Seq.toArray
            =? [| 0 .. 3 |]
    }

    Test "Seq.empty" {
        Seq.empty |> Seq.toArray =? [||]
    }

    Test "Seq.exists" {
        let xs = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5]
        Seq.exists ((=) 9) xs =? true
        Seq.exists ((=) 5) xs =? true
        Seq.exists ((=) -6) xs =? false
    }

    Test "Seq.exists2" {
        let xs1 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5; 6]
        let xs2 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5]
        Seq.exists2 (fun x y -> (x = y) && (x = 0)) xs1 xs2  =? true
        Seq.exists2 (fun x y -> (x = y) && (x = 5)) xs1 xs2  =? true
        Seq.exists2 (fun x y -> (x = y) && (x = -6)) xs1 xs2 =? false
    }

    Test "Seq.filter" {
        let xs = seq { 1 .. 10 }
        let ffalse (x : int) = false
        let ftrue (x : int) = true
        let f x = x <= 5
        Seq.filter f xs |> Seq.toArray =? [| 1 .. 5 |]
        Seq.filter ffalse xs |> Seq.length =? 0
        Seq.filter ftrue xs |> Seq.length =? Seq.length xs
    }

    Test "Seq.find" {
        let xs = seq [7; 1 ; 2; 9; 0; 3; 2; 3; 0; 9; 5]
        Seq.find (fun x -> x % 3 = 0) xs =? 9
        let s0 = seq [None; None; Some 4; None]
        Seq.find Option.isSome s0 =? Some 4
        let nf () = Seq.find ((=) (Some 5)) s0 |> ignore
        Assert.Raises nf
    }

    Test "Seq.findIndex" {
        let xs = seq [7; 1 ; 2; 9; 0; 3; 2; 3; 0; 9; 5]
        Seq.findIndex (fun x -> x % 3 = 0) xs =? 3
        let s0 = seq [None; None; Some 4; None]
        Seq.findIndex (Option.isSome) s0 =? 2
        let nf () = Seq.findIndex ((=) (Some 5)) s0 |> ignore
        Assert.Raises nf
    }

    Test "Seq.fold" {
        Seq.fold (+) 0 <| seq { 1 .. 5 } =? 15
        let s = Seq.init 10 (fun n -> seq { 1 .. 2 })
        Seq.fold (fun x y -> x + Seq.length y) 0 <| s =? 20
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            let a = Array.fold (+) 0 (Array.ofList x)
            Seq.fold (+) 0 <| List.toSeq x =? a)
    }

    Test "Seq.forall" {
        let xs1 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5; 21]
        let xs2 = seq [6; 1 ; 2; 3; 0; 3; 2; 3; 0; 9; 5]
        let a = Seq.forall ((<>) 21)
        a xs2 =? true
        a xs1 =? false
        Assert.For 100 (R.ArrayOf R.Int) (fun s ->
            let f a = a % 2 = 0
            Seq.forall f s =? not (Seq.exists (not << f) s))
    }

    Test "Seq.forall2" {
        Seq.forall2 (fun x y -> x = y) [1;2;3] [1;3;3] =? false
        Assert.For 100 (R.ArrayOf R.Int) (fun xs ->
            Seq.forall2 (=) xs xs =? true)
    }

    Test "Seq.groupBy" {
        let gs =
            [| 0 .. 9 |]
            |> Seq.groupBy (fun n -> if n < 5 then 0 else 1)
            |> Seq.map (fun (k,xs) -> k, Seq.toArray xs)
            |> Seq.toArray
        gs =? [| (0, [| 0 .. 4 |]); (1, [| 5 .. 9 |]) |]
        let gs =
            [2; 1; 2]
            |> Seq.groupBy id
            |> Seq.map (fun (k,xs) -> k, Seq.toArray xs)
            |> Seq.toArray
        gs =? [| (2, [| 2; 2 |]); (1, [| 1 |]) |]
    }

    Test "Seq.head" {
        let s = seq ["apa"]
        Seq.head s =? "apa"
        Seq.head [1; 2; 3] =? 1
        Assert.Raises (fun () -> ignore (Seq.head Seq.empty))
    }

    Test "Seq.init" {
        let s = Seq.init 10 (fun n -> n)
        Seq.toArray s =? [|0..9|]
    }

    Test "Seq.initInfinite" {
        let n = Seq.initInfinite (fun x -> x)
        Seq.take 5 n |> Seq.toArray =? [| 0 .. 4 |]
        Seq.take 10 n |> Seq.toArray =? [| 0 .. 9 |]
    }

    Test "Seq.isEmpty" {
        Seq.isEmpty Seq.empty =? true
        Seq.isEmpty [1] =? false
    }

    Test "Seq.iter" {
        let r = ref 0
        Seq.iter (fun x -> r := !r + x) [1..10]
        !r =? 55
    }

    Test "Seq.iter2" {
        let n = ref 0
        Seq.iter2 (fun a b -> n := !n * a + b) [1 .. 10] [1 .. 3]
        !n =? 15
    }

    Test "Seq.iteri" {
        let r = ref 0
        Seq.iteri (fun i x -> r := !r * i + x) [1..10]
        !r =? 1972819
    }

    Test "Seq.length" {
        Seq.length [1; 2; 3] =? 3
        Seq.length [] =? 0
    }

    Test "Seq.map" {
        let oneToTen = seq { 1 .. 10 }
        let s = Seq.map (fun x -> x % 2) oneToTen
        Seq.toArray s =? [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; |]
    }

    Test "Seq.map2" {
        let oneToTen = seq { 1 .. 10 }
        let tenToTwentytwo = seq { 10 .. 22 }
        let s = Seq.map2 (fun x y -> x % 2) oneToTen tenToTwentytwo
        Seq.toArray s =? [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; |]
    }

    Test "Seq.mapi" {
        let oneToTen = seq { 1 .. 10 }
        let s = Seq.mapi (fun x y -> x % 2) oneToTen
        Seq.toArray s =? [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; |]
    }

    Test "Seq.max" {
        Seq.max [1; 2; 3; 2; 1] =? 3
    }

    Test "Seq.maxBy" {
        Seq.maxBy (fun x -> x + 1) [1; 2; 3; 2; 1] =? 3
    }

    Test "Seq.min" {
        Seq.min [4; 5; 3; 5; 4] =? 3
    }

    Test "Seq.minBy" {
        Seq.minBy (fun x -> x + 1) [4; 5; 3; 5; 4] =? 3
    }

    Test "Seq.nth" {
        Seq.nth 2 [1; 2; 3] =? 3
    }

    Test "Seq.ofArray" {
        Seq.ofArray [| 1..3 |] |> Seq.toArray =? [| 1..3 |]
    }

    Test "Seq.ofList" {
        Seq.ofList [1..3] |> Seq.toList =? [1..3]
    }

    Test "Seq.pairwise" {
        seq {1..4}
        |> Seq.pairwise
        |> Seq.toArray =? [| (1, 2); (2, 3); (3, 4) |]
    }

    Test "Seq.pick" {
        let finder x = if x = 4 then Some x else None
        Seq.pick finder [4] =? 4
    }

    Test "Seq.readonly" {
        let a = [| 1 .. 3 |]
        let b = Seq.readonly a
        Seq.toArray b =? [| 1 .. 3 |]
    }

    Test "Seq.reduce" {
        Seq.reduce (+) [1;1;1;1;1] =? 5
        Seq.reduce (+) [3] =? 3
    }

    Test "Seq.scan" {
        let l3 = seq [seq [2;1]; seq [1;2;3]]
        Seq.scan (fun x y -> x + Seq.length y) 0 l3
        |> Seq.toArray =? [|0;2;5|]
        Assert.For 100 (R.ArrayOf R.Int) (fun x ->
            let bySeq = Seq.scan (+) 0 x
            let byArray = Array.scan (+) 0 x
            Seq.toArray bySeq =? byArray)
    }

    Test "Seq.singleton" {
        let six = Seq.singleton 6
        Seq.toArray six =? [| 6 |]
    }

    Test "Seq.skip" {
        let s = seq { 1 .. 10 }
        Seq.toArray (Seq.skip 5 s) =? [| 6; 7; 8; 9; 10 |]
    }

    Test "Seq.skipWhile" {
        let s = seq {1..10}
        Seq.toArray (Seq.skipWhile ((>) 6) s)  =? [|6;7;8;9;10|]
        Seq.toArray (Seq.skipWhile ((>) 6) []) =? [||]
    }

    Test "Seq.sort" {
        Seq.sort [| 1; 8; -5; 2 |]
        |> Seq.toArray =? [| -5; 1; 2; 8 |]
    }

    Test "Seq.sortBy" {
        let s = Seq.sortBy (fun x -> 10 - x) [6;7;8;9;10]
        Seq.toArray s =? [|10;9;8;7;6|]
    }

    Test "Seq.sum" {
        let s1 = seq {1..4}
        let s2 = seq [1.; 2.; 3.; 4.]
        Seq.sum s1 =? 10
        Seq.sum s2 =? 10.
        Seq.sum<int> [] =? 0
    }

    Test "Seq.sumBy" {
        Seq.sumBy (fun x -> x * x) [|1 .. 4|] =? 30
        Seq.sumBy (fun x -> x * x) [||] =? 0
    }

    Test "Seq.take" {
        let s = seq { 1 .. 10 }
        Seq.toArray (Seq.take 5 s) =? [| 1; 2; 3; 4; 5 |]
    }

    Test "Seq.takeWhile" {
        let s = seq {1..10}
        Seq.toArray (Seq.takeWhile ((>) 6) s)
            =? [|1;2;3;4;5|]
    }

    Test "Seq.toArray" {
        Seq.toArray [1..3] =? [| 1..3 |]
    }

    Test "Seq.toList" {
        Seq.toList [|1..3|] =? [1..3]
    }

    Test "Seq.truncate" {
        Seq.truncate 3 [1..10] |> Seq.toArray =? [|1; 2; 3|]
        Seq.truncate 3 [1]     |> Seq.toArray =? [|1|]
    }

    Test "Seq.tryFind" {
        Seq.tryFind (fun x -> fst x = 3)
            [(1, "A"); (2, "B"); (3, "C")] =? Some (3, "C")
        Seq.tryFind (fun x -> fst x = 3)
            [(1, "A"); (2, "B")] =? None
    }

    Test "Seq.tryFindIndex" {
        Seq.tryFindIndex (fun x -> fst x = 3)
            [(1, "A"); (2, "B"); (3, "C")] =? Some 2
        Seq.tryFindIndex (fun x -> fst x = 3)
            [(1, "A"); (2, "B")] =? None
    }

    Test "Seq.tryPick" {
        let finder x = if x = 4 then Some x else None
        Seq.tryPick finder [4] =? Some 4
        Seq.tryPick finder [3] =? None
    }

    Test "Seq.unfold" {
        Seq.unfold (fun s ->
            if s < 3 then
                Some (s, s + 1)
            else
                None)
            0
        |> Seq.toArray =? [| 0; 1; 2 |]
    }

    Test "Seq.windowed" {
        Seq.windowed 3 [1..5]
        |> Seq.toArray =?
            [| [| 1; 2; 3 |]
               [| 2; 3; 4 |]
               [| 3; 4; 5 |] |]
    }

    Test "Seq.zip" {
        let (s1:int seq), s2 = seq [], seq [1]
        let s1,s2 = seq ["a"],seq [1]
        let a1 = Seq.zip s1 s2
        Seq.toArray a1 =? [| ("a", 1) |]
    }

    Test "Seq.zip3" {
        let (arr1: int seq), arr2, arr3 = seq [], seq [1], seq [1]
        let arr1, arr2, arr3 = seq ["a"], seq [1], seq [(1, 2)]
        let a112 = Seq.zip3 arr1 arr2 arr3
        Seq.toArray a112 =? [| ("a", 1, (1, 2)) |]
    }

    Test "Comprehensions" {
        seq {for i = 1 to 10 do yield i}
        |> Seq.toArray =? [| 1 .. 10 |]
        seq {for x in 1..5 -> x * x}
        |> Seq.toArray =? [| 1; 4; 9; 16; 25 |]
        let vec1 = seq [1; 2; 3]
        let vec2 = seq [4; 5; 6]
        let l2 =
            seq {
                for x in vec1 do
                    for y in vec2 do
                        yield x * y
            }
        Seq.toArray l2 =? [|4; 5; 6; 8; 10; 12; 12; 15; 18|]
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
        Seq.toArray (l3 5) =? sol
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
        Seq.toArray fibonacci =? [| 2; 3; 5; 8; 13 |]
        let r = ref 0
        Assert.Raises (fun () ->
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
            |> Seq.toArray
            |> ignore)
        !r =? 1
    }


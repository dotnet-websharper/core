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

module IntelliFactory.WebSharper.Tests.List

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
module R = IntelliFactory.WebSharper.Testing.Random

[<JavaScript>]
let Tests =

    Section "List"

    Test "List.Cons" {
        List.Cons (1, []) =? [1]
        1 :: 2 :: 3 :: [] =? [1; 2; 3]
    }

    Test "List.append" {
        List.append [1; 2; 3] [4; 5] =? [1..5]
        List.append [] [] =? []
        List.append [1] [] =? [1]
        List.append [] [1] =? [1]
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            List.append x [] =? x
            List.append [] x =? x)
        Assert.For 100 (R.Tuple2Of (R.ListOf R.Int, R.ListOf R.Int))
            (fun (x, y) ->
                List.length (List.append x y) =?
                    List.length x + List.length y)
        Assert.For 100 (R.Tuple2Of (R.ListOf R.Int,R.ListOf R.Int))
            (fun (x, y) ->
                List.append x y =?
                    (Array.append (Array.ofList x) (Array.ofList y)
                    |> Array.toList))
    }

    Test "List.average" {
        List.average [1. .. 10.] =? 5.5
    }

    Test "List.avarageBy" {
        List.averageBy (fun x -> x * x) [1. .. 10.] =? 38.5
    }

    Test "List.choose" {
        let ok x =
            if x % 2 = 0 then
                Some (string x)
            else
                None
        List.choose ok [1; 2; 3; 4; 5] =? ["2"; "4"]
        List.choose id [None; Some 4; None] =? [4]
    }

    Test "List.collect" {
        let l = [0; 1; 2; 3; 4]
        let foo (x:int) = [x]
        List.collect foo l =? [0; 1; 2; 3; 4]
    }

    Test "List.concat" {
        List.concat [| [1; 2]; [3; 4] |] =? [1..4]
    }

    Test "List.empty" {
        List.empty =? []
        List.Empty =? []
    }

    Test "List.exists" {
        List.exists (fun x -> x % 2 = 1) [2; 4] =? false
        List.exists (fun _ -> true) [] =? false
        List.exists ((=) 8) [1..10] =? true
    }

    Test "List.exists2" {
        List.exists2 (fun x y -> ((x + y) % 2 = 0)) [1..5] [6..10] =? false
        List.exists2 (fun _ _ -> true) [] [] =? false
        let isSeven x = x = 7
        List.exists2 (fun x y -> isSeven x || isSeven y) [1..5] [6..10] =? true
    }

    Test "List.filter" {
        List.filter (fun x -> x > 0) [-5 .. 5] =? [1..5]
    }

    Test "List.find" {
        let wasFound = List.find Option.isSome [None; Some 4] =? Some 4
        let notFound () = List.find ((=) (Some 5)) [None; Some 4] |> ignore
        Assert.Raises notFound
    }

    Test "List.findIndex" {
        List.findIndex Option.isSome [None; Some 4] =? 1
        let nf () = List.findIndex ((=) (Some 5)) [None; Some 4] |> ignore
        Assert.Raises nf
    }

    Test "List.fold" {
        List.fold (+) 0 [1;1;1;1] =? 4
        List.fold (fun x y -> x + List.length y) 0 [[1]; [2;3]] =? 3
    }

    Test "List.fold2" {
        let first (x: int list) = x.[0]
        let oneTest = [[2; 1]; [1; 2]]
        let l2 = [[1]; [2]]
        List.fold2 (fun x y z -> x + List.length y + first z) 0 oneTest l2 =? 7
        let nf () = List.fold2 (fun _ _ _ -> true) true [] [1] |> ignore
        Assert.Raises nf
        Assert.For 100 (R.ListOf (R.Tuple2Of (R.Int,R.Boolean))) (fun x ->
            let (x,y) = List.unzip x
            let conditionalAdder s x y = if y then s + x else s
            let xArr, yArr = (Array.ofList x, Array.ofList y)
            let byList  = List.fold2  conditionalAdder 0 x y
            let byArray = Array.fold2 conditionalAdder 0 xArr yArr
            byList =? byArray)
    }

    Test "List.foldBack" {
        List.foldBack (+) [1;1;1;1] 0 =? 4
        List.foldBack (fun y x -> x + List.length y) [[1]; [2;3]] 0 =? 3
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            List.foldBack (+) x 0 =? Array.foldBack (+) (Array.ofList x) 0)
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            List.fold  (+) 0 x =? List.foldBack (+) x 0)
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            List.fold (-) 0 x =? Array.fold (-) 0 (Array.ofList x))
    }

    Test "List.foldBack2" {
        List.foldBack2 (fun y z x -> x + List.length y + List.head z)
            [[2; 1]; [1; 2]] [[1]; [2]] 0 =? 7
        let nf () = List.foldBack2 (fun _ _ _ -> true) [] [1] true |> ignore
        Assert.Raises nf
        Assert.For 100 (R.ListOf (R.Tuple2Of (R.Int, R.Boolean))) (fun x ->
            let (x,y) = List.unzip x
            let conditionalAdder s x y = if y then s + x else s
            let left  = List.fold2  conditionalAdder 0 x y
            let conditionalAdder x y s = conditionalAdder s x y
            let right = List.foldBack2 conditionalAdder x y 0
            left =? right)
    }

    Test "List.forall" {
        List.forall (fun x -> x % 2 = 0) [2; 4; 6] =? true
        List.forall (fun _ -> false) [] =? true
        List.forall ((=) 1) [1; 2; 3] =? false
    }

    Test "List.forall2" {
        List.forall2 (fun x y -> (x + y) % 2 = 1) [1..5] [6..10] =? true
        List.forall2 (fun _ _ -> false) [] [] =? true
        let isSeven x = x = 7
        List.forall2 (fun x y -> isSeven x || isSeven y) [1..4] [6..9] =? false
        let nf () = List.forall2 (fun _ _ -> true) [] [1] |> ignore
        Assert.Raises nf
    }

    Test "List.head" {
        [1; 2; 3].Head =? 1
        List.head [1; 2; 3] =? 1
        Assert.Raises (fun () -> List.head [] |> ignore)
    }

    Test "List.init" {
        Assert.Raises (fun () -> ignore (List.init -3 ((+) 5)))
        List.init 3 ((*) 2) =? [0; 2; 4]
    }

    Test "List.isEmpty" {
        [].IsEmpty =? true
        [1].IsEmpty =? false
        List.isEmpty [] =? true
        List.isEmpty [1] =? false
    }

    Test "List.iter" {
        let r = ref 0
        List.iter (fun x -> r := !r + 1) [1..10]
        !r =? 10
    }

    Test "List.iter2" {
        let cell = [| 0 |]
        let incrBy x = cell.[0] <- cell.[0] + x
        List.iter2 (fun x y -> incrBy (List.length x + y)) [[1]; [2;3]] [5..6]
        cell.[0] =? 14
        let nf () = List.iter2 (fun _ _ -> ()) [] [1]
        Assert.Raises nf
    }

    Test "List.iteri" {
        let l = [4; 3; 2; 1; 0]
        let arr = [| 0 |]
        List.iteri (fun x y -> arr.[0] <- arr.[0] + x + y) l
        arr.[0] =? 20
    }

    Test "List.iteri2" {
        let cell = [|0|]
        let incrBy x = cell.[0] <- cell.[0] + x
        List.iteri2 (fun idx x y -> incrBy (List.length x + y + idx))
            [[1]; [2;3]; []] [5..7]
        cell.[0] =? 24
        let nf () = List.iteri2 (fun _ _ _ -> ()) [] [1]
        Assert.Raises nf
    }

    Test "List.length" {
        [1; 2; 3].Length =? 3
        List.length [1; 2; 3] =? 3
        List.length [] =? 0
        List.length [null; "not null"; null; "not null"] =? 4
    }

    Test "List.map" {
        let oneToTen = [1 .. 10]
        List.map (fun x -> x % 2) oneToTen =?
            [1; 0; 1; 0; 1; 0; 1; 0; 1; 0]
        let funcs = [| (+) 1; (*) 2; (fun x -> x); (fun x -> x * x) |]
        Assert.For 100 (R.Tuple3Of(R.ListOf R.Int,
                                   R.OneOf funcs,
                                   R.OneOf funcs))
            (fun (x, f1, f2) ->
                let map1 = x
                           |> List.map f1
                           |> List.map f2
                let map2 = x |> List.map (f1 >> f2)
                map1 =? map2)
    }

    Test "List.map2" {
        let f (x : list<int>) y = List.length x + y
        List.map2 f [[1]; [2; 3]; []] [5..7] =? [6; 8; 7]
    }

    Test "List.map3" {
        let f (x : int list) (y : int) (z : int) = (List.length x + y + z)
        List.map3 f [[1]; [2; 3]; []] [5..7] [1..3] =? [7; 10; 10]
    }

    Test "List.mapi" {
        let l = [0; 1; 2; 3; 4]
        let foo i x = l.[(i + 1) % 5] + x
        List.mapi foo l =? [1; 3; 5; 7; 4]
    }

    Test "List.mapi2" {
        let f idx (x : list<int>) y = idx + List.length x + y
        List.mapi2 f [[1]; [2; 3]; []] [5..7] =? [6; 9; 9]
    }

    Test "List.max" {
        List.max [1; 2; 3; 2; 1] =? 3
    }

    Test "List.maxBy" {
        List.maxBy id [1; 2; 3; 2; 1] =? 3
    }

    Test "List.min" {
        List.min [4; 5; 3; 5; 4] =? 3
    }

    Test "List.minBy" {
        List.minBy id [4; 5; 3; 5; 4] =? 3
    }

    Test "List.nth" {
        let f () = List.nth [1; 2; 3] 4 |> ignore
        Assert.Raises f
        List.nth [1; 2; 3] 2 =? 3
        [1; 2; 3].[1] =? 2
    }

    Test "List.ofArray" {
        List.ofArray [| 1; 2; 3 |] =? [1; 2; 3]
    }

    Test "List.ofSeq" {
        List.ofSeq [| 1; 2; 3 |] =? [1; 2; 3]
    }

    Test "List.partition" {
        List.partition (fun x -> x > 0) [-3 .. 3]
            =? ([1; 2; 3], [-3; -2; -1; 0])
    }

    Test "List.permute" {
        List.permute (fun x -> (x + 3) % 5) [1..5] =? [3; 4; 5; 1; 2]
    }

    Test "List.pick" {
        let finder x = if x = 4 then Some x else None
        List.pick finder [4] =? 4
    }

    Test "List.reduce" {
        List.reduce (+) [1; 1; 1; 1; 1] =? 5
        List.reduce (+) [3] =? 3
    }

    Test "List.reduceBack" {
        List.reduceBack (+) [1; 1; 1; 1; 1] =? 5
        List.reduceBack (+) [3] =? 3
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            if not (List.isEmpty x) then
                let left   = List.reduce (+) x
                let right  = List.reduceBack (+) x
                left =? right)
    }

    Test "List.replicate" {
        List.replicate 5 "V" =? ["V"; "V"; "V"; "V"; "V"]
        Assert.For 100 (R.Tuple2Of (R.Natural, R.Int)) (
            fun (size, elem) ->
                let l = List.replicate size elem
                List.length l =? size
                List.iter (fun x -> x =? elem) l)
    }

    Test "List.rev" {
        List.rev [1; 2; 3] =? [3; 2; 1]
        List.rev [] =? []
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            let doubleRev = List.rev >> List.rev
            doubleRev x =? x
            let l = List.rev x @ x
            List.rev l =? l)
    }

    Test "List.scan" {
        let l3 = [[2; 1]; [1; 2; 3]]
        List.scan (fun x y -> x + List.length y) 0 l3 =? [0; 2; 5]
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            let byList  = List.scan  (+) 0 x
            let byArray = Array.scan (+) 0 (Array.ofList x)
            byList =? Array.toList byArray)
    }

    Test "List.scanBack" {
        List.scanBack (+) [4] 0 =? [4; 0]
        let l3 = [[2;1]; [1;2;3]]
        List.scanBack (fun y x -> x + List.length y) l3 0 =? [5; 3; 0]
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            let byList  = List.scanBack  (+) x 0
            let byArray = Array.scanBack (+) (Array.ofList x) 0
            byList =? Array.toList byArray)
    }

    Test "List.sort" {
        List.sort [4; 1; 15] =? [1; 4; 15]
    }

    Test "List.sortBy" {
        List.sortBy (fun x -> x * x) [-3; -2; 1; 4] =? [1; -2; -3; 4]
    }

    Test "List.sortWith" {
        let l = [[1; 2; 3]; []; [1; 2]]
        let comparer (x:int list) (y:int list) = List.length x - List.length y
        let l = List.sortWith comparer l
        l =? [[]; [1; 2]; [1; 2; 3]]
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            let firstSort = List.sortWith (-) x
            let secondSort = List.sortWith (-) firstSort
            firstSort =? secondSort)
    }

    Test "List.sum" {
        List.sum<int> []    =? 0
        List.sum<float> []  =? 0.
        List.sum [1. .. 4.] =? 10.
        List.sum [1 .. 4]   =? 10
    }

    Test "List.sumBy" {
        let oneToFourInt   = [1..4] |> List.map Some
        let oneToFourFloat = [1.; 2.; 3.; 4.] |> List.map Some
        let getInt = function
            | Some x -> x
            | _ -> 0
        let getFloat = function
            | Some x -> x
            | _ -> 0.
        let sumInt   = List.sumBy getInt oneToFourInt
        let sumFloat = List.sumBy getFloat oneToFourFloat
        let sumEmpty = List.sumBy getInt []
        sumInt   =? 10
        sumFloat =? 10.
        sumEmpty =? 0
    }

    Test "List.tail" {
        Assert.Raises (fun () -> List.tail [] |> ignore)
        List.tail [1;2;3] =? [2; 3]
        [1; 2; 3].Tail =? [2; 3]
    }

    Test "List.toArray" {
        List.toArray [1; 2] =? [| 1; 2 |]
    }

    Test "List.toSeq" {
        Array.ofSeq (List.toSeq [1; 2]) =? [| 1; 2 |]
    }

    Test "List.tryFind" {
        List.tryFind (fun x -> x = 2) [0..3] =? Some 2
        List.tryFind (fun x -> x = 8) [0..3] =? None
    }

    Test "List.tryFindIndex" {
        List.tryFindIndex (fun x -> x = 2) [1..3] =? Some 1
        List.tryFindIndex (fun x -> x = 8) [0..3] =? None
    }

    Test "List.tryPick" {
        let finder x = if x = 4 then Some x else None
        List.tryPick finder [4] =? Some 4
        List.tryPick finder [3] =? None
    }

    Test "List.unzip" {
        List.unzip [("a", 1); ("b", 2)] =? (["a"; "b"], [1; 2])
    }

    Test "List.unzip3" {
        List.unzip3 [("a", 1, true); ("b", 2, false)] =?
            (["a"; "b"], [1; 2], [true; false])
    }

    Test "List.zip" {
        let nf () = List.zip [] [1] |> ignore
        Assert.Raises nf
        List.zip ["a"] [1] =? [("a", 1)]
        Assert.For 100 (R.ListOf (R.Tuple2Of (R.Int, R.Int))) (fun l ->
            let (x, y) = List.unzip l
            List.zip x y =? l)
    }

    Test "List.zip3" {
        let ((arr1 : int list), arr2, arr3) = ([], [1], [1])
        let f1 () = List.zip3 arr1 arr2 arr3 |> ignore
        let f2 () = List.zip3 arr2 arr1 arr3 |> ignore
        let f3 () = List.zip3 arr1 arr3 arr2 |> ignore
        Assert.Raises f1
        Assert.Raises f2
        Assert.Raises f3
        let (arr1, arr2, arr3) = (["a"], [1], [(1, 2)])
        List.zip3 arr1 arr2 arr3 =? [("a", 1, (1, 2))]
    }

    Test "Comprehensions" {
        let l1 = [for x in 1 .. 10 -> x * x]
        l1 =? [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
        let vec1 = [1; 2; 3]
        let vec2 = [4; 5; 6]
        let l2   = [for x in vec1 do
                      for y in vec2 do
                        yield x * y]
        l2 =? [4; 5; 6; 8; 10; 12; 12; 15; 18]
        let l3 n = [for row in 1 .. n do
                      for col in 1 .. n do
                        if (row+col) % 2 = 0 then
                          yield (row,col)]
        let m = [(1, 1); (1, 3); (1, 5); (2, 2); (2, 4); (3, 1);
                 (3, 3); (3, 5); (4, 2); (4, 4); (5, 1); (5, 3); (5, 5)]
        l3 5 =? m
        let fibonacci =
            let rec fibonacciInner a b =
                [
                    let c = a + b
                    if c < 10 then
                        yield c
                        yield! fibonacciInner b c
                    else
                        yield c
                ]
            fibonacciInner 1 1
        fibonacci =? [2; 3; 5; 8; 13]
    }

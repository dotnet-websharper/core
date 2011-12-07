// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

module IntelliFactory.WebSharper.Tests.Array

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing
module R = IntelliFactory.WebSharper.Testing.Random

[<JavaScript>]
let Tests =
    Section "Array"

    Test "Range" {
        [| 1; 2; 3 |] =? [| 1 .. 3 |]
    }

    Test "Comprehensions" {
        let l1 = [| for x in 1 .. 10 -> x * x |]
        l1 = [| 1; 4; 9; 16; 25; 36; 49; 64; 81; 100 |] |? "squares"
        let vec1 = [| 1; 2; 3 |]
        let vec2 = [| 4; 5; 6 |]
        let l2 = [| for x in vec1 do for y in vec2 do yield x * y |]
        l2 = [| 4; 5; 6; 8; 10; 12; 12; 15; 18 |] |? "products"
        let l3 n = [| for row in 1 .. n do
                        for col in 1 .. n do
                            if (row+col) % 2 = 0 then
                                yield (row,col) |]
        let a = [| (1, 1); (1, 3); (1, 5); (2, 2);
                    (2, 4); (3, 1); (3, 3); (3, 5);
                    (4, 2); (4, 4); (5, 1); (5, 3); (5, 5) |]
        l3 5 = a |? "pairs"
        let fibonacci =
            let rec fibonacciInner a b =
                [|
                    let c = a + b
                    if c < 10 then
                        yield c
                        yield! fibonacciInner b c
                    else
                        yield c
                |]
            fibonacciInner 1 1
        [| 2; 3; 5; 8; 13 |] = fibonacci |? "fibs"
    }

    Test "Array.append" {
        let (arr1, arr2) = ([| 1; 2; 3 |], [| 4; 5 |])
        Array.append arr1 arr2 =? [| 1; 2; 3; 4; 5 |]
        arr1 =? [| 1; 2; 3|]
        arr2 =? [| 4; 5 |]
        Array.append [| 1 |] [| |]  =? [| 1 |]
        Assert.For 100 (R.ArrayOf R.Int) (fun x ->
            Array.append x [||] =? x)
        Assert.For 100 (R.ArrayOf R.Int) (fun x ->
            Array.append [||] x =? x)
    }

    Test "Array.average" {
        Array.average [| 1. .. 10. |] =? 5.5
    }

    Test "Array.averageBy" {
        Array.averageBy (fun x -> x + 1.) [| 1. .. 10. |] =? 6.5
    }

    Test "Array.blit" {
        let (arr1, arr2) = ([|1; 2; 3|], [|4; 5|])
        Array.blit arr1 1 arr2 0 2
        arr2 =? [| 2; 3 |]
        Assert.Raises (fun () -> Array.blit arr1 1 arr2 -1 2)
    }

    Test "Array.choose" {
        let res = Array.choose (fun x -> x) [| None; None; Some 4; None |]
        res =? [| 4 |]
    }

    Test "Array.collect" {
        Array.collect (fun x -> [| x |]) [| 1..10 |] =? [|1..10|]
        Assert.For 100 (R.ArrayOf R.Int) (fun x ->
            Array.collect (fun x -> [| x |]) x =? x)
    }

    Test "Array.concat" {
        Array.concat [| [| 1 |]; [||]; [| 2; 3 |] |] =? [| 1; 2; 3 |]
    }

    Test "Array.copy" {
        Assert.For 100 (R.ArrayOf R.Int) (fun x ->
            Array.copy x =? x)
    }

    Test "Array.create" {
        Array.create 5 "Value" =? [| for i in 1 .. 5 -> "Value" |]
        Assert.For 100 (R.Tuple2Of (R.Natural, R.Int)) (
            fun (size, elem) ->
                let a = Array.create size elem
                a.Length =? size
                Array.iter (fun x -> x =? elem) a)
    }

    Test "Array.empty" {
        Array.empty =? [||]
    }

    Test "Array.exists" {
        let a = [| 0; 2; 4; 8; 10 |]
        Array.exists (fun x -> x % 2 = 1) a =? false
        Array.exists (fun _ -> true) [||]   =? false
        Array.exists ((=) 8) a              =? true
    }

    Test "Array.exists2" {
        Array.exists2 (fun x y -> ((x + y) % 2 = 0))
            [| 1 .. 5 |] [| 6 .. 10 |] =? false
        Array.exists2 (fun _ _ -> true) [||] [||] =? false
        let isSeven x = x = 7
        Array.exists2 (fun x y -> (isSeven x) || (isSeven y))
            [| 1 .. 5 |] [| 6 .. 10 |] =? true
        Assert.Raises (fun () ->
            Array.exists2 (fun _ _ -> true) [||] [|1|] |> ignore)
    }

    Test "Array.fill" {
        let arr = Array.copy [| 1; 1; 1; 1; 1 |]
        Array.fill arr 1 3 3
        arr =? [| 1; 3; 3; 3; 1 |]
        Assert.Raises (fun () -> Array.fill arr 10 3 3)
    }

    Test "Array.filter" {
        Array.filter ((fun x -> x % 2) >> ((=) 1)) [| 1 .. 5 |]
            =? [| 1; 3; 5 |]
    }

    Test "Array.find" {
        let finder x = if x = 4 then Some x else None
        Array.tryPick finder [| 4 |] =? Some 4
        Array.tryPick finder [| 3 |] =? None
        Array.find (Option.isSome) [| None; None; Some 4; None |] =? Some 4
        Assert.Raises (fun () ->
            Array.find ((=) (Some 5)) [| None; None; Some 4; None |]
            |> ignore)
    }

    Test "Array.findIndex" {
        Array.findIndex (Option.isSome) [| None; None; Some 4; None |] =? 2
        Assert.Raises (fun () ->
            Array.findIndex ((=) (Some 5)) [| None; None; Some 4; None |]
            |> ignore)
    }

    Test "Array.fold" {
        Array.fold (+) 0 [| 1; 1; 1; 1; 1 |] =? 5
        Array.fold (fun x y -> x + List.length y) 0 [| [2; 1]; [1; 2] |] =? 4
    }

    Test "Array.foldBack" {
        Array.foldBack (+) [| 1; 1; 1; 1; 1 |] 0 =? 5
        Array.foldBack (fun y x -> x + List.length y)
            [| [2; 1]; [1; 2] |] 0 =? 4
        Assert.For 100 (R.ArrayOf R.Int) (fun x ->
            let left = Array.fold (+) 0 x
            let right = Array.foldBack (+) x 0
            left =? right)
    }

    Test "Array.fold2" {
        Array.fold2 (fun x y z -> x + (List.length y) + List.head z) 0
            [| [2; 1]; [1; 2] |] [| [1]; [2] |] =? 7
        Assert.Raises (fun () ->
            Array.fold2 (fun _ _ _ -> true) true [||] [|1|] |> ignore)
    }

    Test "Array.foldBack2" {
        let arr2 = [| [1]; [2] |]
        Array.foldBack2
            (fun y z x -> x + List.length y + List.head z)
            [| [2; 1]; [1; 2] |]
            arr2
            0 =? 7
        Assert.Raises (fun () ->
            Array.foldBack2 (fun _ _ _ -> true) [||] [|1|] true |> ignore)
    }

    Test "Array.forall" {
        let a = [| 0; 2; 4; 8; 10 |]
        Array.forall (fun x -> x % 2 = 0) a =? true
        Array.forall (fun _ -> false) [||]  =? true
        Array.forall ((=) 8) a              =? false
        let r = R.ArrayOf R.Int
        Assert.For 100 (R.Tuple2Of (R.Int, r)) (fun (f, l) ->
            let p1 = Array.forall ((=) f) l
            let p2 = Array.exists ((<>) f) l |> not
            p1 =? p2)
    }

    Test "Array.forall2" {
        Array.forall2 (fun x y -> ((x + y) % 2 = 1))
            [| 1 .. 5 |] [| 6 .. 10 |] =? true
        Array.forall2 (fun _ _ -> false) [||] [||] =? true
        let isSeven x = x = 7
        Array.forall2 (fun x y -> (isSeven x) || (isSeven y))
            [| 1 .. 5 |] [| 6 .. 10 |] =? false
        Assert.Raises (fun () ->
            Array.forall2 (fun _ _ -> true) [||] [|1|] |> ignore)
    }

    Test "Array.get & Array.set" {
        let size = 6
        let arr = [| 0 .. 5 |]
        let rec iterate f = function
            | n when n = size -> ()
            | n -> f n; iterate f (n + 1)
        let indexEqualValue (n : int) =
            Array.get arr n =? n
        iterate indexEqualValue 0
        arr.[3] =? 3
        arr.[3] <- 5
        arr.[3] =? 5
        iterate (fun n -> Array.set arr n 5) 0
        Array.create 6 5 =? arr
    }

    Test "Array.init" {
        Array.init 0 (fun _ -> 0) =? [||]
        Array.init 5 (( * ) 2) =? [| 0; 2; 4; 6; 8 |]
    }

    Test "Array.iter" {
        let cell = [| 0 |]
        let incrBy x = cell.[0] <- cell.[0] + x
        Array.iter incrBy [| 5; 10 |]
        cell.[0] =? 15
    }

    Test "Array.iteri" {
        let cell = [| 0 |]
        let array = [| 0; 1; 2; 3; 4 |]
        let foo i x = array.[i] <- array.[(i + 1) % 5] + x
        Array.iteri foo array
        array =? [| 1; 3; 5; 7; 5 |]
    }

    Test "Array.iter2" {
        let cell = [| 0 |]
        let incrBy x = cell.[0] <- cell.[0] + x
        Array.iter2 (fun x y -> incrBy (List.length x + y))
            [| [1; 2]; []; [1; 2; 3] |]
            [| 1; 2; 3 |]
        cell.[0] =? 11
        Assert.Raises (fun () ->
            Array.iter2 (fun _ _ -> ()) [||] [| 1 |])
    }

    Test "Array.iteri2" {
        let cell = [| 0 |]
        let incrBy x = cell.[0] <- cell.[0] + x
        let d = [| [1; 2]; []; [1; 2; 3] |]
        let differentLength () = Array.iteri2 (fun _ _ _ -> ()) [||] [|1|]
        Assert.Raises differentLength
        Array.iteri2 (fun idx x y -> (incrBy (List.length x + y + idx)))
            d [| 1 .. 3 |]
        cell.[0] =? 14
    }

    Test "Array.isEmpty" {
        Array.isEmpty [||] =? true
        Array.isEmpty [| 1 |] =? false
    }

    Test "Array.length" {
        Array.length [||] =? 0
        Array.length [| 1 .. 10 |] =? 10
        Array.length (Array.zeroCreate 5) =? 5
        let r = R.ArrayOf R.Int
        Assert.For 100 (R.Tuple2Of (r, r)) (fun (x, y) ->
            Array.length x + Array.length y =? Array.length (Array.append x y))
    }

    Test "Array.map" {
        Array.map (fun x -> x % 2) [| 1 .. 10 |] =?
            [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0 |]
        let funcs = [| (+) 1; ( * ) 2; (fun x -> x); (fun x -> x * x)|]
        let rA  = R.ArrayOf R.Int
        let rF = R.OneOf funcs
        Assert.For 100 (R.Tuple3Of (rA, rF, rF)) (fun (arr, f1, f2) ->
                let map1 =
                    arr
                    |> Array.map f1
                    |> Array.map f2
                let map2 =
                    arr
                    |> Array.map (f1 >> f2)
                map1 =? map2)
    }

    Test "Array.mapi" {
        let array = [| 0; 1; 2; 3; 4 |]
        let foo i x = array.[(i + 1) % 5] + x
        Array.mapi foo array =? [| 1; 3; 5; 7; 4 |]
    }

    Test "Array.map2" {
        let a1 = [| [1; 2]; []; [1; 2; 3] |]
        let a2 = [| 1; 2; 3 |]
        Array.map2 (fun x y -> List.length x + y) a1 a2
            =? [| 3; 2; 6 |]
    }

    Test "Array.mapi2" {
        let a1 = [| [1; 2]; []; [1; 2; 3] |]
        let a2 = [| 1; 2; 3 |]
        Array.mapi2 (fun idx x y -> (idx + List.length x + y)) a1 a2
            =? [| 3; 3; 8 |]
    }

    Test "Array.max" {
        Array.max [| 1; 2; 3; 2; 1 |] =? 3
    }

    Test "Array.maxBy" {
        [| 1; 2; 3; 2; 1 |]
        |> Array.maxBy id =? 3
    }

    Test "Array.min" {
        Array.min [| 4; 5; 3; 5; 4 |] =? 3
    }

    Test "Array.minBy" {
        Array.minBy id [| 4; 5; 3; 5; 4 |] =? 3
    }

    Test "Array.ofList" {
        Array.ofList [1;2;3] =? [|1; 2; 3|]
    }

    Test "Array.ofSeq" {
        Array.ofSeq [1;2;3] =? [|1; 2; 3|]
    }

    Test "Array.partition" {
        let (even, odd) =
            [| 1 .. 5 |]
            |> Array.partition (fun x -> x % 2 = 0)
        even =? [| 2; 4 |]
        odd  =? [| 1; 3; 5|]
        let funcs = [| (=) 3; (fun x -> x % 2 = 0) |]
        let r = R.Tuple2Of (R.ArrayOf R.Int, R.OneOf funcs)
        Assert.For 100 r (fun (a, f) ->
            let (x, y) = Array.partition f a
            Array.length (Array.append x y) =? Array.length x + Array.length y)
    }

    Test "Array.permute" {
        Array.permute (fun x -> (x + 3) % 5) [| 1 .. 5 |]
            =? [| 3; 4; 5; 1; 2 |]
    }

    Test "Array.pick" {
        let finder x = if x = 4 then Some x else None
        Array.pick finder [| 4 |] =? 4
    }

    Test "Array.reduce" {
        Array.reduce (+) [| 1; 1; 1; 1; 1 |] =? 5
        Array.reduce (+) [| 3 |] =? 3
    }

    Test "Array.reduceBack" {
        Array.reduceBack (+) [| 1; 1; 1; 1; 1 |] =? 5
        Array.reduceBack (+) [| 3 |] =? 3
        Assert.For 100 (R.ArrayOf R.Int) (fun a ->
            (Array.isEmpty a || (Array.reduce (+) a = Array.reduceBack (+) a))
                =? true)
    }

    Test "Array.rev" {
        Assert.For 100 (R.ArrayOf R.Int) (fun a ->
            Array.rev (Array.rev a) =? a)
        Array.rev [||] =? [||]
        Assert.For 100 R.Int (fun x -> [| x |] =? Array.rev [| x |])
    }

    Test "Array.scan" {
        [| [2; 1]; [1; 2; 3] |]
        |> Array.scan (fun x y -> x + List.length y ) 0 =?
            [| 0; 2; 5 |]
    }

    Test "Array.scanBack" {
        Array.scanBack (fun y x -> x + List.length y)
            [| [2; 1]; [1; 2; 3] |] 0 =? [| 5; 3; 0 |]
    }

    Test "Array.sort" {
        let a = [| 1; 8; 4 |]
        Array.sort a =? [| 1; 4; 8 |]
        a =? [| 1; 8; 4 |]
    }

    Test "Array.sortBy" {
        let a = [| 1; 0; -2 |]
        Array.sortBy (fun x -> x * x) a =? [| 0; 1; -2 |]
        a =? [| 1; 0; -2 |]
    }

    Test "Array.sortInPlace" {
        let a = [| 1; 4; 2 |]
        Array.sortInPlace a
        a =? [| 1; 2; 4 |]
    }

    Test "Array.sortInPlaceBy" {
        let a = [| 1; 0; -2 |]
        Array.sortInPlaceBy (fun x -> x * x) a
        a =? [| 0; 1; -2 |]
    }

    Test "Array.sortInPlaceWith" {
        let arr = [| [1; 2; 3]; []; [1; 2] |]
        let comparer (x : int list) (y : int list) =
            List.length x - List.length y
        Array.sortInPlaceWith comparer arr
        arr =? [| []; [1; 2]; [1; 2; 3] |]
        Assert.For 100 (R.ArrayOf (R.ListOf R.Int)) (fun arr ->
            let copy = Array.copy arr
            Array.sortInPlaceWith comparer copy
            Array.sortWith comparer arr =? copy)
    }

    Test "Array.sortWith" {
        let a = [| 1; 0; -2 |]
        let b = Array.sortWith (fun x y -> compare (abs x) (abs y)) a
        b =? [| 0; 1; -2 |]
        b <>? a
    }

    Test "Array.sub" {
        Array.sub [| 1 .. 10 |] 5 5 =? [| 6 .. 10 |]
    }

    Test "Array.sum" {
        Array.sum<int> [| |] =? 0
        Array.sum<float> [| |] =? 0.
        Array.sum [| 1 .. 4 |] =? 10
        Array.sum [| 1. .. 4. |] =? 10.
    }

    Test "Array.sumBy" {
        let oneToFourInt = [| 1 .. 4 |] |> Array.map Some
        let oneToFourFloat = [| 1.; 2.; 3.; 4. |] |> Array.map Some
        let getInt = function
            | Some x -> x
            | _ -> 0
        let getFloat = function
            | Some x -> x
            | _ -> 0.
        Array.sumBy getInt oneToFourInt =? 10
        Array.sumBy getFloat oneToFourFloat =? 10.
        Array.sumBy getInt [||] =? 0
    }

    Test "Array.toList" {
        let arr = [| 1; 2; 3; 4; 5 |]
        let l = [1; 2; 3; 4; 5]
        l =? Array.toList arr
        Assert.For 100 (R.ListOf R.Int) (fun x ->
            Array.toList (Array.ofList x) =? x)
    }

    Test "Array.toSeq" {
        Array.ofSeq (Array.toSeq [| 1; 2; 3 |]) =? [| 1; 2; 3 |]
    }

    Test "Array.tryFind" {
        let finder elem =
            match elem with
            | Some x -> x = 4
            | _      -> false
        Array.tryFind finder [| None; None; Some 4; None |] =? Some (Some 4)
        Array.tryFind (fun _ -> false) [| None; None; Some 4; None |] =? None
    }

    Test "Array.tryFindIndex" {
        Array.tryFindIndex (fun x -> x = 5) [| 1 .. 10 |] =? Some 4
        Array.tryFindIndex (fun x -> x = 5) [| 1 .. 3 |]  =? None
    }

    Test "Array.tryPick" {
        let f x = if x = 5 then Some (x + 1) else None
        Array.tryPick f [| 1 .. 10 |] =? Some 6
        Array.tryPick f [| 1 .. 3 |] =? None
    }

    Test "Array.unzip" {
        let r = R.Tuple2Of (R.Int, R.Boolean)
        Assert.For 100 (R.ArrayOf r) (fun arr ->
            let (x, y) = Array.unzip arr
            Array.zip x y =? arr)
    }

    Test "Array.unzip3" {
        let r = R.Tuple3Of (R.Int, R.Boolean, R.Int)
        Assert.For 100 (R.ArrayOf r) (fun arr ->
            let (x, y, z) = Array.unzip3 arr
            Array.zip3 x y z =? arr)
    }

    Test "Array.zeroCreate" {
        (Array.zeroCreate 2 : int []) =?
            [| Unchecked.defaultof<_>; Unchecked.defaultof<_> |]
    }

    Test "Array.zip" {
        let arr1, arr2 = [||], [| 1 |]
        let f () = Array.zip arr1 arr2 |> ignore
        Assert.Raises f
        let (arr1, arr2) = ([| "a" |], [| 1 |])
        Array.zip arr1 arr2 =? [| ("a", 1) |]
    }

    Test "Array.zip3" {
        let (arr1, arr2, arr3) = ([||], [| 1 |], [| 1 |])
        let f1 () = Array.zip3 arr1 arr2 arr3 |> ignore
        let f2 () = Array.zip3 arr2 arr1 arr3 |> ignore
        let f3 () = Array.zip3 arr1 arr3 arr2 |> ignore
        Assert.Raises f1
        Assert.Raises f2
        Assert.Raises f3
        let (arr1, arr2, arr3) = ([| "a" |], [| 1 |], [| (1, 2) |])
        Array.zip3 arr1 arr2 arr3 =? [| ("a", 1, (1, 2)) |]
    }

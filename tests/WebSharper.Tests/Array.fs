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

module WebSharper.Tests.Array

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing
module R = WebSharper.Testing.Random

[<JavaScript>]
let Tests =
    Section "Array" {

        Test "Range" {
            Equal [| 1; 2; 3 |]  [| 1 .. 3 |]
        }

        Test "Comprehensions" {
            let l1 = [| for x in 1 .. 10 -> x * x |]
            EqualM l1 [| 1; 4; 9; 16; 25; 36; 49; 64; 81; 100 |] "squares"
            let vec1 = [| 1; 2; 3 |]
            let vec2 = [| 4; 5; 6 |]
            let l2 = [| for x in vec1 do for y in vec2 do yield x * y |]
            EqualM l2 [| 4; 5; 6; 8; 10; 12; 12; 15; 18 |] "products"
            let l3 n = [| for row in 1 .. n do
                            for col in 1 .. n do
                                if (row+col) % 2 = 0 then
                                    yield (row,col) |]
            let a = [| (1, 1); (1, 3); (1, 5); (2, 2);
                        (2, 4); (3, 1); (3, 3); (3, 5);
                        (4, 2); (4, 4); (5, 1); (5, 3); (5, 5) |]
            EqualM (l3 5) a "pairs"
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
            EqualM [| 2; 3; 5; 8; 13 |] fibonacci "fibs"
        }

        Test "Array.append" {
            let (arr1, arr2) = ([| 1; 2; 3 |], [| 4; 5 |])
            Equal (Array.append arr1 arr2) [| 1; 2; 3; 4; 5 |]
            Equal arr1 [| 1; 2; 3|]
            Equal arr2 [| 4; 5 |]
            Equal (Array.append [| 1 |] [| |]) [| 1 |]
            ForR 100 (R.ArrayOf R.Int) (fun x -> Do {
                Equal (Array.append x [||]) x
                Equal (Array.append [||] x) x
            })
        }

        Test "Array.average" {
            Equal (Array.average [| 1. .. 10. |]) 5.5
        }

        Test "Array.averageBy" {
            Equal (Array.averageBy (fun x -> x + 1.) [| 1. .. 10. |]) 6.5
        }

        Test "Array.blit" {
            let (arr1, arr2) = ([|1; 2; 3|], [|4; 5|])
            Array.blit arr1 1 arr2 0 2
            Equal arr2 [| 2; 3 |]
            Raises (Array.blit arr1 1 arr2 -1 2)
        }

        Test "Array.choose" {
            let res = Array.choose (fun x -> x) [| None; None; Some 4; None |]
            Equal res [| 4 |]
        }

        Test "Array.collect" {
            Equal (Array.collect (fun x -> [| x |]) [| 1..10 |]) [|1..10|]
            ForR 100 (R.ArrayOf R.Int) (fun x -> Do {
                Equal (Array.collect (fun x -> [| x |]) x) x
            })
        }

        Test "Array.concat" {
            Equal (Array.concat [| [| 1 |]; [||]; [| 2; 3 |] |]) [| 1; 2; 3 |]
        }

        Test "Array.copy" {
            ForR 100 (R.ArrayOf R.Int) (fun x -> Do {
                Equal (Array.copy x) x
            })
        }

        Test "Array.create" {
            Equal (Array.create 5 "Value") [| for i in 1 .. 5 -> "Value" |]
            ForR 100 (R.Tuple2Of (R.Natural, R.Int)) (fun (size, elem) -> Do {
                let a = Array.create size elem
                Equal a.Length size
                True (Array.forall ((=) elem) a)
            })
        }

        Test "Array.empty" {
            Equal Array.empty [||]
        }

        Test "Array.exists" {
            let a = [| 0; 2; 4; 8; 10 |]
            True (not (Array.exists (fun x -> x % 2 = 1) a))
            True (not (Array.exists (fun _ -> true) [||]))
            True (Array.exists ((=) 8) a)
        }

        Test "Array.exists2" {
            True (not (Array.exists2 (fun x y -> ((x + y) % 2 = 0))
                        [| 1 .. 5 |] [| 6 .. 10 |]))
            True (not (Array.exists2 (fun _ _ -> true)
                        [||] [||]))
            let isSeven x = x = 7
            True (Array.exists2 (fun x y -> (isSeven x) || (isSeven y))
                    [| 1 .. 5 |] [| 6 .. 10 |])
            Raises (Array.exists2 (fun _ _ -> true) [||] [|1|])
        }

        Test "Array.fill" {
            let arr = Array.copy [| 1; 1; 1; 1; 1 |]
            Array.fill arr 1 3 3
            Equal arr [| 1; 3; 3; 3; 1 |]
            Raises (Array.fill arr 10 3 3)
        }

        Test "Array.filter" {
            Equal (Array.filter ((fun x -> x % 2) >> ((=) 1)) [| 1 .. 5 |])
                [| 1; 3; 5 |]
        }

        Test "Array.find" {
            let finder x = if x = 4 then Some x else None
            Equal (Array.tryPick finder [| 4 |]) (Some 4)
            Equal (Array.tryPick finder [| 3 |]) None
            Equal (Array.find (Option.isSome) [| None; None; Some 4; None |]) (Some 4)
            Raises (Array.find ((=) (Some 5)) [| None; None; Some 4; None |])
        }

        Test "Array.findIndex" {
            Equal (Array.findIndex (Option.isSome) [| None; None; Some 4; None |]) 2
            Raises (Array.findIndex ((=) (Some 5)) [| None; None; Some 4; None |])
        }

        Test "Array.fold" {
            Equal (Array.fold (+) 0 [| 1; 1; 1; 1; 1 |]) 5
            Equal (Array.fold (fun x y -> x + List.length y) 0 [| [2; 1]; [1; 2] |]) 4
        }

        Test "Array.foldBack" {
            Equal (Array.foldBack (+) [| 1; 1; 1; 1; 1 |] 0) 5
            Equal (Array.foldBack (fun y x -> x + List.length y)
                    [| [2; 1]; [1; 2] |] 0) 4
            ForR 100 (R.ArrayOf R.Int) (fun x -> Do {
                let left = Array.fold (+) 0 x
                let right = Array.foldBack (+) x 0
                Equal left right
            })
        }

        Test "Array.fold2" {
            Equal (Array.fold2 (fun x y z -> x + (List.length y) + List.head z) 0
                    [| [2; 1]; [1; 2] |] [| [1]; [2] |]) 7
            Raises (Array.fold2 (fun _ _ _ -> true) true [||] [|1|])
        }

        Test "Array.foldBack2" {
            let arr2 = [| [1]; [2] |]
            Equal (Array.foldBack2
                    (fun y z x -> x + List.length y + List.head z)
                    [| [2; 1]; [1; 2] |]
                    arr2
                    0) 7
            Raises (Array.foldBack2 (fun _ _ _ -> true) [||] [|1|] true)
        }

        Test "Array.forall" {
            let a = [| 0; 2; 4; 8; 10 |]
            True (Array.forall (fun x -> x % 2 = 0) a)
            True (Array.forall (fun _ -> false) [||])
            True (not (Array.forall ((=) 8) a))
            let r = R.ArrayOf R.Int
            ForR 100 (R.Tuple2Of (R.Int, r)) (fun (f, l) -> Do {
                let p1 = Array.forall ((=) f) l
                let p2 = Array.exists ((<>) f) l |> not
                Equal p1 p2
            })
        }

        Test "Array.forall2" {
            True (Array.forall2 (fun x y -> ((x + y) % 2 = 1))
                    [| 1 .. 5 |] [| 6 .. 10 |])
            True (Array.forall2 (fun _ _ -> false) [||] [||])
            let isSeven x = x = 7
            True (not (Array.forall2 (fun x y -> (isSeven x) || (isSeven y))
                        [| 1 .. 5 |] [| 6 .. 10 |]))
            Raises (Array.forall2 (fun _ _ -> true) [||] [|1|])
        }

        Test "Array.get & Array.set" {
            let size = 6
            let arr = [| 0 .. size - 1 |]
            For { 0 .. size - 1 } (fun n -> Do {
                JsEqualM (Array.get arr n) n ("get " + string n)
            })
            JsEqualM arr.[3] 3 "before set"
            arr.[3] <- 5
            JsEqualM arr.[3] 5 "after set"
            do for n = 0 to size - 1 do
                Array.set arr n 8
            EqualM (Array.create size 8) arr "all set"
            Raises arr.[-1]
            Raises arr.[size]
        }

        Test "Array.init" {
            Equal (Array.init 0 (fun _ -> 0)) [||]
            Equal (Array.init 5 (( * ) 2)) [| 0; 2; 4; 6; 8 |]
        }

        Test "Array.iter" {
            let cell = [| 0 |]
            let incrBy x = cell.[0] <- cell.[0] + x
            Array.iter incrBy [| 5; 10 |]
            Equal cell.[0] 15
        }

        Test "Array.iteri" {
            let cell = [| 0 |]
            let array = [| 0; 1; 2; 3; 4 |]
            let foo i x = array.[i] <- array.[(i + 1) % 5] + x
            Array.iteri foo array
            Equal array [| 1; 3; 5; 7; 5 |]
        }

        Test "Array.iter2" {
            let cell = [| 0 |]
            let incrBy x = cell.[0] <- cell.[0] + x
            Array.iter2 (fun x y -> incrBy (List.length x + y))
                [| [1; 2]; []; [1; 2; 3] |]
                [| 1; 2; 3 |]
            Equal cell.[0] 11
            Raises (Array.iter2 (fun _ _ -> ()) [||] [| 1 |])
        }

        Test "Array.iteri2" {
            let cell = [| 0 |]
            let incrBy x = cell.[0] <- cell.[0] + x
            let d = [| [1; 2]; []; [1; 2; 3] |]
            Raises (Array.iteri2 (fun _ _ _ -> ()) [||] [|1|])
            Array.iteri2 (fun idx x y -> (incrBy (List.length x + y + idx)))
                d [| 1 .. 3 |]
            Equal cell.[0] 14
        }

        Test "Array.isEmpty" {
            True (Array.isEmpty [||])
            True (not (Array.isEmpty [| 1 |]))
        }

        Test "Array.length" {
            Equal (Array.length [||]) 0
            Equal (Array.length [| 1 .. 10 |]) 10
            Equal (Array.length (Array.zeroCreate 5)) 5
            let r = R.ArrayOf R.Int
            ForR 100 (R.Tuple2Of (r, r)) (fun (x, y) -> Do {
                Equal (Array.length x + Array.length y) (Array.length (Array.append x y))
            })
        }

        Test "Array.map" {
            Equal (Array.map (fun x -> x % 2) [| 1 .. 10 |])
                [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0 |]
            let funcs = [| (+) 1; ( * ) 2; (fun x -> x); (fun x -> x * x)|]
            let rA  = R.ArrayOf R.Int
            let rF = R.OneOf funcs
            ForR 100 (R.Tuple3Of (rA, rF, rF)) (fun (arr, f1, f2) -> Do {
                let map1 =
                    arr
                    |> Array.map f1
                    |> Array.map f2
                let map2 =
                    arr
                    |> Array.map (f1 >> f2)
                Equal map1 map2
            })
        }

        Test "Array.mapi" {
            let array = [| 0; 1; 2; 3; 4 |]
            let foo i x = array.[(i + 1) % 5] + x
            Equal (Array.mapi foo array) [| 1; 3; 5; 7; 4 |]
        }

        Test "Array.map2" {
            let a1 = [| [1; 2]; []; [1; 2; 3] |]
            let a2 = [| 1; 2; 3 |]
            Equal (Array.map2 (fun x y -> List.length x + y) a1 a2)
                [| 3; 2; 6 |]
        }

        Test "Array.mapi2" {
            let a1 = [| [1; 2]; []; [1; 2; 3] |]
            let a2 = [| 1; 2; 3 |]
            Equal (Array.mapi2 (fun idx x y -> (idx + List.length x + y)) a1 a2)
                [| 3; 3; 8 |]
        }

        Test "Array.max" {
            Equal (Array.max [| 1; 2; 3; 2; 1 |]) 3
        }

        Test "Array.maxBy" {
            Equal (Array.maxBy id [| 1; 2; 3; 2; 1 |]) 3
        }

        Test "Array.min" {
            Equal (Array.min [| 4; 5; 3; 5; 4 |]) 3
        }

        Test "Array.minBy" {
            Equal (Array.minBy id [| 4; 5; 3; 5; 4 |]) 3
        }

        Test "Array.ofList" {
            Equal (Array.ofList [1;2;3]) [|1; 2; 3|]
        }

        Test "Array.ofSeq" {
            Equal (Array.ofSeq [1;2;3]) [|1; 2; 3|]
        }

        Test "Array.partition" {
            let (even, odd) =
                [| 1 .. 5 |]
                |> Array.partition (fun x -> x % 2 = 0)
            Equal even [| 2; 4 |]
            Equal odd [| 1; 3; 5|]
            let funcs = [| (=) 3; (fun x -> x % 2 = 0) |]
            let r = R.Tuple2Of (R.ArrayOf R.Int, R.OneOf funcs)
            ForR 100 r (fun (a, f) -> Do {
                let (x, y) = Array.partition f a
                Equal (Array.length (Array.append x y)) (Array.length x + Array.length y)
            })
        }

        Test "Array.permute" {
            Equal (Array.permute (fun x -> (x + 3) % 5) [| 1 ; 2; 3 ; 4; 5 |])
                [| 3; 4; 5; 1; 2 |]
        }

        Test "Array.pick" {
            let finder x = if x = 4 then Some x else None
            Equal (Array.pick finder [| 4 |]) 4
        }

        Test "Array.reduce" {
            Equal (Array.reduce (+) [| 1; 1; 1; 1; 1 |]) 5
            Equal (Array.reduce (+) [| 3 |]) 3
        }

        Test "Array.reduceBack" {
            Equal (Array.reduceBack (+) [| 1; 1; 1; 1; 1 |]) 5
            Equal (Array.reduceBack (+) [| 3 |]) 3
            ForR 100 (R.ArrayOf R.Int) (fun a -> Do {
                True (Array.isEmpty a || (Array.reduce (+) a = Array.reduceBack (+) a))
            })
        }

        Test "Array.rev" {
            ForR 100 (R.ArrayOf R.Int) (fun a -> Do {
                Equal (Array.rev (Array.rev a)) a
            })
            Equal (Array.rev [||]) [||]
            ForR 100 R.Int (fun x -> Do { Equal [| x |] (Array.rev [| x |]) })
        }

        Test "Array.scan" {
            Equal (Array.scan (fun x y -> x + List.length y ) 0 [| [2; 1]; [1; 2; 3] |])
                [| 0; 2; 5 |]
        }

        Test "Array.scanBack" {
            Equal (Array.scanBack (fun y x -> x + List.length y) [| [2; 1]; [1; 2; 3] |] 0)
                [| 5; 3; 0 |]
        }

        Test "Array.sort" {
            let a = [| 1; 8; 4 |]
            Equal (Array.sort a) [| 1; 4; 8 |]
            Equal a [| 1; 8; 4 |]
        }

        Test "Array.sortBy" {
            let a = [| 1; 0; -2 |]
            Equal (Array.sortBy (fun x -> x * x) a) [| 0; 1; -2 |]
            Equal a [| 1; 0; -2 |]
        }

        Test "Array.sortInPlace" {
            let a = [| 1; 4; 2 |]
            Array.sortInPlace a
            Equal a [| 1; 2; 4 |]
        }

        Test "Array.sortInPlaceBy" {
            let a = [| 1; 0; -2 |]
            Array.sortInPlaceBy (fun x -> x * x) a
            Equal a [| 0; 1; -2 |]
        }

        Test "Array.sortInPlaceWith" {
            let arr = [| [1; 2; 3]; []; [1; 2] |]
            let comparer (x : int list) (y : int list) =
                List.length x - List.length y
            Array.sortInPlaceWith comparer arr
            Equal arr [| []; [1; 2]; [1; 2; 3] |]
            ForR 100 (R.ArrayOf (R.ListOf R.Int)) (fun arr -> Do {
                let copy = Array.copy arr
                Array.sortInPlaceWith comparer copy
                Equal (Array.sortWith comparer arr) copy
            })
        }

        Test "Array.sortWith" {
            let a = [| 1; 0; -2 |]
            let b = Array.sortWith (fun x y -> compare (abs x) (abs y)) a
            Equal b [| 0; 1; -2 |]
            NotEqual b a
        }

        Test "Array.sub" {
            Equal (Array.sub [| 1 .. 10 |] 5 5) [| 6 .. 10 |]
        }

        Test "Array.sum" {
            Equal (Array.sum<int> [| |]) 0
            Equal (Array.sum<float> [| |]) 0.
            Equal (Array.sum [| 1 .. 4 |]) 10
            Equal (Array.sum [| 1. .. 4. |]) 10.
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
            Equal (Array.sumBy getInt oneToFourInt) 10
            Equal (Array.sumBy getFloat oneToFourFloat) 10.
            Equal (Array.sumBy getInt [||]) 0
        }

        Test "Array.toList" {
            let arr = [| 1; 2; 3; 4; 5 |]
            let l = [1; 2; 3; 4; 5]
            Equal l (Array.toList arr)
            ForR 100 (R.ListOf R.Int) (fun x -> Do {
                Equal (Array.toList (Array.ofList x)) x
            })
        }

        Test "Array.toSeq" {
            Equal (Array.ofSeq (Array.toSeq [| 1; 2; 3 |])) [| 1; 2; 3 |]
        }

        Test "Array.tryFind" {
            let finder elem =
                match elem with
                | Some x -> x = 4
                | _      -> false
            Equal (Array.tryFind finder [| None; None; Some 4; None |]) (Some (Some 4))
            Equal (Array.tryFind (fun _ -> false) [| None; None; Some 4; None |]) None
        }

        Test "Array.tryFindIndex" {
            Equal (Array.tryFindIndex (fun x -> x = 5) [| 1 .. 10 |]) (Some 4)
            Equal (Array.tryFindIndex (fun x -> x = 5) [| 1 .. 3 |]) None
        }

        Test "Array.tryPick" {
            let f x = if x = 5 then Some (x + 1) else None
            Equal (Array.tryPick f [| 1 .. 10 |]) (Some 6)
            Equal (Array.tryPick f [| 1 .. 3 |]) None
        }

        Test "Array.unzip" {
            let r = R.Tuple2Of (R.Int, R.Boolean)
            ForR 100 (R.ArrayOf r) (fun arr -> Do {
                let (x, y) = Array.unzip arr
                Equal (Array.zip x y) arr
            })
        }

        Test "Array.unzip3" {
            let r = R.Tuple3Of (R.Int, R.Boolean, R.Int)
            ForR 100 (R.ArrayOf r) (fun arr -> Do {
                let (x, y, z) = Array.unzip3 arr
                Equal (Array.zip3 x y z) arr
            })
        }

        Test "Array.zeroCreate" {
            Equal (Array.zeroCreate 2 : int [])
                [| Unchecked.defaultof<_>; Unchecked.defaultof<_> |]
        }

        Test "Array.zip" {
            let arr1, arr2 = [||], [| 1 |]
            Raises (Array.zip arr1 arr2)
            let (arr1, arr2) = ([| "a" |], [| 1 |])
            Equal (Array.zip arr1 arr2) [| ("a", 1) |]
        }

        Test "Array.zip3" {
            let (arr1, arr2, arr3) = ([||], [| 1 |], [| 1 |])
            Raises (Array.zip3 arr1 arr2 arr3)
            Raises (Array.zip3 arr2 arr1 arr3)
            Raises (Array.zip3 arr1 arr3 arr2)
            let (arr1, arr2, arr3) = ([| "a" |], [| 1 |], [| (1, 2) |])
            Equal (Array.zip3 arr1 arr2 arr3) [| ("a", 1, (1, 2)) |]
        }

        Test "GetArraySlice" {
            let arr = [| 0; 1; 2; 3; 4; 5 |]
            Equal arr.[.. 2] [| 0; 1; 2 |]
            Equal arr.[4 ..] [| 4 ; 5 |]
            Equal arr.[2 .. 4] [| 2; 3; 4 |]
        }

        Test "Extensions" {       
            Equal ([| "a"; "b"; "c" |].JS.Join()) "a,b,c"
            Equal ([| "a"; "b"; "c" |].JS.Join("-")) "a-b-c"

            Equal ([| 0; 1; 2; 3 |].JS.Slice(1)) [| 1; 2; 3 |]
            Equal ([| 0; 1; 2; 3 |].JS.Slice(1, 3)) [| 1; 2 |]

            let a = [| 0 |]
            Equal (a.JS.Push(1)) 2
            Equal (a.JS.Push(2, 3)) 4
            Equal a [| 0; 1; 2; 3 |]

            let b = [| 0; 1; 2; 3 |];
            Equal (b.JS.Splice(1, 2, 4, 4, 4)) [| 1; 2 |]
            Equal b [| 0; 4; 4; 4; 3 |]

            Equal ([| 0; 1; 2 |].JS.Sort(fun (a, b) -> b - a)) [| 2; 1; 0 |]

            Equal ([| 0; 1; 2 |].JS.Map(fun (x, i, arr) -> if arr = [| 0; 1; 2 |] then x + 1 + i else 0))
                [| 1; 3; 5 |]
            Equal ([| 0; 1; 2 |].JS.Map((fun t (x, i, a) -> if a = [| 0; 1; 2 |] then x + t + i else 0), 1))
                [| 1; 3; 5 |]

            Equal ([| 1; 2; 3 |].JS.Reduce(fun (a, b, i, arr) -> if arr = [| 1; 2; 3 |] then a + b + i else 0))
                9
            Equal ([| 1; 2; 3 |].JS.Reduce((fun (a, b, i, arr) -> if arr = [| 1; 2; 3 |] then a + b + i else 0), 1))
                10
        }

    }
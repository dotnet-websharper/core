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

module WebSharper.Tests.List

open WebSharper
open WebSharper.Testing
module R = WebSharper.Testing.Random

[<JavaScript>]
let Tests =

    Section "List" {

        Test "List.Cons" {
            Equal (List.Cons (1, [])) [1]
            Equal (1 :: 2 :: 3 :: []) [1; 2; 3]
        }

        Test "List.append" {
            Equal (List.append [1; 2; 3] [4; 5]) [1..5]
            Equal (List.append [] []) []
            Equal (List.append [1] []) [1]
            Equal (List.append [] [1]) [1]
            ForR 100 (R.ListOf R.Int) (fun x -> Do {
                Equal (List.append x []) x
                Equal (List.append [] x) x
            })
            ForR 100 (R.Tuple2Of (R.ListOf R.Int, R.ListOf R.Int))
                (fun (x, y) -> Do {
                    Equal (List.length (List.append x y))
                        (List.length x + List.length y)
                })
            ForR 100 (R.Tuple2Of (R.ListOf R.Int,R.ListOf R.Int))
                (fun (x, y) -> Do {
                    Equal (List.append x y)
                        (Array.append (Array.ofList x) (Array.ofList y)
                        |> Array.toList)
                })
        }

        Test "List.average" {
            Equal (List.average [1. .. 10.]) 5.5
        }

        Test "List.avarageBy" {
            Equal (List.averageBy (fun x -> x * x) [1. .. 10.]) 38.5
        }

        Test "List.choose" {
            let ok x =
                if x % 2 = 0 then
                    Some (string x)
                else
                    None
            Equal (List.choose ok [1; 2; 3; 4; 5]) ["2"; "4"]
            Equal (List.choose id [None; Some 4; None]) [4]
        }

        Test "List.collect" {
            let l = [0; 1; 2; 3; 4]
            let foo (x:int) = [x]
            Equal (List.collect foo l) [0; 1; 2; 3; 4]
        }

        Test "List.concat" {
            Equal (List.concat [| [1; 2]; [3; 4] |]) [1..4]
        }

        Test "List.empty" {
            Equal List.empty []
            Equal List.Empty []
        }

        Test "List.exists" {
            False (List.exists (fun x -> x % 2 = 1) [2; 4])
            False (List.exists (fun _ -> true) [])
            True (List.exists ((=) 8) [1..10])
        }

        Test "List.exists2" {
            False (List.exists2 (fun x y -> ((x + y) % 2 = 0)) [1..5] [6..10])
            False (List.exists2 (fun _ _ -> true) [] [])
            let isSeven x = x = 7
            True (List.exists2 (fun x y -> isSeven x || isSeven y) [1..5] [6..10])
        }

        Test "List.filter" {
            Equal (List.filter (fun x -> x > 0) [-5 .. 5]) [1..5]
        }

        Test "List.find" {
            Equal (List.find Option.isSome [None; Some 4]) (Some 4)
            Raises (List.find ((=) (Some 5)) [None; Some 4])
        }

        Test "List.findIndex" {
            Equal (List.findIndex Option.isSome [None; Some 4]) 1
            Raises (List.findIndex ((=) (Some 5)) [None; Some 4])
        }

        Test "List.fold" {
            Equal (List.fold (+) 0 [1;1;1;1]) 4
            Equal (List.fold (fun x y -> x + List.length y) 0 [[1]; [2;3]]) 3
        }

        Test "List.fold2" {
            let first (x: int list) = x.[0]
            let oneTest = [[2; 1]; [1; 2]]
            let l2 = [[1]; [2]]
            Equal (List.fold2 (fun x y z -> x + List.length y + first z) 0 oneTest l2) 7
            Raises (List.fold2 (fun _ _ _ -> true) true [] [1])
            ForR 100 (R.ListOf (R.Tuple2Of (R.Int,R.Boolean))) (fun x -> Do {
                let (x,y) = List.unzip x
                let conditionalAdder s x y = if y then s + x else s
                let xArr, yArr = (Array.ofList x, Array.ofList y)
                let byList  = List.fold2  conditionalAdder 0 x y
                let byArray = Array.fold2 conditionalAdder 0 xArr yArr
                Equal byList byArray
            })
        }

        Test "List.foldBack" {
            Equal (List.foldBack (+) [1;1;1;1] 0) 4
            Equal (List.foldBack (fun y x -> x + List.length y) [[1]; [2;3]] 0) 3
            ForR 100 (R.ListOf R.Int) (fun x -> Do {
                Equal (List.foldBack (+) x 0) (Array.foldBack (+) (Array.ofList x) 0)
            })
            ForR 100 (R.ListOf R.Int) (fun x -> Do {
                Equal (List.fold  (+) 0 x) (List.foldBack (+) x 0)
            })
            ForR 100 (R.ListOf R.Int) (fun x -> Do {
                Equal (List.fold (-) 0 x) (Array.fold (-) 0 (Array.ofList x))
            })
        }

        Test "List.foldBack2" {
            Equal (List.foldBack2 (fun y z x -> x + List.length y + List.head z)
                    [[2; 1]; [1; 2]] [[1]; [2]] 0)
                7
            Raises (List.foldBack2 (fun _ _ _ -> true) [] [1] true)
            ForR 100 (R.ListOf (R.Tuple2Of (R.Int, R.Boolean))) (fun x -> Do {
                let (x,y) = List.unzip x
                let conditionalAdder s x y = if y then s + x else s
                let left  = List.fold2  conditionalAdder 0 x y
                let conditionalAdder x y s = conditionalAdder s x y
                let right = List.foldBack2 conditionalAdder x y 0
                Equal left right
            })
        }

        Test "List.forall" {
            True (List.forall (fun x -> x % 2 = 0) [2; 4; 6])
            True (List.forall (fun _ -> false) [])
            False (List.forall ((=) 1) [1; 2; 3])
        }

        Test "List.forall2" {
            True (List.forall2 (fun x y -> (x + y) % 2 = 1) [1..5] [6..10])
            True (List.forall2 (fun _ _ -> false) [] [])
            let isSeven x = x = 7
            False (List.forall2 (fun x y -> isSeven x || isSeven y) [1..4] [6..9])
            Raises (List.forall2 (fun _ _ -> true) [] [1])
        }

        Test "List.head" {
            Equal [1; 2; 3].Head 1
            Equal (List.head [1; 2; 3]) 1
            Raises (List.head [])
        }

        Test "List.init" {
            Raises (List.init -3 ((+) 5))
            Equal (List.init 3 ((*) 2)) [0; 2; 4]
        }

        Test "List.isEmpty" {
            True [].IsEmpty
            False [1].IsEmpty
            True (List.isEmpty [])
            False (List.isEmpty [1])
        }

        Test "List.iter" {
            let r = ref 0
            List.iter (fun x -> r := !r + 1) [1..10]
            Equal !r 10
        }

        Test "List.iter2" {
            let cell = [| 0 |]
            let incrBy x = cell.[0] <- cell.[0] + x
            List.iter2 (fun x y -> incrBy (List.length x + y)) [[1]; [2;3]] [5..6]
            Equal cell.[0] 14
            Raises (List.iter2 (fun _ _ -> ()) [] [1])
        }

        Test "List.iteri" {
            let l = [4; 3; 2; 1; 0]
            let arr = [| 0 |]
            List.iteri (fun x y -> arr.[0] <- arr.[0] + x + y) l
            Equal arr.[0] 20
        }

        Test "List.iteri2" {
            let cell = [|0|]
            let incrBy x = cell.[0] <- cell.[0] + x
            List.iteri2 (fun idx x y -> incrBy (List.length x + y + idx))
                [[1]; [2;3]; []] [5..7]
            Equal cell.[0] 24
            Raises (List.iteri2 (fun _ _ _ -> ()) [] [1])
        }

        Test "List.length" {
            Equal [1; 2; 3].Length 3
            Equal (List.length [1; 2; 3]) 3
            Equal (List.length []) 0
            Equal (List.length [null; "not null"; null; "not null"]) 4
        }

        Test "List.map" {
            let oneToTen = [1 .. 10]
            Equal (List.map (fun x -> x % 2) oneToTen)
                [1; 0; 1; 0; 1; 0; 1; 0; 1; 0]
            let funcs = [| (+) 1; (*) 2; (fun x -> x); (fun x -> x * x) |]
            ForR 100 (R.Tuple3Of(R.ListOf R.Int,
                                       R.OneOf funcs,
                                       R.OneOf funcs))
                (fun (x, f1, f2) -> Do {
                    let map1 = x
                               |> List.map f1
                               |> List.map f2
                    let map2 = x |> List.map (f1 >> f2)
                    Equal map1 map2
                })
        }

        Test "List.map2" {
            let f (x : list<int>) y = List.length x + y
            Equal (List.map2 f [[1]; [2; 3]; []] [5..7]) [6; 8; 7]
        }

        Test "List.map3" {
            let f (x : int list) (y : int) (z : int) = (List.length x + y + z)
            Equal (List.map3 f [[1]; [2; 3]; []] [5..7] [1..3]) [7; 10; 10]
        }

        Test "List.mapi" {
            let l = [0; 1; 2; 3; 4]
            let foo i x = l.[(i + 1) % 5] + x
            Equal (List.mapi foo l) [1; 3; 5; 7; 4]
        }

        Test "List.mapi2" {
            let f idx (x : list<int>) y = idx + List.length x + y
            Equal (List.mapi2 f [[1]; [2; 3]; []] [5..7]) [6; 9; 9]
        }

        Test "List.max" {
            Equal (List.max [1; 2; 3; 2; 1]) 3
        }

        Test "List.maxBy" {
            Equal (List.maxBy id [1; 2; 3; 2; 1]) 3
        }

        Test "List.min" {
            Equal (List.min [4; 5; 3; 5; 4]) 3
        }

        Test "List.minBy" {
            Equal (List.minBy id [4; 5; 3; 5; 4]) 3
        }

        Test "List.nth" {
            Raises (List.nth [1; 2; 3] 4)
            Equal (List.nth [1; 2; 3] 2) 3
            Equal [1; 2; 3].[1] 2
        }

        Test "List.ofArray" {
            Equal (List.ofArray [| 1; 2; 3 |]) [1; 2; 3]
        }

        Test "List.ofSeq" {
            Equal (List.ofSeq [| 1; 2; 3 |]) [1; 2; 3]
        }

        Test "List.partition" {
            Equal (List.partition (fun x -> x > 0) [-3 .. 3])
                ([1; 2; 3], [-3; -2; -1; 0])
        }

        Test "List.permute" {
            Equal (List.permute (fun x -> (x + 3) % 5) [1..5]) [3; 4; 5; 1; 2]
        }

        Test "List.pick" {
            let finder x = if x = 4 then Some x else None
            Equal (List.pick finder [4]) 4
        }

        Test "List.reduce" {
            Equal (List.reduce (+) [1; 1; 1; 1; 1]) 5
            Equal (List.reduce (+) [3]) 3
        }

        Test "List.reduceBack" {
            Equal (List.reduceBack (+) [1; 1; 1; 1; 1]) 5
            Equal (List.reduceBack (+) [3]) 3
            ForR 100 (R.ListOf R.Int) (fun x ->
                if List.isEmpty x then Do { () }
                else
                    Do {
                        let left   = List.reduce (+) x
                        let right  = List.reduceBack (+) x
                        Equal left right
                        ()
                    })
        }

        Test "List.replicate" {
            Equal (List.replicate 5 "V") ["V"; "V"; "V"; "V"; "V"]
            ForR 100 (R.Tuple2Of (R.Natural, R.Int)) (fun (size, elem) -> Do {
                let l = List.replicate size elem
                Equal (List.length l) size
                For l (fun x -> Do { Equal x elem })
            })
        }

        Test "List.rev" {
            Equal (List.rev [1; 2; 3]) [3; 2; 1]
            Equal (List.rev []) []
            ForR 100 (R.ListOf R.Int) (fun x -> Do {
                let doubleRev = List.rev >> List.rev
                Equal (doubleRev x) x
                let l = List.rev x @ x
                Equal (List.rev l) l
            })
        }

        Test "List.scan" {
            let l3 = [[2; 1]; [1; 2; 3]]
            Equal (List.scan (fun x y -> x + List.length y) 0 l3) [0; 2; 5]
            ForR 100 (R.ListOf R.Int) (fun x -> Do {
                let byList  = List.scan  (+) 0 x
                let byArray = Array.scan (+) 0 (Array.ofList x)
                Equal byList (Array.toList byArray)
            })
        }

        Test "List.scanBack" {
            Equal (List.scanBack (+) [4] 0) [4; 0]
            let l3 = [[2;1]; [1;2;3]]
            Equal (List.scanBack (fun y x -> x + List.length y) l3 0) [5; 3; 0]
            ForR 100 (R.ListOf R.Int) (fun x -> Do {
                let byList  = List.scanBack  (+) x 0
                let byArray = Array.scanBack (+) (Array.ofList x) 0
                Equal byList (Array.toList byArray)
            })
        }

        Test "List.sort" {
            Equal (List.sort [4; 1; 15])
                [1; 4; 15]
        }

        Test "List.sortBy" {
            Equal (List.sortBy (fun x -> x * x) [-3; -2; 1; 4])
                [1; -2; -3; 4]
        }

        Test "List.sortWith" {
            let l = [[1; 2; 3]; []; [1; 2]]
            let comparer (x:int list) (y:int list) = List.length x - List.length y
            let l = List.sortWith comparer l
            Equal l [[]; [1; 2]; [1; 2; 3]]
            ForR 100 (R.ListOf R.Int) (fun x -> Do {
                let firstSort = List.sortWith (-) x
                let secondSort = List.sortWith (-) firstSort
                Equal firstSort secondSort
            })
        }

        Test "List.sum" {
            Equal (List.sum<int> [])    0
            Equal (List.sum<float> [])  0.
            Equal (List.sum [1. .. 4.]) 10.
            Equal (List.sum [1 .. 4])   10
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
            Equal sumInt   10
            Equal sumFloat 10.
            Equal sumEmpty 0
        }

        Test "List.tail" {
            Raises (List.tail [])
            Equal (List.tail [1;2;3]) [2; 3]
            Equal [1; 2; 3].Tail [2; 3]
        }

        Test "List.toArray" {
            Equal (List.toArray [1; 2]) [| 1; 2 |]
        }

        Test "List.toSeq" {
            Equal (Array.ofSeq (List.toSeq [1; 2])) [| 1; 2 |]
        }

        Test "List.tryFind" {
            Equal (List.tryFind (fun x -> x = 2) [0..3]) (Some 2)
            Equal (List.tryFind (fun x -> x = 8) [0..3]) None
        }

        Test "List.tryFindIndex" {
            Equal (List.tryFindIndex (fun x -> x = 2) [1..3]) (Some 1)
            Equal (List.tryFindIndex (fun x -> x = 8) [0..3]) None
        }

        Test "List.tryPick" {
            let finder x = if x = 4 then Some x else None
            Equal (List.tryPick finder [4]) (Some 4)
            Equal (List.tryPick finder [3]) None
        }

        Test "List.unzip" {
            Equal (List.unzip [("a", 1); ("b", 2)])
                (["a"; "b"], [1; 2])
        }

        Test "List.unzip3" {
            Equal (List.unzip3 [("a", 1, true); ("b", 2, false)])
                (["a"; "b"], [1; 2], [true; false])
        }

        Test "List.zip" {
            Raises (List.zip [] [1])
            Equal (List.zip ["a"] [1]) [("a", 1)]
            ForR 100 (R.ListOf (R.Tuple2Of (R.Int, R.Int))) (fun l -> Do {
                let (x, y) = List.unzip l
                Equal (List.zip x y) l
            })
        }

        Test "List.zip3" {
            let ((arr1 : int list), arr2, arr3) = ([], [1], [1])
            Raises (List.zip3 arr1 arr2 arr3)
            Raises (List.zip3 arr2 arr1 arr3)
            Raises (List.zip3 arr1 arr3 arr2)
            let (arr1, arr2, arr3) = (["a"], [1], [(1, 2)])
            Equal (List.zip3 arr1 arr2 arr3)
                [("a", 1, (1, 2))]
        }

        Test "Comprehensions" {
            let l1 = [for x in 1 .. 10 -> x * x]
            Equal l1 [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
            let vec1 = [1; 2; 3]
            let vec2 = [4; 5; 6]
            let l2   = [for x in vec1 do
                          for y in vec2 do
                            yield x * y]
            Equal l2 [4; 5; 6; 8; 10; 12; 12; 15; 18]
            let l3 n = [for row in 1 .. n do
                          for col in 1 .. n do
                            if (row+col) % 2 = 0 then
                              yield (row,col)]
            let m = [(1, 1); (1, 3); (1, 5); (2, 2); (2, 4); (3, 1);
                     (3, 3); (3, 5); (4, 2); (4, 4); (5, 1); (5, 3); (5, 5)]
            Equal (l3 5) m
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
            Equal fibonacci [2; 3; 5; 8; 13]
        }

    }
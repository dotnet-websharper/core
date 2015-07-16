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

    TestCategory "List" {

        Test "List.Cons" {
            equal (List.Cons (1, [])) [1]
            equal (1 :: 2 :: 3 :: []) [1; 2; 3]
        }

        Test "List.append" {
            equal (List.append [1; 2; 3] [4; 5]) [1..5]
            equal (List.append [] []) []
            equal (List.append [1] []) [1]
            equal (List.append [] [1]) [1]
            property (fun x -> Do {
                equal (List.append x []) x
                equal (List.append [] x) x
            })
            property (fun (x, y) -> Do {
                    equal (List.length (List.append x y))
                        (List.length x + List.length y)
                })
            property (fun (x, y) -> Do {
                    equal (List.append x y)
                        (Array.append (Array.ofList x) (Array.ofList y)
                        |> Array.toList)
                })
        }

        Test "List.average" {
            equal (List.average [1. .. 10.]) 5.5
        }

        Test "List.avarageBy" {
            equal (List.averageBy (fun x -> x * x) [1. .. 10.]) 38.5
        }

        Test "List.choose" {
            let ok x =
                if x % 2 = 0 then
                    Some (string x)
                else
                    None
            equal (List.choose ok [1; 2; 3; 4; 5]) ["2"; "4"]
            equal (List.choose id [None; Some 4; None]) [4]
        }

        Test "List.collect" {
            let l = [0; 1; 2; 3; 4]
            let foo (x:int) = [x]
            equal (List.collect foo l) [0; 1; 2; 3; 4]
        }

        Test "List.concat" {
            equal (List.concat [| [1; 2]; [3; 4] |]) [1..4]
        }

        Test "List.empty" {
            equal List.empty []
            equal List.Empty []
        }

        Test "List.exists" {
            isFalse (List.exists (fun x -> x % 2 = 1) [2; 4])
            isFalse (List.exists (fun _ -> true) [])
            isTrue (List.exists ((=) 8) [1..10])
        }

        Test "List.exists2" {
            isFalse (List.exists2 (fun x y -> ((x + y) % 2 = 0)) [1..5] [6..10])
            isFalse (List.exists2 (fun _ _ -> true) [] [])
            let isSeven x = x = 7
            isTrue (List.exists2 (fun x y -> isSeven x || isSeven y) [1..5] [6..10])
        }

        Test "List.filter" {
            equal (List.filter (fun x -> x > 0) [-5 .. 5]) [1..5]
        }

        Test "List.find" {
            equal (List.find Option.isSome [None; Some 4]) (Some 4)
            raises (List.find ((=) (Some 5)) [None; Some 4])
        }

        Test "List.findIndex" {
            equal (List.findIndex Option.isSome [None; Some 4]) 1
            raises (List.findIndex ((=) (Some 5)) [None; Some 4])
        }

        Test "List.fold" {
            equal (List.fold (+) 0 [1;1;1;1]) 4
            equal (List.fold (fun x y -> x + List.length y) 0 [[1]; [2;3]]) 3
        }

        Test "List.fold2" {
            let first (x: int list) = x.[0]
            let oneTest = [[2; 1]; [1; 2]]
            let l2 = [[1]; [2]]
            equal (List.fold2 (fun x y z -> x + List.length y + first z) 0 oneTest l2) 7
            raises (List.fold2 (fun _ _ _ -> true) true [] [1])
            property (fun x -> Do {
                let (x,y) = List.unzip x
                let conditionalAdder s x y = if y then s + x else s
                let xArr, yArr = (Array.ofList x, Array.ofList y)
                let byList  = List.fold2  conditionalAdder 0 x y
                let byArray = Array.fold2 conditionalAdder 0 xArr yArr
                equal byList byArray
            })
        }

        Test "List.foldBack" {
            equal (List.foldBack (+) [1;1;1;1] 0) 4
            equal (List.foldBack (fun y x -> x + List.length y) [[1]; [2;3]] 0) 3
            property (fun x -> Do {
                equal (List.foldBack (+) x 0) (Array.foldBack (+) (Array.ofList x) 0)
            })
            property (fun x -> Do {
                equal (List.fold  (+) 0 x) (List.foldBack (+) x 0)
            })
            property (fun x -> Do {
                equal (List.fold (-) 0 x) (Array.fold (-) 0 (Array.ofList x))
            })
        }

        Test "List.foldBack2" {
            equal (List.foldBack2 (fun y z x -> x + List.length y + List.head z)
                    [[2; 1]; [1; 2]] [[1]; [2]] 0)
                7
            raises (List.foldBack2 (fun _ _ _ -> true) [] [1] true)
            property (fun x -> Do {
                let (x,y) = List.unzip x
                let conditionalAdder s x y = if y then s + x else s
                let left  = List.fold2  conditionalAdder 0 x y
                let conditionalAdder x y s = conditionalAdder s x y
                let right = List.foldBack2 conditionalAdder x y 0
                equal left right
            })
        }

        Test "List.forall" {
            isTrue (List.forall (fun x -> x % 2 = 0) [2; 4; 6])
            isTrue (List.forall (fun _ -> false) [])
            isFalse (List.forall ((=) 1) [1; 2; 3])
        }

        Test "List.forall2" {
            isTrue (List.forall2 (fun x y -> (x + y) % 2 = 1) [1..5] [6..10])
            isTrue (List.forall2 (fun _ _ -> false) [] [])
            let isSeven x = x = 7
            isFalse (List.forall2 (fun x y -> isSeven x || isSeven y) [1..4] [6..9])
            raises (List.forall2 (fun _ _ -> true) [] [1])
        }

        Test "List.head" {
            equal [1; 2; 3].Head 1
            equal (List.head [1; 2; 3]) 1
            raises (List.head [])
        }

        Test "List.init" {
            raises (List.init -3 ((+) 5))
            equal (List.init 3 ((*) 2)) [0; 2; 4]
        }

        Test "List.isEmpty" {
            isTrue [].IsEmpty
            isFalse [1].IsEmpty
            isTrue (List.isEmpty [])
            isFalse (List.isEmpty [1])
        }

        Test "List.iter" {
            let r = ref 0
            List.iter (fun x -> r := !r + 1) [1..10]
            equal !r 10
        }

        Test "List.iter2" {
            let cell = [| 0 |]
            let incrBy x = cell.[0] <- cell.[0] + x
            List.iter2 (fun x y -> incrBy (List.length x + y)) [[1]; [2;3]] [5..6]
            equal cell.[0] 14
            raises (List.iter2 (fun _ _ -> ()) [] [1])
        }

        Test "List.iteri" {
            let l = [4; 3; 2; 1; 0]
            let arr = [| 0 |]
            List.iteri (fun x y -> arr.[0] <- arr.[0] + x + y) l
            equal arr.[0] 20
        }

        Test "List.iteri2" {
            let cell = [|0|]
            let incrBy x = cell.[0] <- cell.[0] + x
            List.iteri2 (fun idx x y -> incrBy (List.length x + y + idx))
                [[1]; [2;3]; []] [5..7]
            equal cell.[0] 24
            raises (List.iteri2 (fun _ _ _ -> ()) [] [1])
        }

        Test "List.length" {
            equal [1; 2; 3].Length 3
            equal (List.length [1; 2; 3]) 3
            equal (List.length []) 0
            equal (List.length [null; "not null"; null; "not null"]) 4
        }

        Test "List.map" {
            let oneToTen = [1 .. 10]
            equal (List.map (fun x -> x % 2) oneToTen)
                [1; 0; 1; 0; 1; 0; 1; 0; 1; 0]
            let funcs = [| (+) 1; (*) 2; (fun x -> x); (fun x -> x * x) |]
            propertyWith (R.Tuple3Of(R.Auto(), R.OneOf funcs, R.OneOf funcs))
                (fun (x, f1, f2) -> Do {
                    let map1 = x
                               |> List.map f1
                               |> List.map f2
                    let map2 = x |> List.map (f1 >> f2)
                    equal map1 map2
                })
        }

        Test "List.map2" {
            let f (x : list<int>) y = List.length x + y
            equal (List.map2 f [[1]; [2; 3]; []] [5..7]) [6; 8; 7]
        }

        Test "List.map3" {
            let f (x : int list) (y : int) (z : int) = (List.length x + y + z)
            equal (List.map3 f [[1]; [2; 3]; []] [5..7] [1..3]) [7; 10; 10]
        }

        Test "List.mapi" {
            let l = [0; 1; 2; 3; 4]
            let foo i x = l.[(i + 1) % 5] + x
            equal (List.mapi foo l) [1; 3; 5; 7; 4]
        }

        Test "List.mapi2" {
            let f idx (x : list<int>) y = idx + List.length x + y
            equal (List.mapi2 f [[1]; [2; 3]; []] [5..7]) [6; 9; 9]
        }

        Test "List.max" {
            equal (List.max [1; 2; 3; 2; 1]) 3
        }

        Test "List.maxBy" {
            equal (List.maxBy id [1; 2; 3; 2; 1]) 3
        }

        Test "List.min" {
            equal (List.min [4; 5; 3; 5; 4]) 3
        }

        Test "List.minBy" {
            equal (List.minBy id [4; 5; 3; 5; 4]) 3
        }

        Test "List.nth" {
            raises (List.nth [1; 2; 3] 4)
            equal (List.nth [1; 2; 3] 2) 3
            equal [1; 2; 3].[1] 2
        }

        Test "List.ofArray" {
            equal (List.ofArray [| 1; 2; 3 |]) [1; 2; 3]
        }

        Test "List.ofSeq" {
            equal (List.ofSeq [| 1; 2; 3 |]) [1; 2; 3]
        }

        Test "List.partition" {
            equal (List.partition (fun x -> x > 0) [-3 .. 3])
                ([1; 2; 3], [-3; -2; -1; 0])
        }

        Test "List.permute" {
            equal (List.permute (fun x -> (x + 3) % 5) [1..5]) [3; 4; 5; 1; 2]
        }

        Test "List.pick" {
            let finder x = if x = 4 then Some x else None
            equal (List.pick finder [4]) 4
        }

        Test "List.reduce" {
            equal (List.reduce (+) [1; 1; 1; 1; 1]) 5
            equal (List.reduce (+) [3]) 3
        }

        Test "List.reduceBack" {
            equal (List.reduceBack (+) [1; 1; 1; 1; 1]) 5
            equal (List.reduceBack (+) [3]) 3
            property (fun x ->
                if List.isEmpty x then Do { () }
                else
                    Do {
                        let left   = List.reduce (+) x
                        let right  = List.reduceBack (+) x
                        equal left right
                        ()
                    })
        }

        Test "List.replicate" {
            equal (List.replicate 5 "V") ["V"; "V"; "V"; "V"; "V"]
            propertyWith (R.Tuple2Of (R.Natural, R.Int)) (fun (size, elem) -> Do {
                let l = List.replicate size elem
                equal (List.length l) size
                forEach l (fun x -> Do { equal x elem })
            })
        }

        Test "List.rev" {
            equal (List.rev [1; 2; 3]) [3; 2; 1]
            equal (List.rev []) []
            property (fun x -> Do {
                let doubleRev = List.rev >> List.rev
                equal (doubleRev x) x
                let l = List.rev x @ x
                equal (List.rev l) l
            })
        }

        Test "List.scan" {
            let l3 = [[2; 1]; [1; 2; 3]]
            equal (List.scan (fun x y -> x + List.length y) 0 l3) [0; 2; 5]
            property (fun x -> Do {
                let byList  = List.scan  (+) 0 x
                let byArray = Array.scan (+) 0 (Array.ofList x)
                equal byList (Array.toList byArray)
            })
        }

        Test "List.scanBack" {
            equal (List.scanBack (+) [4] 0) [4; 0]
            let l3 = [[2;1]; [1;2;3]]
            equal (List.scanBack (fun y x -> x + List.length y) l3 0) [5; 3; 0]
            property (fun x -> Do {
                let byList  = List.scanBack  (+) x 0
                let byArray = Array.scanBack (+) (Array.ofList x) 0
                equal byList (Array.toList byArray)
            })
        }

        Test "List.sort" {
            equal (List.sort [4; 1; 15])
                [1; 4; 15]
        }

        Test "List.sortBy" {
            equal (List.sortBy (fun x -> x * x) [-3; -2; 1; 4])
                [1; -2; -3; 4]
        }

        Test "List.sortWith" {
            let l = [[1; 2; 3]; []; [1; 2]]
            let comparer (x:int list) (y:int list) = List.length x - List.length y
            let l = List.sortWith comparer l
            equal l [[]; [1; 2]; [1; 2; 3]]
            property (fun x -> Do {
                let firstSort = List.sortWith (-) x
                let secondSort = List.sortWith (-) firstSort
                equal firstSort secondSort
            })
        }

        Test "List.sum" {
            equal (List.sum<int> [])    0
            equal (List.sum<float> [])  0.
            equal (List.sum [1. .. 4.]) 10.
            equal (List.sum [1 .. 4])   10
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
            equal sumInt   10
            equal sumFloat 10.
            equal sumEmpty 0
        }

        Test "List.tail" {
            raises (List.tail [])
            equal (List.tail [1;2;3]) [2; 3]
            equal [1; 2; 3].Tail [2; 3]
        }

        Test "List.toArray" {
            equal (List.toArray [1; 2]) [| 1; 2 |]
        }

        Test "List.toSeq" {
            equal (Array.ofSeq (List.toSeq [1; 2])) [| 1; 2 |]
        }

        Test "List.tryFind" {
            equal (List.tryFind (fun x -> x = 2) [0..3]) (Some 2)
            equal (List.tryFind (fun x -> x = 8) [0..3]) None
        }

        Test "List.tryFindIndex" {
            equal (List.tryFindIndex (fun x -> x = 2) [1..3]) (Some 1)
            equal (List.tryFindIndex (fun x -> x = 8) [0..3]) None
        }

        Test "List.tryPick" {
            let finder x = if x = 4 then Some x else None
            equal (List.tryPick finder [4]) (Some 4)
            equal (List.tryPick finder [3]) None
        }

        Test "List.unzip" {
            equal (List.unzip [("a", 1); ("b", 2)])
                (["a"; "b"], [1; 2])
        }

        Test "List.unzip3" {
            equal (List.unzip3 [("a", 1, true); ("b", 2, false)])
                (["a"; "b"], [1; 2], [true; false])
        }

        Test "List.zip" {
            raises (List.zip [] [1])
            equal (List.zip ["a"] [1]) [("a", 1)]
            property (fun l -> Do {
                let (x, y) = List.unzip l
                equal (List.zip x y) l
            })
        }

        Test "List.zip3" {
            let ((arr1 : int list), arr2, arr3) = ([], [1], [1])
            raises (List.zip3 arr1 arr2 arr3)
            raises (List.zip3 arr2 arr1 arr3)
            raises (List.zip3 arr1 arr3 arr2)
            let (arr1, arr2, arr3) = (["a"], [1], [(1, 2)])
            equal (List.zip3 arr1 arr2 arr3)
                [("a", 1, (1, 2))]
        }

        Test "Comprehensions" {
            let l1 = [for x in 1 .. 10 -> x * x]
            equal l1 [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
            let vec1 = [1; 2; 3]
            let vec2 = [4; 5; 6]
            let l2   = [for x in vec1 do
                          for y in vec2 do
                            yield x * y]
            equal l2 [4; 5; 6; 8; 10; 12; 12; 15; 18]
            let l3 n = [for row in 1 .. n do
                          for col in 1 .. n do
                            if (row+col) % 2 = 0 then
                              yield (row,col)]
            let m = [(1, 1); (1, 3); (1, 5); (2, 2); (2, 4); (3, 1);
                     (3, 3); (3, 5); (4, 2); (4, 4); (5, 1); (5, 3); (5, 5)]
            equal (l3 5) m
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
            equal fibonacci [2; 3; 5; 8; 13]
        }

        #if FSHARP40

        Test "List.contains" {
            isTrue (List.contains 0 [ 0 .. 4 ])
        }

        Test "List.chunkBySize" {
            equal [ [ 1 .. 4 ]; [ 5 .. 8 ] ] (List.chunkBySize 4 [ 1 .. 8 ])
            equal [ [ 1 .. 4 ]; [ 5 .. 8 ]; [ 9; 10 ] ] (List.chunkBySize 4 [ 1 .. 10 ])
            raises (List.chunkBySize 0)
        }

        Test "List.compareWith" {
            equal (List.compareWith (fun _ -> failwith "Should not be evaluated") List.empty List.empty) 0
            equal (List.compareWith (fun _ -> failwith "Should not be evaluated") List.empty [ 1 ]) -1
            equal (List.compareWith (fun _ _ -> 1) [ 0; 1 ] [ 0; 2 ]) 1
        }

        Test "List.countBy" {
            equal (List.countBy (fun _ -> failwith "Should not be evaluated") List.empty) []
            equal (List.countBy id [ 1; 2; 2; 3; 3; 3 ]) [ 1, 1; 2, 2; 3, 3 ]
        }

        Test "List.distinct" {
            equal (List.distinct (fun _ -> failwith "Should not be evaluated") List.empty) []
            equal (List.distinct id [ 1; 2; 2; 3; 3; 3 ]) [ 1; 2; 3 ]
        }

        Test "List.distinctBy" {
            equal (List.distinctBy (fun _ -> failwith "Should not be evaluated") List.empty) []
            equal (List.distinct List.sum [ [ 0; 1 ]; [ 1; 0 ]; [ 1; 2 ] ]) [ [ 0; 1 ]; [ 1; 2 ] ]
        }

        Test "List.splitInto" {
            equal (List.splitInto 2 List.empty) List.empty
            raises ((List.splitInto 0) List.empty)
        }

        Test "List.exactlyOne" {
            equal (List.exactlyOne [ 0 ]) 0
            raises (List.exactlyOne [ 0; 1 ])
        }

        Test "List.except" {
            equal (List.except List.empty [ 0; 1 ]) [ 0; 1 ]
            equal (List.except [ 0 ] [ 0; 1 ]) [ 1 ]
        }

        Test "List.findBack" {
            raises (List.findBack (fun _ -> true) List.empty)
            equal (List.findBack (fun x -> x % 5 = 0) [ 1 .. 10 ]) 10
        }

        Test "List.findIndexBack" {
            raises (List.findIndexBack (fun _ -> true) List.empty)
            equal (List.findIndexBack (fun x -> x % 5 = 0) [ 1 .. 10 ]) 9
        }

        Test "List.groupBy" {
            equal (List.groupBy (fun (x : string) -> x.Length) [ "x"; "xx"; "xy"; "xyz" ]) [ 1, [ "x" ]; 2, [ "xx"; "xy" ]; 3, [ "xyz" ] ]
        }

        Test "List.indexed" {
            equal (List.indexed [ 0 .. 4 ]) (List.zip [ 0 .. 4 ] [ 0 .. 4 ])
            equal (List.indexed List.empty) List.empty
        }

        Test "List.item" {
            property (fun x -> Do {
                let list = [ x ]

                equal (List.item 0 list) x
            })
        }

        Test "List.last" {
            equal (List.last [ 0 .. 4 ]) 4
            raises (List.last List.empty)
        }

        Test "List.mapFold" {
            equal (List.mapFold (fun s x -> (x + 1, s + x)) 0 [ 0 .. 4 ]) (List.map ((+) 1) [ 0 .. 4 ], List.sum [ 0 .. 4 ])
        }

        Test "List.mapFoldBack" {
            equal (List.mapFoldBack (fun x s -> (x + 1, s + x)) [ 0 .. 4 ] 0) (List.map ((+) 1) [ 0 .. 4 ], List.sum [ 0 .. 4 ])
        }

        Test "List.pairwise" {
            equal (List.pairwise List.empty) List.empty
            equal (List.pairwise [ 0 ]) List.empty
            equal (List.pairwise [ 0; 1 ]) [ 0, 1 ]
        }

        Test "List.singleton" {
            property (fun x -> Do {
                equal (List.singleton x) [ x ]
            })
        }

        Test "List.skip" {
            raises (List.skip 1 List.empty)
            equal (List.skip 3 [ 0 .. 4 ]) [ 3; 4 ]
        }

        Test "List.skipWhile" {
            raises (List.skipWhile (fun _ -> true) List.empty)
            equal (List.skipWhile (fun _ -> true) [ 0 .. 4 ]) List.empty
            equal (List.skipWhile (fun x -> x % 5 > 0) [ 0 .. 9 ]) [ 5 .. 9 ]
        }

        Test "List.sortDescending" {
            equal (List.sortDescending [ 0 .. 4 ]) [ 4 .. -1 .. 0 ]
        }

        Test "List.sortByDescending" {
            equal (List.sortByDescending (fun (x : string) -> x.Length) [ ".."; "."; "....."; "..."; "...."; ]) [ "....."; "...."; "..."; ".."; "." ]
        }

        Test "List.take" {
            raises (List.take 1 List.empty)
            equal (List.take 2 [ 0 .. 4 ]) [ 0; 1 ]
        }

        Test "List.takeWhile" {
            raises (List.takeWhile (fun _ -> true) List.empty)
            equal (List.takeWhile (fun x -> x % 5 > 0) [ 1 .. 10 ]) [ 1 .. 4 ]
        }

        Test "List.truncate" {
            equal (List.truncate 1 List.empty) List.empty
            equal (List.truncate 3 [ 0 .. 4 ]) [ 3; 4 ]
        }

        Test "List.tryFindBack" {
            equal (List.tryFindBack (fun _ -> true) List.empty) None
            equal (List.tryFindBack (fun x -> x % 5 = 0) [ 1 .. 10 ]) (Some 10)
        }

        Test "List.tryFindIndexBack" {
            equal (List.tryFindIndexBack (fun _ -> true) List.empty) None
            equal (List.tryFindIndexBack (fun x -> x % 5 = 0) [ 1 .. 10 ]) (Some 9)
        }

        Test "List.tryHead" {
            equal (List.tryHead List.empty) None
            property (fun x -> Do {
                equal (List.tryHead [ x; 0 ]) (Some x)
            })
        }

        Test "List.tryItem" {
            equal (List.tryItem 0 List.empty) None
            property (fun x -> Do {
                equal (List.tryItem 0 [ x; 1 ]) (Some x)
            })
        }

        Test "List.tryLast" {
            equal (List.tryLast List.empty) None
            property (fun x -> Do {
                equal (List.tryLast [ 0; x ]) (Some x)
            })
        }

        Test "List.unfold" {
            equal (List.unfold (fun _ -> None) 0) List.empty
            equal (List.unfold (fun x -> if x < 20 then Some (x + 1, 2 * x) else None) 1) [ 2; 3; 5; 9; 17 ]
        }

        Test "List.where" {
            equal (List.where (fun x -> x % 2 = 0) [ 0 .. 4 ]) [ 0; 2; 4 ]
            equal (List.where (fun _ -> true) List.empty) List.empty
        }

        Test "List.windowed" {
            raises (List.windowed 0 List.empty)
            equal (List.windowed 1 [ 0 .. 4 ]) [ for x in [ 0 .. 4 ] do yield [| x |] ]
        }

        Test "List.splitAt" {
            raises (List.splitAt -1 List.empty)
            equal (List.splitAt 2 [ 0 .. 4 ]) ([ 0; 1 ], [ 2; 3; 4 ])
        }

        #endif

    }

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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
    TestCategory "Array" {

        Test "Range" {
            equal [| 1; 2; 3 |]  [| 1 .. 3 |]
        }

        Test "Comprehensions" {
            let l1 = [| for x in 1 .. 10 -> x * x |]
            equalMsg l1 [| 1; 4; 9; 16; 25; 36; 49; 64; 81; 100 |] "squares"
            let vec1 = [| 1; 2; 3 |]
            let vec2 = [| 4; 5; 6 |]
            let l2 = [| for x in vec1 do for y in vec2 do yield x * y |]
            equalMsg l2 [| 4; 5; 6; 8; 10; 12; 12; 15; 18 |] "products"
            let l3 n = [| for row in 1 .. n do
                            for col in 1 .. n do
                                if (row+col) % 2 = 0 then
                                    yield (row,col) |]
            let a = [| (1, 1); (1, 3); (1, 5); (2, 2);
                        (2, 4); (3, 1); (3, 3); (3, 5);
                        (4, 2); (4, 4); (5, 1); (5, 3); (5, 5) |]
            equalMsg (l3 5) a "pairs"
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
            equalMsg fibonacci [| 2; 3; 5; 8; 13 |] "fibs"
        }

        Test "Array.append" {
            let (arr1, arr2) = ([| 1; 2; 3 |], [| 4; 5 |])
            equal (Array.append arr1 arr2) [| 1; 2; 3; 4; 5 |]
            equal arr1 [| 1; 2; 3|]
            equal arr2 [| 4; 5 |]
            equal (Array.append [| 1 |] [| |]) [| 1 |]
            property (fun x -> Do {
                equal (Array.append x [||]) x
                equal (Array.append [||] x) x
            })
        }

        Test "Array.average" {
            equal (Array.average [| 1. .. 10. |]) 5.5
        }

        Test "Array.averageBy" {
            equal (Array.averageBy (fun x -> x + 1.) [| 1. .. 10. |]) 6.5
        }

        Test "Array.blit" {
            let (arr1, arr2) = ([|1; 2; 3|], [|4; 5|])
            Array.blit arr1 1 arr2 0 2
            equal arr2 [| 2; 3 |]
            raises (Array.blit arr1 1 arr2 -1 2)
        }

        Test "Array.choose" {
            let res = Array.choose (fun x -> x) [| None; None; Some 4; None |]
            equal res [| 4 |]
        }

        Test "Array.collect" {
            equal (Array.collect (fun x -> [| x |]) [| 1..10 |]) [|1..10|]
            property (fun x -> Do {
                equal (Array.collect (fun x -> [| x |]) x) x
            })
        }

        Test "Array.concat" {
            equal (Array.concat [| [| 1 |]; [||]; [| 2; 3 |] |]) [| 1; 2; 3 |]
        }

        Property "Array.copy" (fun x -> Do {
            equal (Array.copy x) x
        })

        Test "Array.create" {
            equal (Array.create 5 "Value") [| for i in 1 .. 5 -> "Value" |]
            propertyWith (R.Tuple2Of (R.Natural, R.Int)) (fun (size, elem) -> Do {
                let a = Array.create size elem
                equal a.Length size
                isTrue (Array.forall ((=) elem) a)
            })
        }

        Test "Array.empty" {
            equal Array.empty [||]
        }

        Test "Array.exists" {
            let a = [| 0; 2; 4; 8; 10 |]
            isTrue (not (Array.exists (fun x -> x % 2 = 1) a))
            isTrue (not (Array.exists (fun _ -> true) [||]))
            isTrue (Array.exists ((=) 8) a)
        }

        Test "Array.exists2" {
            isTrue (not (Array.exists2 (fun x y -> ((x + y) % 2 = 0))
                        [| 1 .. 5 |] [| 6 .. 10 |]))
            isTrue (not (Array.exists2 (fun _ _ -> true)
                        [||] [||]))
            let isSeven x = x = 7
            isTrue (Array.exists2 (fun x y -> (isSeven x) || (isSeven y))
                        [| 1 .. 5 |] [| 6 .. 10 |])
            raises (Array.exists2 (fun _ _ -> true) [||] [|1|])
        }

        Test "Array.fill" {
            let arr = Array.copy [| 1; 1; 1; 1; 1 |]
            Array.fill arr 1 3 3
            equal arr [| 1; 3; 3; 3; 1 |]
            raises (Array.fill arr 10 3 3)
        }

        Test "Array.filter" {
            equal (Array.filter ((fun x -> x % 2) >> ((=) 1)) [| 1 .. 5 |])
                [| 1; 3; 5 |]
        }

        Test "Array.find" {
            let finder x = if x = 4 then Some x else None
            equal (Array.tryPick finder [| 4 |]) (Some 4)
            equal (Array.tryPick finder [| 3 |]) None
            equal (Array.find (Option.isSome) [| None; None; Some 4; None |]) (Some 4)
            raises (Array.find ((=) (Some 5)) [| None; None; Some 4; None |])
        }

        Test "Array.findIndex" {
            equal (Array.findIndex (Option.isSome) [| None; None; Some 4; None |]) 2
            raises (Array.findIndex ((=) (Some 5)) [| None; None; Some 4; None |])
        }

        Test "Array.fold" {
            equal (Array.fold (+) 0 [| 1; 1; 1; 1; 1 |]) 5
            equal (Array.fold (fun x y -> x + List.length y) 0 [| [2; 1]; [1; 2] |]) 4
        }

        Test "Array.foldBack" {
            equal (Array.foldBack (+) [| 1; 1; 1; 1; 1 |] 0) 5
            equal (Array.foldBack (fun y x -> x + List.length y)
                    [| [2; 1]; [1; 2] |] 0) 4
            property (fun x -> Do {
                let left = Array.fold (+) 0 x
                let right = Array.foldBack (+) x 0
                equal left right
            })
        }

        Test "Array.fold2" {
            equal (Array.fold2 (fun x y z -> x + (List.length y) + List.head z) 0
                    [| [2; 1]; [1; 2] |] [| [1]; [2] |]) 7
            raises (Array.fold2 (fun _ _ _ -> true) true [||] [|1|])
        }

        Test "Array.foldBack2" {
            let arr2 = [| [1]; [2] |]
            equal (Array.foldBack2
                    (fun y z x -> x + List.length y + List.head z)
                    [| [2; 1]; [1; 2] |]
                    arr2
                    0) 7
            raises (Array.foldBack2 (fun _ _ _ -> true) [||] [|1|] true)
        }

        Test "Array.forall" {
            let a = [| 0; 2; 4; 8; 10 |]
            isTrue (Array.forall (fun x -> x % 2 = 0) a)
            isTrue (Array.forall (fun _ -> false) [||])
            isTrue (not (Array.forall ((=) 8) a))
            property (fun (f, l) -> Do {
                let p1 = Array.forall ((=) f) l
                let p2 = Array.exists ((<>) f) l |> not
                equal p1 p2
            })
        }

        Test "Array.forall2" {
            isTrue (Array.forall2 (fun x y -> ((x + y) % 2 = 1))
                        [| 1 .. 5 |] [| 6 .. 10 |])
            isTrue (Array.forall2 (fun _ _ -> false) [||] [||])
            let isSeven x = x = 7
            isTrue (not (Array.forall2 (fun x y -> (isSeven x) || (isSeven y))
                        [| 1 .. 5 |] [| 6 .. 10 |]))
            raises (Array.forall2 (fun _ _ -> true) [||] [|1|])
        }

        Test "Array.get & Array.set" {
            let size = 6
            let arr = [| 0 .. size - 1 |]
            forEach { 0 .. size - 1 } (fun n -> Do {
                equalMsg (Array.get arr n) n ("get " + string n)
            })
            equalMsg arr.[3] 3 "before set"
            arr.[3] <- 5
            equalMsg arr.[3] 5 "after set"
            do for n = 0 to size - 1 do
                Array.set arr n 8
            equalMsg (Array.create size 8) arr "all set"
            raises arr.[-1]
            raises arr.[size]
        }

        Test "Array.init" {
            equal (Array.init 0 (fun _ -> 0)) [||]
            equal (Array.init 5 (( * ) 2)) [| 0; 2; 4; 6; 8 |]
        }

        Test "Array.iter" {
            let cell = [| 0 |]
            let incrBy x = cell.[0] <- cell.[0] + x
            Array.iter incrBy [| 5; 10 |]
            equal cell.[0] 15
        }

        Test "Array.iteri" {
            let cell = [| 0 |]
            let array = [| 0; 1; 2; 3; 4 |]
            let foo i x = array.[i] <- array.[(i + 1) % 5] + x
            Array.iteri foo array
            equal array [| 1; 3; 5; 7; 5 |]
        }

        Test "Array.iter2" {
            let cell = [| 0 |]
            let incrBy x = cell.[0] <- cell.[0] + x
            Array.iter2 (fun x y -> incrBy (List.length x + y))
                [| [1; 2]; []; [1; 2; 3] |]
                [| 1; 2; 3 |]
            equal cell.[0] 11
            raises (Array.iter2 (fun _ _ -> ()) [||] [| 1 |])
        }

        Test "Array.iteri2" {
            let cell = [| 0 |]
            let incrBy x = cell.[0] <- cell.[0] + x
            let d = [| [1; 2]; []; [1; 2; 3] |]
            raises (Array.iteri2 (fun _ _ _ -> ()) [||] [|1|])
            Array.iteri2 (fun idx x y -> (incrBy (List.length x + y + idx)))
                d [| 1 .. 3 |]
            equal cell.[0] 14
        }

        Test "Array.isEmpty" {
            isTrue (Array.isEmpty [||])
            isTrue (not (Array.isEmpty [| 1 |]))
        }

        Test "Array.length" {
            equal (Array.length [||]) 0
            equal (Array.length [| 1 .. 10 |]) 10
            equal (Array.length (Array.zeroCreate 5)) 5
            property (fun (x, y) -> Do {
                equal (Array.length x + Array.length y) (Array.length (Array.append x y))
            })
        }

        Test "Array.map" {
            equal (Array.map (fun x -> x % 2) [| 1 .. 10 |])
                [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0 |]
            let funcs = [| (+) 1; ( * ) 2; (fun x -> x); (fun x -> x * x)|]
            let rA  = R.ArrayOf R.Int
            let rF = R.OneOf funcs
            propertyWith (R.Tuple3Of (rA, rF, rF)) (fun (arr, f1, f2) -> Do {
                let map1 =
                    arr
                    |> Array.map f1
                    |> Array.map f2
                let map2 =
                    arr
                    |> Array.map (f1 >> f2)
                equal map1 map2
            })
        }

        Test "Array.mapi" {
            let array = [| 0; 1; 2; 3; 4 |]
            let foo i x = array.[(i + 1) % 5] + x
            equal (Array.mapi foo array) [| 1; 3; 5; 7; 4 |]
        }

        Test "Array.map2" {
            let a1 = [| [1; 2]; []; [1; 2; 3] |]
            let a2 = [| 1; 2; 3 |]
            equal (Array.map2 (fun x y -> List.length x + y) a1 a2)
                [| 3; 2; 6 |]
        }

        Test "Array.mapi2" {
            let a1 = [| [1; 2]; []; [1; 2; 3] |]
            let a2 = [| 1; 2; 3 |]
            equal (Array.mapi2 (fun idx x y -> (idx + List.length x + y)) a1 a2)
                [| 3; 3; 8 |]
        }

        Test "Array.max" {
            equal (Array.max [| 1; 2; 3; 2; 1 |]) 3
        }

        Test "Array.maxBy" {
            equal (Array.maxBy id [| 1; 2; 3; 2; 1 |]) 3
        }

        Test "Array.min" {
            equal (Array.min [| 4; 5; 3; 5; 4 |]) 3
        }

        Test "Array.minBy" {
            equal (Array.minBy id [| 4; 5; 3; 5; 4 |]) 3
        }

        Test "Array.ofList" {
            equal (Array.ofList [1;2;3]) [|1; 2; 3|]
        }

        Test "Array.ofSeq" {
            equal (Array.ofSeq [1;2;3]) [|1; 2; 3|]
        }

        Test "Array.partition" {
            let (even, odd) =
                [| 1 .. 5 |]
                |> Array.partition (fun x -> x % 2 = 0)
            equal even [| 2; 4 |]
            equal odd [| 1; 3; 5|]
            let funcs = [| (=) 3; (fun x -> x % 2 = 0) |]
            propertyWith (R.Tuple2Of (R.ArrayOf R.Int, R.OneOf funcs)) (fun (a, f) -> Do {
                let (x, y) = Array.partition f a
                equal (Array.length (Array.append x y)) (Array.length x + Array.length y)
            })
        }

        Test "Array.permute" {
            equal (Array.permute (fun x -> (x + 3) % 5) [| 1 ; 2; 3 ; 4; 5 |])
                [| 3; 4; 5; 1; 2 |]
        }

        Test "Array.pick" {
            let finder x = if x = 4 then Some x else None
            equal (Array.pick finder [| 4 |]) 4
        }

        Test "Array.reduce" {
            equal (Array.reduce (+) [| 1; 1; 1; 1; 1 |]) 5
            equal (Array.reduce (+) [| 3 |]) 3
        }

        Test "Array.reduceBack" {
            equal (Array.reduceBack (+) [| 1; 1; 1; 1; 1 |]) 5
            equal (Array.reduceBack (+) [| 3 |]) 3
            property (fun a -> Do {
                isTrue (Array.isEmpty a || (Array.reduce (+) a = Array.reduceBack (+) a))
            })
        }

        Test "Array.rev" {
            property (fun a -> Do {
                equal (Array.rev (Array.rev a)) a
            })
            equal (Array.rev [||]) [||]
            property (fun x -> Do { equal [| x |] (Array.rev [| x |]) })
        }

        Test "Array.scan" {
            equal (Array.scan (fun x y -> x + List.length y ) 0 [| [2; 1]; [1; 2; 3] |])
                [| 0; 2; 5 |]
        }

        Test "Array.scanBack" {
            equal (Array.scanBack (fun y x -> x + List.length y) [| [2; 1]; [1; 2; 3] |] 0)
                [| 5; 3; 0 |]
        }

        Test "Array.sort" {
            let a = [| 1; 8; 4 |]
            equal (Array.sort a) [| 1; 4; 8 |]
            equal a [| 1; 8; 4 |]
        }

        Test "Array.sortBy" {
            let a = [| 1; 0; -2 |]
            equal (Array.sortBy (fun x -> x * x) a) [| 0; 1; -2 |]
            equal a [| 1; 0; -2 |]
        }

        Test "Array.sortInPlace" {
            let a = [| 1; 4; 2 |]
            Array.sortInPlace a
            equal a [| 1; 2; 4 |]
        }

        Test "Array.sortInPlaceBy" {
            let a = [| 1; 0; -2 |]
            Array.sortInPlaceBy (fun x -> x * x) a
            equal a [| 0; 1; -2 |]
        }

        Test "Array.sortInPlaceWith" {
            let arr = [| [1; 2; 3]; []; [1; 2] |]
            let comparer (x : int list) (y : int list) =
                List.length x - List.length y
            Array.sortInPlaceWith comparer arr
            equal arr [| []; [1; 2]; [1; 2; 3] |]
            property (fun arr -> Do {
                let copy = Array.copy arr
                Array.sortInPlaceWith comparer copy
                equal (Array.sortWith comparer arr) copy
            })
        }

        Test "Array.sortWith" {
            let a = [| 1; 0; -2 |]
            let b = Array.sortWith (fun x y -> compare (abs x) (abs y)) a
            equal b [| 0; 1; -2 |]
            notEqual b a
        }

        Test "Array.sub" {
            equal (Array.sub [| 1 .. 10 |] 5 5) [| 6 .. 10 |]
        }

        Test "Array.sum" {
            equal (Array.sum<int> [| |]) 0
            equal (Array.sum<float> [| |]) 0.
            equal (Array.sum [| 1 .. 4 |]) 10
            equal (Array.sum [| 1. .. 4. |]) 10.
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
            equal (Array.sumBy getInt oneToFourInt) 10
            equal (Array.sumBy getFloat oneToFourFloat) 10.
            equal (Array.sumBy getInt [||]) 0
        }

        Test "Array.toList" {
            let arr = [| 1; 2; 3; 4; 5 |]
            let l = [1; 2; 3; 4; 5]
            equal l (Array.toList arr)
            property (fun x -> Do {
                equal (Array.toList (Array.ofList x)) x
            })
        }

        Test "Array.toSeq" {
            equal (Array.ofSeq (Array.toSeq [| 1; 2; 3 |])) [| 1; 2; 3 |]
        }

        Test "Array.tryFind" {
            let finder elem =
                match elem with
                | Some x -> x = 4
                | _      -> false
            equal (Array.tryFind finder [| None; None; Some 4; None |]) (Some (Some 4))
            equal (Array.tryFind (fun _ -> false) [| None; None; Some 4; None |]) None
        }

        Test "Array.tryFindIndex" {
            equal (Array.tryFindIndex (fun x -> x = 5) [| 1 .. 10 |]) (Some 4)
            equal (Array.tryFindIndex (fun x -> x = 5) [| 1 .. 3 |]) None
        }

        Test "Array.tryPick" {
            let f x = if x = 5 then Some (x + 1) else None
            equal (Array.tryPick f [| 1 .. 10 |]) (Some 6)
            equal (Array.tryPick f [| 1 .. 3 |]) None
        }

        Property "Array.unzip" (fun arr -> Do {
            let (x, y) = Array.unzip arr
            equal (Array.zip x y) arr
        })

        Property "Array.unzip3" (fun arr -> Do {
            let (x, y, z) = Array.unzip3 arr
            equal (Array.zip3 x y z) arr
        })

        Test "Array.zeroCreate" {
            equal (Array.zeroCreate 2 : int [])
                [| Unchecked.defaultof<_>; Unchecked.defaultof<_> |]
        }

        Test "Array.zip" {
            let arr1, arr2 = [||], [| 1 |]
            raises (Array.zip arr1 arr2)
            let (arr1, arr2) = ([| "a" |], [| 1 |])
            equal (Array.zip arr1 arr2) [| ("a", 1) |]
        }

        Test "Array.zip3" {
            let (arr1, arr2, arr3) = ([||], [| 1 |], [| 1 |])
            raises (Array.zip3 arr1 arr2 arr3)
            raises (Array.zip3 arr2 arr1 arr3)
            raises (Array.zip3 arr1 arr3 arr2)
            let (arr1, arr2, arr3) = ([| "a" |], [| 1 |], [| (1, 2) |])
            equal (Array.zip3 arr1 arr2 arr3) [| ("a", 1, (1, 2)) |]
        }

        Test "GetArraySlice" {
            let arr = [| 0; 1; 2; 3; 4; 5 |]
            equal arr.[.. 2] [| 0; 1; 2 |]
            equal arr.[4 ..] [| 4 ; 5 |]
            equal arr.[2 .. 4] [| 2; 3; 4 |]
        }

        Test "Extensions" {       
            equal ([| "a"; "b"; "c" |].JS.Join()) "a,b,c"
            equal ([| "a"; "b"; "c" |].JS.Join("-")) "a-b-c"

            equal ([| 0; 1; 2; 3 |].JS.Slice(1)) [| 1; 2; 3 |]
            equal ([| 0; 1; 2; 3 |].JS.Slice(1, 3)) [| 1; 2 |]

            let a = [| 0 |]
            equal (a.JS.Push(1)) 2
            equal (a.JS.Push(2, 3)) 4
            equal a [| 0; 1; 2; 3 |]

            let b = [| 0; 1; 2; 3 |];
            equal (b.JS.Splice(1, 2, 4, 4, 4)) [| 1; 2 |]
            equal b [| 0; 4; 4; 4; 3 |]

            equal ([| 0; 1; 2 |].JS.Sort(fun (a, b) -> b - a)) [| 2; 1; 0 |]

            equal ([| 0; 1; 2 |].JS.Map(fun (x, i, arr) -> if arr = [| 0; 1; 2 |] then x + 1 + i else 0))
                [| 1; 3; 5 |]
            equal ([| 0; 1; 2 |].JS.Map((fun t (x, i, a) -> if a = [| 0; 1; 2 |] then x + t + i else 0), 1))
                [| 1; 3; 5 |]

            equal ([| 1; 2; 3 |].JS.Reduce(fun (a, b, i, arr) -> if arr = [| 1; 2; 3 |] then a + b + i else 0))
                9
            equal ([| 1; 2; 3 |].JS.Reduce((fun (a, b, i, arr) -> if arr = [| 1; 2; 3 |] then a + b + i else 0), 1))
                10
        }

        #if FSHARP40

        Test "Array.contains" {
            isTrue (Array.contains 0 [| 0 .. 4 |])
        }

        Test "Array.chunkBySize" {
            equal [| [| 1 .. 4 |]; [| 5 .. 8 |] |] (Array.chunkBySize 4 [| 1 .. 8 |])
            equal [| [| 1 .. 4 |]; [| 5 .. 8 |]; [| 9; 10 |] |] (Array.chunkBySize 4 [| 1 .. 10 |])
            raises (Array.chunkBySize 0 [||])
        }

        Test "Array.compareWith" {
            equal (Array.compareWith (fun _ -> failwith "Should not be evaluated") Array.empty Array.empty) 0
            equal (Array.compareWith (fun _ -> failwith "Should not be evaluated") Array.empty [| 1 |]) -1
            equal (Array.compareWith (fun _ _ -> 1) [| 0; 1 |] [| 0; 2 |]) 1
        }

        Test "Array.countBy" {
            equal (Array.countBy (fun _ -> failwith "Should not be evaluated") Array.empty) [||]
            equal (Array.countBy id [| 1; 2; 2; 3; 3; 3 |]) [| 1, 1; 2, 2; 3, 3 |]
        }

        Test "Array.distinct" {
            equal (Array.distinct Array.empty) [||]
            equal (Array.distinct [| 1; 2; 2; 3; 3; 3 |]) [| 1; 2; 3 |]
        }

        Test "Array.distinctBy" {
            equal (Array.distinctBy (fun _ -> failwith "Should not be evaluated") Array.empty) [||]
            equal (Array.distinctBy Array.sum [| [| 0; 1 |]; [| 1; 0 |]; [| 1; 2 |] |]) [| [| 0; 1 |]; [| 1; 2 |] |]
        }

        Test "Array.splitInto" {
            equal (Array.splitInto 2 Array.empty) Array.empty
            raises ((Array.splitInto 0) Array.empty)
        }

        Test "Array.exactlyOne" {
            equal (Array.exactlyOne [| 0 |]) 0
            raises (Array.exactlyOne [| 0; 1 |])
        }

        Test "Array.except" {
            equal (Array.except Array.empty [| 0; 1 |]) [| 0; 1 |]
            equal (Array.except [| 0 |] [| 0; 1 |]) [| 1 |]
        }

        Test "Array.findBack" {
            raises (Array.findBack (fun _ -> true) Array.empty)
            equal (Array.findBack (fun x -> x % 5 = 0) [| 1 .. 10 |]) 10
        }

        Test "Array.findIndexBack" {
            raises (Array.findIndexBack (fun _ -> true) Array.empty)
            equal (Array.findIndexBack (fun x -> x % 5 = 0) [| 1 .. 10 |]) 9
        }

        Test "Array.groupBy" {
            equal (Array.groupBy (fun (x : string) -> x.Length) [| "x"; "xx"; "xy"; "xyz" |]) [| 1, [| "x" |]; 2, [| "xx"; "xy" |]; 3, [| "xyz" |] |]
        }

        Test "Array.indexed" {
            equal (Array.indexed [| 0 .. 4 |]) (Array.zip [| 0 .. 4 |] [| 0 .. 4 |])
            equal (Array.indexed Array.empty) Array.empty
        }

        Test "Array.item" {
            property (fun x -> Do {
                let list = [| x |]

                equal (Array.item 0 list) x
            })
        }

        Test "Array.last" {
            equal (Array.last [| 0 .. 4 |]) 4
            raises (Array.last Array.empty)
        }

        Test "Array.mapFold" {
            equal (Array.mapFold (fun s x -> (x + 1, s + x)) 0 [| 0 .. 4 |]) (Array.map ((+) 1) [| 0 .. 4 |], Array.sum [| 0 .. 4 |])
        }

        Test "Array.mapFoldBack" {
            equal (Array.mapFoldBack (fun x s -> (x + 1, s + x)) [| 0 .. 4 |] 0) (Array.map ((+) 1) [| 0 .. 4 |], Array.sum [| 0 .. 4 |])
        }

        Test "Array.pairwise" {
            equal (Array.pairwise Array.empty) Array.empty
            equal (Array.pairwise [| 0 |]) Array.empty
            equal (Array.pairwise [| 0; 1 |]) [| 0, 1 |]
        }

        Test "Array.singleton" {
            equal (Array.singleton 42) [| 42 |]
//            property (fun x -> Do {
//                equal (Array.singleton x) [| x |]
//            })
        }

        Test "Array.skip" {
            raises (Array.skip 1 Array.empty)
            equal (Array.skip 3 [| 0 .. 4 |]) [| 3; 4 |]
        }

        Test "Array.skipWhile" {
            equal (Array.skipWhile (fun _ -> true) [| 0 .. 4 |]) [||]
            equal (Array.skipWhile (fun x -> x < 5) [| 0 .. 9 |]) [| 5 .. 9 |]
        }

        Test "Array.sortDescending" {
            equal (Array.sortDescending [| 0 .. 4 |]) [| 4 .. -1 .. 0 |]
        }

        Test "Array.sortByDescending" {
            equal (Array.sortByDescending (fun (x : string) -> x.Length) [| ".."; "."; "....."; "..."; "...."; |]) [| "....."; "...."; "..."; ".."; "." |]
        }

        Test "Array.take" {
            raises (Array.take 1 Array.empty)
            equal (Array.take 2 [| 0 .. 4 |]) [| 0; 1 |]
        }

        Test "Array.takeWhile" {
            equal (Array.takeWhile (fun x -> x % 5 > 0) [| 1 .. 10 |]) [| 1 .. 4 |]
        }

        Test "Array.truncate" {
            equal (Array.truncate 1 Array.empty) Array.empty
            equal (Array.truncate 3 [| 0 .. 4 |]) [| 3; 4 |]
        }

        Test "Array.tryFindBack" {
            equal (Array.tryFindBack (fun _ -> true) Array.empty) None
            equal (Array.tryFindBack (fun x -> x % 5 = 0) [| 1 .. 10 |]) (Some 10)
        }

        Test "Array.tryFindIndexBack" {
            equal (Array.tryFindIndexBack (fun _ -> true) Array.empty) None
            equal (Array.tryFindIndexBack (fun x -> x % 5 = 0) [| 1 .. 10 |]) (Some 9)
        }

        Test "Array.tryHead" {
            equal (Array.tryHead Array.empty) None
            property (fun x -> Do {
                equal (Array.tryHead [| x; 0 |]) (Some x)
            })
        }

        Test "Array.tryItem" {
            equal (Array.tryItem 0 Array.empty) None
            property (fun x -> Do {
                equal (Array.tryItem 0 [| x; 1 |]) (Some x)
            })
        }

        Test "Array.tryLast" {
            equal (Array.tryLast Array.empty) None
            property (fun x -> Do {
                equal (Array.tryLast [| 0; x |]) (Some x)
            })
        }

        Test "Array.unfold" {
            equal (Array.unfold (fun _ -> None) 0) Array.empty
            equal (Array.unfold (fun x -> if x < 20 then Some (x + 1, 2 * x) else None) 1) [| 2; 3; 5; 9; 17 |]
        }

        Test "Array.where" {
            equal (Array.where (fun x -> x % 2 = 0) [| 0 .. 4 |]) [| 0; 2; 4 |]
            equal (Array.where (fun _ -> true) Array.empty) Array.empty
        }

        Test "Array.windowed" {
            raises (Array.windowed 0 [||])
            equal (Array.windowed 1 [| 0 .. 4 |]) [|[|0|]; [|1|]; [|2|]; [|3|]; [|4|]|]
            equal (Array.windowed 2 [| 0 .. 4 |]) [|[|0; 1|]; [|1; 2|]; [|2; 3|]; [|3; 4|]|]
            equal (Array.windowed 5 [| 0 .. 4 |]) [|[|0; 1; 2; 3; 4|]|]
            equal (Array.windowed 6 [| 0 .. 4 |]) [||]
        }

        Test "Array.splitAt" {
            raises (Array.splitAt -1 Array.empty)
            equal (Array.splitAt 2 [| 0 .. 4 |]) ([| 0; 1 |], [| 2; 3; 4 |])
        }

        Test "Array.head" {
            equal (Array.head [| 0; 1 |]) 0
            raises (Array.head Array.empty)
        }

        Test "Array.map3" {
            raises (Array.map3 (fun a b c -> ()) [| 0; 1 |] [| 0 .. 2 |] [| 0 .. 2 |])
            equal (Array.map3 id [||] [||] [||]) Array.empty
            equal (Array.map3 (fun x y z -> x + y + z) [| 0; 1 |] [| 2; 3 |] [| 4; 5 |]) [| 6; 9 |]
        }

        Test "Array.replicate" {
            raises (Array.replicate -1 Array.empty)
            equal (Array.replicate 100 0) [| for _ in [ 1 .. 100 ] do yield 0 |]
        }

        Test "Array.tail" {
            raises (Array.tail Array.empty)
            equal (Array.tail [| 1 .. 3|]) [| 2; 3 |]
        }

        #endif

    }

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

module WebSharper.Collections.Tests.Set

open System
open WebSharper
open WebSharper.Testing

[<JavaScript>]
let RandomInts = Random.ListOf Random.Int

[<JavaScript>]
let SevenS = [1; 2; 3; 4; 5; 6; 7]

[<JavaScript>]
let DoubleSS = [1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7]

[<JavaScript>]
let SevenDS = [1; 2; 3; 4; 5; 6; 7; 7]

[<JavaScript>]
let FTeenS = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14]

[<JavaScript>]
let EightFTeenS = [8; 9; 10; 11; 12; 13; 14]

[<JavaScript>]
let Tests =

    Section "Set" {

        Test "Empty" {
            isTrue (Set.isEmpty Set.empty)
        }

        Test "Singleton" {
            let s = Set.singleton 1
            isFalse (Set.isEmpty s)
            equal (Set.count s) 1
            isTrue (Set.contains 1 s)
            isFalse (Set.contains 2 s)
        }

        Test "ToList" {
            equal (Set.toList Set.empty) []
            equal (Set.toList (Set.singleton 1)) [1]
        }

        Test "Equality" {
            equal Set.empty Set.empty
            let s1 = Set.singleton 1
            let s2 = Set.singleton 1
            equal s1 s2
        }

        Test "NoDuplicates" {
            let t (x: list<int>) = Set.toArray (Set.ofList x)
            equal (t SevenDS) (t SevenS)
            equal (t SevenS) (t DoubleSS)
            forRandom 100 (Random.ListOf Random.Int) (fun x -> Do {
                equal (t x) (Seq.toArray (Seq.sort (Seq.distinct x)))
            })
        }

        Test "Set.add" {
            let set3 = Set.ofList DoubleSS
            let set1 = Set.ofList SevenDS
            let set2 = Set.ofList SevenS
            equal set1 set2
            equal set2 set3
            isFalse (Set.isEmpty (Set.add 1 Set.empty))
            equal set2 (Set.add 1 set2)
            notEqual set2 (Set.add 8 set2)
        }

        Test "Set.contains" {
            let set = Set.ofList SevenS
            isTrue (Set.contains 1 set)
            isFalse (Set.contains 9 set)
            forRandom 100 (RandomInts) (fun x -> Do {
                isTrue (Set.contains 6 (Set.add 6 (Set.ofList x)))
            })
        }

        Test "Set.count" {
            let set3 = Set.ofList DoubleSS
            let set1 = Set.ofList SevenDS
            let set2 = Set.ofList SevenS
            equal (Set.count set1) (Set.count set2)
            equal (Set.count Set.empty) 0
        }

        Test "Set.difference" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList EightFTeenS
            equal (Set.difference set1 set1) Set.empty
            equal (Set.difference set1 set2) set1
        }

        Test "Set.exists" {
            let set = Set.ofList SevenS
            isFalse (Set.exists (fun x -> x = 8) set)
            isFalse (Set.exists (fun _ -> true) Set.empty)
            isTrue (Set.exists (fun x -> x = 1) set)
        }

        Test "Set.filter" {
            let s1 = Set.ofList ([1..10] @ [1..10])
            let sLow = Set.ofList [1..5]
            let sHigh = Set.ofList [6..10]
            let sLow_ = Set.filter (fun x -> x <= 5) s1
            let sHigh_ = Set.filter (fun x -> x > 5) s1
            equal (Set.toArray sLow) (Set.toArray sLow_)
            equal (Set.toArray sHigh) (Set.toArray sHigh_)
        }

        Test "Set.fold" {
            let xs = [1..10]
            let set = Set.ofList xs
            let sum = List.fold (+) 10 xs
            let sum_ = Set.fold (+) 10 (Set.ofList xs)
            equal sum sum_
        }

        Test "Set.foldBack" {
            let r1 = List.foldBack (fun x y -> x - y) [1; 2] (-1)
            let r2 = Set.foldBack (fun x y -> x - y) (Set.ofList [1; 2]) (-1)
            equal r1 r2
        }

        Test "Set.fold consistency" {
            forRandom 100 RandomInts (fun x -> Do {
                equal (Set.fold (+) 0 (Set.ofList x))
                    (Set.foldBack (+) (Set.ofList x) 0)
            })
        }

        Test "Set.forall" {
            let set = Set.ofList SevenS
            let f x = x/10<10
            isFalse (Set.forall (fun x -> x = 6) set)
            isFalse (Set.forall (fun x -> x = 13) set)
            isTrue (Set.forall f set)
            isTrue (Set.forall f Set.empty)
        }

        Test "Set.intersect" {
            let set1 = Set.ofList [1..10]
            let set2 = Set.ofList [5..20]
            equal (Set.toArray <| Set.intersect set1 set2) [|5..10|]
            equal (Set.toArray <| Set.intersect set1 (Set.ofList [11..20])) [||]
        }

        Test "Set.intersectMany" {
            let set0 = Set.ofList [1..3]
            let set1 = Set.ofList [3..6]
            let set2 = Set.ofList [3..9]
            let seqOfSet = seq [ set0; set1; set2 ]
            equal (Set.intersectMany seqOfSet |> Set.toArray) [|3|]
        }

        Test "Set.isEmpty" {
            isTrue (Set.isEmpty Set.empty)
            isFalse (Set.isEmpty (Set.add 1 Set.empty))
            isTrue (Set.isEmpty (Set.remove 1 <| Set.add 1 Set.empty))
        }

        Test "Set.isProperSubset" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList [1; 2; 3]
            isFalse (Set.isProperSubset set1 set1)
            isTrue (Set.isProperSubset set2 set1)
            isFalse (Set.isProperSubset set1 set1)
        }

        Test "Set.isSubset" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList [1; 2; 3]
            isTrue (Set.isSubset set2 set1)
            isTrue (Set.isSubset set1 set1)
            isFalse (Set.isSubset set1 set2)
        }

        Test "Set.isProperSuperset" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList [1; 2; 3]
            isFalse (Set.isProperSuperset set1 set1)
            isTrue (Set.isProperSuperset set1 set2)
            isFalse (Set.isProperSuperset set1 set1)
        }

        Test "Set.isSuperset" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList [1; 2; 3]
            isTrue (Set.isSuperset set1 set1)
            isTrue (Set.isSuperset set1 set2)
            isFalse (Set.isSuperset set2 set1)
        }

        Test "Set.map" {
            let set = Set.ofList SevenS
            let sol = Set.ofList [2; 3; 4; 5; 6; 7; 8]
            equal (Set.map ((+) 1) set) sol
        }

        Test "Set.maxElement" {
            let set = Set.ofList SevenS
            equal (Set.maxElement set) 7
            raises (Set.maxElement Set.empty)
        }

        Test "Set.minElement" {
            let set = Set.ofList SevenS
            equal (Set.minElement set) 1
            raises (Set.minElement Set.empty)
        }

        Test "Set.partition" {
            let set = Set.ofList FTeenS
            let f x = x % 2 = 0
            let sol1 = Set.ofList [2; 4; 6; 8; 10; 12; 14]
            let sol2 = Set.ofList [1; 3; 5; 7; 9; 11; 13]
            equal (Set.partition f set) (sol1, sol2)
            equal (Set.partition id Set.empty) (Set.empty, Set.empty)
        }

        Test "Set.remove" {
            let emp = Set.remove 1 <| Set.add 1 Set.empty
            isTrue (Set.isEmpty emp)
            equal (Set.remove 4 emp) Set.empty
            let s2 = Set.ofSeq [1;2]
            let s1 = Set.remove 1 s2
            equal s1.Count 1
            let s1 = Set.remove 100 s1
            equal s1.Count 1
            let s0 = Set.remove 2 s1
            equal s0.Count 0
        }

        Test "Set.union" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList EightFTeenS
            let set3 = Set.ofList FTeenS
            equal (Set.union set1 set1) set1
            equal (Set.union set1 set2) set3
            forRandom 100 RandomInts (fun x -> Do {
                let s = Set.ofList x
                equal (Set.union s s) s
            })
        }

        Test "Set.unionMany" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList EightFTeenS
            let set3 = Set.ofList FTeenS
            let seqOfSet = seq [ set1; set2; set3 ]
            equal (Set.unionMany seqOfSet) set3
        }

    }
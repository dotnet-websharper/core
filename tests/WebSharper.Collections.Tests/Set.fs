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
            True (Set.isEmpty Set.empty)
        }

        Test "Singleton" {
            let s = Set.singleton 1
            False (Set.isEmpty s)
            Equal (Set.count s) 1
            True (Set.contains 1 s)
        }

        Test "ToList" {
            Equal (Set.toList Set.empty) []
            Equal (Set.toList (Set.singleton 1)) [1]
        }

        Test "Equality" {
            Equal Set.empty Set.empty
            let s1 = Set.singleton 1
            let s2 = Set.singleton 1
            Equal s1 s2
        }

        Test "NoDuplicates" {
            let t (x: list<int>) = Set.toArray (Set.ofList x)
            Equal (t SevenDS) (t SevenS)
            Equal (t SevenS) (t DoubleSS)
            ForR 100 (Random.ListOf Random.Int) (fun x -> Do {
                Equal (t x) (Seq.toArray (Seq.sort (Seq.distinct x)))
            })
        }

        Test "Set.add" {
            let set3 = Set.ofList DoubleSS
            let set1 = Set.ofList SevenDS
            let set2 = Set.ofList SevenS
            Equal set1 set2
            Equal set2 set3
            False (Set.isEmpty (Set.add 1 Set.empty))
            Equal set2 (Set.add 1 set2)
            NotEqual set2 (Set.add 8 set2)
        }

        Test "Set.contains" {
            let set = Set.ofList SevenS
            True (Set.contains 1 set)
            False (Set.contains 9 set)
            ForR 100 (RandomInts) (fun x -> Do {
                True (Set.contains 6 (Set.add 6 (Set.ofList x)))
            })
        }

        Test "Set.count" {
            let set3 = Set.ofList DoubleSS
            let set1 = Set.ofList SevenDS
            let set2 = Set.ofList SevenS
            Equal (Set.count set1) (Set.count set2)
            Equal (Set.count Set.empty) 0
        }

        Test "Set.difference" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList EightFTeenS
            Equal (Set.difference set1 set1) Set.empty
            Equal (Set.difference set1 set2) set1
        }

        Test "Set.exists" {
            let set = Set.ofList SevenS
            False (Set.exists (fun x -> x = 8) set)
            False (Set.exists (fun _ -> true) Set.empty)
            True (Set.exists (fun x -> x = 1) set)
        }

        Test "Set.filter" {
            let s1 = Set.ofList ([1..10] @ [1..10])
            let sLow = Set.ofList [1..5]
            let sHigh = Set.ofList [6..10]
            let sLow_ = Set.filter (fun x -> x <= 5) s1
            let sHigh_ = Set.filter (fun x -> x > 5) s1
            Equal (Set.toArray sLow) (Set.toArray sLow_)
            Equal (Set.toArray sHigh) (Set.toArray sHigh_)
        }

        Test "Set.fold" {
            let xs = [1..10]
            let set = Set.ofList xs
            let sum = List.fold (+) 10 xs
            let sum_ = Set.fold (+) 10 (Set.ofList xs)
            Equal sum sum_
        }

        Test "Set.foldBack" {
            let r1 = List.foldBack (fun x y -> x - y) [1; 2] (-1)
            let r2 = Set.foldBack (fun x y -> x - y) (Set.ofList [1; 2]) (-1)
            Equal r1 r2
        }

        Test "Set.fold consistency" {
            ForR 100 RandomInts (fun x -> Do {
                Equal (Set.fold (+) 0 (Set.ofList x))
                    (Set.foldBack (+) (Set.ofList x) 0)
            })
        }

        Test "Set.forall" {
            let set = Set.ofList SevenS
            let f x = x/10<10
            False (Set.forall (fun x -> x = 6) set)
            False (Set.forall (fun x -> x = 13) set)
            True (Set.forall f set)
            True (Set.forall f Set.empty)
        }

        Test "Set.intersect" {
            let set1 = Set.ofList [1..10]
            let set2 = Set.ofList [5..20]
            Equal (Set.toArray <| Set.intersect set1 set2) [|5..10|]
            Equal (Set.toArray <| Set.intersect set1 (Set.ofList [11..20])) [||]
        }

        Test "Set.intersectMany" {
            let set0 = Set.ofList [1..3]
            let set1 = Set.ofList [3..6]
            let set2 = Set.ofList [3..9]
            let seqOfSet = seq [ set0; set1; set2 ]
            Equal (Set.intersectMany seqOfSet |> Set.toArray) [|3|]
        }

        Test "Set.isEmpty" {
            True (Set.isEmpty Set.empty)
            False (Set.isEmpty (Set.add 1 Set.empty))
            True (Set.isEmpty (Set.remove 1 <| Set.add 1 Set.empty))
        }

        Test "Set.isProperSubset" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList [1; 2; 3]
            False (Set.isProperSubset set1 set1)
            True (Set.isProperSubset set2 set1)
            False (Set.isProperSubset set1 set1)
        }

        Test "Set.isSubset" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList [1; 2; 3]
            True (Set.isSubset set2 set1)
            True (Set.isSubset set1 set1)
            False (Set.isSubset set1 set2)
        }

        Test "Set.isProperSuperset" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList [1; 2; 3]
            False (Set.isProperSuperset set1 set1)
            True (Set.isProperSuperset set1 set2)
            False (Set.isProperSuperset set1 set1)
        }

        Test "Set.isSuperset" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList [1; 2; 3]
            True (Set.isSuperset set1 set1)
            True (Set.isSuperset set1 set2)
            False (Set.isSuperset set2 set1)
        }

        Test "Set.map" {
            let set = Set.ofList SevenS
            let sol = Set.ofList [2; 3; 4; 5; 6; 7; 8]
            Equal (Set.map ((+) 1) set) sol
        }

        Test "Set.maxElement" {
            let set = Set.ofList SevenS
            Equal (Set.maxElement set) 7
            Raises (Set.maxElement Set.empty)
        }

        Test "Set.minElement" {
            let set = Set.ofList SevenS
            Equal (Set.minElement set) 1
            Raises (Set.minElement Set.empty)
        }

        Test "Set.partition" {
            let set = Set.ofList FTeenS
            let f x = x % 2 = 0
            let sol1 = Set.ofList [2; 4; 6; 8; 10; 12; 14]
            let sol2 = Set.ofList [1; 3; 5; 7; 9; 11; 13]
            Equal (Set.partition f set) (sol1, sol2)
            Equal (Set.partition id Set.empty) (Set.empty, Set.empty)
        }

        Test "Set.remove" {
            let emp = Set.remove 1 <| Set.add 1 Set.empty
            True (Set.isEmpty emp)
            Equal (Set.remove 4 emp) Set.empty
            let s2 = Set.ofSeq [1;2]
            let s1 = Set.remove 1 s2
            Equal s1.Count 1
            let s1 = Set.remove 100 s1
            Equal s1.Count 1
            let s0 = Set.remove 2 s1
            Equal s0.Count 0
        }

        Test "Set.union" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList EightFTeenS
            let set3 = Set.ofList FTeenS
            Equal (Set.union set1 set1) set1
            Equal (Set.union set1 set2) set3
            ForR 100 RandomInts (fun x -> Do {
                let s = Set.ofList x
                Equal (Set.union s s) s
            })
        }

        Test "Set.unionMany" {
            let set1 = Set.ofList SevenS
            let set2 = Set.ofList EightFTeenS
            let set3 = Set.ofList FTeenS
            let seqOfSet = seq [ set1; set2; set3 ]
            Equal (Set.unionMany seqOfSet) set3
        }

    }
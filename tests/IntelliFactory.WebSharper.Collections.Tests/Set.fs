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

module IntelliFactory.WebSharper.Collections.Tests.Set

open System
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Testing

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

    Section "Set"

    Test "Empty" {
        Assert.True (Set.isEmpty Set.empty)
    }

    Test "Singleton" {
        let s = Set.singleton 1
        Assert.False (Set.isEmpty s)
        Assert.Equals 1 (Set.count s)
        Assert.True (Set.contains 1 s)
    }

    Test "ToList" {
        Assert.Equals [] (Set.toList Set.empty)
        Assert.Equals [1] (Set.toList (Set.singleton 1))
    }

    Test "Equality" {
        Assert.Equals Set.empty Set.empty
        let s1 = Set.singleton 1
        let s2 = Set.singleton 1
        Assert.Equals s1 s2
    }

    Test "NoDuplicates" {
        let t (x: list<int>) = Set.toArray (Set.ofList x)
        Assert.Equals (t SevenDS) (t SevenS)
        Assert.Equals (t SevenS) (t DoubleSS)
        Assert.For 100 (Random.ListOf Random.Int) (fun x ->
            t x =? Seq.toArray (Seq.sort (Seq.distinct x)))
    }

    Test "Set.add" {
        let set3 = Set.ofList DoubleSS
        let set1 = Set.ofList SevenDS
        let set2 = Set.ofList SevenS
        Assert.Equals set1 set2
        Assert.Equals set2 set3
        Assert.False (Set.isEmpty (Set.add 1 Set.empty))
        Assert.Equals set2 (Set.add 1 set2)
        Assert.NotEquals set2 (Set.add 8 set2)
    }

    Test "Set.contains" {
        let set = Set.ofList SevenS
        Assert.Equals true <| Set.contains 1 set
        Assert.Equals false <| Set.contains 9 set
        Assert.For 100 (RandomInts) (fun x ->
            Set.contains 6 (Set.add 6 (Set.ofList x)) =? true)
    }

    Test "Set.count" {
        let set3 = Set.ofList DoubleSS
        let set1 = Set.ofList SevenDS
        let set2 = Set.ofList SevenS
        Assert.Equals (Set.count set1) <| Set.count set2
        Assert.Equals 0 <| Set.count Set.empty
    }

    Test "Set.difference" {
        let set1 = Set.ofList SevenS
        let set2 = Set.ofList EightFTeenS
        Assert.Equals Set.empty <| Set.difference set1 set1
        Assert.Equals set1 <| Set.difference set1 set2
    }

    Test "Set.exists" {
        let set = Set.ofList SevenS
        Assert.True (not <| Set.exists (fun x -> x = 8) set)
        Assert.True (not <| Set.exists (fun _ -> true) Set.empty)
        Assert.True <| Set.exists (fun x -> x = 1) set
    }

    Test "Set.filter" {
        let s1 = Set.ofList ([1..10] @ [1..10])
        let sLow = Set.ofList [1..5]
        let sHigh = Set.ofList [6..10]
        let sLow_ = Set.filter (fun x -> x <= 5) s1
        let sHigh_ = Set.filter (fun x -> x > 5) s1
        Assert.Equals (Set.toArray sLow) (Set.toArray sLow_)
        Assert.Equals (Set.toArray sHigh) (Set.toArray sHigh_)
    }

    Test "Set.fold" {
        let xs = [1..10]
        let set = Set.ofList xs
        let sum = List.fold (+) 10 xs
        let sum_ = Set.fold (+) 10 (Set.ofList xs)
        Assert.Equals sum sum_
    }

    Test "Set.foldBack" {
        let r1 = List.foldBack (fun x y -> x - y) [1; 2] (-1)
        let r2 = Set.foldBack (fun x y -> x - y) (Set.ofList [1; 2]) (-1)
        Assert.Equals r1 r2
    }

    Test "Set.fold consistency" {
        Assert.For 100 RandomInts (fun x ->
            Set.fold (+) 0 (Set.ofList x) =?
                Set.foldBack (+) (Set.ofList x) 0)
    }

    Test "Set.forall" {
        let set = Set.ofList SevenS
        let f x = x/10<10
        Assert.Equals false <| Set.forall (fun x -> x = 6) set
        Assert.Equals false <| Set.forall (fun x -> x = 13) set
        Assert.Equals true <| Set.forall f set
        Assert.Equals true <| Set.forall f Set.empty
    }

    Test "Set.intersect" {
        let set1 = Set.ofList [1..10]
        let set2 = Set.ofList [5..20]
        Assert.Equals [|5..10|]  (Set.toArray <| Set.intersect set1 set2)
        Assert.Equals [||]  (Set.toArray <| Set.intersect set1 (Set.ofList [11..20]))
    }

    Test "Set.intersectMany" {
        let set0 = Set.ofList [1..3]
        let set1 = Set.ofList [3..6]
        let set2 = Set.ofList [3..9]
        let seqOfSet = seq [ set0; set1; set2 ]
        Assert.Equals [|3|] (Set.intersectMany seqOfSet |> Set.toArray)
    }

    Test "Set.isEmpty" {
        Assert.True (Set.isEmpty Set.empty)
        Assert.False (Set.isEmpty (Set.add 1 Set.empty))
        Assert.True (Set.isEmpty (Set.remove 1 <| Set.add 1 Set.empty))
    }

    Test "Set.isProperSubset" {
        let set1 = Set.ofList SevenS
        let set2 = Set.ofList [1; 2; 3]
        Assert.False (Set.isProperSubset set1 set1)
        Assert.True (Set.isProperSubset set2 set1)
        Assert.False (Set.isProperSubset set1 set1)
    }

    Test "Set.isSubset" {
        let set1 = Set.ofList SevenS
        let set2 = Set.ofList [1; 2; 3]
        Assert.True (Set.isSubset set2 set1)
        Assert.True (Set.isSubset set1 set1)
        Assert.False (Set.isSubset set1 set2)
    }

    Test "Set.isProperSuperset" {
        let set1 = Set.ofList SevenS
        let set2 = Set.ofList [1; 2; 3]
        Assert.False (Set.isProperSuperset set1 set1)
        Assert.True (Set.isProperSuperset set1 set2)
        Assert.False (Set.isProperSuperset set1 set1)
    }

    Test "Set.isSuperset" {
        let set1 = Set.ofList SevenS
        let set2 = Set.ofList [1; 2; 3]
        Assert.True (Set.isSuperset set1 set1)
        Assert.True (Set.isSuperset set1 set2)
        Assert.False (Set.isSuperset set2 set1)
    }

    Test "Set.map" {
        let set = Set.ofList SevenS
        let sol = Set.ofList [2; 3; 4; 5; 6; 7; 8]
        Assert.Equals sol <| Set.map ((+) 1) set
    }

    Test "Set.maxElement" {
        let set = Set.ofList SevenS
        Assert.Equals 7 <| Set.maxElement set
        Assert.Raises (fun () -> Set.maxElement Set.empty)
    }

    Test "Set.minElement" {
        let set = Set.ofList SevenS
        Assert.Equals 1 <| Set.minElement set
        Assert.Raises (fun () -> Set.minElement Set.empty)
    }

    Test "Set.partition" {
        let set = Set.ofList FTeenS
        let f x = x % 2 = 0
        let sol1 = Set.ofList [2; 4; 6; 8; 10; 12; 14]
        let sol2 = Set.ofList [1; 3; 5; 7; 9; 11; 13]
        Assert.Equals (sol1, sol2) (Set.partition f set)
        Assert.Equals (Set.empty, Set.empty) <| Set.partition id Set.empty
    }

    Test "Set.remove" {
        let emp = Set.remove 1 <| Set.add 1 Set.empty
        Assert.Equals true <| Set.isEmpty emp
        Assert.Equals Set.empty <| Set.remove 4 emp
        let s2 = Set.ofSeq [1;2]
        let s1 = Set.remove 1 s2
        Assert.Equals 1 s1.Count
        let s1 = Set.remove 100 s1
        Assert.Equals 1 s1.Count
        let s0 = Set.remove 2 s1
        Assert.Equals 0 s0.Count
    }

    Test "Set.union" {
        let set1 = Set.ofList SevenS
        let set2 = Set.ofList EightFTeenS
        let set3 = Set.ofList FTeenS
        Assert.Equals set1 <| Set.union set1 set1
        Assert.Equals set3 <| Set.union set1 set2
        Assert.For 100 RandomInts (fun x ->
            let s = Set.ofList x
            Set.union s s =? s)
    }

    Test "Set.unionMany" {
        let set1 = Set.ofList SevenS
        let set2 = Set.ofList EightFTeenS
        let set3 = Set.ofList FTeenS
        let seqOfSet = seq [ set1; set2; set3 ]
        Assert.Equals set3 <| Set.unionMany seqOfSet
    }

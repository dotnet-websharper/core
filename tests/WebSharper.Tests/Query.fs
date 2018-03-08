// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

module WebSharper.Tests.Query

open System
open FSharp.Linq
open System.Linq
open FSharp.Linq.NullableOperators
open WebSharper
open WebSharper.Testing

[<JavaScript>]
let Tests =
    let s = seq { 1 .. 10 } 
    let n = Seq.singleton (Nullable()) 
    let sn = s |> Seq.map Nullable |> Seq.append n
    
    TestCategory "Query" {

        Test "all" {
            let q =
                query {
                    for x in s do
                    all (x > 0)
                }
            isTrue q 
        }

        Test "averageBy" {
            let q =
                query {
                    for x in s do
                    averageBy (float x)
                }
            equal q 5.5
        }

        Test "averageByNullable" {
            let q =
                query {
                    for x in sn do
                    averageByNullable (Nullable.float x)
                }
            equal q (Nullable 5.5)
            let q2 =
                query {
                    for x in n do
                    averageByNullable (Nullable.float x)
                }
            equal q2 (Nullable())
        }

        Test "contains" {
            let q =
                query {
                    for x in s do
                    contains 11
                }
            isFalse q
        }

        Test "where/count" {
            let q =
                query {
                    for x in s do
                    where (x % 2 = 0)
                    count
                }
            equal q 5
        }

        Test "distinct" {
            let s2 = s |> Seq.append (Seq.singleton 1) 
            let q =
                query {
                    for x in s2 do
                    distinct
                }
            equal (Array.ofSeq q) (Array.ofSeq s)
        }

        Test "take/exactlyOne" {
            let q =
                query {
                    for x in s do
                    take 1
                    exactlyOne
                }
            equal q 1
        }

        Test "exactlyOneOrDefault" {
            let q =
                query {
                    for x in Seq.empty<int> do
                    exactlyOneOrDefault
                }
            equal q 0
        }

        Test "exists" {
            let q =
                query {
                    for x in s do
                    exists (x > 9)
                }
            isTrue q 
        }

        Test "skip/find" {
            let q =
                query {
                    for x in s do
                    skip 4
                    find (x % 2 = 0)
                }
            equal q 6
        }

        Test "groupBy" {
            let q =
                query {
                    for x in s do
                    groupBy (x % 2) into g
                    select (g.Key, g.Count())
                }
            equal (Array.ofSeq q) [| 1, 5; 0, 5 |]
        }

        Test "groupJoin/select" {
            let s2 = [| "aaa"; "bb"; "cc"; "d" |]
            let q = 
                query {
                    for x in s do
                    take 4
                    groupJoin y in s2 on (x = y.Length) into g
                    select (Array.append [| string x |] (Array.ofSeq g))
                }
            equal (Array.ofSeq q) [| [|"1"; "d"|]; [|"2"; "bb"; "cc"|]; [|"3"; "aaa"|]; [|"4"|] |]
        }
        
        Test "groupValBy" {
            let q = 
                query {
                    for x in s do
                    groupValBy (x * 10) (x % 2) into g
                    select (g.Key, g.ToArray())
                }
            equal (Array.ofSeq q) [| 1, [|10; 30; 50; 70; 90|]; 0, [|20; 40; 60; 80; 100|] |]
        }

        Test "head" {
            let q = 
                query {
                    for x in s do
                    head
                }
            equal q 1
        }

        Test "headOrDefault" {
            let q =
                query {
                    for x in Seq.empty<int> do
                    headOrDefault
                }
            equal q 0
        }

        Test "join" {
            let s2 = [| "aaa"; "bb"; "cc"; "d" |]
            let q = 
                query {
                    for x in s do
                    join y in s2 on (x = y.Length)
                    select (string x + y)
                }
            equal (Array.ofSeq q) [| "1d"; "2bb"; "2cc"; "3aaa" |]
        }

        Test "last" {
            let q = 
                query {
                    for x in s do
                    last
                }
            equal q 10
        }

        Test "lastOrDefault" {
            let q =
                query {
                    for x in Seq.empty<int> do
                    lastOrDefault
                }
            equal q 0
        }

        Test "leftOuterJoin/select" {
            let s2 = [| "aaa"; "bb"; "cc"; "d" |]
            let q = 
                query {
                    for x in s do
                    take 4
                    leftOuterJoin y in s2 on (x = y.Length) into g
                    select (Array.append [| string x |] (Array.ofSeq g))
                }
            equal (Array.ofSeq q) [| [|"1"; "d"|]; [|"2"; "bb"; "cc"|]; [|"3"; "aaa"|]; [|"4"; null|] |]
        }

        Test "maxBy" {
            let q = 
                query {
                    for x in s do
                    maxBy (x % 5)
                }
            equal q 4
        }

        Test "maxByNullable" {
            let q =
                query {
                    for x in sn do
                    maxByNullable (x ?% 5)
                }
            equal q (Nullable 4)
            let q2 =
                query {
                    for x in n do
                    maxByNullable (x ?% 5)
                }
            equal q2 (Nullable())
        }

        Test "minBy" {
            let q = 
                query {
                    for x in s do
                    minBy (-x)
                }
            equal q -10
        }

        Test "minByNullable" {
            let q =
                query {
                    for x in sn do
                    minByNullable (0 -? x)
                }
            equal q (Nullable -10)
            let q2 =
                query {
                    for x in n do
                    minByNullable (0 -? x)
                }
            equal q2 (Nullable())
        }

        Test "nth" {
            let q = 
                query {
                    for x in s do
                    nth 5
                }
            equal q 6
        }

        Test "skipWhile" {
            let q =
                query {
                    for x in s do
                    skipWhile (x < 7)
                }
            equal (Array.ofSeq q) [| 7; 8; 9; 10 |]
        }

        Test "sortBy" {
            let q =
                query {
                    for x in s do
                    sortBy ((x % 3) * 10 + x)
                }
            equal (Array.ofSeq q) [|3; 6; 9; 1; 4; 7; 10; 2; 5; 8|]
        }

        Test "sortByDescending" {
            let q =
                query {
                    for x in s do
                    sortByDescending ((x % 3) * 10 + x)
                }
            equal (Array.ofSeq q) [|8; 5; 2; 10; 7; 4; 1; 9; 6; 3|]
        }

        Test "sumBy" {
            let q =
                query {
                    for x in s do
                    sumBy (x + 1)
                }
            equal q 65
        }

        Test "sumByNullable" {
            let q =
                query {
                    for x in sn do
                    sumByNullable (x ?+ 1)
                }
            equal q (Nullable 65)
            let q2 =
                query {
                    for x in n do
                    sumByNullable (x ?+ 1)
                }
            equal q2 (Nullable 0)
        }

        Test "takeWhile" {
            let q =
                query {
                    for x in s do
                    takeWhile (x < 4)
                }
            equal (Array.ofSeq q) [| 1; 2; 3 |]
        }

        Test "sortBy/thenBy" {
            let q =
                query {
                    for x in s do
                    sortBy ((x % 3))
                    thenBy (-x)
                }
            equal (Array.ofSeq q) [|9; 6; 3; 10; 7; 4; 1; 8; 5; 2|]
        }

        Test "sortBy/thenByDescending" {
            let q =
                query {
                    for x in s do
                    sortBy ((x % 3))
                    thenByDescending x
                }
            equal (Array.ofSeq q) [|9; 6; 3; 10; 7; 4; 1; 8; 5; 2|]
        }

        Test "where/yield" {
            let q =
                query {
                    for x in s do
                    where (x < 4)
                    yield x + 1
                }
            equal (Array.ofSeq q) [| 2; 3; 4|]
        }

    }

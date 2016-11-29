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

module WebSharper.Collections.Tests.HashSet

open WebSharper
open WebSharper.Testing

type HS<'T> = System.Collections.Generic.HashSet<'T>

[<JavaScript>]
let Tests =
    TestCategory "HashSet" {

        Test "Construction" {
            let s = HS [ "a"; "b"; "c"; "a" ]
            equal s.Count 3
            equal (Set.ofSeq s) (Set [ "a"; "b"; "c" ])
        }

        Test "Element operations" {
            let s = HS [ "a"; "b"; "c"; "a" ]
            isFalse (s.Add("b"))
            isTrue (s.Remove("a"))
            equal s.Count 2
            s.Clear()
            equal s.Count 0
        }

        Test "Set operations" {
            let s = HS [ 1 .. 5 ]
            s.ExceptWith(seq { 4 .. 7 })
            equal (Set.ofSeq s) (Set [ 1 .. 3 ])
            s.UnionWith(seq { 4 .. 7 })
            equal (Set.ofSeq s) (Set [ 1 .. 7 ])
            isTrue ((HS [ 1 .. 5]).IsSubsetOf(HS [ 0 .. 7 ]))
        }

        Test "GetEnumerator" {
            let s = HS [ "a"; "b"; "c"; "a" ]
//            let first = 
//                let mutable e = s.GetEnumerator()
//                e.MoveNext() |> ignore
//                e.Current
//            equal first "a"
            equal (Seq.length s) 3
            s.Remove "a" |> ignore
            equal (Seq.length s) 2
            s.Remove "c" |> ignore
            equal (Seq.length s) 1
            s.Remove "b" |> ignore
            equal (Array.ofSeq s) [||]
        }
    }

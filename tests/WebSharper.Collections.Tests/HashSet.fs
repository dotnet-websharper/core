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

module WebSharper.Tests.HashSet

open WebSharper
open WebSharper.Testing

type HS<'T> = System.Collections.Generic.HashSet<'T>

[<JavaScript>]
let Tests =
    Section "HashSet"

    Test "Construction" {
        let s = HS [ "a"; "b"; "c"; "a" ]
        s.Count =? 3
        Set.ofSeq s =? Set [ "a"; "b"; "c" ]
    }

    Test "Element operations" {
        let s = HS [ "a"; "b"; "c"; "a" ]
        s.Add("b") =? false
        s.Remove("a") =? true
        s.Count =? 2
        s.Clear()
        s.Count =? 0
    }

    Test "Set operations" {
        let s = HS [ 1 .. 5 ]
        s.ExceptWith(seq { 4 .. 7 })
        Set.ofSeq s =? Set [ 1 .. 3 ]
        s.UnionWith(seq { 4 .. 7 })
        Set.ofSeq s =? Set [ 1 .. 7 ]
        (HS [ 1 .. 5]).IsSubsetOf(HS [ 0 .. 7 ]) =? true
    }

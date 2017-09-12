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

module WebSharper.Collections.Tests.Array

open System
open WebSharper
open WebSharper.Testing
type private A = System.Array

[<JavaScript>]
let Tests =

    TestCategory "System.Array" {
        let smallInt = Random.Within -20 20
        let len = Random.Within 0 99
        let invComparer =
            { new System.Collections.Generic.IComparer<int> with
                member __.Compare(x, y) = compare y x
            }

        Test "BinarySearch (basic)" {
            propertyWith (Random.Tuple2Of (Random.ArrayOf smallInt, smallInt)) (fun (haystack, needle) -> Do {
                Array.sortInPlace haystack
                let expected =
                    match Array.tryFindIndex (fun y -> y >= needle) haystack with
                    | None -> ~~~haystack.Length
                    | Some i ->
                        if haystack.[i] = needle then i else ~~~i
                let actual = Array.BinarySearch(haystack, needle)
                equal actual expected
            })
        }

        Test "BinarySearch (comparer)" {
            propertyWith (Random.Tuple2Of (Random.ArrayOf smallInt, smallInt)) (fun (haystack, needle) -> Do {
                Array.sortInPlaceBy (~-) haystack
                let expected =
                    match Array.tryFindIndex (fun y -> y <= needle) haystack with
                    | None -> ~~~haystack.Length
                    | Some i ->
                        if haystack.[i] = needle then i else ~~~i
                let actual = Array.BinarySearch(haystack, needle, invComparer)
                equal actual expected
            })
        }

        Test "BinarySearch (bounds)" {
            propertyWith
                (Random.Tuple4Of (Random.ArrayOf smallInt, len, len, smallInt)
                |> Random.SuchThat (fun (a, s, l, x) -> s + l <= a.Length))
                (fun (haystack, start, length, needle) -> Do {
                Array.sortInPlace haystack
                let expected =
                    match Array.tryFindIndex (fun y -> y >= needle)
                            haystack.[start..start+length-1] with
                    | None -> ~~~(start+length)
                    | Some j ->
                        let i = j + start
                        if haystack.[i] = needle then i else ~~~i
                let actual = Array.BinarySearch(haystack, start, length, needle)
                equal actual expected
            })
        }

        Test "BinarySearch (bounds, comparer)" {
            propertyWith
                (Random.Tuple4Of (Random.ArrayOf smallInt, len, len, smallInt)
                |> Random.SuchThat (fun (a, s, l, x) -> s + l <= a.Length))
                (fun (haystack, start, length, needle) -> Do {
                Array.sortInPlaceBy (~-) haystack
                let expected =
                    match Array.tryFindIndex (fun y -> y <= needle)
                            haystack.[start..start+length-1] with
                    | None -> ~~~(start+length)
                    | Some j ->
                        let i = j + start
                        if haystack.[i] = needle then i else ~~~i
                let actual = Array.BinarySearch(haystack, start, length, needle, invComparer)
                equal actual expected
            })
        }

    }

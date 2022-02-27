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
open WebSharper.JavaScript
open WebSharper.Testing
type private A = System.Array

[<JavaScript>]
let Tests =

    TestCategory "System.Array" {

        Test "AsReadOnly" {
            property (fun (a: _[]) -> Do {
                let b = System.Array.AsReadOnly(a)
                forEach {0 .. a.Length - 1} (fun i -> Do {
                    equal (As<_[]> b).[i] a.[i]
                })
            })
        }

        let smallInt = RandomValues.Within -20 20
        let len = RandomValues.Within 0 99
        let invComparer =
            { new System.Collections.Generic.IComparer<int> with
                member __.Compare(x, y) = compare y x
            }

        Test "BinarySearch (basic)" {
            propertyWith (
                RandomValues.Tuple2Of (RandomValues.ArrayOf smallInt, smallInt)
            ) (fun (haystack, needle) -> Do {
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
            propertyWith (
                RandomValues.Tuple2Of (RandomValues.ArrayOf smallInt, smallInt)
            ) (fun (haystack, needle) -> Do {
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
            propertyWith (
                RandomValues.Tuple4Of (RandomValues.ArrayOf smallInt, len, len, smallInt)
                |> RandomValues.SuchThat (fun (a, s, l, x) -> s + l <= a.Length)
            ) (fun (haystack, start, length, needle) -> Do {
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
            propertyWith (
                RandomValues.Tuple4Of (RandomValues.ArrayOf smallInt, len, len, smallInt)
                |> RandomValues.SuchThat (fun (a, s, l, x) -> s + l <= a.Length)
            ) (fun (haystack, start, length, needle) -> Do {
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

        Test "Clear" {
            propertyWith (
                RandomValues.Tuple3Of (RandomValues.ArrayOf smallInt, len, len)
                |> RandomValues.SuchThat (fun (a, s, l) -> s + l <= a.Length)
            ) (fun (a, s, l) -> Do {
                let b = Array.copy a
                System.Array.Clear(b, s, l)
                forEach {0 .. s - 1} (fun i -> Do { equal b.[i] a.[i] })
                forEach {s .. s + l - 1} (fun i -> Do { equal b.[i] 0 })
                forEach {s + l .. a.Length - 1} (fun i -> Do { equal b.[i] a.[i] })
            })
        }

        Test "ConvertAll" {
            property (fun (a: int[]) -> Do {
                let b = System.Array.ConvertAll(a, fun x -> string (x + 1))
                equal a.Length b.Length
                forEach {0 .. a.Length - 1} (fun i -> Do {
                    equal b.[i] (string (a.[i] + 1))
                })
            })
        }

        Test "Empty" {
            equal (System.Array.Empty<int>()) Array.empty
        }

        Test "Exists" {
            isFalseMsg (System.Array.Exists([||], fun _ -> true)) "empty always false"
            isTrueMsg (System.Array.Exists([| 1; 2; 3 |], fun x -> x = 2)) "true"
            isFalseMsg (System.Array.Exists([| 1; 2; 3 |], fun x -> x = 4)) "false"
        }

        Test "Find" {
            equalMsg (System.Array.Find([||], fun _ -> true)) 0 "empty returns default"
            equalMsg (System.Array.Find([| 1; 3; 4 |], fun x -> x % 2 = 0)) 4 "found"
            equalMsg (System.Array.Find([| 1; 2; 3; 4 |], fun x -> x % 2 = 0)) 2 "found multiple"
            equalMsg (System.Array.Find([| 1; 3; 4 |], fun x -> x % 5 = 2)) 0 "not found"
        }

        Test "FindAll" {
            equalMsg (System.Array.FindAll([||], fun _ -> true)) [||] "empty returns empty"
            equalMsg (System.Array.FindAll([| 1; 2; 3; 4 |], fun x -> x % 2 = 0)) [| 2; 4 |] "found"
            equalMsg (System.Array.FindAll([| 1; 3; 4 |], fun x -> x % 5 = 2)) [||] "not found"
        }

        Test "FindIndex (basic)" {
            equalMsg (System.Array.FindIndex([||], fun _ -> true)) -1 "empty returns -1"
            equalMsg (System.Array.FindIndex([| 1; 3; 4 |], fun x -> x % 2 = 0)) 2 "found"
            equalMsg (System.Array.FindIndex([| 1; 2; 3; 4 |], fun x -> x % 2 = 0)) 1 "found multiple"
            equalMsg (System.Array.FindIndex([| 1; 3; 4 |], fun x -> x % 5 = 2)) -1 "not found"
        }

        Test "FindIndex (lower bound)" {
            equalMsg (System.Array.FindIndex([||], 0, fun _ -> true)) -1 "empty returns -1"
            equalMsg (System.Array.FindIndex([| 1; 3; 4 |], 1, fun x -> x % 2 = 0)) 2 "found"
            equalMsg (System.Array.FindIndex([| 0; 1; 2; 3; 4 |], 1, fun x -> x % 2 = 0)) 2 "found multiple"
            equalMsg (System.Array.FindIndex([| 1; 3; 4 |], 2, fun x -> x % 3 = 0)) -1 "not found because before lower bound"
            equalMsg (System.Array.FindIndex([| 1; 3; 4 |], 1, fun x -> x % 5 = 2)) -1 "not found anywhere"
        }

        Test "FindIndex (both bounds)" {
            equalMsg (System.Array.FindIndex([||], 0, 0, fun _ -> true)) -1 "empty returns -1"
            equalMsg (System.Array.FindIndex([| 1; 3; 4; 5 |], 1, 2, fun x -> x % 2 = 0)) 2 "found"
            equalMsg (System.Array.FindIndex([| 1; 3; 6; 7 |], 1, 2, fun x -> x % 3 = 0)) 1 "found multiple"
            equalMsg (System.Array.FindIndex([| 1; 3; 4; 5 |], 2, 2, fun x -> x % 3 = 0)) -1 "not found because before lower bound"
            equalMsg (System.Array.FindIndex([| 1; 3; 4; 5 |], 1, 2, fun x -> x % 5 = 0)) -1 "not found because after upper bound"
            equalMsg (System.Array.FindIndex([| 1; 3; 4 |], 1, 2, fun x -> x % 5 = 2)) -1 "not found anywhere"
        }

        Test "FindLast" {
            equalMsg (System.Array.FindLast([||], fun _ -> true)) 0 "empty returns default"
            equalMsg (System.Array.FindLast([| 1; 3; 4 |], fun x -> x % 2 = 0)) 4 "found"
            equalMsg (System.Array.FindLast([| 1; 2; 3; 4 |], fun x -> x % 2 = 0)) 4 "found multiple"
            equalMsg (System.Array.FindLast([| 1; 3; 4 |], fun x -> x % 5 = 2)) 0 "not found"
        }

        Test "FindLastIndex (basic)" {
            equalMsg (System.Array.FindLastIndex([||], fun _ -> true)) -1 "empty returns -1"
            equalMsg (System.Array.FindLastIndex([| 1; 3; 4 |], fun x -> x % 2 = 0)) 2 "found"
            equalMsg (System.Array.FindLastIndex([| 1; 2; 3; 4 |], fun x -> x % 2 = 0)) 3 "found multiple"
            equalMsg (System.Array.FindLastIndex([| 1; 3; 4 |], fun x -> x % 5 = 2)) -1 "not found"
        }

        Test "FindLastIndex (lower bound)" {
            equalMsg (System.Array.FindLastIndex([||], 0, fun _ -> true)) -1 "empty returns -1"
            equalMsg (System.Array.FindLastIndex([| 1; 3; 4 |], 1, fun x -> x % 2 = 0)) 2 "found"
            equalMsg (System.Array.FindLastIndex([| 0; 1; 2; 3; 4 |], 1, fun x -> x % 2 = 0)) 4 "found multiple"
            equalMsg (System.Array.FindLastIndex([| 1; 3; 4 |], 2, fun x -> x % 3 = 0)) -1 "not found because before lower bound"
            equalMsg (System.Array.FindLastIndex([| 1; 3; 4 |], 1, fun x -> x % 5 = 2)) -1 "not found anywhere"
        }

        Test "FindLastIndex (both bounds)" {
            equalMsg (System.Array.FindLastIndex([||], 0, 0, fun _ -> true)) -1 "empty returns -1"
            equalMsg (System.Array.FindLastIndex([| 1; 3; 4; 5 |], 1, 2, fun x -> x % 2 = 0)) 2 "found"
            equalMsg (System.Array.FindLastIndex([| 1; 3; 6; 7 |], 1, 2, fun x -> x % 3 = 0)) 2 "found multiple"
            equalMsg (System.Array.FindLastIndex([| 1; 3; 4; 5 |], 2, 2, fun x -> x % 3 = 0)) -1 "not found because before lower bound"
            equalMsg (System.Array.FindLastIndex([| 1; 3; 4; 5 |], 1, 2, fun x -> x % 5 = 0)) -1 "not found because after upper bound"
            equalMsg (System.Array.FindLastIndex([| 1; 3; 4 |], 1, 2, fun x -> x % 5 = 2)) -1 "not found anywhere"
        }

        Test "ForEach" {
            let x = ref 1
            System.Array.ForEach([|2; 3; 4|], fun y -> x := !x + y)
            equal !x 10
        }

        Test "IndexOf (basic)" {
            equalMsg (System.Array.IndexOf([||], "42")) -1 "empty returns -1"
            equalMsg (System.Array.IndexOf([| "1"; "3"; "4" |], "4")) 2 "found"
            equalMsg (System.Array.IndexOf([| "1"; "2"; "2"; "4" |], "2")) 1 "found multiple"
            equalMsg (System.Array.IndexOf([| "1"; "3"; "4" |], "5")) -1 "not found"
        }

        Test "IndexOf (lower bound)" {
            equalMsg (System.Array.IndexOf([||], "42", 0)) -1 "empty returns -1"
            equalMsg (System.Array.IndexOf([| "1"; "3"; "4" |], "4", 1)) 2 "found"
            equalMsg (System.Array.IndexOf([| "0"; "1"; "2"; "2"; "4" |], "2", 1)) 2 "found multiple"
            equalMsg (System.Array.IndexOf([| "1"; "3"; "4" |], "3", 2)) -1 "not found because before lower bound"
            equalMsg (System.Array.IndexOf([| "1"; "3"; "4" |], "5", 1)) -1 "not found anywhere"
        }

        Test "IndexOf (both bounds)" {
            equalMsg (System.Array.IndexOf([||], "42", 0, 0)) -1 "empty returns -1"
            equalMsg (System.Array.IndexOf([| "1"; "3"; "4"; "5" |], "4", 1, 2)) 2 "found"
            equalMsg (System.Array.IndexOf([| "1"; "3"; "3"; "7" |], "3", 1, 2)) 1 "found multiple"
            equalMsg (System.Array.IndexOf([| "1"; "3"; "4"; "5" |], "3", 2, 2)) -1 "not found because before lower bound"
            equalMsg (System.Array.IndexOf([| "1"; "3"; "4"; "5" |], "5", 1, 2)) -1 "not found because after upper bound"
            equalMsg (System.Array.IndexOf([| "1"; "3"; "4" |], "5", 1, 2)) -1 "not found anywhere"
        }

        Test "LastIndexOf (basic)" {
            equalMsg (System.Array.LastIndexOf([||], "42")) -1 "empty returns -1"
            equalMsg (System.Array.LastIndexOf([| "1"; "3"; "4" |], "4")) 2 "found"
            equalMsg (System.Array.LastIndexOf([| "1"; "2"; "2"; "4" |], "2")) 2 "found multiple"
            equalMsg (System.Array.LastIndexOf([| "1"; "3"; "4" |], "5")) -1 "not found"
        }

        Test "LastIndexOf (lower bound)" {
            equalMsg (System.Array.LastIndexOf([||], "42", 0)) -1 "empty returns -1"
            equalMsg (System.Array.LastIndexOf([| "1"; "3"; "4" |], "4", 1)) 2 "found"
            equalMsg (System.Array.LastIndexOf([| "0"; "1"; "2"; "2"; "4" |], "2", 1)) 3 "found multiple"
            equalMsg (System.Array.LastIndexOf([| "1"; "3"; "4" |], "3", 2)) -1 "not found because before lower bound"
            equalMsg (System.Array.LastIndexOf([| "1"; "3"; "4" |], "5", 1)) -1 "not found anywhere"
        }

        Test "LastIndexOf (both bounds)" {
            equalMsg (System.Array.LastIndexOf([||], "42", 0, 0)) -1 "empty returns -1"
            equalMsg (System.Array.LastIndexOf([| "1"; "3"; "4"; "5" |], "4", 1, 2)) 2 "found"
            equalMsg (System.Array.LastIndexOf([| "1"; "3"; "3"; "7" |], "3", 1, 2)) 2 "found multiple"
            equalMsg (System.Array.LastIndexOf([| "1"; "3"; "4"; "5" |], "3", 2, 2)) -1 "not found because before lower bound"
            equalMsg (System.Array.LastIndexOf([| "1"; "3"; "4"; "5" |], "5", 1, 2)) -1 "not found because after upper bound"
            equalMsg (System.Array.LastIndexOf([| "1"; "3"; "4" |], "5", 1, 2)) -1 "not found anywhere"
        }

        Test "Resize" {
            let r = ref null
            System.Array.Resize(r, 3)
            equalMsg (!r) [|0; 0; 0|] "null"
            propertyWith (
                RandomValues.Tuple2Of(RandomValues.ArrayOf smallInt, len)
            ) (fun (a, n) -> Do {
                let r = ref a
                System.Array.Resize(r, n)
                equal (!r).Length n
                forEach {0 .. min n a.Length - 1} (fun i -> Do { equal (!r).[i] a.[i] })
                forEach {min n a.Length .. n - 1} (fun i -> Do { equal (!r).[i] Unchecked.defaultof<_> })
            })
        }

        Test "Sort" {
            propertyWith (
                RandomValues.Tuple4Of(RandomValues.ArrayOf smallInt, RandomValues.ArrayOf RandomValues.StringReadable, len, len)
                |> RandomValues.SuchThat (fun (keys, items, index, length) ->
                    keys.Length = items.Length && index + length < keys.Length
                )
            ) (fun (keys, items, index, length) -> Do {
                let origKeys = Array.copy keys
                let origItems = Array.copy items
                System.Array.Sort(keys, items, index, length, invComparer)
                forEach {0 .. index - 1} (fun i -> Do {
                    equalMsg keys.[i] origKeys.[i] "keys before index aren't touched"
                    equalMsg items.[i] origItems.[i] "items before index aren't touched"
                })
                forEach {index + length .. keys.Length - 1} (fun i -> Do {
                    equalMsg keys.[i] origKeys.[i] "keys after index aren't touched"
                    equalMsg items.[i] origItems.[i] "items after index aren't touched"
                })
                forEach {index .. index + length - 2} (fun i -> Do {
                    isTrueMsg (invComparer.Compare(keys.[i], keys.[i + 1]) <= 0) "keys are sorted"
                })
                forEach {index .. index + length - 1} (fun i -> Do {
                    let k = origKeys.[i]
                    let v = origItems.[i]
                    isTrueMsg
                        (Array.exists2 (fun k' v' -> k = k' && v = v') keys items)
                        "items are sorted"
                })
            })
        }

        Test "TrueForAll" {
            isTrueMsg (System.Array.TrueForAll([||], fun _ -> false)) "empty always true"
            isTrueMsg (System.Array.TrueForAll([| 1; 2; 3 |], fun x -> x < 4)) "true"
            isFalseMsg (System.Array.TrueForAll([| 1; 2; 3 |], fun x -> x < 3)) "false"
        }

    }

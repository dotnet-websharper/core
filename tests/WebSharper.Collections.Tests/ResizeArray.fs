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

module WebSharper.Collections.Tests.ResizeArray

open System
open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

[<JavaScript>]
let Tests =

    TestCategory "ResizeArray" {

        Test "ctor" {
            let a = ResizeArray()
            equal (a.ToArray()) [||]
            let b = ResizeArray(3)
            equal (b.ToArray()) [||]
            let c = ResizeArray([1;2;3])
            equal (c.ToArray()) [| 1; 2; 3 |]
        }

        Test "Add" {
            let a = ResizeArray([1])
            a.Add(2)
            a.Add(3)
            equal (a.ToArray()) [| 1; 2; 3 |]
        }

        Test "AddRange" {
            let a = ResizeArray([1])
            a.AddRange([2;3])
            equal (a.ToArray()) [| 1; 2; 3 |]
        }

        Test "AsReadOnly" {
            property (fun (a: ResizeArray<_>) -> Do {
                let b = a.AsReadOnly()
                forEach {0 .. a.Count - 1} (fun i -> Do {
                    equal a.[i] (As<_[]> b).[i]
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
                RandomValues.Tuple2Of (RandomValues.ResizeArrayOf smallInt, smallInt)
            ) (fun (haystack, needle) -> Do {
                haystack.Sort()
                let expected =
                    match haystack.FindIndex(fun y -> y >= needle) with
                    | -1 -> ~~~haystack.Count
                    | i -> if haystack.[i] = needle then i else ~~~i
                let actual = haystack.BinarySearch(needle)
                equal actual expected
            })
        }

        Test "BinarySearch (comparer)" {
            propertyWith (
                RandomValues.Tuple2Of (RandomValues.ResizeArrayOf smallInt, smallInt)
            ) (fun (haystack, needle) -> Do {
                haystack.Sort(fun x y -> compare y x)
                let expected =
                    match haystack.FindIndex (fun y -> y <= needle) with
                    | -1 -> ~~~haystack.Count
                    | i -> if haystack.[i] = needle then i else ~~~i
                let actual = haystack.BinarySearch(needle, invComparer)
                equal actual expected
            })
        }

        Test "BinarySearch (bounds, comparer)" {
            propertyWith (
                RandomValues.Tuple4Of (RandomValues.ResizeArrayOf smallInt, len, len, smallInt)
                |> RandomValues.SuchThat (fun (a, s, l, x) -> s + l <= a.Count)
            ) (fun (haystack, start, length, needle) -> Do {
                haystack.Sort(fun x y -> compare y x)
                let expected =
                    match haystack.GetRange(start, length).FindIndex(fun y -> y <= needle) with
                    | -1 -> ~~~(start+length)
                    | j ->
                        let i = j + start
                        if haystack.[i] = needle then i else ~~~i
                let actual = haystack.BinarySearch(start, length, needle, invComparer)
                equal actual expected
            })
        }

        Test "seq" {
            let l = [2;3]
            let a = ResizeArray(l)
            isTrue (Seq.forall2 (=) a l)
        }

        Test "Clear" {
            let a = ResizeArray([1;2;3])
            a.Clear()
            a.Add(1)
            equal (a.ToArray()) [| 1 |]
        }

        Test "ConvertAll" {
            property (fun (a: ResizeArray<int>) -> Do {
                let b = a.ConvertAll(fun x -> string (x + 1))
                equal a.Count b.Count
                forEach {0 .. a.Count - 1} (fun i -> Do {
                    equal b.[i] (string (a.[i] + 1))
                })
            })
        }

        Test "CopyTo" {
            let a = ResizeArray([1;2;3])
            let x = [| 0; 0; 0; 0; 0 |]
            a.CopyTo(x)
            equal x [| 1; 2; 3; 0; 0 |]
            a.CopyTo(x, 1)
            equal x [| 1; 1; 2; 3; 0 |]
            a.CopyTo(1, x, 3, 2)
            equal x [| 1; 1; 2; 2; 3 |]
        }

        Test "Count" {
            let a = ResizeArray([1; 2])
            equal a.Count 2
        }

        Test "Exists" {
            isFalseMsg (ResizeArray([||]).Exists(fun _ -> true)) "empty always false"
            isTrueMsg (ResizeArray([| 1; 2; 3 |]).Exists(fun x -> x = 2)) "true"
            isFalseMsg (ResizeArray([| 1; 2; 3 |]).Exists(fun x -> x = 4)) "false"
        }

        Test "Find" {
            equalMsg (ResizeArray([||]).Find(fun _ -> true)) 0 "empty returns default"
            equalMsg (ResizeArray([| 1; 3; 4 |]).Find(fun x -> x % 2 = 0)) 4 "found"
            equalMsg (ResizeArray([| 1; 2; 3; 4 |]).Find(fun x -> x % 2 = 0)) 2 "found multiple"
            equalMsg (ResizeArray([| 1; 3; 4 |]).Find(fun x -> x % 5 = 2)) 0 "not found"
        }

        Test "FindAll" {
            equalMsg (ResizeArray([||]).FindAll(fun _ -> true).ToArray()) [||] "empty returns empty"
            equalMsg (ResizeArray([| 1; 2; 3; 4 |]).FindAll(fun x -> x % 2 = 0).ToArray()) [| 2; 4 |] "found"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindAll(fun x -> x % 5 = 2).ToArray()) [||] "not found"
        }

        Test "FindIndex (basic)" {
            equalMsg (ResizeArray([||]).FindIndex(fun _ -> true)) -1 "empty returns -1"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindIndex(fun x -> x % 2 = 0)) 2 "found"
            equalMsg (ResizeArray([| 1; 2; 3; 4 |]).FindIndex(fun x -> x % 2 = 0)) 1 "found multiple"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindIndex(fun x -> x % 5 = 2)) -1 "not found"
        }

        Test "FindIndex (lower bound)" {
            equalMsg (ResizeArray([||]).FindIndex(0, fun _ -> true)) -1 "empty returns -1"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindIndex(1, fun x -> x % 2 = 0)) 2 "found"
            equalMsg (ResizeArray([| 0; 1; 2; 3; 4 |]).FindIndex(1, fun x -> x % 2 = 0)) 2 "found multiple"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindIndex(2, fun x -> x % 3 = 0)) -1 "not found because before lower bound"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindIndex(1, fun x -> x % 5 = 2)) -1 "not found anywhere"
        }

        Test "FindIndex (both bounds)" {
            equalMsg (ResizeArray([||]).FindIndex(0, 0, fun _ -> true)) -1 "empty returns -1"
            equalMsg (ResizeArray([| 1; 3; 4; 5 |]).FindIndex(1, 2, fun x -> x % 2 = 0)) 2 "found"
            equalMsg (ResizeArray([| 1; 3; 6; 7 |]).FindIndex(1, 2, fun x -> x % 3 = 0)) 1 "found multiple"
            equalMsg (ResizeArray([| 1; 3; 4; 5 |]).FindIndex(2, 2, fun x -> x % 3 = 0)) -1 "not found because before lower bound"
            equalMsg (ResizeArray([| 1; 3; 4; 5 |]).FindIndex(1, 2, fun x -> x % 5 = 0)) -1 "not found because after upper bound"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindIndex(1, 2, fun x -> x % 5 = 2)) -1 "not found anywhere"
        }

        Test "FindLast" {
            equalMsg (ResizeArray([||]).FindLast(fun _ -> true)) 0 "empty returns default"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindLast(fun x -> x % 2 = 0)) 4 "found"
            equalMsg (ResizeArray([| 1; 2; 3; 4 |]).FindLast(fun x -> x % 2 = 0)) 4 "found multiple"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindLast(fun x -> x % 5 = 2)) 0 "not found"
        }

        Test "FindLastIndex (basic)" {
            equalMsg (ResizeArray([||]).FindLastIndex(fun _ -> true)) -1 "empty returns -1"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindLastIndex(fun x -> x % 2 = 0)) 2 "found"
            equalMsg (ResizeArray([| 1; 2; 3; 4 |]).FindLastIndex(fun x -> x % 2 = 0)) 3 "found multiple"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindLastIndex(fun x -> x % 5 = 2)) -1 "not found"
        }

        Test "FindLastIndex (lower bound)" {
            equalMsg (ResizeArray([||]).FindLastIndex(0, fun _ -> true)) -1 "empty returns -1"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindLastIndex(1, fun x -> x % 2 = 0)) 2 "found"
            equalMsg (ResizeArray([| 0; 1; 2; 3; 4 |]).FindLastIndex(1, fun x -> x % 2 = 0)) 4 "found multiple"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindLastIndex(2, fun x -> x % 3 = 0)) -1 "not found because before lower bound"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindLastIndex(1, fun x -> x % 5 = 2)) -1 "not found anywhere"
        }

        Test "FindLastIndex (both bounds)" {
            equalMsg (ResizeArray([||]).FindLastIndex(0, 0, fun _ -> true)) -1 "empty returns -1"
            equalMsg (ResizeArray([| 1; 3; 4; 5 |]).FindLastIndex(1, 2, fun x -> x % 2 = 0)) 2 "found"
            equalMsg (ResizeArray([| 1; 3; 6; 7 |]).FindLastIndex(1, 2, fun x -> x % 3 = 0)) 2 "found multiple"
            equalMsg (ResizeArray([| 1; 3; 4; 5 |]).FindLastIndex(2, 2, fun x -> x % 3 = 0)) -1 "not found because before lower bound"
            equalMsg (ResizeArray([| 1; 3; 4; 5 |]).FindLastIndex(1, 2, fun x -> x % 5 = 0)) -1 "not found because after upper bound"
            equalMsg (ResizeArray([| 1; 3; 4 |]).FindLastIndex(1, 2, fun x -> x % 5 = 2)) -1 "not found anywhere"
        }

        Test "ForEach" {
            let x = ref 1
            ResizeArray([|2; 3; 4|]).ForEach(fun y -> x := !x + y)
            equal !x 10
        }

        Test "GetRange" {
            let a = ResizeArray([1;2;3;4;5])
            equal (a.GetRange(2, 2).ToArray()) [|3; 4|]
        }

        Test "IndexOf (basic)" {
            equalMsg (ResizeArray([||]).IndexOf("42")) -1 "empty returns -1"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).IndexOf("4")) 2 "found"
            equalMsg (ResizeArray([| "1"; "2"; "2"; "4" |]).IndexOf("2")) 1 "found multiple"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).IndexOf("5")) -1 "not found"
        }

        Test "IndexOf (lower bound)" {
            equalMsg (ResizeArray([||]).IndexOf("42", 0)) -1 "empty returns -1"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).IndexOf("4", 1)) 2 "found"
            equalMsg (ResizeArray([| "0"; "1"; "2"; "2"; "4" |]).IndexOf("2", 1)) 2 "found multiple"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).IndexOf("3", 2)) -1 "not found because before lower bound"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).IndexOf("5", 1)) -1 "not found anywhere"
        }

        Test "IndexOf (both bounds)" {
            equalMsg (ResizeArray([||]).IndexOf("42", 0, 0)) -1 "empty returns -1"
            equalMsg (ResizeArray([| "1"; "3"; "4"; "5" |]).IndexOf("4", 1, 2)) 2 "found"
            equalMsg (ResizeArray([| "1"; "3"; "3"; "7" |]).IndexOf("3", 1, 2)) 1 "found multiple"
            equalMsg (ResizeArray([| "1"; "3"; "4"; "5" |]).IndexOf("3", 2, 2)) -1 "not found because before lower bound"
            equalMsg (ResizeArray([| "1"; "3"; "4"; "5" |]).IndexOf("5", 1, 2)) -1 "not found because after upper bound"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).IndexOf("5", 1, 2)) -1 "not found anywhere"
        }

        Test "Insert" {
            let a = ResizeArray([1;2;3])
            a.Insert(1, -1)
            equal (a.ToArray()) [| 1; -1; 2; 3 |]
        }

        Test "InsertRange" {
            let a = ResizeArray([1;2;3])
            a.InsertRange(1, [-1; -2])
            equal (a.ToArray()) [| 1; -1; -2; 2; 3 |]
        }

        Test "Item" {
            let a = ResizeArray([1;2;3])
            equal a.[0] 1
            equal a.[1] 2
            equal a.[2] 3
        }

        Test "set_Item" {
            let a = ResizeArray([1;2;3])
            a.[0] <- 4
            a.[1] <- 5
            a.[2] <- 6
            equal a.[0] 4
            equal a.[1] 5
            equal a.[2] 6
        }

        Test "LastIndexOf (basic)" {
            equalMsg (ResizeArray([||]).LastIndexOf("42")) -1 "empty returns -1"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).LastIndexOf("4")) 2 "found"
            equalMsg (ResizeArray([| "1"; "2"; "2"; "4" |]).LastIndexOf("2")) 2 "found multiple"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).LastIndexOf("5")) -1 "not found"
        }

        Test "LastIndexOf (lower bound)" {
            equalMsg (ResizeArray([||]).LastIndexOf("42", 0)) -1 "empty returns -1"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).LastIndexOf("4", 1)) 2 "found"
            equalMsg (ResizeArray([| "0"; "1"; "2"; "2"; "4" |]).LastIndexOf("2", 1)) 3 "found multiple"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).LastIndexOf("3", 2)) -1 "not found because before lower bound"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).LastIndexOf("5", 1)) -1 "not found anywhere"
        }

        Test "LastIndexOf (both bounds)" {
            equalMsg (ResizeArray([||]).LastIndexOf("42", 0, 0)) -1 "empty returns -1"
            equalMsg (ResizeArray([| "1"; "3"; "4"; "5" |]).LastIndexOf("4", 1, 2)) 2 "found"
            equalMsg (ResizeArray([| "1"; "3"; "3"; "7" |]).LastIndexOf("3", 1, 2)) 2 "found multiple"
            equalMsg (ResizeArray([| "1"; "3"; "4"; "5" |]).LastIndexOf("3", 2, 2)) -1 "not found because before lower bound"
            equalMsg (ResizeArray([| "1"; "3"; "4"; "5" |]).LastIndexOf("5", 1, 2)) -1 "not found because after upper bound"
            equalMsg (ResizeArray([| "1"; "3"; "4" |]).LastIndexOf("5", 1, 2)) -1 "not found anywhere"
        }

        Test "Remove" {
            let a = ResizeArray([1;2;3;2])
            a.Remove(2) |> ignore
            equal (a.ToArray()) [|1;3;2|]
        }

        Test "RemoveAll" {
            let a = ResizeArray([1;2;3;2])
            a.RemoveAll(fun x -> x = 2) |> ignore
            equal (a.ToArray()) [|1;3|]
        }

        Test "RemoveAt" {
            let a = ResizeArray([1;2;3])
            a.RemoveAt(1)
            equal (a.ToArray()) [|1; 3|]
        }

        Test "RemoveRange" {
            let a = ResizeArray([1;2;3;4;5])
            a.RemoveRange(2, 2)
            equal (a.ToArray()) [|1; 2; 5|]
        }

        Test "Reverse" {
            let a = ResizeArray([1;2;3;4;5])
            a.Reverse()
            equal (a.ToArray()) [| 5; 4; 3; 2; 1|]
            a.Reverse(2, 2)
            equal (a.ToArray()) [| 5; 4; 2; 3;  1|]
        }

        Test "Sort" {
            propertyWith (
                RandomValues.Tuple3Of(RandomValues.ResizeArrayOf smallInt, len, len)
                |> RandomValues.SuchThat (fun (keys, index, length) -> index + length < keys.Count)
            ) (fun (keys, index, length) -> Do {
                let origKeys = ResizeArray(keys)
                keys.Sort(index, length, invComparer)
                forEach {0 .. index - 1} (fun i -> Do {
                    equalMsg keys.[i] origKeys.[i] "keys before index aren't touched"
                })
                forEach {index + length .. keys.Count - 1} (fun i -> Do {
                    equalMsg keys.[i] origKeys.[i] "keys after index aren't touched"
                })
                forEach {index .. index + length - 2} (fun i -> Do {
                    isTrueMsg (invComparer.Compare(keys.[i], keys.[i + 1]) <= 0) "keys are sorted"
                })
            })
        }

        Test "TrueForAll" {
            isTrueMsg (ResizeArray([||]).TrueForAll(fun _ -> false)) "empty always true"
            isTrueMsg (ResizeArray([| 1; 2; 3 |]).TrueForAll(fun x -> x < 4)) "true"
            isFalseMsg (ResizeArray([| 1; 2; 3 |]).TrueForAll(fun x -> x < 3)) "false"
        }

    }

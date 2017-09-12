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

namespace WebSharper

open System
open WebSharper.JavaScript
module F = WebSharper.IntrinsicFunctionProxy
type private IComparer = System.Collections.IComparer
type private IComparer<'T> = System.Collections.Generic.IComparer<'T>

[<AutoOpen; JavaScript>]
module ArrayProxy =

    let binarySearch (haystack: 'T[]) (comparer: 'T -> int) start finish =
        if start < 0 then raise (ArgumentOutOfRangeException("index", "Non-negative number required."))
        if finish > haystack.Length then raise (ArgumentException("Offset and length were out of bounds for the array or count is greater than the number of elements from index to the end of the source collection."))
        if finish < start then raise (ArgumentOutOfRangeException("length", "Non-negative number required."))
        let rec search left right =
            if left > right then
                ~~~left
            else
                let pivot = (left + right) / 2
                let cmp = comparer haystack.[pivot]
                if left = right then
                    if cmp = 0 then left
                    elif cmp > 0 then ~~~(left + 1)
                    else ~~~left
                elif cmp <= 0 then
                    search left pivot
                else
                    search (pivot + 1) right
        search start (finish - 1)

   let objBinarySearchComparer (needle: obj) =
       // Check for an implementation of IComparable
       if needle?CompareTo0 then
           As<IComparable>(needle).CompareTo
       else
           fun x ->
               if x?CompareTo0 then
                   -As<IComparable>(x).CompareTo(needle)
               else
                   InvalidOperationException(
                       "Failed to compare two elements in the array.",
                       ArgumentException("At least one object must implement IComparable."))
                   |> raise

[<Proxy(typeof<System.Array>)>]
type private ArrayProxy =

    [<Inline>]
    static member BinarySearch(haystack: System.Array, needle: obj) : int =
        binarySearch (As<obj[]> haystack) (objBinarySearchComparer needle) 0 haystack.Length

    [<Inline>]
    static member BinarySearch(haystack: System.Array, needle: obj, comparer: IComparer) : int =
        binarySearch (As<obj[]> haystack) (fun o -> comparer.Compare(needle, o)) 0 haystack.Length

    [<Inline>]
    static member BinarySearch(haystack: System.Array, start: int, length: int, needle: obj) : int =
        binarySearch (As<obj[]> haystack) (objBinarySearchComparer needle) start (start + length)

    [<Inline>]
    static member BinarySearch(haystack: System.Array, start: int, length: int, needle: obj, comparer: IComparer) : int =
        binarySearch (As<obj[]> haystack) (fun o -> comparer.Compare(needle, o)) start (start + length)

    static member BinarySearch<'T>(haystack: 'T[], needle: 'T) : int =
        let compare y = compare (As<System.IComparable<'T>> needle) (As<System.IComparable<'T>> y)
        binarySearch haystack compare 0 haystack.Length

    static member BinarySearch<'T>(haystack: 'T[], start: int, length: int, needle: 'T) : int =
        let compare y = compare (As<System.IComparable<'T>> needle) (As<System.IComparable<'T>> y)
        binarySearch haystack compare start (start + length)

    static member BinarySearch<'T>(haystack: 'T[], needle: 'T, comparer: IComparer<'T>) : int =
        binarySearch haystack (fun o -> comparer.Compare(needle, o)) 0 haystack.Length

    static member BinarySearch<'T>(haystack: 'T[], start: int, length: int, needle: 'T, comparer: IComparer<'T>) : int =
        binarySearch haystack (fun o -> comparer.Compare(needle, o)) start (start + length)

    [<Inline "$array.reverse()">]
    static member Reverse(array: System.Array) = X<unit>

    [<Name "WebSharper.Arrays.reverse">]
    static member Reverse(array: System.Array, offset: int, length: int) =
        let a = Array.rev (Array.sub (As array) offset length)
        Array.blit a 0 (As array) offset a.Length

    member this.Length
        with [<Inline>] get() = F.GetLength (As this)   

    [<Inline>]
    member this.GetEnumerator() = Enumerator.Get0 (As this)         

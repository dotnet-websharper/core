// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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
type private JSArray<'T> = WebSharper.JavaScript.Array<'T>
type private Comparer<'T> = System.Collections.Generic.Comparer<'T>

[<AutoOpen; JavaScript>]
module ArrayProxy =

    [<Name "WebSharper.Arrays.binarySearch">]
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

    [<Name "WebSharper.Arrays.binarySearchComparer">]
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

    [<Name "WebSharper.Arrays.sortInternal">]
    let sortInternal (keys: 'K[]) (index: int) (length: int) (comp: 'K * 'K -> int) (swap: int -> int -> unit) : unit =
        let partition l r =
            let pivot = keys.JS.[r]
            let mutable i = l - 1
            for j = l to r - 1 do
                if comp(keys.JS.[j], pivot) < 0 then
                    i <- i + 1
                    swap i j
            if comp(keys.JS.[r], keys.JS.[i + 1]) < 0 then
                swap (i + 1) r
            i + 1
        let rec quicksort l r =
            if l < r then
                let p = partition l r
                quicksort l (p - 1)
                quicksort (p + 1) r
        quicksort index (index + length - 1)

    [<Name "WebSharper.Arrays.sortSub">]
    let sortSub (keys: 'K[]) (index: int) (length: int) (comp: 'K * 'K -> int) : unit =
        let swap i j =
            let k = keys.JS.[i]
            keys.JS.[i] <- keys.JS.[j]
            keys.JS.[j] <- k
        sortInternal keys index length comp swap

    [<Name "WebSharper.Arrays.sortByKeys">]
    let sortByKeys (keys: 'K[]) (items: 'V[]) (index: int) (length: int) (comp: 'K * 'K -> int) : unit =
        let swap i j =
            let k = keys.JS.[i]
            keys.JS.[i] <- keys.JS.[j]
            keys.JS.[j] <- k
            let v = items.JS.[i]
            items.JS.[i] <- items.JS.[j]
            items.JS.[j] <- v
        sortInternal keys index length comp swap

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

    [<Inline>]
    static member BinarySearch<'T>(haystack: 'T[], needle: 'T) : int =
        let compare y = compare (As<IComparable> needle) (As<IComparable> y)
        binarySearch haystack compare 0 haystack.Length

    [<Inline>]
    static member BinarySearch<'T>(haystack: 'T[], start: int, length: int, needle: 'T) : int =
        let compare y = compare (As<IComparable> needle) (As<IComparable> y)
        binarySearch haystack compare start (start + length)

    [<Inline>]
    static member BinarySearch<'T>(haystack: 'T[], needle: 'T, comparer: IComparer<'T>) : int =
        binarySearch haystack (fun o -> comparer.Compare(needle, o)) 0 haystack.Length

    [<Inline>]
    static member BinarySearch<'T>(haystack: 'T[], start: int, length: int, needle: 'T, comparer: IComparer<'T>) : int =
        binarySearch haystack (fun o -> comparer.Compare(needle, o)) start (start + length)

    [<Name "WebSharper.Arrays.clear">]
    static member Clear(array: System.Array, index: int, length: int) : unit =
        if isNull array then raise (ArgumentNullException("array"))
        if index < 0 || length < 0 || index + length > array.Length then raise (IndexOutOfRangeException())
        for i = index to index + length - 1 do
            (As<JSArray<obj>> array).[i] <-
                match JS.TypeOf (As<JSArray<obj>> array).[i] with
                | JS.Number -> box 0
                | _ -> null

    [<Inline>]
    member this.Clone() =
        Array.copy (As<obj[]> this) :> obj

    [<Name "WebSharper.Arrays.constrainedCopy">]
    static member ConstrainedCopy(src: System.Array, srcIndex: int, dst: System.Array, dstIndex: int, length: int) =
        if src ===. dst && dstIndex <= srcIndex + length then
            let tmp = Array.init length (fun i -> (As<obj[]> src).[srcIndex + i])
            Array.blit tmp 0 (As<obj[]> dst) dstIndex length
        else
            Array.blit (As<obj[]> src) srcIndex (As<obj[]> dst) dstIndex length

    [<Inline>]
    static member Copy(src: System.Array, srcIndex: int, dst: System.Array, dstIndex: int, length: int) =
        Array.blit (As<obj[]> src) srcIndex (As<obj[]> dst) dstIndex length

    [<Inline>]
    member this.CopyTo(dst: System.Array, index: int) =
        Enumerator.ArrayCopyTo (As<System.Array> this) dst index

    [<Inline>]
    static member Copy(src: System.Array, dst: System.Array, length: int) =
        Array.blit (As<obj[]> src) 0 (As<obj[]> dst) 0 length

    [<Inline>]
    static member ConvertAll<'T, 'U>(array: 'T[], converter: Converter<'T, 'U>) : 'U[] =
        Array.map converter.Invoke array

    [<Inline>]
    static member Empty<'T>() : 'T[] =
        Array.empty

    [<Inline>]
    static member Exists<'T>(array: 'T[], predicate: Predicate<'T>) : bool =
        Array.exists predicate.Invoke array

    [<Inline>]
    static member Find<'T>(array: 'T[], predicate: Predicate<'T>) : 'T =
        defaultArg (Array.tryFind predicate.Invoke array) Unchecked.defaultof<'T>

    [<Inline>]
    static member FindAll<'T>(array: 'T[], predicate: Predicate<'T>) : 'T[] =
        Array.filter predicate.Invoke array

    [<Name "WebSharper.Arrays.findIndexBound">]
    static member FindIndex<'T>(array: 'T[], startIndex: int, count: int, predicate: Predicate<'T>) : int =
        if isNull array then raise (ArgumentNullException("array"))
        if isNull predicate then raise (ArgumentNullException("match"))
        if startIndex < 0 then raise (ArgumentOutOfRangeException("startIndex", "Index was out of range. Must be non-negative and less than the size of the collection."))
        if count < 0 || startIndex + count > array.Length then raise (ArgumentOutOfRangeException("count", "Count must be positive and count must refer to a location within the string/array/collection."))
        let rec f finish i =
            if i = finish then
                -1
            elif predicate.Invoke(array.[i]) then
                i
            else
                f finish (i + 1)
        f (startIndex + count) startIndex

    [<Inline>]
    static member FindIndex<'T>(array: 'T[], startIndex: int, predicate: Predicate<'T>) : int =
        System.Array.FindIndex<'T>(array, startIndex, array.Length - startIndex, predicate)

    [<Inline>]
    static member FindIndex<'T>(array: 'T[], predicate: Predicate<'T>) : int =
        System.Array.FindIndex<'T>(array, 0, array.Length, predicate)

    [<Inline>]
    static member FindLast<'T>(array: 'T[], predicate: Predicate<'T>) : 'T =
        defaultArg (Array.tryFindBack predicate.Invoke array) Unchecked.defaultof<'T>

    [<Name "WebSharper.Arrays.findLastIndexBound">]
    static member FindLastIndex<'T>(array: 'T[], startIndex: int, count: int, predicate: Predicate<'T>) : int =
        if isNull array then raise (ArgumentNullException("array"))
        if isNull predicate then raise (ArgumentNullException("match"))
        if startIndex < 0 then raise (ArgumentOutOfRangeException("startIndex", "Index was out of range. Must be non-negative and less than the size of the collection."))
        if count < 0 || startIndex + count > array.Length then raise (ArgumentOutOfRangeException("count", "Count must be positive and count must refer to a location within the string/array/collection."))
        let rec f i =
            if i < startIndex then
                -1
            elif predicate.Invoke(array.[i]) then
                i
            else
                f (i - 1)
        f (startIndex + count - 1)

    [<Inline>]
    static member FindLastIndex<'T>(array: 'T[], startIndex: int, predicate: Predicate<'T>) : int =
        System.Array.FindLastIndex<'T>(array, startIndex, array.Length - startIndex, predicate)

    [<Inline>]
    static member FindLastIndex<'T>(array: 'T[], predicate: Predicate<'T>) : int =
        System.Array.FindLastIndex<'T>(array, 0, array.Length, predicate)

    [<Inline>]
    static member ForEach<'T>(array: 'T[], action: Action<'T>) : unit =
        Array.iter action.Invoke array

    [<Inline>]
    member this.GetValue(i: int) =
        (As<obj[]> this).[i]

    [<Inline>]
    static member IndexOf(haystack: System.Array, needle: obj, startIndex: int, count:  int) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindIndex(As<obj[]> haystack, startIndex, count, Predicate(predicate))

    [<Inline>]
    static member IndexOf(haystack: System.Array, needle: obj, startIndex: int) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindIndex(As<obj[]> haystack, startIndex, Predicate(predicate))

    [<Inline>]
    static member IndexOf(haystack: System.Array, needle: obj) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindIndex(As<obj[]> haystack, Predicate(predicate))

    [<Inline>]
    static member IndexOf<'T when 'T : null and 'T : equality>(haystack: 'T[], needle: 'T, startIndex: int, count:  int) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindIndex(haystack, startIndex, count, Predicate(predicate))

    [<Inline>]
    static member IndexOf<'T when 'T : null and 'T : equality>(haystack: 'T[], needle: 'T, startIndex: int) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindIndex(haystack, startIndex, Predicate(predicate))

    [<Inline>]
    static member IndexOf<'T when 'T : null and 'T : equality>(haystack: 'T[], needle: 'T) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindIndex(haystack, Predicate(predicate))

    [<Inline>]
    static member LastIndexOf(haystack: System.Array, needle: obj, startIndex: int, count:  int) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindLastIndex(As<obj[]> haystack, startIndex, count, Predicate(predicate))

    [<Inline>]
    static member LastIndexOf(haystack: System.Array, needle: obj, startIndex: int) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindLastIndex(As<obj[]> haystack, startIndex, Predicate(predicate))

    [<Inline>]
    static member LastIndexOf(haystack: System.Array, needle: obj) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindLastIndex(As<obj[]> haystack, Predicate(predicate))

    [<Inline>]
    static member LastIndexOf<'T when 'T : null and 'T : equality>(haystack: 'T[], needle: 'T, startIndex: int, count:  int) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindLastIndex(haystack, startIndex, count, Predicate(predicate))

    [<Inline>]
    static member LastIndexOf<'T when 'T : null and 'T : equality>(haystack: 'T[], needle: 'T, startIndex: int) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindLastIndex(haystack, startIndex, Predicate(predicate))

    [<Inline>]
    static member LastIndexOf<'T when 'T : null and 'T : equality>(haystack: 'T[], needle: 'T) : int =
        let predicate = if isNull needle then isNull else needle.Equals
        System.Array.FindLastIndex(haystack, Predicate(predicate))

    [<Inline>]
    static member Resize<'T>(array: byref<'T[]>, newSize: int) =
        let a = Array.zeroCreate newSize
        if not (isNull array) then
            Array.blit array 0 a 0 (min newSize array.Length)
        array <- a

    [<Inline "$array.reverse()">]
    static member Reverse(array: System.Array) = X<unit>

    [<Inline "$array.reverse()">]
    static member Reverse<'T>(array: 'T[]) = X<unit>

    [<Name "WebSharper.Arrays.reverse">]
    static member Reverse(array: System.Array, offset: int, length: int) =
        let a = Array.rev (Array.sub (As array) offset length)
        Array.blit a 0 (As array) offset a.Length

    [<Inline>]
    static member Reverse(array: 'T[], offset: int, length: int) =
        System.Array.Reverse(As<System.Array> array, offset, length)

    [<Inline>]
    member this.SetValue(v: obj, index: int) =
        (As<obj[]> this).[index] <- v

    [<Inline>]
    static member Sort<'K, 'V>(keys: 'K[], items: 'V[], index: int, length: int, comp: IComparer<'K>) : unit =
        sortByKeys keys items index length comp.Compare

    [<Inline>]
    static member Sort<'K, 'V>(keys: 'K[], items: 'V[], index: int, length: int) : unit =
        sortByKeys keys items index length (fun (x, y) -> compare (As x) (As y))

    [<Inline>]
    static member Sort<'K, 'V>(keys: 'K[], items: 'V[], comparer: IComparer<'K>) : unit =
        sortByKeys keys items 0 keys.Length comparer.Compare

    [<Inline>]
    static member Sort<'K, 'V>(keys: 'K[], items: 'V[]) : unit =
        sortByKeys keys items 0 keys.Length (fun (x, y) -> compare (As x) (As y))

    [<Inline>]
    static member Sort<'K>(keys: 'K[], index: int, length: int, comparer: IComparer<'K>) : unit =
        sortSub keys index length comparer.Compare

    [<Inline>]
    static member Sort<'K>(keys: 'K[], index: int, length: int) : unit =
        sortSub keys index length (fun (x, y) -> compare (As x) (As y))

    [<Inline>]
    static member Sort<'K>(keys: 'K[], comparer: IComparer<'K>) : unit =
        sortSub keys 0 keys.Length comparer.Compare

    [<Inline>]
    static member Sort<'K>(keys: 'K[]) : unit =
        sortSub keys 0 keys.Length (fun (x, y) -> compare (As x) (As y))

    [<Inline>]
    static member Sort<'K>(keys: 'K[], comparison: Comparison<'K>) =
        sortSub keys 0 keys.Length comparison.Invoke

    [<Inline>]
    static member Sort(keys: System.Array, index: int, length: int, comparer: IComparer) : unit =
        sortSub (As<obj[]> keys) index length comparer.Compare

    [<Inline>]
    static member Sort(keys: System.Array, index: int, length: int) : unit =
        sortSub (As<obj[]> keys) index length (fun (x, y) -> compare (As x) (As y))

    [<Inline>]
    static member Sort(keys: System.Array, comparer: IComparer) : unit =
        sortSub (As<obj[]> keys) 0 keys.Length comparer.Compare

    [<Inline>]
    static member Sort(keys: System.Array, items: System.Array, index: int, length: int, comp: IComparer) : unit =
        sortByKeys (As<obj[]> keys) (As<obj[]> items) index length comp.Compare

    [<Inline>]
    static member Sort(keys: System.Array, items: System.Array, index: int, length: int) : unit =
        sortByKeys (As<obj[]> keys) (As<obj[]> items) index length (fun (x, y) -> compare (As x) (As y))

    [<Inline>]
    static member Sort(keys: System.Array, items: System.Array, comp: IComparer) : unit =
        sortByKeys (As<obj[]> keys) (As<obj[]> items) 0 keys.Length comp.Compare

    [<Inline>]
    static member Sort(keys: System.Array, items: System.Array) : unit =
        sortByKeys (As<obj[]> keys) (As<obj[]> items) 0 keys.Length (fun (x, y) -> compare (As x) (As y))

    [<Inline>]
    static member Sort(keys: System.Array) : unit =
        sortSub (As<obj[]> keys) 0 keys.Length (fun (x, y) -> compare (As x) (As y))

    [<Inline>]
    static member TrueForAll<'T>(array : 'T[], predicate: Predicate<'T>) : bool =
        Array.forall predicate.Invoke array

    member this.Length
        with [<Inline>] get() = F.GetLength (As this)   

    [<Inline>]
    member this.GetEnumerator() = Enumerator.Get0 (As this)         

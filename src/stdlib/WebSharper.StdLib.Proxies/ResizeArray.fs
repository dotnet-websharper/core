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

module private WebSharper.Collections.ResizeArray

open WebSharper
open WebSharper.JavaScript
type private IComparer<'T> = System.Collections.Generic.IComparer<'T>

[<Name "WebSharper.Collections.ListEnumerator">]
[<Proxy(typeof<System.Collections.Generic.List.Enumerator<_>>)>]
type ResizeArrayEnumeratorProxy<'T> (arr: 'T[]) =
    let mutable i = -1

    member this.MoveNext() =
        i <- i + 1
        i < arr.Length

    member this.Current with get() = arr.[i]

    interface System.Collections.IEnumerator with
        member this.MoveNext() = this.MoveNext()
        member this.Current = 
            (As<System.Collections.Generic.IEnumerator<obj>> this).Current  
        [<JavaScript(false)>]
        member this.Reset() = ()

    interface System.Collections.Generic.IEnumerator<'T> with
        member this.Current = 
            if i = -1 then
                failwith "Enumeration has not started. Call MoveNext."
            elif i >= arr.Length then
                failwith "Enumeration already finished."
            else
                arr.[i]

    interface System.IDisposable with
        member this.Dispose() = ()

[<Proxy(typeof<System.Collections.Generic.List<_>>)>]
[<Name "WebSharper.Collections.List">]
[<Prototype false>]
[<Type "$0[]">]
type ResizeArrayProxy<'T> [<Inline "$wsruntime.MarkResizable($_arr)">] (_arr: 'T []) =

    [<Inline "$wsruntime.MarkResizable([])">]
    new () =
        new ResizeArrayProxy<'T>([||])

    [<Inline "$wsruntime.MarkResizable([])">]
    new (size: int) =
        new ResizeArrayProxy<'T>([||])

    [<Inline>]
    new (el: seq<'T>) =
        new ResizeArrayProxy<'T>(Seq.toArray el)

    [<Inline>]
    member this.GetEnumerator() =
        As<System.Collections.Generic.List.Enumerator<'T>>(new ResizeArrayEnumeratorProxy<'T>(As<'T[]> this))

    [<Inline>]
    member this.Add(x: 'T) : unit =
        As<'T[]>(this).JS.Push(x) |> ignore

    [<Inline>]
    member this.AddRange(x: seq<'T>) : unit =
        Seq.iter this.Add x

    [<Inline>]
    member this.AsReadOnly() : System.Collections.ObjectModel.ReadOnlyCollection<'T> =
        System.Array.AsReadOnly(As<'T[]> this)

    [<Inline>]
    member this.BinarySearch(start: int, length: int, item: 'T, comparer: IComparer<'T>) : int =
        System.Array.BinarySearch(As<'T[]> this, start, length, item, comparer)

    [<Inline>]
    member this.BinarySearch(item: 'T) : int =
        System.Array.BinarySearch(As<'T[]> this, item)

    [<Inline>]
    member this.BinarySearch(item: 'T, comparer: IComparer<'T>) : int =
        System.Array.BinarySearch(As<'T[]> this, item, comparer)

    [<Inline>]
    member this.Clear() : unit =
        (As<'T[]> this).JS.Splice(0, this.Count) |> ignore

    [<Inline>]
    member this.Contains(item: 'T) : bool =
        System.Array.Exists(As<'T[]> this, fun x -> System.Collections.Generic.EqualityComparer.Default.Equals(item, x))

    [<Inline>]
    member this.ConvertAll<'U>(conv: System.Converter<'T, 'U>) : ResizeArray<'U> =
        ResizeArray<'U>(System.Array.ConvertAll(As<'T[]> this, conv))

    [<Inline>]
    member this.CopyTo(arr: 'T[]) : unit =
        this.CopyTo(arr, 0)

    [<Inline>]
    member this.CopyTo(arr: 'T[], offset: int) : unit =
        this.CopyTo(0, arr, offset, this.Count)

    [<Inline>]
    member this.CopyTo(index: int, target: 'T[], offset: int, count: int) : unit =
        Array.blit (As<'T[]> this) index target offset count

    [<Inline>]
    member this.Count : int = (As<'T[]> this).Length

    [<Inline>]
    member this.Exists(pred: System.Predicate<'T>) : bool =
        System.Array.Exists(As<'T[]> this, pred)

    [<Inline>]
    member this.Find(pred: System.Predicate<'T>) : 'T =
        System.Array.Find(As<'T[]> this, pred)

    [<Inline>]
    member this.FindAll(pred: System.Predicate<'T>) : ResizeArray<'T> =
        ResizeArray<'T>(System.Array.FindAll(As<'T[]> this, pred))

    [<Inline>]
    member this.FindIndex(pred: System.Predicate<'T>) : int =
        System.Array.FindIndex(As<'T[]> this, pred)

    [<Inline>]
    member this.FindIndex(start: int, pred: System.Predicate<'T>) : int =
        System.Array.FindIndex(As<'T[]> this, start, pred)

    [<Inline>]
    member this.FindIndex(start: int, count: int, pred: System.Predicate<'T>) : int =
        System.Array.FindIndex(As<'T[]> this, start, count, pred)

    [<Inline>]
    member this.FindLast(pred: System.Predicate<'T>) : 'T =
        System.Array.FindLast(As<'T[]> this, pred)

    [<Inline>]
    member this.FindLastIndex(pred: System.Predicate<'T>) : int =
        System.Array.FindLastIndex(As<'T[]> this, pred)

    [<Inline>]
    member this.FindLastIndex(start: int, pred: System.Predicate<'T>) : int =
        System.Array.FindLastIndex(As<'T[]> this, start, pred)

    [<Inline>]
    member this.FindLastIndex(start: int, count: int, pred: System.Predicate<'T>) : int =
        System.Array.FindLastIndex(As<'T[]> this, start, count, pred)

    [<Inline>]
    member this.ForEach(action: System.Action<'T>) : unit =
        System.Array.ForEach(As<'T[]> this, action)

    [<Inline>]
    member this.GetRange(index: int, count: int) : ResizeArray<'T> =
        As (ResizeArrayProxy<'T>(Array.sub (As<'T[]> this) index count))

    [<Inline>]
    member this.IndexOf(item: 'T) : int =
        System.Array.IndexOf(As<'T[]> this, item)

    [<Inline>]
    member this.IndexOf(item: 'T, start: int) : int =
        System.Array.IndexOf(As<'T[]> this, item, start)

    [<Inline>]
    member this.IndexOf(item: 'T, start: int, count: int) : int =
        System.Array.IndexOf(As<'T[]> this, item, start, count)

    [<Inline>]
    member this.Insert(index: int, item: 'T) : unit =
        (As<'T[]> this).JS.Splice(index, 0, item) |> ignore

    [<Inline>]
    member this.InsertRange(index: int, items: seq<'T>) : unit =
        (As<'T[]> this).JS.Splice(index, 0, Array.ofSeq items) |> ignore

    member this.Item
        with [<Inline>] get (x: int) : 'T = (As<'T[]> this).[x]
        and [<Inline>] set (x: int) (v: 'T) = (As<'T[]> this).[x] <- v

    [<Inline>]
    member this.LastIndexOf(item: 'T) : int =
        System.Array.LastIndexOf(As<'T[]> this, item)

    [<Inline>]
    member this.LastIndexOf(item: 'T, start: int) : int =
        System.Array.LastIndexOf(As<'T[]> this, item, start)

    [<Inline>]
    member this.LastIndexOf(item: 'T, start: int, count: int) : int =
        System.Array.LastIndexOf(As<'T[]> this, item, start, count)

    member this.Remove(item: 'T) : bool =
        match this.IndexOf(item) with
        | -1 -> false
        | n -> this.RemoveAt(n); true

    member this.RemoveAll(pred: System.Predicate<'T>) : int =
        let mutable removed = 0
        let mutable i = 0
        while i < this.Count do
            if pred.Invoke((As<'T[]> this).JS.[i]) then
                let mutable j = i + 1
                while j < this.Count && pred.Invoke((As<'T[]> this).JS.[j]) do
                    j <- j + 1
                removed <- removed + j - i
                (As<'T[]> this).JS.Splice(i, j - i) |> ignore
            else
                i <- i + 1
        removed

    [<Inline>]
    member this.RemoveAt(x: int) : unit =
        (As<'T[]> this).JS.Splice(x, 1) |> ignore

    [<Inline>]
    member this.RemoveRange(index: int, count: int) : unit =
        (As<'T[]> this).JS.Splice(index, count) |> ignore

    [<Inline>]
    member this.Reverse() : unit =
        System.Array.Reverse(As<'T[]> this)

    [<Inline>]
    member this.Reverse(index: int, count: int) : unit =
        System.Array.Reverse(As<'T[]> this, index, count)

    [<Inline>]
    member this.Sort() : unit =
        System.Array.Sort(As<'T[]> this)

    [<Inline>]
    member this.Sort(comp: IComparer<'T>) : unit =
        System.Array.Sort(As<'T[]> this, comp)

    [<Inline>]
    member this.Sort(start: int, length: int, comp: IComparer<'T>) : unit =
        System.Array.Sort(As<'T[]> this, start, length, comp)

    [<Inline>]
    member this.Sort(comp: System.Comparison<'T>) : unit =
        System.Array.Sort(As<'T[]> this, comp)

    [<Inline>]
    member this.ToArray() : 'T [] =
        Array.copy (As<'T[]> this)

    [<Inline>]
    member this.TrimExcess() = ()

    [<Inline>]
    member this.TrueForAll(pred: System.Predicate<'T>) : bool =
        System.Array.TrueForAll(As<'T[]> this, pred)

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

module private WebSharper.Collections.ResizeArray

open WebSharper
open WebSharper.JavaScript

[<Inline "$arr.push($x)">]
let push (arr: 'T []) (x: 'T) = ()

[<Direct "Array.prototype.splice.apply($arr, [$index, $howMany].concat($items))">]
let splice (arr: 'T []) (index: int) (howMany: int) (items: 'T[]) : 'T [] = items

[<Name "WebSharper.Collections.ListEnumerator">]
[<Proxy(typeof<System.Collections.Generic.List.Enumerator<_>>)>]
type ResizeArrayEnumeratorProxy<'T> [<JavaScript>] (arr: 'T[]) =
    let mutable i = -1

    [<JavaScript>] 
    member this.MoveNext() =
        i <- i + 1
        i < arr.Length

    [<JavaScript>] 
    member this.Current with get() = arr.[i]

    interface System.Collections.IEnumerator with
        [<JavaScript>] 
        member this.MoveNext() = this.MoveNext()
        [<JavaScript>]
        member this.Current with get() = box (arr.[i])
        member this.Reset() = failwith "IEnumerator.Reset not supported"

    interface System.Collections.Generic.IEnumerator<'T> with
        [<JavaScript>]
        member this.Current with get() = arr.[i]

    interface System.IDisposable with
        [<JavaScript>] 
        member this.Dispose() = ()

[<Proxy(typeof<System.Collections.Generic.List<_>>)>]
[<Name "WebSharper.Collections.List">]
type ResizeArrayProxy<'T> [<JavaScript>] (arr: 'T []) =

    new () =
        new ResizeArrayProxy<'T>([||])

    new (size: int) =
        new ResizeArrayProxy<'T>([||])

    new (el: seq<'T>) =
        new ResizeArrayProxy<'T>(Seq.toArray el)

    [<Inline>]
    member this.GetEnumerator() =
        As<System.Collections.Generic.List.Enumerator<'T>>(new ResizeArrayEnumeratorProxy<'T>(arr))

    interface 'T seq with
        member this.GetEnumerator() = (As<System.Collections.IEnumerable> arr).GetEnumerator()
        member this.GetEnumerator() = (As<seq<'T>> arr).GetEnumerator()

    member this.Add(x: 'T) : unit =
        push arr x

    member this.AddRange(x: seq<'T>) : unit =
        Seq.iter this.Add x

    member this.Clear() : unit =
        splice arr 0 arr.Length [||] |> ignore

    member this.CopyTo(arr: 'T[]) : unit =
        this.CopyTo(arr, 0)

    member this.CopyTo(arr: 'T[], offset: int) : unit =
        this.CopyTo(0, arr, offset, this.Count)

    member this.CopyTo(index: int, target: 'T[], offset: int, count: int) : unit =
        Array.blit arr index target offset count

    member this.Count : int = arr.Length

    member this.GetRange(index: int, count: int) : ResizeArray<'T> =
        As (ResizeArrayProxy<'T>(Array.sub arr index count))

    member this.Insert(index: int, items: 'T) : unit =
        splice arr index 0 [| items |] |> ignore

    member this.InsertRange(index: int, items: seq<'T>) : unit =
        splice arr index 0 (Seq.toArray items) |> ignore

    member this.Item
        with [<JavaScript>] get (x: int) : 'T = arr.[x]
        and [<JavaScript>] set (x: int) (v: 'T) = arr.[x] <- v

    member this.RemoveAt(x: int) : unit =
        splice arr x 1 [||] |> ignore

    member this.RemoveRange(index: int, count: int) : unit =
        splice arr index count [||] |> ignore

    member this.Reverse() : unit =
        System.Array.Reverse(arr)

    member this.Reverse(index: int, count: int) : unit =
        System.Array.Reverse(arr, index, count)

    member this.ToArray() : 'T [] =
        Array.copy arr

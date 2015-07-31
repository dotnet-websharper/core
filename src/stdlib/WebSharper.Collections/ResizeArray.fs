// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

[<WebSharper.Pervasives.ProxyAttribute(typeof<System.Collections.Generic.List<_>>)>]
type ResizeArrayProxy<'T> [<JavaScript>] (arr: 'T []) =

    [<JavaScript>]
    new () =
        new ResizeArrayProxy<'T>([||])

    [<JavaScript>]
    new (size: int) =
        new ResizeArrayProxy<'T>([||])

    [<JavaScript>]
    new (el: seq<'T>) =
        new ResizeArrayProxy<'T>(Seq.toArray el)

    [<JavaScript>]
    member this.GetEnumerator() =
        (As<seq<obj>> arr).GetEnumerator()

    [<JavaScript>]
    member this.Add(x: 'T) : unit =
        push arr x

    [<JavaScript>]
    member this.AddRange(x: seq<'T>) : unit =
        Seq.iter this.Add x

    [<JavaScript>]
    member this.Clear() : unit =
        splice arr 0 arr.Length [||] |> ignore

    [<JavaScript>]
    member this.CopyTo(arr: 'T[]) : unit =
        this.CopyTo(arr, 0)

    [<JavaScript>]
    member this.CopyTo(arr: 'T[], offset: int) : unit =
        this.CopyTo(0, arr, offset, this.Count)

    [<JavaScript>]
    member this.CopyTo(index: int, target: 'T[], offset: int, count: int) : unit =
        Array.blit arr index target offset count

    [<JavaScript>]
    member this.Count : int = arr.Length

    [<JavaScript>]
    member this.GetRange(index: int, count: int) : ResizeArray<'T> =
        As (ResizeArrayProxy<'T>(Array.sub arr index count))

    [<JavaScript>]
    member this.Insert(index: int, items: 'T) : unit =
        splice arr index 0 [| items |] |> ignore

    [<JavaScript>]
    member this.InsertRange(index: int, items: seq<'T>) : unit =
        splice arr index 0 (Seq.toArray items) |> ignore

    member this.Item
        with [<JavaScript>] get (x: int) : 'T = arr.[x]
        and [<JavaScript>] set (x: int) (v: 'T) = arr.[x] <- v

    [<JavaScript>]
    member this.RemoveAt(x: int) : unit =
        splice arr x 1 [||] |> ignore

    [<JavaScript>]
    member this.RemoveRange(index: int, count: int) : unit =
        splice arr index count [||] |> ignore

    [<JavaScript>]
    member this.Reverse() : unit =
        System.Array.Reverse(arr)

    [<JavaScript>]
    member this.Reverse(index: int, count: int) : unit =
        System.Array.Reverse(arr, index, count)

    [<JavaScript>]
    member this.ToArray() : 'T [] =
        Array.copy arr

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module private IntelliFactory.WebSharper.Collections.ResizeArray

open IntelliFactory.WebSharper

[<Inline "$arr.push($x)">]
let push (arr: 'T []) (x: 'T) = ()

[<Direct "Array.prototype.splice.apply($arr, [$index, $howMany].concat($items))">]
let splice (arr: 'T []) (index: int) (howMany: int) (items: 'T[]) : 'T [] = items

[<IntelliFactory.WebSharper.Pervasives.ProxyAttribute(typeof<System.Collections.Generic.List<_>>)>]
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

    [<JavaScript>]
    member this.Item with get (x: int) : 'T = arr.[x]

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

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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

open WebSharper.JavaScript

type private Index = System.Index
type private Range = System.Range

/// `System.Index` is a number: `n` for an index from the start, `~n` (that is `-n - 1`)
/// for `^n`, the same encoding .NET uses internally.
[<Proxy(typeof<Index>)>]
[<Prototype(false)>]
[<Type "number">]
type private IndexProxy =

    [<Inline "0">]
    new () = {}

    [<Inline "$fromEnd ? ~$value : $value">]
    new (value: int, fromEnd: bool) = {}

    static member Start
        with [<Inline "0">] get () = X<Index>

    static member End
        with [<Inline "-1">] get () = X<Index>

    [<Inline "$value">]
    static member FromStart(value: int) = X<Index>

    [<Inline "~$value">]
    static member FromEnd(value: int) = X<Index>

    [<Inline "$value">]
    static member op_Implicit(value: int) = X<Index>

    member this.Value
        with [<Inline>] get () =
            let i = As<int> this
            if i < 0 then ~~~i else i

    member this.IsFromEnd
        with [<Inline "$this < 0">] get () = X<bool>

    [<Inline>]
    member this.GetOffset(length: int) =
        let i = As<int> this
        if i < 0 then length + i + 1 else i

    [<Inline "$this === $other">]
    member this.Equals(other: Index) = X<bool>

    [<Inline>]
    override this.Equals(other: obj) = As<obj> this ===. other

    [<Inline "$this">]
    override this.GetHashCode() = X<int>

    [<Inline>]
    override this.ToString() =
        let i = As<int> this
        if i < 0 then "^" + string (~~~i) else string i

/// `System.Range` is an array of its start and end, as `Index` values.
[<Proxy(typeof<Range>)>]
[<Prototype(false)>]
[<Type "[number, number]">]
type private RangeProxy =

    [<Inline "[$start, $finish]">]
    new (start: Index, finish: Index) = {}

    static member All
        with [<Inline "[0, -1]">] get () = X<Range>

    [<Inline "[$start, -1]">]
    static member StartAt(start: Index) = X<Range>

    [<Inline "[0, $finish]">]
    static member EndAt(finish: Index) = X<Range>

    member this.Start
        with [<Inline "$this[0]">] get () = X<Index>

    member this.End
        with [<Inline "$this[1]">] get () = X<Index>

    /// The start offset and length of the range in a collection of the given length.
    [<JavaScript>]
    member this.GetOffsetAndLength(length: int) : struct (int * int) =
        let s = this.Start.GetOffset(length)
        let e = this.End.GetOffset(length)
        if s < 0 || e > length || s > e then
            raise (System.ArgumentOutOfRangeException("length"))
        struct (s, e - s)

    [<Inline>]
    member this.Equals(other: Range) = Unchecked.equals (As<obj> this) (As<obj> other)

    [<Inline>]
    override this.Equals(other: obj) = Unchecked.equals (As<obj> this) other

    [<Inline>]
    override this.GetHashCode() = Unchecked.hash (As<obj> this)

    [<Inline>]
    override this.ToString() = this.Start.ToString() + ".." + this.End.ToString()

[<Proxy(typeof<System.Runtime.CompilerServices.RuntimeHelpers>)>]
type private CompilerServicesRuntimeHelpersProxy =

    /// Used by C# for ranges of arrays: `a[1..^1]`
    [<JavaScript>]
    static member GetSubArray<'T>(array: 'T[], range: Range) : 'T[] =
        let struct (offset, length) = range.GetOffsetAndLength(array.Length)
        Array.sub array offset length

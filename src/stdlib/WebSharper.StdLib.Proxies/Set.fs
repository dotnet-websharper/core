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

namespace WebSharper.Collections

open System.Collections
open System.Collections.Generic
open WebSharper
open WebSharper.JavaScript
module T = BalancedTree

/// Implements a proxy for the F# Set type.
[<Name "FSharpSet">]
[<Proxy(typeof<Set<_>>)>]
type private FSharpSet<'T when 'T : comparison>

    internal (tree: T.Tree<'T>) =

        new (s: seq<'T>) = new FSharpSet<'T>(T.OfSeq s)

        new () = new FSharpSet<'T>(T.Empty)

        member this.add(x: Set<'T>) =
            Set.union (As this) x

        member this.sub(x: Set<'T>) =
            Set.difference (As this) x

        member this.Add x : Set<'T> =
            As (FSharpSet<'T>(T.Add x tree))

        member this.Contains v = T.Contains v tree

        [<Name("Count")>]
        member this.Count = T.Count tree

        member this.IsEmpty = T.IsEmpty tree

        member internal this.Tree = tree

        member this.IsProperSubsetOf(s: Set<'T>) =
            this.IsSubsetOf s && this.Count < s.Count

        member this.IsProperSupersetOf(s: Set<'T>) =
            this.IsSupersetOf s && this.Count > s.Count

        member this.IsSubsetOf(s: Set<'T>) =
            Seq.forall s.Contains this

        member this.IsSupersetOf(s: Set<'T>) =
            Seq.forall this.Contains s

        member this.MaximumElement = Seq.head (T.Descend tree)

        member this.MinimumElement = Seq.head (T.Ascend tree)

        member this.Remove v : Set<'T> =
            As (FSharpSet<'T>(T.Remove v tree))

        [<Name("GetEnumerator")>]
        member this.GetEnumerator() =
            (T.Ascend tree).GetEnumerator()

        static member (+) (x, y) : Set<'T> =
            Set.union x y        

        static member (-) (x, y) : Set<'T> =
            Set.difference x y        

        override this.GetHashCode() =
            -1741749453 + ((Seq.toArray this).GetHashCode())

        override this.Equals(other: obj) =
            this.Count = (As<FSharpSet<'T>> other).Count
            && Seq.forall2 ( = ) this (As<FSharpSet<'T>> other)

        interface IEnumerable with
            [<JavaScript(false)>]
            member this.GetEnumerator() = X<_>

        interface IEnumerable<'T> with
            [<JavaScript(false)>]
            member this.GetEnumerator() = X<_>

        interface System.IComparable with
            member this.CompareTo other =
                Seq.compareWith compare this (As<FSharpSet<'T>> other)

        interface ICollection<'T> with
            member this.IsReadOnly = true
            [<JavaScript(false)>]
            member this.Count = X<int>  
            member this.Add(p) = failwith "Set values cannot be mutated."
            member this.Clear() = failwith "Set values cannot be mutated."
            [<JavaScript(false)>]
            member this.Contains(p) = X<bool>
            member this.CopyTo(arr: 'T[], index: int) =
                (Seq.toArray this).CopyTo(arr, index)
            member this.Remove(p) = failwith "Set values cannot be mutated."

[<Proxy "System.ReadOnlySpan`1, netstandard">]
type private ReadOnlySpanProxy<'T> = class end
        
[<Proxy "Microsoft.FSharp.Collections.FSharpSet, FSharp.Core">]
type private SetProxy =
    static member Create (items: ReadOnlySpanProxy<'T>) : 'T Set =
        Set.ofArray (As items)   
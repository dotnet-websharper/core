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

namespace WebSharper.Collections

open System.Collections
open System.Collections.Generic
open WebSharper
open WebSharper.JavaScript
module T = BalancedTree

[<AutoOpen>]
[<JavaScript>]
module private SetUtil =

    let ofSeq(s: seq<_>) =
        let a = Seq.toArray s
        Array.sortInPlace a
        T.OfSorted a

/// Implements a proxy for the F# Set type.
[<Proxy(typeof<Set<_>>)>]
type private FSharpSet<'T when 'T : comparison>

    internal (tree: T.Tree<'T>) =

        new (s: seq<'T>) = new FSharpSet<'T>(ofSeq s)

        member this.add(x: Set<'T>) =
            Set.union (As this) x

        member this.sub(x: Set<'T>) =
            Set.difference (As this) x

        member this.Add x : Set<'T> =
            As (FSharpSet<'T>(T.Add x tree))

        member this.Contains v = T.Contains v tree

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

        member this.GetEnumerator() =
            (T.Ascend tree).GetEnumerator()

        override this.GetHashCode() =
            -1741749453 + ((Seq.toArray this).GetHashCode())

        override this.Equals(other: obj) =
            this.Count = (As<FSharpSet<'T>> other).Count
            && Seq.forall2 ( = ) this (As<FSharpSet<'T>> other)

        interface IEnumerable with
            member this.GetEnumerator() = this.GetEnumerator() :> _

        interface IEnumerable<'T> with
            member this.GetEnumerator() = this.GetEnumerator()

        interface System.IComparable with
            member this.CompareTo other =
                Seq.compareWith compare this (As<FSharpSet<'T>> other)



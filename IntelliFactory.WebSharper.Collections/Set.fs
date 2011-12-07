// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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

namespace IntelliFactory.WebSharper.Collections

open System.Collections
open System.Collections.Generic
open IntelliFactory.WebSharper
module T = BalancedTree

[<AutoOpen>]
module private SetUtil =

    [<JavaScript>]
    let ofSeq(s: seq<_>) =
        let a = Seq.toArray s
        Array.sortInPlace a
        T.OfSorted a

/// Implements a proxy for the F# Set type.
[<Proxy(typeof<Set<_>>)>]
type private FSharpSet<'T when 'T : comparison>

    [<JavaScript>]
    internal (tree: T.Tree<'T>) =

        [<JavaScript>]
        new (s: seq<'T>) = new FSharpSet<'T>(ofSeq s)

        [<JavaScript>]
        member this.Add x : Set<'T> =
            As (FSharpSet<'T>(T.Add x tree))

        [<JavaScript>]
        member this.Contains v = T.Contains v tree

        [<JavaScript>]
        member this.Count = T.Count tree

        [<JavaScript>]
        member this.IsEmpty = T.IsEmpty tree

        [<JavaScript>]
        member internal this.Tree = tree

        [<JavaScript>]
        member this.IsProperSubsetOf(s: Set<'T>) =
            this.IsSubsetOf s && this.Count < s.Count

        [<JavaScript>]
        member this.IsProperSupersetOf(s: Set<'T>) =
            this.IsSupersetOf s && this.Count > s.Count

        [<JavaScript>]
        member this.IsSubsetOf(s: Set<'T>) =
            Seq.forall s.Contains this

        [<JavaScript>]
        member this.IsSupersetOf(s: Set<'T>) =
            Seq.forall this.Contains s

        [<JavaScript>]
        member this.MaximumElement = Seq.head (T.Descend tree)

        [<JavaScript>]
        member this.MinimumElement = Seq.head (T.Ascend tree)

        [<JavaScript>]
        member this.Remove v : Set<'T> =
            As (FSharpSet<'T>(T.Remove v tree))

        [<JavaScript>]
        member this.GetEnumerator() =
            (T.Ascend tree).GetEnumerator()

        [<JavaScript>]
        override this.GetHashCode() =
            -1741749453 + ((Seq.toArray this).GetHashCode())

        [<JavaScript>]
        override this.Equals(other: obj) =
            this.Count = (As<FSharpSet<'T>> other).Count
            && Seq.forall2 ( = ) this (As<FSharpSet<'T>> other)

        interface IEnumerable with
            member this.GetEnumerator() = X<_>

        interface IEnumerable<'T> with
            member this.GetEnumerator() = X<_>

        interface System.IComparable with
            [<JavaScript>]
            member this.CompareTo other =
                Seq.compareWith compare this (As<FSharpSet<'T>> other)



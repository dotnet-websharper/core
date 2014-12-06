// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Collections

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JavaScript

/// Implements a proxy for the F# Set module.
[<Proxy "Microsoft.FSharp.Collections.SetModule, \
    FSharp.Core, Culture=neutral, \
    PublicKeyToken=b03f5f7f11d50a3a">]
module internal SetModule =
    module T = BalancedTree

    [<Inline>]
    [<JavaScript>]
    let private ToTree (s: Set<'T>) =
        (As<FSharpSet<'T>> s).Tree

    [<Inline>]
    [<JavaScript>]
    let private OfTree (t: T.Tree<'T>) =
        As<Set<'T>> (new FSharpSet<'T>(t))

    [<Inline>]
    [<JavaScript>]
    let Add v (s: Set<_>) = s.Add v

    [<Inline>]
    [<JavaScript>]
    let Contains v (s: Set<_>) = s.Contains v

    [<Inline>]
    [<JavaScript>]
    let Count (s: Set<_>) = s.Count

    [<Inline>]
    [<JavaScript>]
    let Difference (s1: Set<_>) (s2: Set<_>) =
        Set.filter (fun x -> not (s2.Contains x)) s1

    [<Inline>]
    [<JavaScript>]
    let Empty<'T when 'T : comparison> : Set<'T> = OfTree T.Empty

    [<Inline>]
    [<JavaScript>]
    let Exists f (s: Set<'T>) = Seq.exists f s

    [<JavaScript>]
    let Filter f (s: Set<'T>) =
        OfTree (T.OfSorted (Seq.toArray (Seq.filter f s)))

    [<Inline>]
    [<JavaScript>]
    let Fold<'T,'S when 'T : comparison>
        (f: 'S -> 'T -> 'S) (x: 'S) (a: Set<'T>) =
            Seq.fold f x a

    [<JavaScript>]
    let FoldBack (f: 'T -> 'S -> 'S) (a: Set<'T>) (s: 'S) : 'S =
        Seq.fold (fun s x -> f x s) s (T.Descend (ToTree a))

    [<Inline>]
    [<JavaScript>]
    let ForAll f (a: Set<_>) = Seq.forall f a

    [<Inline>]
    [<JavaScript>]
    let Intersect (s1: Set<'T>) (s2: Set<'T>) = Set.filter s2.Contains s1

    [<Inline>]
    [<JavaScript>]
    let IntersectMany (s: seq<Set<_>>) = Seq.reduce Set.intersect s

    [<Inline>]
    [<JavaScript>]
    let IsEmpty (a: Set<_>) = a.IsEmpty

    [<Inline>]
    [<JavaScript>]
    let IsProperSubset (a: Set<_>) b = a.IsProperSubsetOf b

    [<Inline>]
    [<JavaScript>]
    let IsProperSuperset (a: Set<_>) b = a.IsProperSupersetOf b

    [<Inline>]
    [<JavaScript>]
    let IsSubset (a: Set<_>) b = a.IsSubsetOf b

    [<Inline>]
    [<JavaScript>]
    let IsSuperset (a: Set<_>) b = a.IsSupersetOf b

    [<Inline>]
    [<JavaScript>]
    let Iterate f (s: Set<_>) = Seq.iter f s

    [<Inline>]
    [<JavaScript>]
    let Map f (s: Set<_>) = Set.ofSeq (Seq.map f s)

    [<Inline>]
    [<JavaScript>]
    let MaxElement (s: Set<_>) = s.MaximumElement

    [<Inline>]
    [<JavaScript>]
    let MinElement (s: Set<_>) = s.MinimumElement

    [<Inline>]
    [<JavaScript>]
    let OfArray (a: 'T []) = OfTree (T.OfSeq a)

    [<Inline>]
    [<JavaScript>]
    let OfList (a: list<'T>) = OfTree (T.OfSeq a)

    [<Inline>]
    [<JavaScript>]
    let OfSeq (a: seq<'T>) = OfTree (T.OfSeq a)

    [<JavaScript>]
    let Partition f (a: Set<_>) =
        let (x, y) = Array.partition f (Seq.toArray a)
        (Set.ofArray x, Set.ofArray y)

    [<Inline>]
    [<JavaScript>]
    let Remove v (a: Set<_>) = a.Remove v

    [<Inline>]
    [<JavaScript>]
    let Singleton x = Set.add x Set.empty

    [<Inline>]
    [<JavaScript>]
    let ToArray (a: Set<_>) = Seq.toArray a

    [<Inline>]
    [<JavaScript>]
    let ToList (a: Set<_>) = Seq.toList a

    [<Inline>]
    [<JavaScript>]
    let ToSeq (a: Set<_>) : seq<_> = a :> _

    [<Inline>]
    [<JavaScript>]
    let Union (s1: Set<_>) (s2: Set<_>) =
        Set.ofSeq (Seq.append s1 s2)

    [<Inline>]
    [<JavaScript>]
    let UnionMany (sets: seq<Set<_>>) =
        Set.ofSeq (Seq.concat sets)






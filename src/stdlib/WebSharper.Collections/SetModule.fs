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

open WebSharper
open WebSharper.JavaScript

/// Implements a proxy for the F# Set module.
[<Proxy "Microsoft.FSharp.Collections.SetModule, \
    FSharp.Core, Culture=neutral, \
    PublicKeyToken=b03f5f7f11d50a3a">]
[<Name "Set">]
module internal SetModule =
    module T = BalancedTree

    [<Inline>]
    let private ToTree (s: Set<'T>) =
        (As<FSharpSet<'T>> s).Tree

    [<Inline>]
    let private OfTree (t: T.Tree<'T>) =
        As<Set<'T>> (new FSharpSet<'T>(t))

    [<Inline>]
    let Add v (s: Set<_>) = s.Add v

    [<Inline>]
    let Contains v (s: Set<_>) = s.Contains v

    [<Inline>]
    let Count (s: Set<_>) = s.Count

    [<Inline>]
    let Difference (s1: Set<_>) (s2: Set<_>) =
        Set.filter (fun x -> not (s2.Contains x)) s1

    [<Inline>]
    let Empty<'T when 'T : comparison> : Set<'T> = OfTree T.Empty

    [<Inline>]
    let Exists f (s: Set<'T>) = Seq.exists f s

    let Filter f (s: Set<'T>) =
        OfTree (T.OfSorted (Seq.toArray (Seq.filter f s)))

    [<Inline>]
    let Fold<'T,'S when 'T : comparison>
        (f: 'S -> 'T -> 'S) (x: 'S) (a: Set<'T>) =
            Seq.fold f x a

    let FoldBack (f: 'T -> 'S -> 'S) (a: Set<'T>) (s: 'S) : 'S =
        Seq.fold (fun s x -> f x s) s (T.Descend (ToTree a))

    [<Inline>]
    let ForAll f (a: Set<_>) = Seq.forall f a

    [<Inline>]
    let Intersect (s1: Set<'T>) (s2: Set<'T>) = Set.filter s2.Contains s1

    [<Inline>]
    let IntersectMany (s: seq<Set<_>>) = Seq.reduce Set.intersect s

    [<Inline>]
    let IsEmpty (a: Set<_>) = a.IsEmpty

    [<Inline>]
    let IsProperSubset (a: Set<_>) b = a.IsProperSubsetOf b

    [<Inline>]
    let IsProperSuperset (a: Set<_>) b = a.IsProperSupersetOf b

    [<Inline>]
    let IsSubset (a: Set<_>) b = a.IsSubsetOf b

    [<Inline>]
    let IsSuperset (a: Set<_>) b = a.IsSupersetOf b

    [<Inline>]
    let Iterate f (s: Set<_>) = Seq.iter f s

    [<Inline>]
    let Map f (s: Set<_>) = Set.ofSeq (Seq.map f s)

    [<Inline>]
    let MaxElement (s: Set<_>) = s.MaximumElement

    [<Inline>]
    let MinElement (s: Set<_>) = s.MinimumElement

    [<Inline>]
    let OfArray (a: 'T []) = OfTree (T.OfSeq a)

    [<Inline>]
    let OfList (a: list<'T>) = OfTree (T.OfSeq a)

    [<Inline>]
    let OfSeq (a: seq<'T>) = OfTree (T.OfSeq a)

    let Partition f (a: Set<_>) =
        let (x, y) = Array.partition f (Seq.toArray a)
        (Set.ofArray x, Set.ofArray y)

    [<Inline>]
    let Remove v (a: Set<_>) = a.Remove v

    [<Inline>]
    let Singleton x = Set.add x Set.empty

    [<Inline>]
    let ToArray (a: Set<_>) = Seq.toArray a

    [<Inline>]
    let ToList (a: Set<_>) = Seq.toList a

    [<Inline>]
    let ToSeq (a: Set<_>) : seq<_> = a :> _

    [<Inline>]
    let Union (s1: Set<_>) (s2: Set<_>) =
        Set.ofSeq (Seq.append s1 s2)

    [<Inline>]
    let UnionMany (sets: seq<Set<_>>) =
        Set.ofSeq (Seq.concat sets)






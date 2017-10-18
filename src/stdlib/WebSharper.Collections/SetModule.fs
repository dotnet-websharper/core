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

open WebSharper
open WebSharper.JavaScript

/// Implements a proxy for the F# Set module.
[<Proxy "Microsoft.FSharp.Collections.SetModule, \
    FSharp.Core, Culture=neutral, \
    PublicKeyToken=b03f5f7f11d50a3a">]
[<Name "Set">]
module private SetModule =
    module T = BalancedTree

    [<Inline>]
    let Add v (s: FSharpSet<_>) = s.Add v

    [<Inline>]
    let Contains v (s: FSharpSet<_>) = s.Contains v

    [<Inline>]
    let Count (s: FSharpSet<_>) = s.Count

    let Filter f (s: FSharpSet<'T>) =
        FSharpSet(T.OfSorted (Seq.toArray (Seq.filter f s)))

    [<Inline>]
    let Difference (s1: FSharpSet<_>) (s2: FSharpSet<_>) =
        Filter (fun x -> not (s2.Contains x)) s1

    [<Inline>]
    let Empty<'T when 'T : comparison> = FSharpSet<'T>(T.Empty)

    [<Inline>]
    let Exists f (s: FSharpSet<'T>) = Seq.exists f s

    [<Inline>]
    let Fold<'T,'S when 'T : comparison>
        (f: 'S -> 'T -> 'S) (x: 'S) (a: FSharpSet<'T>) =
            Seq.fold f x a

    let FoldBack (f: 'T -> 'S -> 'S) (a: FSharpSet<'T>) (s: 'S) : 'S =
        Seq.fold (fun s x -> f x s) s (T.Descend a.Tree)

    [<Inline>]
    let ForAll f (a: FSharpSet<_>) = Seq.forall f a

    [<Inline>]
    let Intersect (s1: FSharpSet<'T>) (s2: FSharpSet<'T>) = Filter s2.Contains s1

    [<Inline>]
    let IntersectMany (s: seq<FSharpSet<_>>) = Seq.reduce Intersect s

    [<Inline>]
    let IsEmpty (a: FSharpSet<_>) = a.IsEmpty

    [<Inline>]
    let IsProperSubset (a: FSharpSet<_>) b = a.IsProperSubsetOf b

    [<Inline>]
    let IsProperSuperset (a: FSharpSet<_>) b = a.IsProperSupersetOf b

    [<Inline>]
    let IsSubset (a: FSharpSet<_>) b = a.IsSubsetOf b

    [<Inline>]
    let IsSuperset (a: FSharpSet<_>) b = a.IsSupersetOf b

    [<Inline>]
    let Iterate f (s: FSharpSet<_>) = Seq.iter f s

    [<Inline>]
    let Map f (s: FSharpSet<_>) = Set.ofSeq (Seq.map f s)

    [<Inline>]
    let MaxElement (s: FSharpSet<_>) = s.MaximumElement

    [<Inline>]
    let MinElement (s: FSharpSet<_>) = s.MinimumElement

    [<Inline>]
    let OfArray (a: 'T []) = FSharpSet(T.OfSeq a)

    [<Inline>]
    let OfList (a: list<'T>) = FSharpSet(T.OfSeq a)

    [<Inline>]
    let OfSeq (a: seq<'T>) = FSharpSet(T.OfSeq a)

    let Partition f (a: FSharpSet<_>) =
        let (x, y) = Array.partition f (Seq.toArray a)
        (OfArray x, OfArray y)

    [<Inline>]
    let Remove v (a: FSharpSet<_>) = a.Remove v

    [<Inline>]
    let Singleton x = Add x Empty

    [<Inline>]
    let ToArray (a: FSharpSet<_>) = Seq.toArray a

    [<Inline>]
    let ToList (a: FSharpSet<_>) = Seq.toList a

    [<Inline>]
    let ToSeq (a: FSharpSet<_>) : seq<_> = a :> _

    [<Inline>]
    let Union (s1: FSharpSet<_>) (s2: Set<_>) =
        OfSeq (Seq.append s1 s2)

    [<Inline>]
    let UnionMany (sets: seq<FSharpSet<_>>) =
        OfSeq (Seq.concat sets)






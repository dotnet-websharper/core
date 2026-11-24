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

open System
open System.Linq
open System.Collections
open System.Collections.Generic
open WebSharper.Core
open WebSharper.JavaScript

[<Proxy(typeof<IGrouping<_, _>>)>]
type internal IGroupingProxy<'K, 'T> =
    abstract Key : 'K

[<JavaScript>]
type internal Grouping<'K, 'T> (k: 'K, v: ResizeArray<'T>) =

    interface seq<'T> with
        member this.GetEnumerator() =
            v.GetEnumerator()

    interface System.Collections.IEnumerable with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<_>

    interface System.Linq.IGrouping<'K, 'T> with
        member this.Key = k

    [<Inline>]
    member this.AsArray() = As<'T[]> v

[<Proxy(typeof<IOrderedEnumerable<_>>)>]
type internal IOrderedEnumerableProxy<'T> =
    inherit seq<'T>
    abstract CreateOrderedEnumerable<'K>
        : keySelector: Func<'T, 'K>
        * comparer: IComparer<'K>
        * descending: bool
        -> IOrderedEnumerable<'T>

[<JavaScript>]
type internal FsComparer<'T when 'T : comparison>() =
    interface IComparer<'T> with
        member this.Compare(x, y) =
            compare x y

[<JavaScript>]
type internal ProjectionComparer<'T, 'K>(primary: IComparer<'K>, projection: Func<'T, 'K>) =
    interface IComparer<'T> with
        member this.Compare(x, y) =
            primary.Compare(projection.Invoke x, projection.Invoke y)

[<JavaScript>]
type internal CompoundComparer<'T>(primary: IComparer<'T>, secondary: IComparer<'T>) =
    interface IComparer<'T> with
        member this.Compare(x, y) =
            match primary.Compare(x, y) with
            | 0 -> secondary.Compare(x, y)
            | n -> n

[<JavaScript>]
type internal ReverseComparer<'T, 'K>(primary: IComparer<'K>, projection: Func<'T, 'K>) =
    interface IComparer<'T> with
        member this.Compare(x, y) =
            primary.Compare(projection.Invoke y, projection.Invoke x)

[<JavaScript>]
type internal OrderedEnumerable<'T>(source: seq<'T>, primary: IComparer<'T>) =
    interface IOrderedEnumerable<'T> with
        member this.CreateOrderedEnumerable(keySelector, secondary, descending) =
            let secondary =
                if descending then
                    ReverseComparer(secondary, keySelector) :> IComparer<'T>
                else
                    ProjectionComparer(secondary, keySelector) :> IComparer<'T>
            OrderedEnumerable<'T>(source, CompoundComparer(primary, secondary)) :> _
    interface seq<'T> with
        member this.GetEnumerator() =
            let a = Array.ofSeq source
            Array.sortInPlaceWith (fun x y -> primary.Compare(x, y)) a
            (a :> seq<'T>).GetEnumerator()
    interface IEnumerable with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<_>

[<Proxy(typeof<ILookup<_, _>>)>]
type internal ILookupProxy<'K, 'E> =
    inherit seq<IGrouping<'K, 'E>>
    abstract Item : 'K -> seq<'E> with get
    abstract Count : int with get
    abstract Contains : 'K -> bool

/// ToLookup's result: the groups in first-occurrence order, an empty sequence for a missing key
[<JavaScript>]
type internal Lookup<'K, 'E>(d: Dictionary<'K, ResizeArray<'E>>) =

    interface seq<IGrouping<'K, 'E>> with
        member this.GetEnumerator() =
            (d |> Seq.map (fun kv -> Grouping(kv.Key, kv.Value) :> IGrouping<_, _>)).GetEnumerator()

    interface IEnumerable with
        [<JavaScript(false)>]
        member this.GetEnumerator() = X<_>

    interface ILookup<'K, 'E> with
        member this.Item with get k = if d.ContainsKey k then d.[k] :> seq<'E> else Seq.empty
        member this.Count = d.Count
        member this.Contains(k) = d.ContainsKey k

type private LinqMacro() =
    inherit Macro()

    override this.TranslateCall(c) = //e, t, m, a, _) =
        let targ = c.Method.Generics.[0]
        WebSharper.Core.Macros.EqualityComparer.GetDefault(c.Compilation, targ)
        |> MacroResult.Map (fun ec ->
            let m' =
                let t =
                    AST.Type.ConcreteType {
                        Generics = [targ]
                        Entity =
                            typedefof<IEqualityComparer<int>>
                            |> AST.Reflection.ReadTypeDefinition
                    }
                let m = c.Method.Entity.Value
                { m with Parameters = m.Parameters @ [t] }
            let m = { c.Method with Entity = Hashed m' }
            AST.Call(c.This, c.DefiningType, c.Method, c.Arguments @ [ec]))

/// LINQ's empty-sequence errors are InvalidOperationException in .NET
[<JavaScript>]
module internal LinqErrors =
    let noElements () = raise (InvalidOperationException "Sequence contains no elements")
    let nonEmpty (s: seq<'T>) : 'T[] =
        let a = Array.ofSeq s
        if a.Length = 0 then noElements () else a

[<Name "WebSharper.Linq">]
[<Proxy(typeof<System.Linq.Enumerable>)>]
[<JavaScript>]
type private LinqProxy =

    [<Inline>]
    static member Chunk<'T>(source: seq<'T>, size: int) : seq<'T[]> =
        if isNull source then raise (ArgumentNullException("source"))
        if size < 1 then raise (ArgumentOutOfRangeException("size"))
        Seq.chunkBySize size source

    [<Inline>]
    static member Aggregate<'T>(this: seq<'T>, func: Func<'T, 'T, 'T>) : 'T =
        Seq.reduce (fun x y -> func.Invoke(x, y)) this

    [<Inline>]
    static member Aggregate<'T, 'U>(this: seq<'T>, seed: 'U, func: Func<'U, 'T, 'U>) : 'U =
        Seq.fold (fun x y -> func.Invoke(x, y)) seed this

    [<Inline>]
    static member Aggregate<'T, 'U, 'R>(this: seq<'T>, seed: 'U, func: Func<'U, 'T, 'U>, resultSelector: Func<'U, 'R>) : 'R =
        resultSelector.Invoke(Seq.fold (fun x y -> func.Invoke(x, y)) seed this)

    [<Name "AggregateBy">]
    static member AggregateByImpl<'T,'K,'A>(source: seq<'T>, keySelector: Func<'T, 'K>, seedSelector: Func<'K,'A>, func: Func<'A,'T,'A>, keyComparer: IEqualityComparer<'K>) : seq<KeyValuePair<'K,'A>> =
        source.GroupBy(keySelector, keyComparer)
        |> Seq.map (fun g ->
            KeyValuePair(g.Key, As<Grouping<'K, 'T>>(g).AsArray() |> Array.fold (fun x y -> func.Invoke(x, y)) (seedSelector.Invoke(g.Key)))
        )
    
    [<Inline>]
    static member AggregateBy<'T,'K,'A>(source: seq<'T>, keySelector: Func<'T, 'K>, seedSelector: Func<'K,'A>, func: Func<'A,'T,'A>, keyComparer: IEqualityComparer<'K>) : seq<KeyValuePair<'K,'A>> =
        let keyComparer = if isNull keyComparer then EqualityComparer.Default :> IEqualityComparer<'K> else keyComparer
        LinqProxy.AggregateByImpl(source, keySelector, seedSelector, func, keyComparer)

    [<Inline>]
    static member AggregateBy<'T,'K,'A>(source: seq<'T>, keySelector: Func<'T, 'K>, seed: 'A, func: Func<'A,'T,'A>, keyComparer: IEqualityComparer<'K>) : seq<KeyValuePair<'K,'A>> =
        let keyComparer = if isNull keyComparer then EqualityComparer.Default :> IEqualityComparer<'K> else keyComparer
        LinqProxy.AggregateByImpl(source, keySelector, (fun _ -> seed), func, keyComparer)

    [<Inline>]
    static member All<'T>(this: seq<'T>, predicate: Func<'T, bool>) : bool =
        Seq.forall predicate.Invoke this

    [<Inline>]
    static member Any<'T>(this: seq<'T>) : bool =
        not (Seq.isEmpty this)

    [<Inline>]
    static member Any<'T>(this: seq<'T>, predicate: Func<'T, bool>) : bool =
        Seq.exists predicate.Invoke this

    [<Inline>]
    static member AsEnumerable<'T>(this: seq<'T>) : seq<'T> =
        this

    [<Inline>]
    static member Average(this: seq<Nullable<int>>) : Nullable<float> =
        LinqProxy.Average(Seq.cast<Nullable<float>> this)

    [<Inline>]
    static member Average(this: seq<Nullable<int64>>) : Nullable<float> =
        LinqProxy.Average(Seq.cast<Nullable<float>> this)

    static member Average(this: seq<Nullable<float>>) : Nullable<float> =
        let mutable x = [||]
        use e = this.GetEnumerator()
        while e.MoveNext() do
            if e.Current.HasValue then
                x.JS.Push e.Current.Value |> ignore
        if x.Length = 0 then
            Nullable()
        else
            Nullable(Seq.sum x / float x.Length)

    [<Inline>]
    static member Average(this: seq<float>) : float =
        Seq.average (LinqErrors.nonEmpty this)

    [<Inline>]
    static member Average(this: seq<int64>) : float =
        Seq.average (LinqErrors.nonEmpty (Seq.cast<float> this))

    [<Inline>]
    static member Average(this: seq<int>) : float =
        Seq.average (LinqErrors.nonEmpty (Seq.cast<float> this))

    [<Inline>]
    static member Average<'T>(this: seq<'T>, selector: Func<'T, Nullable<int64>>) : Nullable<float> =
        LinqProxy.Average (Seq.cast<Nullable<float>> (Seq.map selector.Invoke this))

    [<Inline>]
    static member Average<'T>(this: seq<'T>, selector: Func<'T, int64>) : float =
        Seq.average (Seq.cast<float> (Seq.map selector.Invoke this))

    [<Inline>]
    static member Average<'T>(this: seq<'T>, selector: Func<'T, Nullable<int>>) : Nullable<float> =
        LinqProxy.Average (Seq.cast<Nullable<float>> (Seq.map selector.Invoke this))

    [<Inline>]
    static member Average<'T>(this: seq<'T>, selector: Func<'T, Nullable<float>>) : Nullable<float> =
        LinqProxy.Average (Seq.map selector.Invoke this)

    [<Inline>]
    static member Average<'T>(this: seq<'T>, selector: Func<'T, float>) : float =
        Seq.average (Seq.map selector.Invoke this)

    [<Inline>]
    static member Average<'T>(this: seq<'T>, selector: Func<'T, int>) : float =
        Seq.average (Seq.cast<float> (Seq.map selector.Invoke this))

    [<Inline>]
    static member Cast<'T>(this: IEnumerable) : seq<'T> =
        Seq.cast this

    /// Enumerable.Cast<T> for a T the C# compiler can test (numbers, bool, string, classes), see CodeReader:
    /// each item is checked when enumerated, null passes, and a value of another type throws InvalidCastException
    static member CastChecked(source: obj, test: obj, typeName: string) : obj =
        let test = As<obj -> bool> test
        box (Seq.cast<obj> (As<IEnumerable> source) |> Seq.map (fun x ->
            if x ==. null || test x then x
            else raise (System.InvalidCastException("Unable to cast object to type '" + typeName + "'."))))

    [<Inline>]
    static member Concat<'T>(this: seq<'T>, second: seq<'T>) : seq<'T> =
        Seq.append this second

    [<Inline>]
    static member Contains<'T>(this: seq<'T>, value: 'T) : bool =
        LinqProxy.Contains(this, value, EqualityComparer.Default)

    [<Inline>]
    static member Contains<'T>(this: seq<'T>, value: 'T, comparer: IEqualityComparer<'T>) : bool =
        Seq.exists (fun x -> comparer.Equals(value, x)) this

    [<Inline>]
    static member Count<'T>(this: seq<'T>) : int =
        Seq.length this

    [<Inline>]
    static member Count<'T>(this: seq<'T>, predicate: Func<'T, bool>) : int =
        Seq.length (Seq.filter predicate.Invoke this)

    [<Name "CountBy">]
    static member CountByImpl<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>, keyComparer: IEqualityComparer<'K>) : seq<KeyValuePair<'K, int>> =
        this.GroupBy(keySelector, keyComparer)
        |> Seq.map (fun g ->
            KeyValuePair(g.Key, As<Grouping<'K, 'T>>(g).AsArray().Length)
        )

    [<Inline>]
    static member CountBy<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>, keyComparer: IEqualityComparer<'K>) : seq<KeyValuePair<'K, int>> =
        let keyComparer = if isNull keyComparer then EqualityComparer.Default :> IEqualityComparer<'K> else keyComparer
        LinqProxy.CountByImpl(this, keySelector, keyComparer)

    [<Inline>]
    static member DefaultIfEmpty<'T>(this: seq<'T>) : seq<'T> =
        LinqProxy.DefaultIfEmpty(this, Unchecked.defaultof<'T>)

    static member DefaultIfEmpty<'T>(this: seq<'T>, defaultValue: 'T) : seq<'T> =
        if Seq.isEmpty this then
            Seq.singleton defaultValue
        else this

    [<Inline>]
    static member Distinct<'T>(this: seq<'T>) : seq<'T> =
        LinqProxy.Distinct(this, EqualityComparer.Default)

    static member Distinct<'T>(this: seq<'T>, comparer: IEqualityComparer<'T>) : seq<'T> =
        seq {
            use e = this.GetEnumerator()
            let tbl = HashSet(comparer)
            while e.MoveNext() do
                if tbl.Add(e.Current) then
                    yield e.Current
        }

    [<Inline>]
    static member ElementAt<'T>(this: seq<'T>, index: int): 'T =
        Seq.item index this

    [<Inline>]
    static member ElementAtOrDefault<'T>(this: seq<'T>, index: int) =
        LinqProxy.JSElementAtOrDefault(this, index, Unchecked.defaultof<'T>)

    [<Name "ElementAtOrDefault">]
    static member JSElementAtOrDefault<'T>(this: seq<'T>, index: int, defaultValue: 'T) : 'T =
        try Seq.item index this
        with _ -> defaultValue

    [<Inline>]
    static member Empty<'T>() : seq<'T> =
        Seq.empty

    [<Inline>]
    static member Except<'T>(this: seq<'T>, second: seq<'T>) : seq<'T> =
        LinqProxy.Except(this, second, EqualityComparer.Default)

    static member Except<'T>(this: seq<'T>, second: seq<'T>, comparer: IEqualityComparer<'T>) : seq<'T> =
        let tbl = HashSet(this, comparer)
        for x in second do tbl.Remove(x) |> ignore
        tbl :> _

    static member First<'T>(this: seq<'T>) : 'T =
        use e = this.GetEnumerator()
        if e.MoveNext() then e.Current else LinqErrors.noElements ()

    [<Inline>]
    static member First<'T>(this: seq<'T>, predicate: Func<'T, bool>) : 'T =
        Seq.find predicate.Invoke this

    [<Inline>]
    static member FirstOrDefault<'T>(this: seq<'T>) : 'T =
        LinqProxy.JSFirstOrDefault(this, Unchecked.defaultof<'T>)

    [<Name "FirstOrDefault">]
    static member JSFirstOrDefault<'T>(this: seq<'T>, defaultValue: 'T) : 'T =
        use e = this.GetEnumerator()
        if e.MoveNext() then e.Current else defaultValue

    [<Inline>]
    static member FirstOrDefault<'T>(this: seq<'T>, predicate: Func<'T, bool>) : 'T =
        LinqProxy.JSFirstOrDefault(this, predicate, Unchecked.defaultof<'T>)

    [<Name "FirstOrDefault$1">]
    static member JSFirstOrDefault<'T>(this: seq<'T>, predicate: Func<'T, bool>, defaultValue: 'T) : 'T =
        match Seq.tryFind predicate.Invoke this with
        | Some x -> x
        | None -> defaultValue

    [<Inline>]
    static member GroupBy<'T, 'K when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>) : seq<IGrouping<'K, 'T>> =
        LinqProxy.GroupBy(this, keySelector, EqualityComparer.Default)

    [<Inline>]
    static member GroupBy<'T, 'K when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>, comparer: IEqualityComparer<'K>) : seq<IGrouping<'K, 'T>> =
        LinqProxy.GroupBy(this, keySelector, (fun x -> x), comparer)

    [<Inline>]
    static member GroupBy<'T, 'K, 'R when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>, resultSelector: Func<'K, seq<'T>, 'R>) : seq<'R> =
        LinqProxy.GroupBy(this, keySelector, resultSelector, EqualityComparer.Default)

    [<Inline>]
    static member GroupBy<'T, 'K, 'E when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>, elementSelector: Func<'T, 'E>) : seq<IGrouping<'K, 'E>> =
        LinqProxy.GroupBy(this, keySelector, elementSelector, EqualityComparer.Default)

    static member GroupBy<'T, 'K, 'E when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>, elementSelector: Func<'T, 'E>, comparer: IEqualityComparer<'K>) : seq<IGrouping<'K, 'E>> =
        Seq.delay (fun () ->
            // Using an array instead of a seq is important here: the returned groupings
            // use a ResizeArray that is filled here, so this enumeration must be finished
            // before the user can enumerate any of the groupings.
            [|
                let t = Dictionary<'K, ResizeArray<'E>>(comparer)
                for x in this do
                    let k = keySelector.Invoke x
                    let e = elementSelector.Invoke x
                    match t.TryGetValue k with
                    | true, a -> a.Add(e)
                    | false, _ ->
                        let a = ResizeArray<'E>()
                        a.Add(e)
                        t.[k] <- a
                        yield Grouping(k, a) :> IGrouping<_,_>
            |] :> _
        )

    [<Inline>]
    static member GroupBy<'T, 'K, 'R when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>, resultSelector: Func<'K, seq<'T>, 'R>, comparer: IEqualityComparer<'K>) : seq<'R> =
        LinqProxy.GroupBy(this, keySelector, (fun x -> x), resultSelector, comparer)

    [<Inline>]
    static member GroupBy<'T, 'K, 'E, 'R when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>, elementSelector: Func<'T, 'E>, resultSelector: Func<'K, seq<'E>, 'R>) : seq<'R> =
        LinqProxy.GroupBy(this, keySelector, elementSelector, resultSelector, EqualityComparer.Default)

    static member GroupBy<'T, 'K, 'E, 'R when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>, elementSelector: Func<'T, 'E>, resultSelector: Func<'K, seq<'E>, 'R>, comparer: IEqualityComparer<'K>) : seq<'R> =
        LinqProxy.GroupBy(this, keySelector, elementSelector, comparer)
        |> Seq.map (fun g -> resultSelector.Invoke(g.Key, g))

    [<Inline>]
    static member GroupJoin<'O, 'I, 'K, 'R>(outer: seq<'O>, inner: seq<'I>, outerKeySelector: Func<'O, 'K>, innerKeySelector: Func<'I, 'K>, resultSelector: Func<'O, seq<'I>, 'R>) : seq<'R> =
        LinqProxy.GroupJoin(outer, inner, outerKeySelector, innerKeySelector, resultSelector, EqualityComparer.Default)

    static member GroupJoin<'O, 'I, 'K, 'R>(outer: seq<'O>, inner: seq<'I>, outerKeySelector: Func<'O, 'K>, innerKeySelector: Func<'I, 'K>, resultSelector: Func<'O, seq<'I>, 'R>, comparer: IEqualityComparer<'K>) : seq<'R> =
        Seq.delay (fun () ->
            let t = Dictionary<'K, 'O * ResizeArray<'I>>(comparer)
            let a = [|
                for o in outer do
                    let k = outerKeySelector.Invoke o
                    match t.TryGetValue k with
                    | true, _ -> ()
                    | false, _ ->
                        let pair = (o, ResizeArray())
                        t.Add(k, pair)
                        yield pair
            |]
            for i in inner do
                let k = innerKeySelector.Invoke i
                match t.TryGetValue k with
                | true, (_, a) -> a.Add(i)
                | false, _ -> ()
            a |> Array.iteri (fun i (o, is) ->
                a.[i] <- As (resultSelector.Invoke(o, is)))
            As a
        )

    [<Inline>]
    static member Index<'T>(this: seq<'T>) : seq<struct (int * 'T)> =
        this |> Seq.mapi (fun i x -> struct (i, x))

    [<Inline>]
    static member InfiniteSequence<'T when 'T :> System.Numerics.INumber<'T>>(start: 'T, step: 'T) : seq<'T> =
        Seq.unfold (fun s -> Some (s, s + step)) start

    [<Inline>]
    static member Intersect<'T>(this: seq<'T>, second: seq<'T>) : seq<'T> =
        LinqProxy.Intersect(this, second, EqualityComparer.Default)

    static member Intersect<'T>(this: seq<'T>, second: seq<'T>, comparer: IEqualityComparer<'T>) : seq<'T> =
        // in the order of the first sequence, like .NET
        seq {
            let t2 = HashSet(second, comparer)
            for x in this do
                if t2.Remove(x) then
                    yield x
        }

    [<Inline>]
    static member Join<'O, 'I, 'K, 'R>(outer: seq<'O>, inner: seq<'I>, outerKeySelector: Func<'O, 'K>, innerKeySelector: Func<'I, 'K>, resultSelector: Func<'O, 'I, 'R>) : IEnumerable<'R> =
        LinqProxy.Join(outer, inner, outerKeySelector, innerKeySelector, resultSelector, EqualityComparer.Default)

    static member Join<'O, 'I, 'K, 'R>(outer: seq<'O>, inner: seq<'I>, outerKeySelector: Func<'O, 'K>, innerKeySelector: Func<'I, 'K>, resultSelector: Func<'O, 'I, 'R>, comparer: IEqualityComparer<'K>) : IEnumerable<'R> =
        Seq.delay (fun () ->
            let t = Dictionary<'K, 'O * ResizeArray<'I>>(comparer)
            let a = [|
                for o in outer do
                    let k = outerKeySelector.Invoke o
                    match t.TryGetValue k with
                    | true, _ -> ()
                    | false, _ ->
                        let pair = (o, ResizeArray())
                        t.Add(k, pair)
                        yield pair
            |]
            for i in inner do
                let k = innerKeySelector.Invoke i
                match t.TryGetValue k with
                | true, (_, a) -> a.Add(i)
                | false, _ -> ()
            [|
                for (o, is) in a do
                    for i in is do
                        yield resultSelector.Invoke(o, i)
            |] :> _
        )

    static member Last<'T>(this: seq<'T>) : 'T =
        let a = LinqErrors.nonEmpty this
        a.[a.Length - 1]

    static member LastPred(this: seq<'T>, predicate: Func<'T, bool>) : option<'T> =
        (None, this)
        ||> Seq.fold (fun acc elt ->
            if predicate.Invoke elt then Some elt else acc)

    static member Last<'T>(this: seq<'T>, predicate: Func<'T, bool>) : 'T =
        match LinqProxy.LastPred(this, predicate) with
        | Some x -> x
        | None -> invalidOp "Sequence contains no matching element"

    [<Inline>]
    static member LastOrDefault<'T>(this: seq<'T>) : 'T =
        Enumerable.LastOrDefault(this, fun _ -> true)

    [<Inline>]
    static member LastOrDefault<'T>(this: seq<'T>, predicate: Func<'T, bool>) : 'T =
        LinqProxy.JSLastOrDefault(this, predicate, Unchecked.defaultof<'T>)

    [<Name "LastOrDefault">]
    static member JSLastOrDefault<'T>(this: seq<'T>, predicate: Func<'T, bool>, defaultValue: 'T) : 'T =
        match LinqProxy.LastPred(this, predicate) with
        | Some x -> x
        | None -> defaultValue

    [<Inline>]
    static member LongCount<'T>(this: seq<'T>) : int64 =
        int64 (Enumerable.Count(this))

    [<Inline>]
    static member LongCount<'T>(this: seq<'T>, predicate: Func<'T, bool>) : int64 =
        int64 (Enumerable.Count(this, predicate))

    [<Inline>]
    static member Max(this: seq<Nullable<int>>) : Nullable<int> =
        As(LinqProxy.Max(Seq.cast<Nullable<float>> this))

    [<Inline>]
    static member Max(this: seq<float>) : float =
        Seq.max (LinqErrors.nonEmpty this)

    [<Inline>]
    static member Max(this: seq<Nullable<int64>>) : Nullable<int64> =
        As(LinqProxy.Max(Seq.cast<Nullable<float>> this))

    [<Inline>]
    static member Max(this: seq<int64>) : int64 =
        Seq.max (LinqErrors.nonEmpty this)

    static member Max(this: seq<Nullable<float>>) : Nullable<float> =
        let s = this |> Seq.choose (fun x -> if x.HasValue then Some x.Value else None)
        if Seq.isEmpty s then Nullable() else Nullable(Seq.max s)

    [<Inline>]
    static member Max(this: seq<int>) : int =
        Seq.max (LinqErrors.nonEmpty this)

    [<Inline>]
    static member Max<'T when 'T : comparison>(this: seq<'T>) : 'T =
        Seq.max (LinqErrors.nonEmpty this)

    [<Inline>]
    static member Max<'T>(this: seq<'T>, selector: Func<'T, int64>) : int64 =
        LinqProxy.Max(Seq.map selector.Invoke this)

    [<Inline>]
    static member Max<'T>(this: seq<'T>, selector: Func<'T, float>) : float =
        LinqProxy.Max(Seq.map selector.Invoke this)

    [<Inline>]
    static member Max<'T>(this: seq<'T>, selector: Func<'T, Nullable<float>>) : Nullable<float> =
        LinqProxy.Max(Seq.map selector.Invoke this)

    [<Inline>]
    static member Max<'T>(this: seq<'T>, selector: Func<'T, Nullable<int64>>) : Nullable<int64> =
        LinqProxy.Max(Seq.map selector.Invoke this)

    [<Inline>]
    static member Max<'T>(this: seq<'T>, selector: Func<'T, Nullable<int>>) : Nullable<int> =
        LinqProxy.Max(Seq.map selector.Invoke this)

    [<Inline>]
    static member Max<'T>(this: seq<'T>, selector: Func<'T, int>) : int =
        LinqProxy.Max(Seq.map selector.Invoke this)

    [<Inline>]
    static member Max<'T, 'R when 'R : comparison>(this: seq<'T>, selector: Func<'T, 'R>) : 'R =
        LinqProxy.Max(Seq.map selector.Invoke this)

    [<Inline>]
    static member Min(this: seq<Nullable<int>>) : Nullable<int> =
        As(LinqProxy.Min(Seq.cast<Nullable<float>> this))

    [<Inline>]
    static member Min(this: seq<float>) : float =
        Seq.min (LinqErrors.nonEmpty this)

    [<Inline>]
    static member Min(this: seq<Nullable<int64>>) : Nullable<int64> =
        As(LinqProxy.Min(Seq.cast<Nullable<float>> this))

    [<Inline>]
    static member Min(this: seq<int64>) : int64 =
        Seq.min (LinqErrors.nonEmpty this)

    static member Min(this: seq<Nullable<float>>) : Nullable<float> =
        let s = this |> Seq.choose (fun x -> if x.HasValue then Some x.Value else None)
        if Seq.isEmpty s then Nullable() else Nullable(Seq.min s)

    [<Inline>]
    static member Min(this: seq<int>) : int =
        Seq.min (LinqErrors.nonEmpty this)

    [<Inline>]
    static member Min<'T when 'T : comparison>(this: seq<'T>) : 'T =
        Seq.min (LinqErrors.nonEmpty this)

    [<Inline>]
    static member Min<'T>(this: seq<'T>, selector: Func<'T, int64>) : int64 =
        LinqProxy.Min(Seq.map selector.Invoke this)

    [<Inline>]
    static member Min<'T>(this: seq<'T>, selector: Func<'T, float>) : float =
        LinqProxy.Min(Seq.map selector.Invoke this)

    [<Inline>]
    static member Min<'T>(this: seq<'T>, selector: Func<'T, Nullable<float>>) : Nullable<float> =
        LinqProxy.Min(Seq.map selector.Invoke this)

    [<Inline>]
    static member Min<'T>(this: seq<'T>, selector: Func<'T, Nullable<int64>>) : Nullable<int64> =
        LinqProxy.Min(Seq.map selector.Invoke this)

    [<Inline>]
    static member Min<'T>(this: seq<'T>, selector: Func<'T, Nullable<int>>) : Nullable<int> =
        LinqProxy.Min(Seq.map selector.Invoke this)

    [<Inline>]
    static member Min<'T>(this: seq<'T>, selector: Func<'T, int>) : int =
        LinqProxy.Min(Seq.map selector.Invoke this)

    [<Inline>]
    static member Min<'T, 'R when 'R : comparison>(this: seq<'T>, selector: Func<'T, 'R>) : 'R =
        LinqProxy.Min(Seq.map selector.Invoke this)

    [<Inline>]
    static member OfType<'T>(this: IEnumerable) : seq<'T> =
        // a type test on 'T: needs the type argument, resolved where OfType is inlined
        Seq.cast<obj> this |> Seq.filter (fun x -> x :? 'T) |> Seq.cast<'T>

    [<Inline>]
    static member OrderBy<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>) : IOrderedEnumerable<'T> =
        LinqProxy.OrderBy(this, keySelector, Comparer<'K>.Default)

    static member OrderBy<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>, comparer: IComparer<'K>) : IOrderedEnumerable<'T> =
        OrderedEnumerable(this, ProjectionComparer(comparer, keySelector)) :> _

    [<Inline>]
    static member OrderByDescending<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>) : IOrderedEnumerable<'T> =
        LinqProxy.OrderByDescending(this, keySelector, Comparer<'K>.Default)

    static member OrderByDescending<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>, comparer: IComparer<'K>) : IOrderedEnumerable<'T> =
        OrderedEnumerable(this, ReverseComparer(comparer, keySelector)) :> _

    static member Range(start: int, count: int) : seq<int> =
        if count < 0 then raise (ArgumentOutOfRangeException("count"))
        Seq.init count ((+) start)

    static member Repeat<'T>(element: 'T, count: int) : seq<'T> =
        Seq.init count (fun _ -> element)

    static member Reverse<'T>(this: seq<'T>) : seq<'T> =
        Array.rev (Array.ofSeq this) :> _

    [<Inline>]
    static member Reverse<'T>(this: 'T[]) : seq<'T> =
        Array.rev this :> _

    static member Select<'T, 'R>(this: seq<'T>, selector: Func<'T, int, 'R>) : seq<'R> =
        Seq.mapi (fun i x -> selector.Invoke(x, i)) this

    [<Inline>]
    static member Select<'T, 'R>(this: seq<'T>, selector: Func<'T, 'R>) : seq<'R> =
        Seq.map selector.Invoke this

    static member SelectMany<'T, 'R>(this: seq<'T>, selector: Func<'T, int, seq<'R>>) : seq<'R> =
        Seq.mapi (fun i x -> selector.Invoke(x, i)) this |> Seq.concat

    [<Inline>]
    static member SelectMany<'T, 'R>(this: seq<'T>, selector: Func<'T, seq<'R>>) : seq<'R> =
        Seq.collect selector.Invoke this

    static member SelectMany<'T, 'C, 'R>(this: seq<'T>, selector: Func<'T, seq<'C>>, collectionSelector: Func<'T, 'C, 'R>) : seq<'R> =
        this
        |> Seq.map (fun t -> t, selector.Invoke t)
        |> Seq.collect (fun (t, cs) ->
            cs |> Seq.map (fun c -> collectionSelector.Invoke(t, c)))

    static member SelectMany<'T, 'C, 'R>(this: seq<'T>, selector: Func<'T, int, seq<'C>>, collectionSelector: Func<'T, 'C, 'R>) : seq<'R> =
        this
        |> Seq.mapi (fun i t -> t, selector.Invoke(t, i))
        |> Seq.collect (fun (t, cs) ->
            cs |> Seq.map (fun c -> collectionSelector.Invoke(t, c)))

    [<Inline>]
    static member Sequence<'T when 'T :> System.Numerics.INumber<'T> and 'T: comparison>(start: 'T, endInclusive: 'T, step: 'T) : seq<'T> =
        seq { start .. step .. endInclusive }

    [<Inline>]
    static member SequenceEqual<'T>(this: seq<'T>, second: seq<'T>) : bool =
        LinqProxy.SequenceEqualImpl(this, second, EqualityComparer.Default)

    [<Name "SequenceEqual">]
    static member SequenceEqualImpl<'T>(this: seq<'T>, second: seq<'T>, comparer: IEqualityComparer<'T>) : bool =
        use e1 = this.GetEnumerator()
        use e2 = this.GetEnumerator()
        let rec go() =
            if e1.MoveNext() then
                e2.MoveNext() && comparer.Equals(e1.Current, e2.Current) && go()
            else
                not (e2.MoveNext())
        go()

    [<Inline>]
    static member SequenceEqual<'T>(this: seq<'T>, second: seq<'T>, comparer: IEqualityComparer<'T>) : bool =
        let comparer = if isNull comparer then EqualityComparer.Default :> IEqualityComparer<'T> else comparer
        LinqProxy.SequenceEqualImpl(this, second, comparer)

    static member Single<'T>(this: seq<'T>) : 'T =
        use e = this.GetEnumerator()
        if not (e.MoveNext()) then LinqErrors.noElements () else
        let x = e.Current
        if e.MoveNext() then raise (InvalidOperationException "Sequence contains more than one element") else x

    static member Single<'T>(this: seq<'T>, predicate: Func<'T, bool>) : 'T =
        let x =
            (None, this)
            ||> Seq.fold (fun state cur ->
                if predicate.Invoke cur then
                    if state.IsSome then
                        invalidOp "Sequence contains more than one matching element"
                    else Some cur
                else state
            )
        match x with
        | None -> invalidOp "Sequence contains no elements"
        | Some x -> x

    [<Inline>]
    static member SingleOrDefault<'T>(this: seq<'T>) : 'T =
        LinqProxy.SingleOrDefault(this, fun _ -> true)

    [<Inline>]
    static member SingleOrDefault<'T>(this: seq<'T>, predicate: Func<'T, bool>) : 'T =
        LinqProxy.JSSingleOrDefault(this, predicate, Unchecked.defaultof<'T>)

    [<Name "SingleOrDefault">]
    static member JSSingleOrDefault<'T>(this: seq<'T>, predicate: Func<'T, bool>, defaultValue: 'T) : 'T =
        use e = this.GetEnumerator()
        let mutable found = None
        while e.MoveNext() do
            if predicate.Invoke e.Current then
                match found with
                | None -> found <- Some e.Current
                | Some _ -> invalidOp "Sequence contains more than one element"
        match found with
        | Some x -> x
        | None -> defaultValue

    static member Skip<'T>(this: seq<'T>, count: int) : seq<'T> =
        seq {
            use e = this.GetEnumerator()
            let mutable i = 0
            while i < count && e.MoveNext() do i <- i + 1
            while e.MoveNext() do yield e.Current
        }

    static member SkipWhile<'T>(this: seq<'T>, predicate: Func<'T, int, bool>) : seq<'T> =
        seq {
            use e = this.GetEnumerator()
            let mutable i = 0
            let mutable predWasTrue = true
            while predWasTrue && e.MoveNext() do
                if predicate.Invoke(e.Current, i) then
                    i <- i + 1
                else
                    predWasTrue <- false
            if not predWasTrue then
                yield e.Current
                while e.MoveNext() do yield e.Current
        }

    static member SkipWhile<'T>(this: seq<'T>, predicate: Func<'T, bool>) : seq<'T> =
        seq {
            use e = this.GetEnumerator()
            let mutable predWasTrue = true
            while predWasTrue && e.MoveNext() do
                if not (predicate.Invoke e.Current) then
                    predWasTrue <- false
            if not predWasTrue then
                yield e.Current
                while e.MoveNext() do yield e.Current
        }

    /// checked like .NET: OverflowException when the sum leaves the Int64 range
    static member Sum(this: seq<int64>) : int64 =
        let s = Seq.sum this
        if JS.Inline("$0 > 9223372036854775807n || $0 < -9223372036854775808n", s) then
            raise (System.OverflowException("Arithmetic operation resulted in an overflow."))
        s

    [<Inline>]
    static member Sum(this: seq<Nullable<int64>>) : Nullable<int64> =
        As(LinqProxy.Sum(Seq.cast<Nullable<float>> this))

    [<Inline>]
    static member Sum(this: seq<float>) : float =
        Seq.sum this

    static member Sum(this: seq<Nullable<float>>) : Nullable<float> =
        let s = this |> Seq.choose (fun x -> if x.HasValue then Some x.Value else None)
        if Seq.isEmpty s then Nullable() else Nullable(Seq.sum s)

    [<Inline>]
    static member Sum(this: seq<Nullable<int>>) : Nullable<int> =
        As(LinqProxy.Sum(Seq.cast<Nullable<float>> this))

    /// checked like .NET: OverflowException when the sum leaves the Int32 range
    static member Sum(this: seq<int>) : int =
        let s = Seq.fold (fun (acc: float) (x: int) -> acc + float x) 0.0 this
        if s > 2147483647.0 || s < -2147483648.0 then
            raise (System.OverflowException("Arithmetic operation resulted in an overflow."))
        As<int> s

    [<Inline>]
    static member Sum<'T>(this: seq<'T>, selector: Func<'T, int64>) : int64 =
        LinqProxy.Sum(Seq.map selector.Invoke this)

    [<Inline>]
    static member Sum<'T>(this: seq<'T>, selector: Func<'T, float>) : float =
        LinqProxy.Sum(Seq.map selector.Invoke this)

    [<Inline>]
    static member Sum<'T>(this: seq<'T>, selector: Func<'T, Nullable<float>>) : Nullable<float> =
        LinqProxy.Sum(Seq.map selector.Invoke this)

    [<Inline>]
    static member Sum<'T>(this: seq<'T>, selector: Func<'T, Nullable<int64>>) : Nullable<int64> =
        LinqProxy.Sum(Seq.map selector.Invoke this)

    [<Inline>]
    static member Sum<'T>(this: seq<'T>, selector: Func<'T, Nullable<int>>) : Nullable<int> =
        LinqProxy.Sum(Seq.map selector.Invoke this)

    [<Inline>]
    static member Sum<'T>(this: seq<'T>, selector: Func<'T, int>) : int =
        LinqProxy.Sum(Seq.map selector.Invoke this)

    /// Take(Range): the items in the range, clamped to the sequence like .NET (never throws)
    static member Take<'T>(this: seq<'T>, range: Range) : seq<'T> =
        Seq.delay (fun () ->
            let a = Array.ofSeq this
            let n = a.Length
            let clamp i = if i < 0 then 0 elif i > n then n else i
            let st = clamp (if range.Start.IsFromEnd then n - range.Start.Value else range.Start.Value)
            let en = clamp (if range.End.IsFromEnd then n - range.End.Value else range.End.Value)
            if en <= st then Seq.empty else Array.sub a st (en - st) :> seq<'T>)

    [<Inline>]
    static member ToLookup<'T, 'K when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>) : ILookup<'K, 'T> =
        LinqProxy.ToLookup(this, keySelector, (fun x -> x), EqualityComparer.Default)

    [<Inline>]
    static member ToLookup<'T, 'K when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>, comparer: IEqualityComparer<'K>) : ILookup<'K, 'T> =
        LinqProxy.ToLookup(this, keySelector, (fun x -> x), comparer)

    [<Inline>]
    static member ToLookup<'T, 'K, 'E when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>, elementSelector: Func<'T, 'E>) : ILookup<'K, 'E> =
        LinqProxy.ToLookup(this, keySelector, elementSelector, EqualityComparer.Default)

    static member ToLookup<'T, 'K, 'E when 'K : equality>(this: seq<'T>, keySelector: Func<'T, 'K>, elementSelector: Func<'T, 'E>, comparer: IEqualityComparer<'K>) : ILookup<'K, 'E> =
        let d = Dictionary<'K, ResizeArray<'E>>(comparer)
        for x in this do
            let k = keySelector.Invoke x
            let e = elementSelector.Invoke x
            if d.ContainsKey k then d.[k].Add e else d.[k] <- ResizeArray [| e |]
        Lookup(d) :> ILookup<'K, 'E>

    static member Take<'T>(this: seq<'T>, count: int) =
        seq {
            use e = this.GetEnumerator()
            let mutable i = 0
            while i < count && e.MoveNext() do
                i <- i + 1
                yield e.Current
        }

    static member TakeWhile<'T>(this: seq<'T>, predicate: Func<'T, int, bool>) : seq<'T> =
        seq {
            use e = this.GetEnumerator()
            let mutable i = 0
            while e.MoveNext() && predicate.Invoke(e.Current, i) do
                i <- i + 1
                yield e.Current
        }

    static member TakeWhile<'T>(this: seq<'T>, predicate: Func<'T, bool>) : seq<'T> =
        seq {
            use e = this.GetEnumerator()
            while e.MoveNext() && predicate.Invoke(e.Current) do
                yield e.Current
        }

    [<Inline>]
    static member ThenBy<'T, 'K>(this: IOrderedEnumerable<'T>, keySelector: Func<'T, 'K>) : IOrderedEnumerable<'T> =
        this.CreateOrderedEnumerable(keySelector, Comparer<_>.Default, false)

    [<Inline>]
    static member ThenBy<'T, 'K>(this: IOrderedEnumerable<'T>, keySelector: Func<'T, 'K>, comparer: IComparer<'K>) : IOrderedEnumerable<'T> =
        this.CreateOrderedEnumerable(keySelector, comparer, false)

    [<Inline>]
    static member ThenByDescending<'T, 'K>(this: IOrderedEnumerable<'T>, keySelector: Func<'T, 'K>) : IOrderedEnumerable<'T> =
        this.CreateOrderedEnumerable(keySelector, Comparer<_>.Default, true)

    [<Inline>]
    static member ThenByDescending<'T, 'K>(this: IOrderedEnumerable<'T>, keySelector: Func<'T, 'K>, comparer: IComparer<'K>) : IOrderedEnumerable<'T> =
        this.CreateOrderedEnumerable(keySelector, comparer, true)

    [<Inline>]
    static member ToArray<'T>(this: seq<'T>) : 'T[] =
        Seq.toArray this

    [<Inline>]
    static member ToDictionary<'T, 'K> (this: seq<'T>, keySelector: Func<'T, 'K>) : Dictionary<'K, 'T> =
        LinqProxy.ToDictionary(this, keySelector, EqualityComparer.Default)

    static member ToDictionary<'T, 'K> (this: seq<'T>, keySelector: Func<'T, 'K>, comparer: IEqualityComparer<'K>) : Dictionary<'K, 'T> =
        let d = Dictionary(comparer)
        Seq.iter (fun x -> d.Add(keySelector.Invoke x, x)) this
        d

    [<Inline>]
    static member ToDictionary<'T, 'K, 'E> (this: seq<'T>, keySelector: Func<'T, 'K>, elementSelector: Func<'T, 'E>) : Dictionary<'K, 'E> =
        LinqProxy.ToDictionary(this, keySelector, elementSelector, EqualityComparer.Default)

    static member ToDictionary<'T, 'K, 'E> (this: seq<'T>, keySelector: Func<'T, 'K>, elementSelector: Func<'T, 'E>, comparer: IEqualityComparer<'K>) : Dictionary<'K, 'E> =
        let d = Dictionary(comparer)
        Seq.iter (fun x -> d.Add(keySelector.Invoke x, elementSelector.Invoke x)) this
        d

    [<Inline>]
    static member ToList<'T>(this: seq<'T>) : List<'T> =
        List<'T>(this)

    //[<Macro(typeof<LinqMacro>)>]
//    static member ToLookup<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>) : ILookup<'K, 'T> =
//        Lookup<'K, 'T, 'T>(this, keySelector, Func<'T,'T>(id), EqualityComparer<'K>.Default) :> _

//    [<Inline>]
//    static member ToLookup<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>, comparer: IEqualityComparer<'K>) : ILookup<'K, 'T> =
//        Lookup<'K, 'T, 'T>(this, keySelector, Func<'T,'T>(id), comparer) :> _

    //[<Macro(typeof<LinqMacro>)>]
//    static member ToLookup<'T, 'K, 'E>(this: seq<'T>, keySelector: Func<'T, 'K>, elementSelector: Func<'T, 'E>) : ILookup<'K, 'E> =
//        Lookup<'K, 'T, 'E>(this, keySelector, elementSelector, EqualityComparer<'K>.Default) :> _

//    [<Inline>]
//    static member ToLookup<'T, 'K, 'E>(this: seq<'T>, keySelector: Func<'T, 'K>, elementSelector: Func<'T, 'E>, comparer: IEqualityComparer<'K>) : ILookup<'K, 'E> =
//        Lookup<'K, 'T, 'E>(this, keySelector, elementSelector, comparer) :> _

    static member TryGetNonEnumeratedCount(this: seq<'T>, count: outref<int>) : bool =
        match box this with
        | null ->
            raise (ArgumentNullException("this"))
        | :? System.Array as a -> 
            count <- a.Length
            true
        | :? string as s -> 
            count <- s.Length
            true
        | :? ICollection as c -> 
            count <- c.Count
            true
        | :? ICollection<'T> as c -> 
            count <- c.Count
            true
        | _ ->
            count <- 0
            false

    [<Inline>]
    static member Union<'T>(this: seq<'T>, second: seq<'T>) : seq<'T> =
        LinqProxy.Union(this, second, EqualityComparer<'T>.Default)

    static member Union<'T>(this: seq<'T>, second: seq<'T>, comparer: IEqualityComparer<'T>) : seq<'T> =
        let tbl = HashSet(this, comparer)
        for e in second do tbl.Add(e) |> ignore
        tbl :> _

    static member Where<'T>(this: seq<'T>, predicate: Func<'T, int, bool>) : seq<'T> =
        seq {
            use e = this.GetEnumerator()
            let mutable i = 0
            while e.MoveNext() do
                if predicate.Invoke(e.Current, i) then
                    yield e.Current
                i <- i + 1
        }

    [<Inline>]
    static member Where<'T>(this: seq<'T>, predicate: Func<'T, bool>) : seq<'T> =
        Seq.filter predicate.Invoke this

    [<Inline>]
    static member Zip<'T, 'U, 'R>(this: seq<'T>, second: seq<'U>, resultSelector: Func<'T, 'U, 'R>) : seq<'R> =
        Seq.map2 (fun x y -> resultSelector.Invoke(x, y)) this second

    /// Zip without a selector: (first, second) tuples, as long as the shorter sequence
    [<Inline>]
    static member Zip<'T, 'U>(this: seq<'T>, second: seq<'U>) : seq<struct ('T * 'U)> =
        Seq.map2 (fun x y -> struct (x, y)) this second

    [<Inline>]
    static member Append<'T>(this: seq<'T>, element: 'T) : seq<'T> =
        Seq.append this [| element |]

    [<Inline>]
    static member Prepend<'T>(this: seq<'T>, element: 'T) : seq<'T> =
        Seq.append [| element |] this

    [<Inline>]
    static member ToHashSet<'T>(this: seq<'T>) : HashSet<'T> =
        HashSet<'T>(this)

    static member TakeLast<'T>(this: seq<'T>, count: int) : seq<'T> =
        let a = Array.ofSeq this
        if count <= 0 then Seq.empty
        elif count >= a.Length then a :> _
        else Array.sub a (a.Length - count) count :> _

    static member SkipLast<'T>(this: seq<'T>, count: int) : seq<'T> =
        let a = Array.ofSeq this
        if count <= 0 then a :> _
        elif count >= a.Length then Seq.empty
        else Array.sub a 0 (a.Length - count) :> _

    static member DistinctBy<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>) : seq<'T> =
        seq {
            let seen = HashSet<'K>()
            for x in this do
                if seen.Add(keySelector.Invoke x) then yield x
        }

    static member MinBy<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>) : 'T =
        let a = Array.ofSeq this
        if a.Length = 0 then raise (InvalidOperationException "Sequence contains no elements")
        let mutable best = a.[0]
        let mutable bestKey = keySelector.Invoke best
        for i in 1 .. a.Length - 1 do
            let k = keySelector.Invoke a.[i]
            if Unchecked.compare (box k) (box bestKey) < 0 then
                best <- a.[i]
                bestKey <- k
        best

    static member MaxBy<'T, 'K>(this: seq<'T>, keySelector: Func<'T, 'K>) : 'T =
        let a = Array.ofSeq this
        if a.Length = 0 then raise (InvalidOperationException "Sequence contains no elements")
        let mutable best = a.[0]
        let mutable bestKey = keySelector.Invoke best
        for i in 1 .. a.Length - 1 do
            let k = keySelector.Invoke a.[i]
            if Unchecked.compare (box k) (box bestKey) > 0 then
                best <- a.[i]
                bestKey <- k
        best

    [<Inline>]
    static member Order<'T>(this: seq<'T>) : IOrderedEnumerable<'T> =
        LinqProxy.OrderBy(this, Func<'T, 'T>(id), Comparer<'T>.Default)

    [<Inline>]
    static member OrderDescending<'T>(this: seq<'T>) : IOrderedEnumerable<'T> =
        LinqProxy.OrderByDescending(this, Func<'T, 'T>(id), Comparer<'T>.Default)

    [<Inline>]
    static member FirstOrDefault<'T>(this: seq<'T>, defaultValue: 'T) : 'T =
        LinqProxy.JSFirstOrDefault(this, defaultValue)

    [<Inline>]
    static member FirstOrDefault<'T>(this: seq<'T>, predicate: Func<'T, bool>, defaultValue: 'T) : 'T =
        LinqProxy.JSFirstOrDefault(this, predicate, defaultValue)

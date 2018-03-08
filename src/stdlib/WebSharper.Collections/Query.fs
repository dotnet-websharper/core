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

namespace WebSharper

open System
open System.Linq
open System.Collections
open System.Collections.Generic
open WebSharper.Core
open WebSharper.JavaScript
open FSharp.Quotations

open FSharp.Linq

[<Proxy(typeof<QuerySource<_,_>>)>]
type internal QuerySourceProxy<'T, 'Q> [<Inline "$source">] (source: IEnumerable<'T>) =

    [<Inline "$this">]
    member this.Source = source
                                
[<Proxy(typeof<QueryBuilder>)>]
[<Name "WebSharper.Query">]
type internal QueryBuilderProxy() =
    [<Inline>]
    member this.All(source: QuerySource<'T, 'Q>, predicate: 'T -> bool) =
        Seq.forall predicate source.Source

    [<Inline>]
    member inline this.AverageBy
            (source: QuerySource<'T, 'Q>, projection: 'T -> ^Value) =
        Seq.averageBy projection source.Source

    [<Name "averageByNullable">]
    static member inline AverageByNullableImpl 
            (source: QuerySource<'T, 'Q>, projection: 'T -> Nullable< ^TValue>) =
        let filtered =
            source.Source |> Seq.choose (fun x ->
                Option.ofNullable (projection x) 
            ) |> Array.ofSeq
        if filtered.Length = 0 then Nullable() else Nullable(Array.average filtered) 

    [<Inline>]
    member inline this.AverageByNullable (source, projection) = QueryBuilderProxy.AverageByNullableImpl(source, projection)

    [<Inline>]
    member this.Contains(source: QuerySource<'T, 'Q>, key: 'T) =
        Seq.contains key source.Source

    [<Inline>]
    member this.Count(source: QuerySource<'T, 'Q>) =
        Seq.length source.Source

    [<Inline>]
    member this.Distinct(source: QuerySource<'T, 'Q>) =
        Seq.distinct source.Source |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.ExactlyOne(source: QuerySource<'T, 'Q>) =
        Seq.exactlyOne source.Source
     
    [<Inline>]
    member this.ExactlyOneOrDefault(source: QuerySource<'T, 'Q>) =
        source.Source.SingleOrDefault()

    [<Inline>]
    member this.Exists(source: QuerySource<'T, 'Q>, predicate: 'T -> bool) =
        Seq.exists predicate source.Source
     
    [<Inline>]
    member this.Find(source: QuerySource<'T, 'Q>, predicate: 'T -> bool) =
        Seq.find predicate source.Source

    [<Inline>]
    member this.For(source: QuerySource<'T, 'Q>, body: 'T -> QuerySource<'TResult, 'Q2>) =
        Seq.collect (fun x -> (body x).Source) source.Source |> QuerySource<'TResult, 'Q>
     
    [<Inline>]
    member this.GroupBy(source: QuerySource<'T, 'Q>, keySelector: 'T -> 'TKey) =
        source.Source.GroupBy(fun x -> keySelector x) |> QuerySource<IGrouping<'TKey, 'T>, 'Q>

    [<Inline>]
    member this.GroupJoin
      (
        outerSource: QuerySource<'TOuter, 'Q>, innerSource: QuerySource<'TInner, 'Q>, 
        outerKeySelector: 'TOuter -> 'TKey, innerKeySelector: 'TInner -> 'TKey,
        resultSelector: 'TOuter -> seq<'TInner> -> 'TResult
      ) =
        outerSource.Source.GroupJoin(
            innerSource.Source, 
            (fun x -> outerKeySelector x), 
            (fun x -> innerKeySelector x), 
            (fun x y -> resultSelector x y)
        ) |> QuerySource<'TResult, 'Q>

    [<Inline>]
    member this.GroupValBy<'T, 'TKey, 'TValue, 'Q>(source: QuerySource<'T, 'Q>, resultSelector: 'T -> 'TValue, keySelector: 'T -> 'TKey) =
        source.Source.GroupBy((fun x -> keySelector x), (fun x -> resultSelector x)) |> QuerySource<IGrouping<'TKey, 'TValue>, 'Q>

    [<Inline>]
    member this.Head(source: QuerySource<'T, 'Q>) =
        Seq.head source.Source

    [<Inline>]
    member this.HeadOrDefault(source: QuerySource<'T, 'Q>) =
        source.Source.FirstOrDefault()
    
    [<Inline>]
    member this.Join
      (
        outerSource: QuerySource<'TOuter, 'Q>, innerSource: QuerySource<'TInner, 'Q>, 
        outerKeySelector: 'TOuter -> 'TKey, innerKeySelector: 'TInner -> 'TKey,
        resultSelector: 'TOuter -> 'TInner -> 'TResult
      ) =
        outerSource.Source.Join(
            innerSource.Source, 
            (fun x -> outerKeySelector x), 
            (fun x -> innerKeySelector x), 
            (fun x y -> resultSelector x y)
        ) |> QuerySource<'TResult, 'Q>

    [<Inline>]
    member this.Last(source: QuerySource<'T, 'Q>) =
        Seq.last source.Source
 
    [<Inline>]
    member this.LastOrDefault(source: QuerySource<'T, 'Q>) =
        source.Source.LastOrDefault()

    [<Inline>]
    member this.LeftOuterJoin
      (
        outerSource: QuerySource<'TOuter, 'Q>, innerSource: QuerySource<'TInner, 'Q>, 
        outerKeySelector: 'TOuter -> 'TKey, innerKeySelector: 'TInner -> 'TKey,
        resultSelector: 'TOuter -> seq<'TInner> -> 'TResult
      ) =
        outerSource.Source.GroupJoin(
            innerSource.Source, 
            (fun x -> outerKeySelector x), 
            (fun x -> innerKeySelector x), 
            (fun x y -> resultSelector x (y.DefaultIfEmpty()))
        ) |> QuerySource<'TResult, 'Q>

    [<Inline>]
    member this.MaxBy(source: QuerySource<'T, 'Q>, valueSelector: 'T -> 'TValue) =
        source.Source.Max(fun x -> valueSelector x)
    
    [<Inline>]
    member this.MaxByNullable(source: QuerySource<'T, 'Q>, valueSelector: 'T -> Nullable<'TValue>) =
        source.Source.Max(fun x -> valueSelector x)

    [<Inline>]
    member this.MinBy(source: QuerySource<'T, 'Q>, valueSelector: 'T -> 'TValue) =
        source.Source.Min(fun x -> valueSelector x)
    
    [<Inline>]
    member this.MinByNullable(source: QuerySource<'T, 'Q>, valueSelector: 'T -> Nullable<'TValue>) =
        source.Source.Min(fun x -> valueSelector x)

    [<Inline>]
    member this.Nth(source: QuerySource<'T, 'Q>, index: int) =
        Seq.item index source.Source

    [<Inline>]
    member this.Quote(q: Expr<'T>) = q

    [<Inline>]
    member this.Run(q: Expr<QuerySource<'T, IQueryable>>) =
        (As<QuerySource<'T, obj>> q).Source |> As<IQueryable<'T>>      
           
    [<Inline>]
    member this.Select(source: QuerySource<'T, 'Q>, projection: 'T -> 'TResult) =
        source.Source |> Seq.map projection |> QuerySource<'TResult, 'Q>

    [<Inline>]
    member this.Skip(source: QuerySource<'T, 'Q>, count: int) =
        source.Source.Skip(count) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.SkipWhile(source: QuerySource<'T, 'Q>, predicate: 'T -> bool) =
        source.Source.SkipWhile(fun x -> predicate x) |> QuerySource<'T, 'Q>
    
    [<Inline>]
    member this.SortBy(source: QuerySource<'T, 'Q>, keySelector: 'T -> 'TKey) =
        source.Source.OrderBy(fun x -> keySelector x) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.SortByDescending(source: QuerySource<'T, 'Q>, keySelector: 'T -> 'TKey) =
        source.Source.OrderByDescending(fun x -> keySelector x) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.SortByNullable(source: QuerySource<'T, 'Q>, keySelector: 'T -> Nullable<'TKey>) =
        source.Source.OrderBy(fun x -> keySelector x) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.SortByNullableDescending(source: QuerySource<'T, 'Q>, keySelector: 'T -> Nullable<'TKey>) =
        source.Source.OrderByDescending(fun x -> keySelector x) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.Source(source: seq<'T>) = 
        QuerySource<'T,  System.Collections.IEnumerable>(source)

    [<Inline>]
    member this.Source(source: IQueryable<'T>) = 
        QuerySource<'T, 'Q>(source)

    [<Inline>]                                                              
    member inline this.SumBy(source: QuerySource<'T, 'Q>, projection: 'T -> ^TValue) =
        Seq.sumBy projection source.Source

    [<Name "sumByNullable">]                                                              
    static member inline SumByNullableImpl(source: QuerySource<'T, 'Q>, projection: 'T -> Nullable<'TValue>) =
        let filtered =
            source.Source |> Seq.choose (fun x ->
                Option.ofNullable (projection x) 
            ) |> Array.ofSeq
        Nullable(Array.sum filtered) 

    [<Inline>]
    member inline this.SumByNullable(source, projection) = QueryBuilderProxy.SumByNullableImpl(source, projection)

    [<Inline>]
    member this.Take(source: QuerySource<'T, 'Q>, count: int) =
        source.Source.Take(count) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.TakeWhile(source: QuerySource<'T, 'Q>, predicate: 'T -> bool) =
        source.Source.TakeWhile(fun x -> predicate x) |> QuerySource<'T, 'Q>
    
    static member CheckThenBySource(source: IEnumerable<'T>) =
        match source with
        | :? IOrderedEnumerable<'T> as e ->
            e
        | _ ->
            failwith "'thenBy' and 'thenByDescending' may only be used with an ordered input"

    [<Inline>]
    member this.ThenBy(source: QuerySource<'T, 'Q>, keySelector: 'T -> 'TKey) =
        QueryBuilderProxy.CheckThenBySource(source.Source).ThenBy(fun x -> keySelector x) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.ThenByDescending(source: QuerySource<'T, 'Q>, keySelector: 'T -> 'TKey) =
        QueryBuilderProxy.CheckThenBySource(source.Source).ThenByDescending(fun x -> keySelector x) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.ThenByNullable(source: QuerySource<'T, 'Q>, keySelector: 'T -> Nullable<'TKey>) =
        QueryBuilderProxy.CheckThenBySource(source.Source).ThenBy(fun x -> keySelector x) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.ThenByNullableDescending(source: QuerySource<'T, 'Q>, keySelector: 'T -> Nullable<'TKey>) =
        QueryBuilderProxy.CheckThenBySource(source.Source).ThenByDescending(fun x -> keySelector x) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.Where(source: QuerySource<'T, 'Q>, predicate: 'T -> bool) =
        source.Source.Where(fun x -> predicate x) |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.Yield(value: 'T) =
        Seq.singleton value |> QuerySource<'T, 'Q>

    [<Inline>]
    member this.YieldFrom(computation: QuerySource<'T, 'Q>) =
        computation
    
    [<Inline>]
    member this.Zero() =
        Seq.empty |> QuerySource<'T, 'Q>

[<WebSharper.Proxy "Microsoft.FSharp.Core.ExtraTopLevelOperators, FSharp.Core">]
module private ExtraTopLevelOperatorsQueryProxy =
    
    [<Inline "null">]
    let query = query

[<WebSharper.Proxy "Microsoft.FSharp.Linq.QueryRunExtensions.HighPriority, FSharp.Core">]
module HighPriorityProxy =                          
    
    [<Inline>]
    let RunQueryAsEnumerable (this: QueryBuilder) (q: Expr<QuerySource<'T, IEnumerable>>) =
        (As<QuerySource<'T, IEnumerable>> q).Source   

[<WebSharper.Proxy "Microsoft.FSharp.Linq.QueryRunExtensions.LowPriority, FSharp.Core">]
module LowPriorityProxy =                          
    
    [<Inline>]
    let RunQueryAsValue (this: QueryBuilder) (q: Expr<'T>) =
        As<'T> q

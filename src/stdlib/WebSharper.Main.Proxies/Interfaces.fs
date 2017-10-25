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

open WebSharper.JavaScript
module M = WebSharper.Core.Macros

[<Proxy(typeof<System.IComparable>)>]
[<Name "WebSharper.IComparableAny">]
type private IComparableProxy =
    [<Name "CompareTo">]
    abstract CompareTo : obj -> int

[<Proxy(typeof<System.IComparable<_>>)>]
[<Name "WebSharper.IComparable">]
type private IComparableProxy<'T> =
    [<Name "CompareTo">]
    abstract CompareTo : 'T -> int

[<Proxy(typeof<System.Collections.IEqualityComparer>)>]
[<Name "WebSharper.IEqualityComparerAny">]
type private IEqualityComparerProxy =
    [<Name "CEquals">]
    abstract Equals : obj * obj -> bool 
    [<Name "CGetHashCode">]
    abstract GetHashCode : obj -> int

[<Proxy(typeof<System.Collections.Generic.IEqualityComparer<_>>)>]
[<Name "WebSharper.IEqualityComparer">]
type private IEqualityComparerProxy<'T> =
    [<Name "CEquals">]
    abstract Equals : 'T * 'T -> bool 
    [<Name "CGetHashCode">]
    abstract GetHashCode : 'T -> int

[<Proxy(typeof<System.Collections.Generic.EqualityComparer<_>>)>]
[<Name "WebSharper.Collections.EqualityComparer">]
[<AbstractClass>]
type private EqualityComparerProxy<'T>() =
    [<Name "CEquals">]
    abstract Equals : 'T * 'T -> bool 
    [<Name "CGetHashCode">]
    abstract GetHashCode : 'T -> int
    interface System.Collections.Generic.IEqualityComparer<'T> with
        [<Inline>]
        member this.Equals(x, y) = this.Equals(x, y)
        [<Inline>]
        member this.GetHashCode(x) = this.GetHashCode(x)
    interface System.Collections.IEqualityComparer with
        [<Inline>]
        member this.Equals(x, y) = this.Equals(As x, As y)
        [<Inline>]
        member this.GetHashCode(x) = this.GetHashCode(As x)
    [<Macro(typeof<M.EqualityComparer>)>]
    static member Default = X<System.Collections.Generic.EqualityComparer<'T>>

[<Proxy(typeof<System.Collections.IComparer>)>]
[<Name "WebSharper.IComparerAny">]
type private IComparerProxy =
    [<Name "Compare">]
    abstract Compare : obj * obj -> int

[<Proxy(typeof<System.Collections.Generic.IComparer<_>>)>]
[<Name "WebSharper.IComparer">]
type private IComparerProxy<'T> =
    [<Name "Compare">]
    abstract Compare : 'T * 'T -> int

[<Proxy(typeof<System.Collections.Generic.Comparer<_>>)>]
[<Name "WebSharper.Collections.Comparer">]
[<AbstractClass>]
type private ComparerProxy<'T>() =
    [<Name "Compare">]
    abstract Compare : 'T * 'T -> int
    interface System.Collections.Generic.IComparer<'T> with
        [<Inline>]
        member this.Compare(x, y) = this.Compare(x, y)
    interface System.Collections.IComparer with
        [<Inline>]
        member this.Compare(x, y) = this.Compare(As x, As y)
    [<Macro(typeof<M.Comparer>)>]
    static member Default = X<System.Collections.Generic.Comparer<'T>>

[<Proxy(typeof<System.IEquatable<_>>)>]
[<Name "WebSharper.IEquatable">]
type private IEquatableProxy<'T> =
    [<Name "EEquals">]
    abstract Equals : 'T -> bool

[<Proxy(typeof<System.Collections.IStructuralEquatable>)>]
[<Name "WebSharper.IStructuralEquatable">]
type private IStructuralEquatableProxy =
    [<Name "SEquals">]
    abstract Equals : obj * System.Collections.IEqualityComparer -> bool 
    [<Name "SGetHashCode">]
    abstract GetHashCode : System.Collections.IEqualityComparer -> int

[<Proxy(typeof<System.Collections.IStructuralComparable>)>]
[<Name "WebSharper.IStructuralComparable">]
type private IStructuralComparableProxy =
    [<Name "SCompareTo">]
    abstract CompareTo : obj * System.Collections.IComparer -> int 

[<Proxy(typeof<System.IDisposable>)>]
[<Name "WebSharper.IDisposable">]
type private IDisposableProxy =
    [<Name "Dispose">]
    abstract member Dispose : unit -> unit

[<Proxy(typeof<System.Collections.IEnumerable>)>]  
[<Name "WebSharper.IEnumerableAny">]
type private IEnumerableProxy =

    [<Name "GetEnumerator">]
    abstract GetEnumerator : unit -> System.Collections.IEnumerator

[<Proxy(typeof<System.Collections.Generic.IEnumerable<_>>)>]  
[<Name "WebSharper.IEnumerable">]
type private IEnumerableProxy<'T> =
    inherit System.Collections.IEnumerable 
    
    [<Name "GetEnumerator">]
    abstract GetEnumerator : unit -> System.Collections.Generic.IEnumerator<'T>
    
[<Proxy(typeof<System.Collections.IEnumerator>)>]
[<Name "WebSharper.IEnumeratorAny">]
type private IEnumeratorProxy =
    [<Name "Current">]
    abstract member Current  : obj
    [<Name "MoveNext">]
    abstract member MoveNext : unit -> bool
    [<Name "Dispose">]
    abstract member Dispose : unit -> unit // fake member so that TS IEnumerator<T> casts to IEnumeratorAny

[<Proxy(typeof<System.Collections.Generic.IEnumerator<_>>)>]
[<Name "WebSharper.IEnumerator">]
type private IEnumeratorProxy<'T> =
    inherit System.IDisposable
    inherit System.Collections.IEnumerator
    [<Name "Current">]
    abstract member Current : 'T

[<Proxy(typeof<System.IObservable<_>>)>]
[<Name "WebSharper.IObservableAny">]
type private IObservableProxy<'T> =
    [<Name "Subscribe">]
    abstract member Subscribe : System.IObserver<'T> -> System.IDisposable

[<Proxy(typeof<System.IObserver<_>>)>]
[<Name "WebSharper.IObserver">]
type private IObserverProxy<'T> =
    [<Name "OnCompleted">]
    abstract member OnCompleted : unit -> unit
    [<Name "OnError">]
    abstract member OnError : exn -> unit
    [<Name "OnNext">]
    abstract member OnNext : 'T -> unit

[<Proxy(typeof<IDelegateEvent<_>>)>]
[<Name "WebSharper.IDelegateEvent">]
type private IDelegateEventProxy<'D> =
    [<Name "AddHandler">]
    abstract AddHandler : 'D -> unit
    [<Name "RemoveHandler">]
    abstract RemoveHandler : 'D -> unit

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

namespace WebSharper

#if JAVASCRIPT
open WebSharper.JavaScript
module M = WebSharper.Core.Macros

[<Proxy(typeof<System.IComparable>)>]
type private IComparableProxy =
    [<Name "CompareTo0">]
    abstract CompareTo : obj -> int

[<Proxy(typeof<System.IComparable<_>>)>]
type private IComparableProxy<'T> =
    [<Name "CompareTo">]
    abstract CompareTo : 'T -> int

[<Proxy(typeof<System.Collections.IEqualityComparer>)>]
type private IEqualityComparerProxy =
    [<Name "CEquals0">]
    abstract Equals : obj * obj -> bool 
    [<Name "CGetHashCode0">]
    abstract GetHashCode : obj -> int

[<Proxy(typeof<System.Collections.Generic.IEqualityComparer<_>>)>]
type private IEqualityComparerProxy<'T> =
    [<Name "CEquals">]
    abstract Equals : 'T * 'T -> bool 
    [<Name "CGetHashCode">]
    abstract GetHashCode : 'T -> int

[<Proxy(typeof<System.Collections.Generic.EqualityComparer<_>>)>]
[<Name "WebSharper.Collections.EqualityComparer">]
[<AbstractClass>]
type private EqualityComparerProxy<'T>() =
    abstract Equals : 'T * 'T -> bool 
    abstract GetHashCode : 'T -> int
    interface System.Collections.Generic.IEqualityComparer<'T> with
        member this.Equals(x, y) = this.Equals(x, y)
        member this.GetHashCode(x) = this.GetHashCode(x)
    interface System.Collections.IEqualityComparer with
        member this.Equals(x, y) = this.Equals(As x, As y)
        member this.GetHashCode(x) = this.GetHashCode(As x)
    [<Macro(typeof<M.EqualityComparer>)>]
    static member Default = X<System.Collections.Generic.EqualityComparer<'T>>

[<Proxy(typeof<System.Collections.IComparer>)>]
type private IComparerProxy =
    [<Name "Compare0">]
    abstract Compare : obj * obj -> int

[<Proxy(typeof<System.Collections.Generic.IComparer<_>>)>]
type private IComparerProxy<'T> =
    [<Name "Compare">]
    abstract Compare : 'T * 'T -> int

[<Proxy(typeof<System.Collections.Generic.Comparer<_>>)>]
[<Name "WebSharper.Collections.Comparer">]
[<AbstractClass>]
type private ComparerProxy<'T>() =
    abstract Compare : 'T * 'T -> int
    interface System.Collections.Generic.IComparer<'T> with
        member this.Compare(x, y) = this.Compare(x, y)
    interface System.Collections.IComparer with
        member this.Compare(x, y) = this.Compare(As x, As y)
    [<Macro(typeof<M.Comparer>)>]
    static member Default = X<System.Collections.Generic.Comparer<'T>>

[<Proxy(typeof<System.IEquatable<_>>)>]
type private IEquatableProxy<'T> =
    [<Name "EEquals">]
    abstract Equals : 'T -> bool

[<Proxy(typeof<System.Collections.IStructuralEquatable>)>]
type private IStructuralEquatableProxy =
    [<Name "SEquals">]
    abstract Equals : obj * System.Collections.IEqualityComparer -> bool 
    [<Name "SGetHashCode">]
    abstract GetHashCode : System.Collections.IEqualityComparer -> int

[<Proxy(typeof<System.Collections.IStructuralComparable>)>]
type private IStructuralComparableProxy =
    [<Name "SCompareTo">]
    abstract CompareTo : obj * System.Collections.IComparer -> int 

[<Proxy(typeof<System.IDisposable>)>]
type private IDisposableProxy =
    [<Name "Dispose">]
    abstract member Dispose : unit -> unit

[<Proxy(typeof<System.Collections.IEnumerable>)>]  
type private IEnumerableProxy =

    [<Name "GetEnumerator0">]
    abstract GetEnumerator : unit -> System.Collections.IEnumerator

[<Proxy(typeof<seq<_>>)>]  
type private IEnumerableProxy<'T> =
    inherit System.Collections.IEnumerable 
    
    [<Name "GetEnumerator">]
    abstract GetEnumerator : unit -> System.Collections.Generic.IEnumerator<'T>
    
[<Proxy(typeof<System.Collections.IEnumerator>)>]
[<Name "WebSharper.IEnumerator">]
type private IEnumeratorProxy =
    [<Name "Current0">]
    abstract member Current  : obj
    [<Name "MoveNext">]
    abstract member MoveNext : unit -> bool
    [<Name "Reset">]
    abstract member Reset    : unit -> unit

[<Proxy(typeof<System.Collections.Generic.IEnumerator<_>>)>]
[<Name "WebSharper.IEnumerator1">]
type private IEnumeratorProxy<'T> =
    [<Name "Current">]
    abstract member Current : 'T

[<Proxy(typeof<System.IObservable<_>>)>]
type private IObservableProxy<'T> =
    [<Name "Subscribe">]
    abstract member Subscribe : System.IObserver<'T> -> System.IDisposable

[<Proxy(typeof<System.IObserver<_>>)>]
type private IObserverProxy<'T> =
    [<Name "OnCompleted">]
    abstract member OnCompleted : unit -> unit
    [<Name "OnError">]
    abstract member OnError : exn -> unit
    [<Name "OnNext">]
    abstract member OnNext : 'T -> unit

[<Proxy(typeof<IDelegateEvent<_>>)>]
type private IDelegateEventProxy<'D> =
    [<Name "AddHandler">]
    abstract AddHandler : 'D -> unit
    [<Name "RemoveHandler">]
    abstract RemoveHandler : 'D -> unit
#endif
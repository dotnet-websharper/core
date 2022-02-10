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

    [<Inline>]
    default this.GetEnumerator() = Enumerator.Get0 (As<System.Collections.IEnumerable> this)

[<Proxy(typeof<seq<_>>, [| typeof<System.Collections.IEnumerable> |])>]  
type private IEnumerableProxy<'T> =
    [<Name "GetEnumerator">]
    abstract GetEnumerator : unit -> System.Collections.Generic.IEnumerator<'T>

    [<Inline>]
    default this.GetEnumerator() = Enumerator.Get (As<System.Collections.Generic.IEnumerable<'T>> this)

[<Proxy(typeof<System.Collections.ICollection>)>]
type private ICollectionProxy =
    inherit System.Collections.IEnumerable
    [<Name "Count">]
    abstract member Count          : int
    [<Name "IsSynchronized">]
    abstract member IsSynchronized : bool
    [<Name "SyncRoot">]
    abstract member SyncRoot       : obj
    [<Name "CopyTo">]
    abstract member CopyTo         : System.Array * int -> unit

[<Proxy(typeof<System.Collections.Generic.ICollection<_>>)>]
type private ICollectionProxy<'T> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'T>
    [<Name "Count">]
    abstract member Count      : int
    [<Name "IsReadOnly">]
    abstract member IsReadOnly : bool
    [<Name "CopyTo">]
    abstract member CopyTo     : 'T [] * int -> unit
    [<Name "Add">]
    abstract member Add        : 'T -> unit
    [<Name "Clear">]
    abstract member Clear      : unit -> unit
    [<Name "Contains">]
    abstract member Contains   : 'T -> bool
    [<Name "Remove">]
    abstract member Remove     : 'T -> bool

[<Proxy(typeof<System.Collections.IList>)>]
type private IListProxy =
    inherit System.Collections.IEnumerable
    inherit System.Collections.ICollection
    [<Name "IsFixedSize">]
    abstract member IsFixedSize : bool
    [<Name "IsReadOnly">]
    abstract member IsReadOnly  : bool
    [<Name "Item">]
    abstract member Item        : int -> obj with get, set
    [<Name "Add">]
    abstract member Add         : obj -> int
    [<Name "Clear">]
    abstract member Clear       : unit -> unit
    [<Name "Contains">]
    abstract member Contains    : obj -> bool
    [<Name "IndexOf">]
    abstract member IndexOf     : obj -> int
    [<Name "Insert">]
    abstract member Insert      : int * obj -> unit
    [<Name "Remove">]
    abstract member Remove      : obj -> unit
    [<Name "RemoveAt">]
    abstract member RemoveAt    : int -> unit

[<Proxy(typeof<System.Collections.Generic.IList<_>>)>]
type private IListProxy<'T> =
    inherit System.Collections.Generic.ICollection<'T>
    [<Name "Item">]
    abstract member Item        : int -> 'T with get, set
    [<Name "IndexOf">]
    abstract member IndexOf     : 'T -> int
    [<Name "Insert">]
    abstract member Insert      : int * 'T -> unit
    [<Name "Remove">]
    abstract member Remove      : 'T -> unit
    [<Name "RemoveAt">]
    abstract member RemoveAt    : int -> unit

[<Proxy(typeof<System.Collections.IDictionary>)>]
type private IDictionaryProxy =
    inherit System.Collections.ICollection
    [<Name "IsFixedSize">]
    abstract member IsFixedSize   : bool
    [<Name "IsReadOnly">] 
    abstract member IsReadOnly    : bool
    [<Name "Item">]
    abstract member Item          : obj -> obj with get, set
    [<Name "Keys">]
    abstract member Keys          : System.Collections.ICollection
    [<Name "Values">]
    abstract member Values        : System.Collections.ICollection
    [<Name "Add">]
    abstract member Add           : obj * obj -> unit
    [<Name "Clear">]
    abstract member Clear         : unit -> unit
    [<Name "ContainsKey">]
    abstract member Contains      : obj -> bool
    [<Name "GetEnumerator">]
    abstract member GetEnumerator : unit -> System.Collections.IDictionaryEnumerator
    [<Name "RemoveKey">]
    abstract member Remove        : obj -> unit

[<Proxy(typeof<System.Collections.IDictionaryEnumerator>)>]
type private IDictionaryEnumeratorProxy =
    inherit System.Collections.IEnumerator
    [<Name "Entry">]
    abstract member Entry  : System.Collections.DictionaryEntry
    [<Name "Key">] 
    abstract member Key    : obj
    [<Name "Value">] 
    abstract member Value  : obj

[<Proxy(typeof<System.Collections.DictionaryEntry>)>]
type private DictionaryEntryProxy(key: obj, value: obj) =
    let mutable innerKey = key
    let mutable innerValue = value
    [<Name "Key">] 
    member __.Key
        with get ()  = innerKey
        and  set key = innerKey <- key
    [<Name "Value">] 
    member __.Value
        with get ()    = innerValue
        and  set value = innerValue <- value
    
[<Proxy(typeof<System.Collections.Generic.IDictionary<_,_>>)>]
type private IDictionaryProxy<'TKey, 'TValue> =
    inherit System.Collections.Generic.ICollection<System.Collections.Generic.KeyValuePair<'TKey, 'TValue>>
    [<Name "Item">]
    abstract member Item        : 'TKey -> 'TValue with get, set
    [<Name "Keys">]
    abstract member Keys        : System.Collections.Generic.ICollection<'TKey>
    [<Name "Values">]
    abstract member Values      : System.Collections.Generic.ICollection<'TValue>
    [<Name "AddWithKey">]
    abstract member Add         : 'TKey * 'TValue -> unit
    [<Name "ContainsKey">]
    abstract member ContainsKey : 'TKey -> bool
    [<Name "RemoveKey">]
    abstract member Remove      : 'TKey -> bool
    [<Name "TryGetValue">]
    abstract member TryGetValue : 'TKey * byref<'TValue> -> bool

[<Proxy(typeof<System.Collections.Generic.ISet<_>>)>]
type private ISetProxy<'T> =
    inherit System.Collections.Generic.ICollection<'T>
    [<Name "AddReturn">]
    abstract member Add         : 'T -> bool
    [<Name "ExceptWith">]
    abstract member ExceptWith  : seq<'T> -> unit
    [<Name "IntersectWith">]
    abstract member IntersectWith : seq<'T> -> unit
    [<Name "IsProperSubsetOf">]
    abstract member IsProperSubsetOf : seq<'T> -> bool
    [<Name "IsProperSupersetOf">]
    abstract member IsProperSupersetOf : seq<'T> -> bool
    [<Name "IsSubsetOf">]
    abstract member IsSubsetOf : seq<'T> -> bool
    [<Name "IsSupersetOf">]
    abstract member IsSupersetOf : seq<'T> -> bool
    [<Name "Overlaps">]
    abstract member Overlaps : seq<'T> -> bool
    [<Name "SetEquals">]
    abstract member SetEquals : seq<'T> -> bool
    [<Name "SymmetricExceptWith">]
    abstract member SymmetricExceptWith : seq<'T> -> unit
    [<Name "UnionWith">]
    abstract member UnionWith : seq<'T> -> unit

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

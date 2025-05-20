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

[<AbstractClass; Proxy(typeof<System.Collections.ICollection>, [| typeof<System.Collections.IEnumerable> |])>]
type private ICollectionProxy =
    [<Name "Count0">]
    abstract member Count : int
    [<Inline>]
    default this.Count = Enumerator.Count0 (As<System.Collections.ICollection> this)

    [<Name "CopyTo0">]
    abstract member CopyTo : System.Array * int -> unit
    [<Inline>]
    default this.CopyTo(array: System.Array, index: int) = Enumerator.CopyTo0 (As<System.Collections.ICollection> this) array index

    [<JavaScript(false)>]
    abstract member IsSynchronized : bool

    [<JavaScript(false)>]
    abstract member SyncRoot : obj

[<Proxy(typeof<System.Collections.Generic.ICollection<_>>, [| typeof<System.Collections.IEnumerable>; typeof<System.Collections.Generic.IEnumerable<_>> |])>]
type private ICollectionProxy<'T> =
    [<Name "Count">]
    abstract member Count : int
    [<Inline>]
    default this.Count = Enumerator.Count (As<System.Collections.Generic.ICollection<'T>> this)

    [<Name "IsReadOnly">]
    abstract member IsReadOnly : bool
    [<Inline>]
    default this.IsReadOnly = Enumerator.IsReadOnly (As<System.Collections.Generic.ICollection<'T>> this)

    [<Name "CopyTo">]
    abstract member CopyTo : 'T [] * int -> unit
    [<Inline>]
    default this.CopyTo(array: 'T [], index: int) = Enumerator.CopyTo (As<System.Collections.Generic.ICollection<'T>> this) array index

    [<Name "Add">]
    abstract member Add : 'T -> unit
    [<Inline>]
    default this.Add(item: 'T) = Enumerator.Add (As<System.Collections.Generic.ICollection<'T>> this) item

    [<Name "Clear">]
    abstract member Clear : unit -> unit
    [<Inline>]
    default this.Clear() = Enumerator.Clear (As<System.Collections.Generic.ICollection<'T>> this)

    [<Name "Contains">]
    abstract member Contains : 'T -> bool
    [<Inline>]
    default this.Contains(item: 'T) = Enumerator.Contains (As<System.Collections.Generic.ICollection<'T>> this) item

    [<Name "Remove">]
    abstract member Remove : 'T -> bool
    [<Inline>]
    default this.Remove(item: 'T) = Enumerator.Remove (As<System.Collections.Generic.ICollection<'T>> this) item

[<Proxy(typeof<System.Collections.Generic.IReadOnlyCollection<_>>, [| typeof<System.Collections.IEnumerable>; typeof<System.Collections.Generic.IEnumerable<_>> |])>]
type private IReadOnlyCollectionProxy<'T> =
    [<Name "Count">]
    abstract member Count : int
    [<Inline>]
    default this.Count = Enumerator.Count (As<System.Collections.Generic.ICollection<'T>> this)

[<Proxy(typeof<System.Collections.IList>, [| typeof<System.Collections.IEnumerable>; typeof<System.Collections.ICollection> |])>]
type private IListProxy =
    [<Name "IsFixedSize">]
    abstract member IsFixedSize : bool
    [<Inline>]
    default this.IsFixedSize = Enumerator.IsFixedSize (As<System.Collections.IList> this)
    
    [<Name "LIsReadOnly">]
    abstract member IsReadOnly : bool
    [<Inline>]
    default this.IsReadOnly = Enumerator.LIsReadOnly (As<System.Collections.IList> this)
    
    [<Name "LItem0">]
    abstract member Item : int -> obj with get, set
    [<Inline>]
    default this.Item 
        with get (index: int) = Enumerator.LItem0Get (As<System.Collections.IList> this) index
        and set (index: int) (value: obj) = Enumerator.LItem0Set (As<System.Collections.IList> this) index value
    
    [<Name "LAdd">]
    abstract member Add : obj -> int
    [<Inline>]
    default this.Add(item: obj) = Enumerator.LAdd (As<System.Collections.IList> this) item

    [<Name "LClear">]
    abstract member Clear : unit -> unit
    [<Inline>]
    default this.Clear() = Enumerator.LClear (As<System.Collections.IList> this)
    
    [<Name "LContains">]
    abstract member Contains : obj -> bool
    [<Inline>]
    default this.Contains(item: obj) = Enumerator.LContains (As<System.Collections.IList> this) item
    
    [<Name "LIndexOf0">]
    abstract member IndexOf : obj -> int
    [<Inline>]
    default this.IndexOf(item: obj) = Enumerator.LIndexOf0 (As<System.Collections.IList> this) item
    
    [<Name "LInsert0">]
    abstract member Insert : int * obj -> unit
    [<Inline>]
    default this.Insert(index: int, item: obj) = Enumerator.LInsert0 (As<System.Collections.IList> this) index item

    [<Name "LRemove0">]
    abstract member Remove : obj -> unit
    [<Inline>]
    default this.Remove(item: obj) = Enumerator.LRemove0 (As<System.Collections.IList> this) item

    [<Name "LRemoveAt0">]
    abstract member RemoveAt : int -> unit
    [<Inline>]
    default this.RemoveAt(index: int) = Enumerator.LRemoveAt0 (As<System.Collections.IList> this) index

[<Proxy(typeof<System.Collections.Generic.IList<_>>, [| typeof<System.Collections.Generic.ICollection<_>> |])>]
type private IListProxy<'T> =
    [<Name "LItem">]
    abstract member Item : int -> 'T with get, set
    [<Inline>]
    default this.Item 
        with get (index: int) = Enumerator.LItemGet (As<System.Collections.Generic.IList<'T>> this) index
        and set (index: int) value = Enumerator.LItemSet (As<System.Collections.Generic.IList<'T>> this) index value
    
    [<Name "LIndexOf">]
    abstract member IndexOf : 'T -> int
    [<Inline>]
    default this.IndexOf(item: 'T) = Enumerator.LIndexOf (As<System.Collections.Generic.IList<'T>> this) item

    [<Name "LInsert">]
    abstract member Insert : int * 'T -> unit
    [<Inline>]
    default this.Insert(index: int, item: 'T) = Enumerator.LInsert (As<System.Collections.Generic.IList<'T>> this) index item
    
    [<Name "LRemoveAt">]
    abstract member RemoveAt : int -> unit
    [<Inline>]
    default this.RemoveAt(index: int) = Enumerator.LRemoveAt (As<System.Collections.Generic.IList<'T>> this) index

[<Proxy(typeof<System.Collections.IDictionary>)>]
type private IDictionaryProxy =
    inherit System.Collections.ICollection
    [<Name "IsFixedSize">]
    abstract member IsFixedSize : bool
    [<Name "IsReadOnly">] 
    abstract member IsReadOnly : bool
    [<Name "Item">]
    abstract member Item : obj -> obj with get, set
    [<Name "Keys">]
    abstract member Keys : System.Collections.ICollection
    [<Name "Values">]
    abstract member Values : System.Collections.ICollection
    [<Name "DAdd">]
    abstract member Add : obj * obj -> unit
    [<Name "Clear">]
    abstract member Clear : unit -> unit
    [<Name "ContainsKey">]
    abstract member Contains : obj -> bool
    [<Name "GetEnumerator">]
    abstract member GetEnumerator : unit -> System.Collections.IDictionaryEnumerator
    [<Name "RemoveKey">]
    abstract member Remove : obj -> unit

[<Proxy(typeof<System.Collections.IDictionaryEnumerator>, [|typeof<System.Collections.IEnumerator>|])>]
type private IDictionaryEnumeratorProxy =
    [<Inline>]
    member this.Entry = As<System.Collections.DictionaryEntry> (As<System.Collections.IEnumerator> this).Current
    [<Inline>]
    member this.Key = (As<System.Collections.DictionaryEntry> (As<System.Collections.IEnumerator> this).Current).Key
    [<Inline>]
    member this.Value = (As<System.Collections.DictionaryEntry> (As<System.Collections.IEnumerator> this).Current).Value

[<Proxy(typeof<System.Collections.DictionaryEntry>)>]
[<Prototype false>]
type private DictionaryEntryProxy [<Inline "{K: $key, V: $value}">] (key: obj, value: obj) =
    member this.Key
        with [<Inline "$this.K">] get () = (As<System.Collections.Generic.KeyValuePair<obj,obj>> this).Key
        //and  set key = innerKey <- key // mutable structs not supported yet
    member this.Value
        with [<Inline "$this.V">] get () = (As<System.Collections.Generic.KeyValuePair<obj,obj>> this).Value
        //and  set value = innerValue <- value // mutable structs not supported yet
    
[<Proxy(typeof<System.Collections.Generic.IDictionary<_,_>>)>]
type private IDictionaryProxy<'TKey, 'TValue> =
    inherit System.Collections.Generic.ICollection<System.Collections.Generic.KeyValuePair<'TKey, 'TValue>>
    [<Name "Item">]
    abstract member Item : 'TKey -> 'TValue with get, set
    [<Name "Keys">]
    abstract member Keys : System.Collections.Generic.ICollection<'TKey>
    [<Name "Values">]
    abstract member Values : System.Collections.Generic.ICollection<'TValue>
    [<Name "DAdd">]
    abstract member Add : 'TKey * 'TValue -> unit
    [<Name "ContainsKey">]
    abstract member ContainsKey : 'TKey -> bool
    [<Name "RemoveKey">]
    abstract member Remove : 'TKey -> bool
    [<Name "TryGetValue">]
    abstract member TryGetValue : 'TKey * outref<'TValue> -> bool

[<Proxy(typeof<System.Collections.Generic.IReadOnlyDictionary<_,_>>)>]
type private IReadOnlyDictionaryProxy<'TKey, 'TValue> =
    inherit System.Collections.Generic.IReadOnlyCollection<System.Collections.Generic.KeyValuePair<'TKey, 'TValue>>
    [<Name "Item">]
    abstract member Item : 'TKey -> 'TValue with get
    [<Name "Keys">]
    abstract member Keys : System.Collections.Generic.IEnumerable<'TKey>
    [<Name "Values">]
    abstract member Values : System.Collections.Generic.IEnumerable<'TValue>
    [<Name "ContainsKey">]
    abstract member ContainsKey : 'TKey -> bool
    [<Name "TryGetValue">]
    abstract member TryGetValue : 'TKey * outref<'TValue> -> bool

[<Proxy(typeof<System.Collections.Generic.ISet<_>>)>]
type private ISetProxy<'T> =
    inherit System.Collections.Generic.ICollection<'T>
    [<Name "SAdd">]
    abstract member Add : 'T -> bool
    [<Name "ExceptWith">]
    abstract member ExceptWith : seq<'T> -> unit
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

[<AbstractClass; Proxy(typeof<System.Collections.IEnumerator>)>]
type private IEnumeratorProxy =
    [<Name "Current0">]
    abstract member Current : obj
    [<Name "MoveNext">]
    abstract member MoveNext : unit -> bool
    
    [<Name "Reset">]
    abstract member Reset : unit -> unit
    [<Inline>]
    default this.Reset() = Enumerator.Reset (As<System.Collections.IEnumerator> this)

[<AbstractClass; Proxy(typeof<System.Collections.Generic.IEnumerator<_>>, [| typeof<System.Collections.IEnumerator> |])>]
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

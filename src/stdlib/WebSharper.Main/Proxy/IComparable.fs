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

[<Proxy(typeof<System.IComparable>)>]
[<Name "IComparable0">]
type private IComparableProxy =
    abstract CompareTo : obj -> int

[<Proxy(typeof<System.IComparable<_>>)>]
[<Name "IComparable">]
type private IComparableProxy<'T> =
    abstract CompareTo : 'T -> int

[<Proxy(typeof<System.Collections.IEqualityComparer>)>]
[<Name "IEqualityComparer0">]
type private IEqualityComparerProxy =
    abstract Equals : obj * obj -> bool 
    abstract GetHashCode : obj -> int

[<Proxy(typeof<System.Collections.Generic.IEqualityComparer<_>>)>]
[<Name "IEqualityComparer">]
type private IEqualityComparerProxy<'T> =
    abstract Equals : 'T * 'T -> bool 
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
    [<Macro(typeof<Macro.EqualityComparer>)>]
    static member Default = X<System.Collections.Generic.EqualityComparer<'T>>

[<Proxy(typeof<System.Collections.IComparer>)>]
[<Name "IComparer0">]
type private IComparerProxy =
    abstract Compare : obj * obj -> int

[<Proxy(typeof<System.Collections.Generic.IComparer<_>>)>]
[<Name "IComparer">]
type private IComparerProxy<'T> =
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
    [<Macro(typeof<Macro.Comparer>)>]
    static member Default = X<System.Collections.Generic.Comparer<'T>>

[<Proxy(typeof<System.IEquatable<_>>)>]
[<Name "IEquatable">]
type private IEquatableProxy<'T> =
    abstract Equals : 'T -> bool

[<Proxy(typeof<System.Collections.IStructuralEquatable>)>]
[<Name "IStructuralEquatable">]
type private IStructuralEquatableProxy =
    abstract Equals : obj * System.Collections.IEqualityComparer -> bool 
    abstract GetHashCode : System.Collections.IEqualityComparer -> int

[<Proxy(typeof<System.Collections.IStructuralComparable>)>]
[<Name "IStructuralComparable">]
type private IStructuralComparableProxy =
    abstract CompareTo : obj * System.Collections.IComparer -> int 

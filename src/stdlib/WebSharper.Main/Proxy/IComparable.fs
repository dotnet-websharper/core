// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

[<Proxy(typeof<System.IComparable>)>]
type private IComparableProxy =
    abstract CompareTo : obj -> int

[<Proxy(typeof<System.IComparable<_>>)>]
type private IComparableProxy<'T> =
    abstract CompareTo : 'T -> int

[<Proxy(typeof<System.Collections.IEqualityComparer>)>]
type private IEqualityComparerProxy =
    abstract Equals : obj * obj -> bool 
    abstract GetHashCode : obj -> int

[<Proxy(typeof<System.Collections.IComparer>)>]
type private IComparerProxy =
    abstract Compare : obj * obj -> int

[<Proxy(typeof<System.IEquatable<_>>)>]
type private IEquatableProxy<'T> =
    abstract Equals : 'T -> bool

[<Proxy(typeof<System.Collections.IStructuralEquatable>)>]
type private IStructuralEquatableProxy =
    abstract Equals : obj * System.Collections.IEqualityComparer -> bool 
    abstract GetHashCode : System.Collections.IEqualityComparer -> int

[<Proxy(typeof<System.Collections.IStructuralComparable>)>]
type private IStructuralComparableProxy =
    abstract CompareTo : obj * System.Collections.IComparer -> int 

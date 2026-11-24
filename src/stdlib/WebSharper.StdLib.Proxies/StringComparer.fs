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

open WebSharper.JavaScript
open System.Collections
open System.Collections.Generic

/// System.StringComparer: the ordinal comparers, with or without case.
/// The culture-sensitive comparers are left out on purpose: the browser's collation
/// does not match .NET's, so they would only be an approximation.
[<Proxy(typeof<System.StringComparer>)>]
[<Name "StringComparer">]
type internal StringComparerProxy(ignoreCase: bool) =

    let norm (s: string) = if ignoreCase && not (isNull s) then s.ToUpper() else s

    static member Ordinal = StringComparerProxy(false)
    static member OrdinalIgnoreCase = StringComparerProxy(true)

    member this.Compare(x: string, y: string) : int =
        if x ===. y then 0
        elif x ==. null then -1
        elif y ==. null then 1
        else
            // like .NET: the difference of the first differing chars, else of the lengths
            let a = norm x
            let b = norm y
            JS.Inline("(() => { const n = Math.min($0.length, $1.length); for (let i = 0; i < n; i++) { const d = $0.charCodeAt(i) - $1.charCodeAt(i); if (d !== 0) return d; } return $0.length - $1.length; })()", a, b)

    member this.Equals(x: string, y: string) : bool =
        if x ===. y then true
        elif x ==. null || y ==. null then false
        else norm x = norm y

    member this.GetHashCode(s: string) : int =
        if s ==. null then raise (System.ArgumentNullException("obj"))
        hash (norm s)

    member this.Compare(x: obj, y: obj) : int = this.Compare(As<string> x, As<string> y)

    member this.Equals(x: obj, y: obj) : bool = this.Equals(As<string> x, As<string> y)

    member this.GetHashCode(o: obj) : int = this.GetHashCode(As<string> o)

    interface IComparer<string> with
        member this.Compare(x, y) = this.Compare(x, y)

    interface IEqualityComparer<string> with
        member this.Equals(x, y) = this.Equals(x, y)
        member this.GetHashCode(s) = this.GetHashCode(s)

    interface IComparer with
        member this.Compare(x, y) = this.Compare(As<string> x, As<string> y)

    interface IEqualityComparer with
        member this.Equals(x: obj, y: obj) = this.Equals(As<string> x, As<string> y)
        member this.GetHashCode(o: obj) = this.GetHashCode(As<string> o)

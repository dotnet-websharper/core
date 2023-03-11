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


[<WebSharper.Proxy "Microsoft.FSharp.Core.LanguagePrimitives, FSharp.Core">]
module private WebSharper.LanguagePrimitivesProxy

#nowarn "77" // get_Zero, get_One warnings

open WebSharper.JavaScript

module M = WebSharper.Core.Macros

[<Inline>]
let GenericEquality<'T> (a: 'T) (b: 'T) = Unchecked.equals a b

[<Inline>]
let GenericEqualityER<'T> (a: 'T) (b: 'T) = Unchecked.equals a b

[<Inline>]
let GenericComparison<'T> (a: 'T) (b: 'T) = Unchecked.compare a b

[<Inline>]
let GenericHash<'T> (x: 'T) = Unchecked.hash x

[<Inline>]
let GenericComparisonWithComparer<'T> (c: System.Collections.IComparer) (a: 'T) (b: 'T) = c.Compare(a, b)

[<Inline>]
let GenericEqualityWithComparer<'T> (c: System.Collections.IEqualityComparer) (a: 'T) (b: 'T) = c.Equals(a, b)
                                
[<Inline>]
let inline GenericZero< ^T when ^T: (static member Zero: ^T)>() = 
    (^T: (static member Zero: ^T) ())

[<Inline>]
let inline GenericOne< ^T when ^T: (static member One: ^T)>() = 
    (^T: (static member One: ^T) ())

[<Inline; Macro(typeof<M.DivideByIntMacro>)>]
let inline DivideByInt< ^T when ^T : (static member DivideByInt : ^T * int -> ^T)>(x: ^T) (y: int): ^T =
    (^T : (static member DivideByInt : ^T * int -> ^T) (x, y))

[<Inline>]
let FastGenericComparer<'T>() = 
    { new System.Collections.Generic.IComparer<'T> with member _.Compare(a, b) = Unchecked.compare<'T> a b }

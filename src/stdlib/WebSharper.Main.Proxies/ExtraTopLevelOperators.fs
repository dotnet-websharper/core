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

[<WebSharper.Proxy "Microsoft.FSharp.Core.ExtraTopLevelOperators, FSharp.Core">]
module private WebSharper.ExtraTopLevelOperatorsProxy

open System.Collections.Generic

open WebSharper.JavaScript
module M = WebSharper.Core.Macros

[<Inline "null">]
let DefaultAsyncBuilder : Control.AsyncBuilder =
    As (AsyncBuilderProxy())

[<Name "create2D" >]
let CreateArray2D (rows : seq<#seq<'T>>) =
    let arr = rows |> Seq.map (Array.ofSeq) |> Array.ofSeq |> As<'T[,]>
    arr?dims <- 2
    arr

[<Inline "+$0">]
let ToDouble<'T> (x: 'T) : double = X

[<Inline "$f(function(x){return x;})">]
let PrintFormatToString (f: Printf.StringFormat<'T>) = X<'T>

[<Inline; JavaScript>]
let PrintFormatToStringThen k f = Printf.ksprintf k f 

[<Inline; JavaScript>]
let PrintFormatLine f = Printf.printfn f 

[<Inline; JavaScript>]
let PrintFormatToStringThenFail f = Printf.failwithf f 

[<Inline; JavaScript>]
let SpliceExpression (e: Microsoft.FSharp.Quotations.Expr<'T>) =
    As<'T> e

[<Inline; JavaScript>]
let SpliceUntypedExpression<'T> (e: Microsoft.FSharp.Quotations.Expr) =
    As<'T> e

[<Name("dict")>]
let MakeDict (s : seq<('K * 'V)>) =
    let d = Dictionary()
    for a, b in s do
        d.Add(a, b)
    d

[<Inline>]
let CreateDictionary (s : seq<('K * 'V)>) : IDictionary<'K, 'V> =
    MakeDict s 

[<Inline>]
let CreateReadOnlyDictionary (s : seq<('K * 'V)>) : IReadOnlyDictionary<'K, 'V> =
    MakeDict s

[<Inline>]
let CreateSet (s : seq<('T)>) : Set<'T> =
    Set s 

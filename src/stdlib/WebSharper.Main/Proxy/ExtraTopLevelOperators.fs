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

[<WebSharper.Proxy
    "Microsoft.FSharp.Core.ExtraTopLevelOperators, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.ExtraTopLevelOperatorsProxy

open WebSharper.JavaScript
module M = WebSharper.Macro

[<Inline "null">]
let DefaultAsyncBuilder : Control.AsyncBuilder =
    As (AsyncBuilderProxy())

[<JavaScript>]
[<Name "WebSharper.Arrays.create2D" >]
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

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

/// Provides seq/list/array proxies
module internal WebSharper.CollectionInternals

open WebSharper.JavaScript

[<JavaScript>]
[<Name "Arrays.splitInto">]
let ArraySplitInto count (arr: 'T[]) =
    if count <= 0 then failwith "Count must be positive"
    let len = arr.Length
    if len = 0 then
        [| |]
    else
        let count = min count len
        let res = Array.zeroCreate count : 'T[][]
        let minChunkSize = len / count
        let mutable startIndex = 0
        for i = 0 to len % count - 1 do
            res.[i] <- Array.sub arr startIndex (minChunkSize + 1)
            startIndex <- startIndex + minChunkSize + 1
        for i = len % count to count - 1 do
            res.[i] <-  Array.sub arr startIndex minChunkSize
            startIndex <- startIndex + minChunkSize
        res

[<JavaScript>]
[<Name "Arrays.contains">]
let ArrayContains (item: 'T) (arr: Array<'T>)  =
    let mutable c = true
    let mutable i = 0
    let l = arr.Length
    while c && i < l do
        if arr.[i] = item then
            c <- false
        else
            i <- i + 1
    not c

[<JavaScript>]
[<Name "Arrays.tryFindBack">]
let ArrayTryFind f (arr: _ []) =
    let mutable res = None
    let mutable i = Array.length arr - 1
    while i > 0 && Option.isNone res do
        if f arr.[i] then res <- Some arr.[i]
        i <- i - 1
    res

[<JavaScript>]
[<Name "Arrays.tryFindIndexBack">]
let ArrayTryFindIndex f (arr: _ []) =
    let mutable res = None
    let mutable i = Array.length arr - 1
    while i > 0 && Option.isNone res do
        if f arr.[i] then res <- Some i
        i <- i - 1
    res

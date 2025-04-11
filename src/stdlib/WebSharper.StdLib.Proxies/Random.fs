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

[<AutoOpen>]
module RandomHelpers =
    [<Inline "Math.floor(Math.random() * 2147483648)">]
    let Next() = X<int>
    
    [<Inline "Math.floor(Math.random() * $maxValue)">]
    let NextMax (maxValue: int) = X<int>

[<Name "Random">]
[<Proxy(typeof<System.Random>)>]
type internal RandomProxy() =
    member this.Next() = Next()

    member this.Next maxValue =
        if maxValue < 0 then
            failwith "'maxValue' must be greater than zero."
        else NextMax maxValue

    member this.Next (minValue: int, maxValue: int) =
        if minValue > maxValue then
            failwith "'minValue' cannot be greater than maxValue."
        else minValue + NextMax (maxValue - minValue)

    member this.NextBytes (buffer: byte[]) =
        for i = 0 to buffer.Length - 1 do
            buffer.[i] <- As (NextMax 256)

    [<Inline "Math.random()">]
    member this.NextDouble() = X<float>

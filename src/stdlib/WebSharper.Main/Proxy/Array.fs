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

namespace WebSharper

open WebSharper.JavaScript
module F = WebSharper.IntrinsicFunctionProxy

[<Proxy(typeof<System.Array>)>]
type private ArrayProxy =

    [<Inline "$array.reverse()">]
    static member Reverse(array: System.Array) = X<unit>

    [<JavaScript>]
    [<Name "WebSharper.Arrays.reverse">]
    static member Reverse(array: System.Array, offset: int, length: int) =
        let a = Array.rev (Array.sub (As array) offset length)
        Array.blit a 0 (As array) offset a.Length

    member this.Length
        with [<Inline; JavaScript>] get() = F.GetLength (As this)            

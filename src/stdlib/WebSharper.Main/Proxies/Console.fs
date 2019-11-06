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

#if JAVASCRIPT
module internal WebSharper.Console

type C = JavaScript.Console

[<Proxy(typeof<System.Console>)>]
type ConsoleProxy =

    [<Inline>]
    static member WriteLine() = C.Log("")
    [<Inline>]
    static member WriteLine(x: bool) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: char) = C.Log(string x)
    [<Inline>]
    static member WriteLine(x: char[]) = C.Log(System.String(x))
    [<Inline>]
    static member WriteLine(x: char[], s: int, c: int) = C.Log(System.String(Array.sub x s c))
    [<Inline>]
    static member WriteLine(x: float) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: int) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: int64) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: obj) = C.Log(string x)
    [<Inline>]
    static member WriteLine(x: single) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: string) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: string, o1: obj) = C.Log(System.String.Format(x, o1))
    [<Inline>]
    static member WriteLine(x: string, o1: obj, o2: obj) = C.Log(System.String.Format(x, o1, o2))
    [<Inline>]
    static member WriteLine(x: string, o1: obj, o2: obj, o3: obj) = C.Log(System.String.Format(x, o1, o2, o3))
    [<Inline>]
    static member WriteLine(x: string, o1: obj, o2: obj, o3: obj, o4: obj) = C.Log(System.String.Format(x, o1, o2, o3, o4))
    [<Inline>]
    static member WriteLine(x: string, o: obj[]) =  C.Log(System.String.Format(x, o))
    [<Inline>]
    static member WriteLine(x: uint32) = C.Log(x)
    [<Inline>]
    static member WriteLine(x: uint64) = C.Log(x)
#else
namespace WebSharper
#endif
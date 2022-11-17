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

[<WebSharper.Proxy
    "Microsoft.FSharp.Core.ExtraTopLevelOperators, \
     FSharp.Core, Culture=neutral, \
     PublicKeyToken=b03f5f7f11d50a3a">]
module private WebSharper.Collections.ExtraTopLevelOperatorsProxy

open WebSharper

open System.Collections.Generic

[<Name("dict")>]
let private MakeDict (s : seq<('K * 'V)>) =
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

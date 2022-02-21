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

module private WebSharper.Collections.ReadOnlyCollection

open WebSharper
open WebSharper.JavaScript

open System.Collections.Generic

[<Proxy(typeof<System.Collections.ObjectModel.ReadOnlyCollection<_>>)>]
[<Name "WebSharper.Collections.ReadOnlyCollection">]
type ReadOnlyCollectionProxy<'T> =

    [<Inline>]
    static member CtorProxy (arr: IList<'T>) = JS.Inline("$wsruntime.MarkReadOnly($0)", Array.ofSeq arr)
    
    member this.Item with [<Inline "$this[$i]">] get (i: int) = X<'T>

    [<Inline "$this.length">]
    member this.Count = X<int>

[<Proxy(typeof<System.Array>)>]
type private ArrayProxy =

    [<Inline>]
    static member AsReadOnly(array: 'T[]) =
        new System.Collections.ObjectModel.ReadOnlyCollection<'T>(array)

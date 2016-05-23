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

module private WebSharper.Collections.ReadOnlyCollection

open WebSharper
open WebSharper.JavaScript

open System.Collections.Generic

[<Proxy(typeof<System.Collections.ObjectModel.ReadOnlyCollection<_>>)>]
[<Name "WebSharper.Collections.ReadOnlyCollection">]
type ReadOnlyCollectionProxy<'T> =
    [<Inline "WebSharper.Arrays.ofSeq($arr)">] 
    new (arr: IList<'T>) = { }
    
    member this.Item with [<Inline "$this[$i]">] get (i: int) = X<'T>

    [<Inline "$this.length">]
    member this.Count = X<int>

//    interface seq<'T> with
//        [<Inline>]
//        member this.GetEnumerator() = As<System.Collections.IEnumerator>(((As<'T[]> this) :> seq<'T>).GetEnumerator())
//        [<Inline>]
//        member this.GetEnumerator() = ((As<'T[]> this) :> seq<'T>).GetEnumerator()


    


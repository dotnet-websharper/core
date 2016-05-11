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

namespace WebSharper

open System.Collections.Generic

[<Proxy(typeof<System.Collections.IEnumerable>)>]  
type private IEnumerableProxy =

    [<Name "GetEnumerator0">]
    abstract GetEnumerator : unit -> System.Collections.IEnumerator

[<Proxy(typeof<seq<_>>)>]  
type private IEnumerableProxy<'T> =
    inherit System.Collections.IEnumerable 
    
    [<Name "GetEnumerator">]
    abstract GetEnumerator : unit -> IEnumerator<'T>
    
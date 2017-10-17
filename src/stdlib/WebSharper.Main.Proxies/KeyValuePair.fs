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

open WebSharper.JavaScript

[<Proxy(typeof<System.Collections.Generic.KeyValuePair<_,_>>)>]
[<Name "WebSharper.KeyValuePair">]
[<Prototype false>]
type private KeyValuePairProxy<'K,'V> =
    {
        K: 'K
        V: 'V
    }
    [<Inline>]
    member this.Key = this.K
    [<Inline>]
    member this.Value = this.V

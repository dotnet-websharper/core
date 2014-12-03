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

namespace IntelliFactory.WebSharper

type private CT  = System.Threading.CancellationToken

[<Proxy(typeof<System.Threading.CancellationTokenSource>)>]
type private CancellationTokenSourceProxy =
    [<Inline "{run: true}">]
    new () = {}

    [<Inline "void ($this.run = false)">]
    member this.Cancel() = X<unit>

    member this.IsCancellationRequested with [<Inline "! $this.run">] get() = X<bool>

    member this.Token with [<Inline "$this">] get() = X<CT>   

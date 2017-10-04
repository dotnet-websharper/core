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

/// Defines operators and functions that are automatically available whenever
/// `WebSharper` is open.
[<AutoOpen>]
module WebSharper.Pervasives

/// Marks a server-side function to be invokable remotely from the client-side.
type RpcAttribute = RemoteAttribute

/// Re-exports Remoting.AddHandler.
let AddRpcHandler (t: System.Type) (h: obj) =
    WebSharper.Core.Remoting.AddHandler t h

/// Implements piping with mutation.
[<Inline "($f($x), $x)">]
let ( |>! ) x f = f x; x

[<Constant true>]
let IsClient = false

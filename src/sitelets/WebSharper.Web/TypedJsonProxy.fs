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
open WebSharper.ClientSideJson
open WebSharper.ClientSideJson.Macro
open System.Threading.Tasks

[<Proxy(typeof<WebSharper.Json>)>]
type internal TypedJsonProxy =

    [<Macro(typeof<SerializeMacro>)>]
    static member Encode<'T> (x: 'T) = X<obj>

    [<Macro(typeof<SerializeMacro>)>]
    static member Serialize<'T> (x: 'T) = X<string>

    [<Macro(typeof<SerializeMacro>)>]
    static member Decode<'T> (x: obj) = X<'T>

    [<Macro(typeof<SerializeMacro>)>]
    static member Deserialize<'T> (x: string) = X<'T>

    [<Inline>]
    static member DecodeAsync<'T> (x: Async<obj>) : Async<'T> =
        async.Bind(x, fun o -> async.Return (Json.Decode<'T> o))

    [<Inline>]
    static member DecodeTask<'T> (x: Task<obj>) : Task<'T> =
        x.ContinueWith(System.Func<Task<obj>, 'T>(fun t -> Json.Decode<'T>(t.Result)))

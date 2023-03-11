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

[<WebSharper.NameAttribute "Result">]
[<WebSharper.Proxy "Microsoft.FSharp.Core.ResultModule, FSharp.Core">]
module private WebSharper.ResultModuleProxy
    
let Bind f r =
    match r with
    | Ok x -> f x
    | Error e -> Error e
        
let Map f r =
    match r with
    | Ok x -> Ok (f x)
    | Error e -> Error e
        
let MapError f r =
    match r with
    | Ok x -> Ok x
    | Error e -> Error (f e)    

let IsOk result =
    match result with
    | Ok _ -> true
    | Error _ -> false

let IsError result =
    match result with
    | Ok _ -> false
    | Error _ -> true

let DefaultValue value result =
    match result with
    | Error _ -> value
    | Ok v -> v

let DefaultWith defThunk result =
    match result with
    | Error error -> defThunk error
    | Ok v -> v

let Count result =
    match result with
    | Error _ -> 0
    | Ok _ -> 1

let Fold<'T, 'Error, 'State> folder (state: 'State) (result: Result<'T, 'Error>) =
    match result with
    | Error _ -> state
    | Ok x -> folder state x

let FoldBack<'T, 'Error, 'State> folder (result: Result<'T, 'Error>) (state: 'State) =
    match result with
    | Error _ -> state
    | Ok x -> folder x state

let Exists predicate result =
    match result with
    | Error _ -> false
    | Ok x -> predicate x

let ForAll predicate result =
    match result with
    | Error _ -> true
    | Ok x -> predicate x

let Contains value result =
    match result with
    | Error _ -> false
    | Ok v -> v = value

let Iterate action result =
    match result with
    | Error _ -> ()
    | Ok x -> action x

let ToArray result =
    match result with
    | Error _ -> [||]
    | Ok x -> [| x |]

let ToList result =
    match result with
    | Error _ -> []
    | Ok x -> [ x ]

let ToSeq result =
    match result with
    | Error _ -> []
    | Ok x -> [ x ]

let ToOption result =
    match result with
    | Error _ -> None
    | Ok x -> Some x

let ToValueOption result =
    match result with
    | Error _ -> ValueNone
    | Ok x -> ValueSome x
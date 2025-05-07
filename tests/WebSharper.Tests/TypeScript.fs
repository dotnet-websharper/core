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

module WebSharper.Tests.TypeScript

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

type Storage<'T> =
    [<Name "SInit">]
    abstract member Init : unit -> 'T[]

type Serializer<'T> =
    {
        Encode : 'T -> obj
        Decode : obj -> 'T
    }

[<JavaScript>]
type private LocalStorageBackend<'T>(id : string, serializer : Serializer<'T>) =
    let storage = JS.Window.LocalStorage

    interface Storage<'T> with
        member x.Init () =
            let item = storage.GetItem(id)
            if item = null then [||]
            else 
                try
                    let arr = As<obj []> <| Json.Parse(item)
                    arr |> Array.map (fun a -> serializer.Decode a)
                with _ -> [||]

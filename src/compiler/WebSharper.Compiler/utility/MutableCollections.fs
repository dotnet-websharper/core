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

namespace WebSharper

module MutableCollections =
    open System
    open System.Collections.Generic

    [<Sealed>]
    type DictionarySet<'T1,'T2 when 'T1 : equality and 'T2 : equality>() =
        let state = Dictionary<'T1,HashSet<'T2>>()

        member bag.Add(key, value) =
            match state.TryGetValue(key) with
            | true, xs -> xs.Add(value) |> ignore
            | _ -> state.Add(key, HashSet([value]))

        member bag.Find(key) =
            match state.TryGetValue(key) with
            | true, xs -> xs :> seq<_>
            | _ -> Seq.empty

    [<Sealed>]
    type MultiDictionary<'T1,'T2 when 'T1 : equality>() =
        let state = Dictionary<'T1,ResizeArray<'T2>>()

        member d.Add(key, value) =
            match state.TryGetValue(key) with
            | true, xs -> xs.Add(value) |> ignore
            | _ -> state.Add(key, ResizeArray([value]))

        member d.Find(key) =
            match state.TryGetValue(key) with
            | true, xs -> xs :> seq<_>
            | _ -> Seq.empty

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper

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

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

namespace WebSharper.UI
open WebSharper

open System.Collections.Generic

type AppendList<'T> =
    | AL0
    | AL1 of 'T
    | AL2 of AppendList<'T> * AppendList<'T>
    | AL3 of 'T []

[<JavaScript>]
module AppendList =

    type T<'T> = AppendList<'T>

    let Empty<'T> : T<'T> = AL0

    let Append x y =
        match x, y with
        | AL0, x | x, AL0 -> x
        | _ -> AL2 (x, y)

    //let Concat xs =
    //    Array.ofSeqNonCopying xs
    //    |> Array.TreeReduce Empty Append

    let Single x =
        AL1 x

    let ToArray xs =
        let out = Queue()
        let rec loop xs =
            match xs with
            | AL0 -> ()
            | AL1 x -> out.Enqueue x
            | AL2 (x, y) -> loop x; loop y
            | AL3 xs -> Array.iter (fun v -> out.Enqueue v) xs
        loop xs
        out.ToArray()

    let FromArray xs =
        match Array.length xs with
        | 0 -> AL0
        | 1 -> AL1 xs.[0]
        | _ -> AL3 (Array.copy xs)

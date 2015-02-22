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
namespace WebSharper.Formlets

open WebSharper
open WebSharper.Html.Client

module internal Utils =

    [<JavaScript>]
    let Maybe d f o =
        match o with
        | Some x    -> f x
        | None      -> d

    [<JavaScript>]
    let MapOption (f: 'T -> 'U) (value: option<'T>) =
        match value with
        | None      -> None
        | Some v    -> Some (f v)

    [<JavaScript>]
    let InTable (rows : list<list<Pagelet>>) : Element =
        let rs =
            rows
            |> List.map (fun cols ->
                let xs =
                    cols
                    |> List.map (fun c -> TD [c])
                TR (unbox (box xs))
            )
        let tb = TBody (unbox box rs)
        Table [tb]

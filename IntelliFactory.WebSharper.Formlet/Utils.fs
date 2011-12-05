// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
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
namespace IntelliFactory.WebSharper.Formlet

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

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
    let InTable (rows : list<list<IPagelet>>) : Html.Element =
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

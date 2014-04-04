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

module F = IntelliFactory.WebSharper.IntrinsicFunctionProxy

[<Proxy(typeof<System.Array>)>]
type private ArrayProxy =

    [<Inline "$array.reverse()">]
    static member Reverse(array: System.Array) = X<unit>

    [<JavaScript>]
    [<Name "IntelliFactory.WebSharper.Arrays.reverse">]
    static member Reverse(array: System.Array, offset: int, length: int) =
        let a = Array.rev (Array.sub (As array) offset length)
        Array.blit a 0 (As array) offset a.Length

    member this.Length
        with [<Inline; JavaScript>] get() = F.GetLength (As this)            
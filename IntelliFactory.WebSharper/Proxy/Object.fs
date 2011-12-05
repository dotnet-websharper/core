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

namespace IntelliFactory.WebSharper

#nowarn "864"
open IntelliFactory.WebSharper

[<Proxy(typeof<System.Object>)>]
type private ObjectProxy =

    [<Inline "{}">]
    new () = {}

    [<Inline>]
    [<JavaScript>]
    member this.GetHashCode() = Unchecked.hash this

    [<Inline>]
    [<JavaScript>]
    member this.Equals(obj: obj) = Unchecked.equals (this :> obj) obj

    [<Inline>]
    [<JavaScript>]
    static member Equals(a: obj, b: obj) = Unchecked.equals a b

    [<Inline "$a === $b">]
    static member ReferenceEquals(a: obj, b: obj) = a ===. b

    [<Inline "String($this)">]
    member this.ToString() = X<string>

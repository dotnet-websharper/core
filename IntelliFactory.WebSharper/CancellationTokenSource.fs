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

type private CT  = System.Threading.CancellationToken

[<Proxy(typeof<System.Threading.CancellationTokenSource>)>]
type private CancellationTokenSourceProxy =
    [<Inline "{run: true}">]
    new () = {}

    [<Inline "void ($this.run = false)">]
    member this.Cancel() = X<unit>

    member this.IsCancellationRequested with [<Inline "! $this.run">] get() = X<bool>

    member this.Token with [<Inline "$this">] get() = X<CT>   

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

[<Proxy(typeof<double>)>]
type private DoubleProxy =

    [<Inline "Math.abs($0) === Infinity">]
    static member IsInfinity(f: double) = X<bool>

    [<Inline "isNaN($0)">]
    static member IsNaN(f: double) = X<bool>

    [<Inline "$0 === -Infinity">]
    static member IsNegativeInfinity (f: double) = X<bool>

    [<Inline "$0 === Infinity">]
    static member IsPositiveInfinity (f: double) = X<bool>

    [<Inline "parseFloat($0)">]
    static member Parse(s: string) = X<double>

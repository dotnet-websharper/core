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

namespace IntelliFactory.WebSharper.Collections

open IntelliFactory.WebSharper

/// Represents a key-value pair with comparison and equality
/// ignoring the value and using only the key.
[<CustomComparison>]
[<CustomEquality>]
type internal Pair<'K,'V when 'K : comparison> =
    {
        Key     : 'K
        Value   : 'V
    }

    [<JavaScript>]
    override this.GetHashCode() = hash this.Key

    [<JavaScript>]
    override this.Equals(other: obj) =
        this.Key = (other :?> Pair<'K,'V>).Key

    interface System.IComparable with
        [<JavaScript>]
        member this.CompareTo(other: obj) =
            compare this.Key (other :?> Pair<'K,'V>).Key

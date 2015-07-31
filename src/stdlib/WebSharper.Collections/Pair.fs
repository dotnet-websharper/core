// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

namespace WebSharper.Collections

open WebSharper

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

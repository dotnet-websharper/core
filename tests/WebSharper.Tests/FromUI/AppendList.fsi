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

/// A list that does not punish too much for appending.
type internal AppendList<'T>

/// Operations on append-lists.
module internal AppendList =

    /// The type synonym.
    type T<'T> = AppendList<'T>

    /// The empty list.
    val Empty<'T> : T<'T>

    /// Appends two lists.
    val Append<'T> : T<'T> -> T<'T> -> T<'T>

    ///// Concatenates many lists.
    //val Concat<'T> : seq<T<'T>> -> T<'T>

    /// Constructs a singleton list.
    val Single : 'T -> T<'T>

    /// Flattens to an array.
    val ToArray : T<'T> -> 'T[]

    /// Constructs from an array.
    val FromArray : 'T[] -> T<'T>

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

namespace IntelliFactory.WebSharper

/// Implements collections used internally by the compiler.
module internal MutableCollections =

    /// A mutable dictionary with a set of values corresponding to each key.
    [<Sealed>]
    type DictionarySet<'T1,'T2 when 'T1 : equality and 'T2 : equality> =

        /// Constructs a new instance.
        new : unit -> DictionarySet<'T1,'T2>

        /// Adds a key-value pair.
        member Add : key: 'T1 * value: 'T2 -> unit

        /// Looks up the set corresponding to a key.
        member Find : 'T1 -> seq<'T2>

    /// A dictionary with multiple values corresponding to each key.
    [<Sealed>]
    type MultiDictionary<'T1,'T2 when 'T1 : equality> =

        /// Constructs a new instance.
        new : unit -> MultiDictionary<'T1,'T2>

        /// Adds a key-value pair.
        member Add : key: 'T1 * value: 'T2 -> unit

        /// Looks up the sequence corresponding to a key.
        member Find : 'T1 -> seq<'T2>
 

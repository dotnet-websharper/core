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
 
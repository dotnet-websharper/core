// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Compiler

/// Utilities for inserting a layer of appdomain isolation.
module internal AppDomainUtility =

    /// Represents an algorithm transforming 'T1 to 'T2.
    type ITransform<'T1,'T2> =

        /// Entry-point to the algorithm.
        abstract Do : 'T1 -> 'T2

    /// Marks a given type.
    [<Sealed>]
    type TypeMarker<'T>

    /// Constructs a type marker.
    val MarkType<'T> : TypeMarker<'T>

    /// Performs a transform in a dedicated AppDomain.
    val TransformWithAppDomain<'A,'B,'T when 'T :> ITransform<'A,'B> and 'T : (new : unit -> 'T)> :
        TypeMarker<'T> -> 'A -> 'B

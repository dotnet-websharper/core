// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

/// Defines macros used by proxy definitions.
module private IntelliFactory.WebSharper.Macro

module M = IntelliFactory.WebSharper.Core.Macros

[<Sealed>]
type Division =
    new : unit -> Division
    interface M.IMacroDefinition

[<Sealed>]
type Char =
    new : unit -> Char
    interface M.IMacroDefinition

[<Sealed>]
type String =
    new : unit -> String
    interface M.IMacroDefinition

[<AbstractClass>]
type CMP =
    interface M.IMacroDefinition

[<Sealed>]
type EQ =
    new : unit -> EQ
    inherit CMP

[<Sealed>]
type NE =
    new : unit -> NE
    inherit CMP

[<Sealed>]
type LT =
    new : unit -> LT
    inherit CMP

[<Sealed>]
type GT =
    new : unit -> GT
    inherit CMP

[<Sealed>]
type LE =
    new : unit -> LE
    inherit CMP

[<Sealed>]
type GE =
    new : unit -> GE
    inherit CMP

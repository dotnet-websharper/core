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

/// Converts mixed Core/Syntax packages to JavaScript.
module IntelliFactory.JavaScript.Packager

module C = IntelliFactory.JavaScript.Core
module S = IntelliFactory.JavaScript.Syntax

/// Represents qualified JavaScript names.
type Address =
    | Global of string
    | Local of Address * string

    /// The parent address, if any.
    member Parent : option<Address>

    /// The local part of the address.
    member LocalName : string

/// Unifies core and syntactic expressions.
type Expression =
    | Core of C.Expression
    | Syntax of S.Expression

/// Represents members. Fields are thunks that are lazily instantiated.
type Member =
    | Field of Expression
    | Method of Expression

type Binding =
    | Class of Class
    | Module of Module
    | Member of Member

and Module = Map<string,Binding>

and Class =
    {
        Base : option<Address>
        Prototype : Map<string,Member>
        Static : Module
    }

/// Converts a module to a program that defines it.
/// The produced programs depend on Runtime.js or Runtime.min.js embedded
/// as a resource within this assembly.
val Package : Module -> Preferences -> Syntax.Program

/// Removes unnecessary empty module bindings.
val Simplify : Module -> Module


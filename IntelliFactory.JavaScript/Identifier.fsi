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

/// Provides utilities for working with JavaScript identifier names.
module IntelliFactory.JavaScript.Identifier

/// Checks if a string is a reserved word in JavaScript.
val IsReserved : string -> bool

/// Checks if a string is a valid JavaScript identifier name.
val IsValid : string -> bool

/// Replaces bad characters by underscore to make an identifier valid.
val MakeValid : string -> string

/// Constructs a compact numeric identifier formatter.
val internal MakeFormatter : unit -> (int -> string)

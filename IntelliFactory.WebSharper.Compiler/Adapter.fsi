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

/// Adapts Mono.Cecil representations to QuotationReader representations.
module internal IntelliFactory.WebSharper.Compiler.Adapter

open Mono.Cecil

module R  = IntelliFactory.WebSharper.Core.Reflection
module QR = IntelliFactory.WebSharper.Core.Quotations

/// Adapts a type definition (a class or a module).
val AdaptTypeDefinition : TypeReference -> R.TypeDefinition

/// Adapts a type.
val AdaptType : TypeReference -> R.Type

/// Adapts a constructor.
val AdaptConstructor : MethodReference -> R.Constructor

/// Adapts a method.
val AdaptMethod : MethodReference -> R.Method

/// Adapts a property.
val AdaptProperty : PropertyReference -> R.Property

/// Adapts a union case.
val AdaptUnionCase : TypeReference -> Name -> R.UnionCase

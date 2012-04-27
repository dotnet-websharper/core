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

module internal IntelliFactory.WebSharper.Compiler.Locator

module R = IntelliFactory.WebSharper.Core.Reflection

val LocateAssembly : Mono.Cecil.AssemblyDefinition -> Location
val LocateMethod : Mono.Cecil.MethodDefinition -> Location
val LocateProperty : Mono.Cecil.PropertyDefinition -> Location
val LocateType : Mono.Cecil.TypeDefinition -> Location
val LocateReflectedType : R.Type -> Location


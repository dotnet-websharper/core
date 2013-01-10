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

module IntelliFactory.WebSharper.Compiler.Locator

module R = IntelliFactory.WebSharper.Core.Reflection

let LocateAssembly (def: AssemblyDefinition) : Location =
    {
        ReadableLocation = def.FullName
        SourceLocation = None
    }

let LocateMethod (def: MethodDefinition) : Location =
    {
        ReadableLocation = def.Name
        SourceLocation = def.SourceLocation
    }

let LocateProperty (def: PropertyDefinition) : Location =
    {
        ReadableLocation = def.Name
        SourceLocation =
            let o x = Option.map x
            match def.GetMethod with
            | None ->
                match def.SetMethod with
                | None -> None
                | Some m -> (LocateMethod m).SourceLocation
            | Some m -> (LocateMethod m).SourceLocation
    }

let LocateType (def: TypeDefinition) : Location =
    {
        ReadableLocation = def.Name
        SourceLocation = None
    }

let LocateReflectedType (def: R.Type) : Location =
    {
        ReadableLocation = def.Name
        SourceLocation = None
    }

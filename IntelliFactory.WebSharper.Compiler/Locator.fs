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

module IntelliFactory.WebSharper.Compiler.Locator

module R = IntelliFactory.WebSharper.Core.Reflection

let LocateAssembly (def: Mono.Cecil.AssemblyDefinition) : Location =
    {
        ReadableLocation = string def
        SourceLocation = None
    }

let LocateMethod (def: Mono.Cecil.MethodDefinition) : Location =
    {
        ReadableLocation = string def
        SourceLocation =
            if def.HasBody && def.Body.Instructions <> null then
                def.Body.Instructions
                |> Seq.tryPick (fun i ->
                    let sP = i.SequencePoint
                    if sP <> null then
                        Some {
                            File = sP.Document.Url
                            Line = sP.StartLine - 1
                            Column = sP.StartColumn
                        }
                    else
                        None)
            else
                None
    }

let LocateProperty (def: Mono.Cecil.PropertyDefinition) : Location =
    {
        ReadableLocation = string def
        SourceLocation =
            let o x = Option.map x 
            match def.GetMethod with
            | null ->
                match def.SetMethod with
                | null -> None
                | m -> (LocateMethod m).SourceLocation
            | m -> (LocateMethod m).SourceLocation
    }

let LocateType (def: Mono.Cecil.TypeDefinition) : Location =
    {
        ReadableLocation = string def
        SourceLocation = None
    }

let LocateReflectedType (def: R.Type) : Location =
    {
        ReadableLocation = string def
        SourceLocation = None
    }

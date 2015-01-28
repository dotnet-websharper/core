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

let LocateField (def: FieldDefinition) : Location =
    {
        ReadableLocation = def.Name
        SourceLocation = None   
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

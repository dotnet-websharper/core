// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2015 IntelliFactory
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

/// Converts mixed Core/Syntax packages to JavaScript.
module WebSharper.Core.JavaScript.Packager

module C = WebSharper.Core.JavaScript.Core
module S = WebSharper.Core.JavaScript.Syntax

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


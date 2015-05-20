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

/// Defines macros used by proxy definitions.
module private WebSharper.Macro

module M = WebSharper.Core.Macros

[<Sealed>]
type Add =
    new : unit -> Add
    interface M.IMacro

[<Sealed>]
type Sub =
    new : unit -> Sub
    interface M.IMacro

[<Sealed>]
type Division =
    new : unit -> Division
    interface M.IMacro

[<Sealed>]
type Char =
    new : unit -> Char
    interface M.IMacro

[<Sealed>]
type String =
    new : unit -> String
    interface M.IMacro

[<Sealed>]
type New =
    new : unit -> New
    interface M.IMacro

[<Sealed>]
type FuncWithArgs =
    new : unit -> FuncWithArgs
    interface M.IMacro

[<Sealed>]
type FuncWithArgsRest =
    new : unit -> FuncWithArgsRest
    interface M.IMacro

[<Sealed>]
type FuncWithThis =
    new : unit -> FuncWithThis
    interface M.IMacro

[<Sealed>]
type PrintF =
    new : unit -> PrintF
    interface M.IMacro

[<AbstractClass>]
type CMP =
    interface M.IMacro

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

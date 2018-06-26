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
module WebSharper.Core.Macros

open WebSharper.Core

[<Sealed>]
type Arith =
    inherit Macro
    new : unit -> Arith

[<Sealed>]
type NumericMacro =
    inherit Macro
    new : unit -> NumericMacro

[<Sealed>]
type Char =
    new : unit -> Char
    inherit Macro

[<Sealed>]
type Range =
    new : unit -> Range
    inherit Macro

[<Sealed>]
type Conversion =
    new : unit -> Conversion
    inherit Macro

[<Sealed>]
type Abs =
    new : unit -> Abs
    inherit Macro

[<Sealed>]
type Sign =
    new : unit -> Sign
    inherit Macro

[<Sealed>]
type String =
    new : unit -> String
    inherit Macro

[<Sealed>]
type New =
    new : unit -> New
    inherit Macro

[<Sealed>]
type FuncWithArgs =
    new : unit -> FuncWithArgs
    inherit Macro

[<Sealed>]
type FuncWithArgsRest =
    new : unit -> FuncWithArgsRest
    inherit Macro

[<Sealed>]
type FuncWithThis =
    new : unit -> FuncWithThis
    inherit Macro

[<Sealed>]
type JSThisCall =
    new : unit -> JSThisCall
    inherit Macro

[<Sealed>]
type JSParamsCall =
    new : unit -> JSParamsCall
    inherit Macro

[<Sealed>]
type JSThisParamsCall =
    new : unit -> JSThisParamsCall
    inherit Macro

[<Sealed>]
type GetJS =
    new : unit -> GetJS
    inherit Macro

[<Sealed>]
type PrintF =
    new : unit -> PrintF
    inherit Macro

[<Sealed>]
type Comp =
    inherit Macro
    new : unit -> Comp

[<Sealed>]
type EqualityComparer =
    new : unit -> EqualityComparer
    static member GetDefault : Metadata.ICompilation * AST.Type -> MacroResult
    inherit Macro

[<Sealed>]
type Comparer =
    new : unit -> Comparer
    static member GetDefault : Metadata.ICompilation * AST.Type -> MacroResult
    inherit Macro

[<Sealed>]
type DefaultOf =
    new : unit -> DefaultOf
    inherit Macro

[<Sealed>]
type DefaultToUndefined =
    new : unit -> DefaultToUndefined
    inherit Macro

[<Sealed>]
type TypeTest =
    new : unit -> TypeTest
    inherit Macro

[<Sealed>]
type InlineJS =
    new : unit -> InlineJS
    inherit Macro

[<Sealed>]
type StringFormat =
    new : unit -> StringFormat
    inherit Macro

[<Sealed>]
type Tuple =
    new : unit -> Tuple
    inherit Macro

[<Sealed>]
type TupleExtensions =
    new : unit -> TupleExtensions
    inherit Macro

val UncheckedEquals : AST.Expression -> AST.Expression -> AST.Expression
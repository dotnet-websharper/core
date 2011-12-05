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

/// Defines macros used by proxy definitions.
module IntelliFactory.WebSharper.Macro

module C = IntelliFactory.JavaScript.Core
module M = IntelliFactory.WebSharper.Core.Macros
module Q = IntelliFactory.WebSharper.Core.Quotations
module R = IntelliFactory.WebSharper.Core.Reflection

let macro x : M.Macro =
    {
        Body         = None
        Expand       = x
        Requirements = []
    }

let isIntegral (t: R.Type) =
    match t with
    | R.Type.Concrete (d, _) ->
        match d.FullName with
        | "System.Decimal"
        | "System.Byte"
        | "System.SByte"
        | "System.Int16"
        | "System.Int32"
        | "System.Int64"
        | "System.UInt16"
        | "System.UInt32"
        | "System.UInt64" -> true
        | _               -> false
    | _ ->
        false

let isScalar (t: R.Type) =
    isIntegral t ||
    match t with
    | R.Type.Concrete (d, _) ->
        match d.FullName with
        | "System.Char"
        | "System.Double"
        | "System.Single"
        | "System.String" -> true
        | _               -> false
    | _ ->
        false

let divisionMacro = macro <| fun tr q ->
    match q with
    | Q.Call (m, [x; y])
    | Q.CallModule (m, [x; y]) ->
        match m.Generics with
        | t :: _ -> if isIntegral t
                    then C.Binary (tr x / tr y,
                                   C.BinaryOperator.``>>``,
                                   !~ (C.Integer 0L))
                    else tr x / tr y
        | _      -> tr x / tr y
    | _ ->
        tr q

[<Sealed>]
type Division() =
    interface M.IMacroDefinition with
        member this.Macro = divisionMacro

type Comparison =
    | ``<``  = 0
    | ``<=`` = 1
    | ``>``  = 2
    | ``>=`` = 3
    | ``=``  = 4
    | ``<>`` = 5

type B = C.BinaryOperator

let toBinaryOperator cmp =
    match cmp with
    | Comparison.``<``  -> B.``<``
    | Comparison.``<=`` -> B.``<=``
    | Comparison.``>``  -> B.``>``
    | Comparison.``>=`` -> B.``>=``
    | Comparison.``=``  -> B.``===``
    | _                 -> B.``!==``

let makeComparison cmp x y =
    let u       = C.Global ["IntelliFactory"; "WebSharper"; "Unchecked"]
    let f m x y = C.Call (u, !~ (C.String m), [x; y])
    let c b i   = C.Binary (f "Compare" x y, b, !~ (C.Integer (int64 i)))
    match cmp with
    | Comparison.``<``  -> c B.``===`` -1
    | Comparison.``<=`` -> c B.``<=`` 0
    | Comparison.``>``  -> c B.``===`` 1
    | Comparison.``>=`` -> c B.``>=`` 0
    | Comparison.``=``  -> f "Equals" x y
    | _                 -> C.Unary (C.UnaryOperator.``!``, f "Equals" x y)

let comparisonMacro cmp = macro <| fun tr q ->
    match q with
    | Q.Call (m, [x; y])
    | Q.CallModule (m, [x; y]) ->
        match m.Generics with
        | t :: _ ->
            if isScalar t then
                C.Binary (tr x, toBinaryOperator cmp, tr y)
            else
                makeComparison cmp (tr x) (tr y)
        | _ ->
            tr q
    | _ ->
        tr q

[<AbstractClass>]
type CMP(c: Comparison) =
    interface M.IMacroDefinition with
        member this.Macro = comparisonMacro c

[<Sealed>] type EQ() = inherit CMP(Comparison.``=``)
[<Sealed>] type NE() = inherit CMP(Comparison.``<>``)
[<Sealed>] type LT() = inherit CMP(Comparison.``<``)
[<Sealed>] type GT() = inherit CMP(Comparison.``>``)
[<Sealed>] type LE() = inherit CMP(Comparison.``<=``)
[<Sealed>] type GE() = inherit CMP(Comparison.``>=``)

let call t m x = C.Call (t, !~ (C.String m), x)
let i x = !~ (C.Integer (int64 x))

let charMacro = macro <| fun tr q ->
    match q with
    | Q.Call (m, [x])
    | Q.CallModule (m, [x]) ->
        match m.Generics with
        | t :: _ ->
            if isIntegral t then tr x else
                match t with
                | R.Type.Concrete (d, _) ->
                    match d.FullName with
                    | "System.String" -> call (tr x) "charCodeAt" [i 0]
                    | "System.Double"
                    | "System.Single" -> tr x
                    | _               -> tr q
                | _ ->
                    tr q
        | _ ->
            tr q
    | _ ->
        tr q

[<Sealed>]
type Char() =
    interface M.IMacroDefinition with
        member this.Macro = charMacro

let stringMacro = macro <| fun tr q ->
    match q with
    | Q.Call (m, [x])
    | Q.CallModule (m, [x]) ->
        match m.Generics with
        | t :: _ ->
            match t.FullName with
            | "System.Char" -> call (C.Global ["String"]) "fromCharCode" [tr x]
            | _             -> call (C.Global []) "String" [tr x]
        | _ ->
            tr q
    | _ ->
        tr q

[<Sealed>]
type String() =
    interface M.IMacroDefinition with
        member this.Macro = stringMacro

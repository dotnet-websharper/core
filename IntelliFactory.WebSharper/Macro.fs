// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

let smallIntegralTypes =
    Set [
        "System.Byte"
        "System.SByte"
        "System.Int16"
        "System.Int32"
        "System.UInt16"
        "System.UInt32"
    ]

let bigIntegralTypes =
    Set [
        "System.Decimal"
        "System.Int64"
        "System.UInt64" 
    ]

let integralTypes = smallIntegralTypes + bigIntegralTypes

let scalarTypes =
    integralTypes
    + Set [
        "System.Char"
        "System.Double"
        "System.Single"
        "System.String" 
        "System.TimeSpan"
        "System.DateTime"
    ]

let isIn (s: string Set) (t: R.Type) = 
    match t with
    | R.Type.Concrete (d, _) ->
        s.Contains d.FullName
    | _ ->
        false

let divisionMacro = macro <| fun tr q ->
    match q with
    | Q.Call (m, [x; y])
    | Q.CallModule (m, [x; y]) ->
        match m.Generics with
        | t :: _ -> if isIn smallIntegralTypes t
                    then C.Binary (tr x / tr y,
                                   C.BinaryOperator.``>>``,
                                   !~ (C.Integer 0L))
                    elif isIn bigIntegralTypes t
                    then C.Call ((C.Global ["Math"]), 
                                 !~ (C.String "floor"),
                                 [tr x / tr y])
                    else tr x / tr y
        | _      -> tr x / tr y
    | _ ->
        failwith "divisionMacro error"

let arithMacro name def = macro <| fun tr q ->
    match q with
    | Q.Call (m, [x; y])
    | Q.CallModule (m, [x; y]) ->
        match m.Generics with
        | t :: _ ->
            if isIn scalarTypes t
                then def (tr x) (tr y)
                else C.Call(tr x, C.Constant (C.String name), [tr y])
        | _ -> def (tr x) (tr y)
    | _ ->
        failwith "arithMacro error"

let addMacro = arithMacro "add" ( + )
let subMacro = arithMacro "sub" ( - )

[<Sealed>]
type Add() =
    interface M.IMacroDefinition with
        member this.Macro = addMacro

[<Sealed>]
type Sub() =
    interface M.IMacroDefinition with
        member this.Macro = subMacro

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
            if isIn scalarTypes t then
                C.Binary (tr x, toBinaryOperator cmp, tr y)
            else
                makeComparison cmp (tr x) (tr y)
        | _ ->
            failwith "comparisonMacro error"
    | _ ->
        failwith "comparisonMacro error"

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
            if isIn integralTypes t then tr x else
                match t with
                | R.Type.Concrete (d, _) ->
                    match d.FullName with
                    | "System.String" -> call (tr x) "charCodeAt" [i 0]
                    | "System.Double"
                    | "System.Single" -> tr x
                    | _               -> failwith "charMacro error"
                | _ ->
                    failwith "charMacro error"
        | _ ->
            failwith "charMacro error"
    | _ ->
        failwith "charMacro error"

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
            failwith "comparisonMacro error"
    | _ ->
        failwith "comparisonMacro error"

[<Sealed>]
type String() =
    interface M.IMacroDefinition with
        member this.Macro = stringMacro

let getFieldsList q =
    let ``is (=>)`` (m: R.Method) =
        m.DeclaringType.FullName = "IntelliFactory.WebSharper.Pervasives"
        && m.Name = "op_EqualsGreater"
    let rec getFieldsListTC l q =
        match q with
        | Q.NewUnionCase (_, [Q.NewTuple [Q.Value (Q.String n); v]; t]) ->
            getFieldsListTC ((n, v) :: l) t         
        | Q.NewUnionCase (_, [Q.CallModule (m, [Q.Value (Q.String n); v]); t])
            when m.Entity |> ``is (=>)`` ->
            getFieldsListTC ((n, v) :: l) t         
        | Q.NewUnionCase (_, []) -> Some (l |> List.rev) 
        | Q.NewArray (_,  l) ->
            l |> List.map (
                function 
                | Q.NewTuple [Q.Value (Q.String n); v] -> n, v 
                | Q.CallModule (m, [Q.Value (Q.String n); v])
                    when m.Entity |> ``is (=>)`` -> n, v
                | _ -> failwith "Wrong type of array passed to New"
            ) |> Some
        | _ -> None
    getFieldsListTC [] q

let newMacro = macro <| fun tr q ->
    match q with
    | Q.Call (_, [Q.Coerce (_, x)])
    | Q.CallModule (_, [Q.Coerce (_, x)])
    | Q.Call (_, [x])
    | Q.CallModule (_, [x]) ->
        match getFieldsList x with
        | Some xl ->
            C.NewObject (xl |> List.map (fun (n, v) -> n, tr v))
        | _ ->
            call (C.Global ["IntelliFactory"; "WebSharper"; "Pervasives"]) "NewFromList" [tr x]
    | _ ->
        failwith "newMacro error"

[<Sealed>]
type New() =
    interface M.IMacroDefinition with
        member this.Macro = newMacro
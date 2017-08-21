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
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WebSharper.Macro

open System.Collections.Generic
open System.Text.RegularExpressions

open WebSharper
open WebSharper.Core
open WebSharper.Core.AST

module M = WebSharper.Core.Metadata
module I = IgnoreSourcePos

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

let isIn (s: string Set) (t: Type) = 
    match t with
    | ConcreteType t ->
        s.Contains t.Entity.Value.FullName
    | _ ->
        false

let traitCallOp (c: MacroCall) =
    match c.Method.Generics with
    | [t; u; v] ->
        TraitCall(
            [ t; u ], 
            NonGeneric (
                Method {
                    MethodName = c.Method.Entity.Value.MethodName
                    Parameters = [ t; u ]
                    ReturnType = v
                    Generics = 0
                }
            ),
            c.Arguments
        )
    | _ ->
        failwith "F# Operator value expecting 3 type arguments"

[<Sealed>]
type Div() =
    inherit Macro()
    override this.TranslateCall(c) =
        match c.Arguments with
        | [x; y] ->
            match c.Method.Generics with
            | t :: _ ->
                if isIn smallIntegralTypes t
                then (x ^/ y) ^>> !~(Int 0)
                elif isIn bigIntegralTypes t
                then Application(Global ["Math"; "trunc"], [x ^/ y], Pure, Some 1)
                elif isIn scalarTypes t
                then x ^/ y
                else traitCallOp c
            | _ ->
                failwith "F# operator expecting 3 type arguments"
            |> MacroOk
        | _ -> MacroError "divisionMacro error"

[<AbstractClass>]
type Arith(name, op) =
    inherit Macro()
    override this.TranslateCall(c) =
        match c.Arguments with
        | [x; y] ->
            match c.Method.Generics with
            | t :: _ when not (isIn scalarTypes t) ->
                traitCallOp c
            | _ -> Binary(x, op, y)
            |> MacroOk
        | _ -> MacroError "divisionMacro error"

[<Sealed>]
type Add() = inherit Arith("op_Addition", BinaryOperator.``+``) 

[<Sealed>]
type Sub() = inherit Arith("op_Subtraction", BinaryOperator.``-``)

[<Sealed>]
type Mul() = inherit Arith("op_Multiply", BinaryOperator.``*``)

type Comparison =
    | ``<``  = 0
    | ``<=`` = 1
    | ``>``  = 2
    | ``>=`` = 3
    | ``=``  = 4
    | ``<>`` = 5

let toBinaryOperator cmp =
    match cmp with
    | Comparison.``<``  -> BinaryOperator.``<``
    | Comparison.``<=`` -> BinaryOperator.``<=``
    | Comparison.``>``  -> BinaryOperator.``>``
    | Comparison.``>=`` -> BinaryOperator.``>=``
    | Comparison.``=``  -> BinaryOperator.``===``
    | _                 -> BinaryOperator.``!==``

let opUncheckedTy, equalsMeth, compareMeth =
    match <@ Unchecked.equals 1 1 @> with
    | FSharp.Quotations.Patterns.Call (_, mi, _) ->
        let cmi = mi.DeclaringType.GetMethod("Compare")
        Reflection.ReadTypeDefinition mi.DeclaringType,
        Reflection.ReadMethod mi,
        Reflection.ReadMethod cmi
    | _ -> failwith "Expecting a Call pattern"

let makeComparison cmp x y =
    let eq x y = Call (None, NonGeneric opUncheckedTy, NonGeneric equalsMeth, [x; y]) 
    let c b i = Binary (Call(None, NonGeneric opUncheckedTy, NonGeneric compareMeth, [x; y]), b, Value(Int i))
    match cmp with
    | Comparison.``<``  -> c BinaryOperator.``===`` -1
    | Comparison.``<=`` -> c BinaryOperator.``<=`` 0
    | Comparison.``>``  -> c BinaryOperator.``===`` 1
    | Comparison.``>=`` -> c BinaryOperator.``>=`` 0
    | Comparison.``=``  -> eq x y
    | _                 -> Unary (UnaryOperator.``!``, eq x y)

let cInt i = Value (Int i)
let cString s = Value (Literal.String s)

[<AbstractClass>]
type CMP(cmp) =
    inherit Macro()
    override this.TranslateCall(c) =
        match c.Arguments with
        | [x; y] ->
            match c.Method.Generics with
            | t :: _ ->
                let comp x y =
                    Binary (x, toBinaryOperator cmp, y)
                if isIn scalarTypes t then
                    comp x y
                else
                    // optimization for checking against argumentless union cases 
                    let tryGetSingletonUnionCaseTag (x: Expression) =
                        match x with
                        | I.NewUnionCase(ct, case, []) ->
                            match c.Compilation.GetCustomTypeInfo ct.Entity with
                            | M.FSharpUnionInfo ui when not ui.HasNull ->
                                ui.Cases |> Seq.mapi (fun i c ->
                                    if c.Name = case && c.Kind = M.SingletonFSharpUnionCase then Some i else None
                                ) |> Seq.tryPick id         
                            | _ -> None
                        | _ -> None
                    
                    match tryGetSingletonUnionCaseTag x, tryGetSingletonUnionCaseTag y with
                    | Some i, Some j -> comp (cInt i) (cInt j)
                    | Some i, _ -> comp (cInt i) (y.[cString "$"])
                    | _, Some j -> comp (x.[cString "$"]) (cInt j)
                    | _ -> makeComparison cmp x y
                |> MacroOk
            | _ ->
                MacroError "comparisonMacro error"
        | _ ->
            MacroError "comparisonMacro error"

[<Sealed>] type EQ() = inherit CMP(Comparison.``=``)
[<Sealed>] type NE() = inherit CMP(Comparison.``<>``)
[<Sealed>] type LT() = inherit CMP(Comparison.``<``)
[<Sealed>] type GT() = inherit CMP(Comparison.``>``)
[<Sealed>] type LE() = inherit CMP(Comparison.``<=``)
[<Sealed>] type GE() = inherit CMP(Comparison.``>=``)

let isComparison = function
    | BinaryOperator.``<`` | BinaryOperator.``>`` | BinaryOperator.``<=`` 
    | BinaryOperator.``>=`` | BinaryOperator.``==`` | BinaryOperator.``!=`` -> true
    | _ -> false

let isOperation = function
    | BinaryOperator.``%`` | BinaryOperator.``*`` | BinaryOperator.``+``
    | BinaryOperator.``-`` | BinaryOperator.``/``
    | BinaryOperator.``<<`` | BinaryOperator.``>>`` | BinaryOperator.``|``
    | BinaryOperator.``&`` | BinaryOperator.``^``
    | BinaryOperator.``&&`` | BinaryOperator.``||`` -> true
    | _ -> false

let toComparison = function
    | BinaryOperator.``<`` -> Comparison.``<``
    | BinaryOperator.``>`` -> Comparison.``>``
    | BinaryOperator.``<=`` -> Comparison.``<=``
    | BinaryOperator.``>=`` -> Comparison.``>=``
    | BinaryOperator.``==`` -> Comparison.``=``
    | BinaryOperator.``!=`` -> Comparison.``<>``
    | _ -> failwith "Operation wasn't a comparison"

// TODO unify these with the oeprations macros

let translateComparison (t: Concrete<TypeDefinition>) a m cmp =
    match a with
    | [x; y] ->
        if scalarTypes.Contains t.Entity.Value.FullName then
            Binary (x, toBinaryOperator cmp, y)
        else
            makeComparison cmp x y
        |> MacroOk
    | _ -> MacroError "numericMacro error"

let translateOperation (t: Concrete<TypeDefinition>) a m op =
    match a with
    | [x; y] ->
        match op with
        | BinaryOperator.``/`` ->
            if smallIntegralTypes.Contains t.Entity.Value.FullName
            then (x ^/ y) ^>> !~(Int 0)
            elif bigIntegralTypes.Contains t.Entity.Value.FullName
            then Application(Global ["Math"; "trunc"], [x ^/ y], Pure, Some 1)
            else x ^/ y
        | _ ->
            Binary (x, op, y)
        |> MacroOk
    | _ -> MacroError "numericMacro error"

let formatExceptionTy, formatExceptionCtor =
    match <@ new System.FormatException() @> with
    | FSharp.Quotations.Patterns.NewObject (ci, _) ->
        Reflection.ReadTypeDefinition ci.DeclaringType,
        Reflection.ReadConstructor ci
    | _ -> failwith "Expected constructor call"

[<Sealed>]
type NumericMacro() =
    inherit Macro()

    let exprParse parsed tru fls =
        let id = Id.New(mut = false)
        Let (id, parsed,
            Conditional(Application(Global ["isNaN"], [Var id], Pure, Some 1),
                tru id,
                fls id
            )
        )

    override this.TranslateCall(c) =
        let name = c.DefiningType.Entity.Value.FullName

        let parseInt x =
            Application(Global ["parseInt"], [x], Pure, Some 1)
        let parseFloat x =
            Application(Global ["parseFloat"], [x], Pure, Some 1)

        let ex =
            Ctor(
                NonGeneric formatExceptionTy,
                formatExceptionCtor,
                [Value (String "Input string was not in a correct format.")]
            )

        match c.Method.Entity.Value.MethodName with
        | BinaryOpName op when isOperation op ->
            translateOperation c.DefiningType c.Arguments c.Method op
        | BinaryOpName op when isComparison op ->
            let cmp = toComparison op
            translateComparison c.DefiningType c.Arguments c.Method cmp
        | UnaryOpName op ->
            match c.Arguments with
            | [x] -> Unary (op, x) |> MacroOk
            | _ -> MacroError "numericMacro error"
        | "op_Increment" ->
            match c.Arguments with
            | [x] ->
                MacroOk (Binary(x, BinaryOperator.``+``, Value (Int 1)))
            | _ -> MacroError "numericMacro error"
        | "op_Decrement" ->
            match c.Arguments with
            | [x] ->
                MacroOk (Binary(x, BinaryOperator.``-``, Value (Int 1)))
            | _ -> MacroError "numericMacro error"
        | "ToString" ->
            match c.This with
            | Some self ->
                // TODO refactor to separate method
                if c.DefiningType.Entity.Value.AssemblyQualifiedName = "System.Char, mscorlib" then
                    Application(Global ["String"; "fromCharCode"], [self], Pure, Some 1)
                else 
                    Application(Global ["String"], [self], Pure, Some 1)
                |> MacroOk 
            | _ -> MacroError "numericMacro error"
        | "Parse" ->
            match c.Arguments with
            | [x] ->
                if name = "System.Single" || name = "System.Double" then
                    exprParse
                    <| parseFloat x
                    <| fun _ -> ex
                    <| fun id -> Var id
                    |> MacroOk
                else MacroError "numericMacro error"
            | _ -> MacroError "numericMacro error"
        | "TryParse" ->
            match c.Arguments with
            | [x; y] ->
                if name = "System.Single" || name = "System.Double" then
                    exprParse
                    <| parseFloat x
                    <| fun _ -> Value (Bool false)
                    <| fun id ->
                        Expression.Sequential [
                            SetRef y (Var id)
                            Value (Bool true)
                        ]
                    |> MacroOk
                else MacroError "numericMacro error"
            | _ -> MacroError "numericMacro error"
        | _ -> MacroFallback

let charTy, charParse =
    let t = typeof<System.Char>
    Reflection.ReadTypeDefinition t,
    Reflection.ReadMethod (t.GetMethod "Parse")

[<Sealed>]
type Char() =
    inherit Macro()
    override this.TranslateCall(c) =
        match c.Arguments with
        | [x] ->
            match c.Method.Generics with
            | t :: _ ->
                if isIn integralTypes t then MacroOk x else
                    match t with
                    | ConcreteType d ->
                        match d.Entity.Value.FullName with
                        | "System.String" ->
                            Call (None, NonGeneric charTy, NonGeneric charParse, [x])
                            |> MacroOk
                        | "System.Char"
                        | "System.Double"
                        | "System.Single" -> MacroOk x
                        | _               -> MacroError "charMacro error"
                    | _ ->
                        MacroError "charMacro error"
            | _ ->
                MacroError "charMacro error"
        | _ ->
            MacroError "charMacro error"

[<Sealed>]
type String() =
    inherit Macro()
    override this.TranslateCall(c) =
        match c.Arguments with
        | [x] ->
            match c.Method.Generics with
            | t :: _ ->
                match t with
                | ConcreteType d ->
                    match d.Entity.Value.FullName with
                    | "System.Char" ->
                        Application(Global ["String"; "fromCharCode"], [x], Pure, Some 1)    
                    | "System.DateTime" ->
                        Application(ItemGet(New(Global [ "Date" ], [x]), Value (Literal.String "toLocaleString"), Pure), [], Pure, None)
                    | _ ->
                        Application(Global ["String"], [x], Pure, Some 1)   
                | _ -> 
                    Application(Global ["String"], [x], Pure, Some 1)   
                |> MacroOk 
            | _ ->
                MacroError "stringMacro error"
        | [] ->
            // when unit argument is erased
            MacroOk (Value (Literal.String ""))
        | _ ->
            MacroError "stringMacro error"

let fsharpListDef =
    TypeDefinition {
        Assembly = "FSharp.Core"
        FullName = "Microsoft.FSharp.Collections.FSharpList`1"  
    }

let listModuleDef =
    TypeDefinition {
        Assembly = "FSharp.Core"
        FullName = "Microsoft.FSharp.Collections.ListModule"
    }

let listOfArrayDef =
    Method {
        MethodName = "OfArray"
        Parameters = [ ArrayType (TypeParameter 0, 1) ]
        ReturnType = GenericType fsharpListDef [ TypeParameter 0 ]
        Generics = 1      
    }

let getFieldsList q =
    let ``is (=>)`` (td: TypeDefinition) (m: Method) =
        td.Value.FullName = "WebSharper.JavaScript.Pervasives"
        && m.Value.MethodName = "op_EqualsGreater"
    let rec getFieldsListTC l q =
        let trItem i =
            match IgnoreExprSourcePos i with    
            | NewArray [I.Value (String n); v] -> n, v 
            | Call (_, td, m, [I.Value (String n); v])
                when ``is (=>)`` td.Entity m.Entity -> n, v
            | _ -> failwith "Wrong type of array passed to New"
        match IgnoreExprSourcePos q with
        | NewUnionCase (_, _, [I.NewArray [I.Value (String n); v]; t]) ->
            getFieldsListTC ((n, v) :: l) t         
        | NewUnionCase (_, _, [I.Call (_, td, m, [I.Value (String n); v]); t])
            when ``is (=>)`` td.Entity m.Entity ->
            getFieldsListTC ((n, v) :: l) t         
        | NewUnionCase (_, _, []) -> Some (l |> List.rev) 
        | Call(None, td, m, [ I.NewArray items ]) when td.Entity = listModuleDef && m.Entity = listOfArrayDef ->
            items |> List.map trItem |> Some
        | NewArray (items) ->
            items |> List.map trItem |> Some
        | _ -> None
    getFieldsListTC [] q

[<Sealed>]
type New() =
    inherit Macro()
    override this.TranslateCall(c) =
        match c.Arguments with
        | [x] -> 
            match getFieldsList x with
            | Some xl ->
                MacroOk <| Object (xl |> List.map (fun (n, v) -> n, v))
            | _ -> MacroFallback
        | _ -> MacroError "New macro Error"

//type FST = Reflection.FSharpType

module JSRuntime =
    let private runtime = ["Runtime"; "IntelliFactory"]
    let private runtimeFunc f p args = Application(GlobalAccess (Address (f :: runtime)), args, p, Some (List.length args))
    let GetOptional value = runtimeFunc "GetOptional" Pure [value]
    let SetOptional obj field value = runtimeFunc "SetOptional" NonPure [obj; field; value]
    let CreateFuncWithArgs f = runtimeFunc "CreateFuncWithArgs" Pure [f]
    let CreateFuncWithArgsRest length f = runtimeFunc "CreateFuncWithArgsRest" Pure [length; f]
    let CreateFuncWithThis f = runtimeFunc "CreateFuncWithThis" Pure [f]

[<Sealed>]
type FuncWithArgs() =
    inherit Macro()
    override this.TranslateCtor(c) =
        match c.Arguments with
        | [func] ->
            match c.DefiningType.Generics.[0] with
            | TupleType _ ->
                JSRuntime.CreateFuncWithArgs func |> MacroOk
            | _ ->
                MacroError "Wrong type argument on FuncWithArgs: 'TArgs must be a tuple"
        | _ ->
            MacroError "funcWithArgsMacro error"

[<Sealed>]
type FuncWithArgsRest() =
    inherit Macro()
    override this.TranslateCtor(c) =
        match c.Arguments with
        | [func] ->
            match c.DefiningType.Generics.[0] with
            | TupleType (ts, _) ->
                JSRuntime.CreateFuncWithArgsRest (Value (Int (List.length ts))) func |> MacroOk
            | _ ->
                MacroError "Wrong type argument on FuncWithArgsRest: 'TArgs must be a tuple"
        | _ ->
            MacroError "funcWithArgsMacro error"

[<Sealed>]
type FuncWithThis() =
    inherit Macro()
    override this.TranslateCtor(c) =
        match c.Arguments with
        | [func] ->
            match c.DefiningType.Generics.[0] with
            | FSharpFuncType _ ->
                JSRuntime.CreateFuncWithThis func |> MacroOk
            | ConcreteType td when 
                (
                    let n = td.Entity.Value.FullName
                    n = "WebSharper.JavaScript.Function" || n.StartsWith "WebSharper.JavaScript.FuncWith" 
                ) ->
                JSRuntime.CreateFuncWithThis func |> MacroOk
            | _ ->
                MacroError "Wrong type argument on FuncWithThis: 'TFunc must be an F# function or JavaScript function type"
        | _ ->
            MacroError "funcWithArgsMacro error"

let ApplItem(on, item, args) = Application(ItemGet(on, Value (AST.String item), Pure), args, NonPure, None)

[<Sealed>]
type JSThisCall() =
    inherit Macro()
    override __.TranslateCall(c) =
        match c.This with
        | Some func ->
            MacroOk (ApplItem(func, "call", c.Arguments))
        | _ -> MacroError "JSCall macro error"

[<Sealed>]
type JSParamsCall() =
    inherit Macro()
    override __.TranslateCall(c) =
        match c.This, List.rev c.Arguments with
        | Some func, pars :: revArgs ->
            let args = ApplItem(NewArray (List.rev revArgs), "concat", [pars])
            MacroOk (ApplItem(func, "apply", [Undefined; args]))
        | _ -> MacroError "JSParamsCall macro error"

[<Sealed>]
type JSThisParamsCall() =
    inherit Macro()
    override __.TranslateCall(c) =
        match c.This, c.Arguments with
        | Some func, this :: afterThis ->
            match List.rev afterThis with
            | pars :: revArgs ->    
                let args = ApplItem(NewArray (List.rev revArgs), "concat", [pars])
                MacroOk (ApplItem(func, "apply", [this; args]))
            | _ -> MacroError "JSThisParamsCall macro error"
        | _ -> MacroError "JSThisParamsCall macro error"

[<Sealed>]
type GetJS() =
    inherit Macro()
    override __.TranslateCall(c) =
        match c.Arguments with
        | [ obj ] -> MacroOk obj
        | [ obj; I.NewArray items ] ->
            if items |> List.forall (function I.Value _ -> true | _ -> false) then
                items |> List.fold (fun x i -> ItemGet(x, i, NonPure)) obj |> MacroOk
            else MacroFallback
        | [ _; _ ] -> MacroFallback
        | _ -> MacroError (sprintf "GetJS macro error, arguments: %+A" c.Arguments)

/// Set of helpers to parse format string
/// Source: https://github.com/fsharp/fsharp/blob/master/src/fsharp/FSharp.Core/printf.fs
module private FormatString =
    [<System.Flags>]
    type FormatFlags = 
        | None = 0
        | LeftJustify = 1
        | PadWithZeros = 2
        | PlusForPositives = 4
        | SpaceForPositives = 8

    let inline hasFlag flags (expected : FormatFlags) = (flags &&& expected) = expected
    let inline isLeftJustify flags = hasFlag flags FormatFlags.LeftJustify
    let inline isPadWithZeros flags = hasFlag flags FormatFlags.PadWithZeros
    let inline isPlusForPositives flags = hasFlag flags FormatFlags.PlusForPositives
    let inline isSpaceForPositives flags = hasFlag flags FormatFlags.SpaceForPositives

    /// Used for width and precision to denote that user has specified '*' flag
    [<Literal>]
    let StarValue = -1
    /// Used for width and precision to denote that corresponding value was omitted in format string
    [<Literal>]
    let NotSpecifiedValue = -2

    [<NoComparison; NoEquality>]
    type FormatSpecifier =
        {
            TypeChar : char
            Precision : int
            Width : int
            Flags : FormatFlags
        }
        member this.IsStarPrecision = this.Precision = StarValue
        member this.IsPrecisionSpecified = this.Precision <> NotSpecifiedValue
        member this.IsStarWidth = this.Width = StarValue
        member this.IsWidthSpecified = this.Width <> NotSpecifiedValue

    let inline isDigit c = c >= '0' && c <= '9'
    let intFromString (s : string) pos = 
        let rec go acc i =
            if isDigit s.[i] then 
                let n = int s.[i] - int '0'
                go (acc * 10 + n) (i + 1)
            else acc, i
        go 0 pos

    let parseFlags (s : string) i : FormatFlags * int = 
        let rec go flags i = 
            match s.[i] with
            | '0' -> go (flags ||| FormatFlags.PadWithZeros) (i + 1)
            | '+' -> go (flags ||| FormatFlags.PlusForPositives) (i + 1)
            | ' ' -> go (flags ||| FormatFlags.SpaceForPositives) (i + 1)
            | '-' -> go (flags ||| FormatFlags.LeftJustify) (i + 1)
            | _ -> flags, i
        go FormatFlags.None i

    let parseWidth (s : string) i : int * int = 
        if s.[i] = '*' then StarValue, (i + 1)
        elif isDigit (s.[i]) then intFromString s i
        else NotSpecifiedValue, i

    let parsePrecision (s : string) i : int * int = 
        if s.[i] = '.' then
            if s.[i + 1] = '*' then StarValue, i + 2
            elif isDigit (s.[i + 1]) then intFromString s (i + 1)
            else failwith "invalid precision value"
        else NotSpecifiedValue, i
    
    let parseTypeChar (s : string) i : char * int = 
        s.[i], (i + 1)

    type Part =
        | StringPart of string
        | FormatPart of FormatSpecifier

    /// modified version of FSharp.Core findNextFormatSpecifier, parses whole format string
    let parseAll (s : string) = 
        let parts = ResizeArray() 
        let rec go i (buf : System.Text.StringBuilder) =
            if i >= s.Length then 
                if buf.Length > 0 then parts.Add (StringPart (string buf))
            else
                let c = s.[i]
                if c = '%' then
                    if i + 1 < s.Length then
                        let f, i1 = parseFlags s (i + 1)
                        let w, i2 = parseWidth s i1
                        let p, i3 = parsePrecision s i2
                        let typeChar, i4 = parseTypeChar s i3
                        // shortcut for the simpliest case
                        // if typeChar is not % or it has star as width\precision - resort to long path
                        if typeChar = '%' && not (w = StarValue || p = StarValue) then 
                            buf.Append('%') |> ignore
                            go i4 buf
                        else 
                            if buf.Length > 0 then parts.Add (StringPart (string buf))
                            parts.Add (
                                FormatPart {
                                    TypeChar  = typeChar
                                    Precision = p
                                    Width     = w
                                    Flags     = f
                                }
                            )
                            go i4 (buf.Clear())
                    else
                        failwith "Missing format specifier"
                else 
                    buf.Append(c) |> ignore
                    go (i + 1) buf
        go 0 (System.Text.StringBuilder())
        parts.ToArray()

let flags =
    System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

let printfHelpersModule =
    TypeDefinition {
        FullName = "WebSharper.PrintfHelpers"
        Assembly = "WebSharper.Main"
    }
let printfHelpers (comp: M.ICompilation) f args = 
    let m = comp.GetClassInfo(printfHelpersModule).Value.Methods.Keys |> Seq.find (fun m -> m.Value.MethodName = f)
    Call(None, NonGeneric printfHelpersModule, NonGeneric m, args)

let stringModule = 
    TypeDefinition {
        FullName = "Microsoft.FSharp.Core.StringModule"
        Assembly = "FSharp.Core"
    }
let stringProxy (comp: M.ICompilation) f args =
    let m = comp.GetClassInfo(stringModule).Value.Methods.Keys |> Seq.find (fun m -> m.Value.MethodName = f)
    Call(None, NonGeneric stringModule, NonGeneric m, args)

let cCall e f args = Application (ItemGet(e, !~ (Literal.String f), Pure), args, Pure, None)
let cCallG a args = Application (Global a, args, Pure, None)

//type FST = Reflection.FSharpType

let (^+) (a: Expression) (b: Expression) =
    match a, b with
    | I.Value (String av), I.Value (String bv) -> Value (AST.String (av + bv))
    | I.Value av, I.Value bv -> Value (AST.String (av.Value.ToString() + bv.Value.ToString()))
    | _ -> a ^+ b

let createPrinter (comp: M.ICompilation) (ts: Type list) fs =
    let parts = FormatString.parseAll fs
    let args = ts |> List.map (fun t -> Id.New(mut = false), Some t)
        
    let rArgs = ref args
    let nextVar() =
        match !rArgs with
        | (a, t) :: r ->
            rArgs := r
            Var a, t
        | _ -> failwithf "wrong number of Printer type arguments found: %d" (List.length ts)  
        
    let withPadding (f: FormatString.FormatSpecifier) t =
        if f.IsWidthSpecified then
            let width = if f.IsStarWidth then nextVar() |> fst else cInt f.Width
            let s = t (nextVar())
            if FormatString.isLeftJustify f.Flags then
                stringProxy comp "PadRight" [s; width]
            else
                if FormatString.isPadWithZeros f.Flags then
                    printfHelpers comp "padNumLeft" [s; width]
                else
                    stringProxy comp "PadLeft" [s; width]
        else t (nextVar())
        
    let numberToString (f: FormatString.FormatSpecifier) t =
        withPadding f (fun (n, _) ->
            if FormatString.isPlusForPositives f.Flags then printfHelpers comp "plusForPos" [n; t n]
            elif FormatString.isSpaceForPositives f.Flags then printfHelpers comp "spaceForPos" [n; t n]
            else t n
        )

    let prettyPrint (t: Type) o = 
        let rec pp (t: Type) (o: Expression) = 
            match t with
            | TupleType (ts, _) ->
                seq {
                    yield cString "("
                    for i = 0 to ts.Length - 1 do 
                        yield pp ts.[i] o.[cInt i] 
                        if i < ts.Length - 1 then yield cString ", "
                    yield cString ")"
                }
                |> Seq.reduce (^+)
            | ArrayType (a, r) ->
                let x = Id.New(mut = false)
                match r with 
                | 1 -> printfHelpers comp "printArray" [ Lambda([x], pp a (Var x)) ; o ]
                | 2 -> printfHelpers comp "printArray2D" [ Lambda([x], pp a (Var x)) ; o ]
                | _ -> printfHelpers comp "prettyPrint" [o]
            | VoidType -> cString "null" 
            | FSharpFuncType _ -> cString "<fun>"
            | ConcreteType ct ->
                match comp.GetCustomTypeInfo ct.Entity with
                | M.FSharpRecordInfo fields ->
                    let td, m = 
                        let key = M.CompositeEntry [ M.StringEntry "Printf"; M.TypeEntry t ]
                        match comp.GetMetadataEntries key with
                        | M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ] :: _ ->
                            gtd, gm
                        | _ ->
                            let gtd, gm, _ = comp.NewGenerated([ "GeneratedPrintf"; "p"])
                            comp.AddMetadataEntry(key, M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ])
                            let body = 
                                let x = Id.New(mut = false)
                                Lambda([x], 
                                    seq {
                                        yield cString "{"
                                        let fields = Array.ofList fields
                                        let gs = ct.Generics |> Array.ofList
                                        for i = 0 to fields.Length - 1 do
                                            let f = fields.[i]
                                            let ftypRes = f.RecordFieldType.SubstituteGenerics gs
                                            let item =
                                                if f.Optional then
                                                    JSRuntime.GetOptional (ItemGet(Var x, cString f.JSName, Pure))
                                                else 
                                                    (Var x).[cString f.JSName]
                                            yield cString (f.Name + " = ") ^+ pp ftypRes item
                                            if i < fields.Length - 1 then yield cString "; "
                                        yield cString "}"
                                    }
                                    |> Seq.reduce (^+)
                                ) 
                            comp.AddGeneratedCode(gm, body) |> ignore
                            gtd, gm
                    Call(None, NonGeneric td, NonGeneric m, [o])
                | M.FSharpUnionInfo u ->
                    if ct.Entity.Value.FullName = "Microsoft.FSharp.Collections.FSharpList`1" then
                        let x = Id.New(mut = false)
                        printfHelpers comp "printList" [ Lambda([x], pp ct.Generics.[0] (Var x)) ; o ]    
                    else
                        let td, m =
                            let key = M.CompositeEntry [ M.StringEntry "Printf"; M.TypeEntry t ]
                            match comp.GetMetadataEntries key with
                            | M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ] :: _ ->
                                gtd, gm
                            | _ ->
                                let gtd, gm, _ = comp.NewGenerated([ comp.AssemblyName.Replace(".","$") + "_GeneratedPrintf"; "p" ])
                                comp.AddMetadataEntry(key, M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ])
                                let gs = ct.Generics |> Array.ofList
                                let body =
                                    let x = Id.New(mut = false)
                                    Lambda([x],                                         
                                        let caseInfo =
                                            u.Cases |> Seq.mapi (fun tag c ->
                                                match c.Kind with
                                                | M.ConstantFSharpUnionCase cVal -> 
                                                    if cVal = Null then Choice3Of3 () 
                                                    else Choice1Of3 (cVal, cString c.Name)
                                                | M.SingletonFSharpUnionCase ->
                                                    Choice2Of3(tag, cString c.Name)    
                                                | M.NormalFSharpUnionCase fs -> 
                                                    Choice2Of3(
                                                        tag,
                                                        match fs.Length with
                                                        | 0 -> cString c.Name
                                                        | 1 ->
                                                            let ityRes = fs.[0].UnionFieldType.SubstituteGenerics gs 
                                                            cString (c.Name + " ") ^+ pp ityRes (Var x).[cString "$0"]
                                                        | _ -> 
                                                            seq {
                                                                yield cString (c.Name + " (")
                                                                for i = 0 to fs.Length - 1 do
                                                                    let ityRes = fs.[i].UnionFieldType.SubstituteGenerics gs
                                                                    yield pp ityRes (Var x).[cString ("$" + string i)]
                                                                    if i < fs.Length - 1 then yield cString ", "
                                                                yield cString ")"
                                                            }
                                                            |> Seq.reduce (^+)
                                                    )
                                            )
                                        let withoutNullCheck =
                                            caseInfo
                                            |> Seq.fold (fun s cInfo ->
                                                match s with
                                                | None -> 
                                                    match cInfo with
                                                    | Choice1Of3 (_, e) 
                                                    | Choice2Of3 (_, e) -> Some e
                                                    | Choice3Of3 () -> None
                                                | Some s -> 
                                                    match cInfo with
                                                    | Choice1Of3 (cVal, e) -> Some <| Conditional (Var x ^== Value cVal, e, s)
                                                    | Choice2Of3 (tag, e) -> Some <| Conditional ((Var x).[cString "$"] ^== cInt tag, e, s)
                                                    | Choice3Of3 () -> Some s
                                            ) None |> Option.get
                                        if caseInfo |> Seq.exists (function Choice3Of3 () -> true | _ -> false) then
                                            Conditional(Var x ^== Value Null, cString "null", withoutNullCheck)    
                                        else withoutNullCheck    
                                    )
                                comp.AddGeneratedCode(gm, body) |> ignore
                                gtd, gm
                        Call(None, NonGeneric td, NonGeneric m, [o])
                | _ ->
                    printfHelpers comp "prettyPrint" [o]
            | _ -> printfHelpers comp "prettyPrint" [o]
        pp t o

    let inner = 
        if Array.isEmpty parts then cString "" else
        parts
        |> Seq.map (function
            | FormatString.StringPart s -> cString s
            | FormatString.FormatPart f ->
                match f.TypeChar with
                | 'b'
                | 'O' -> 
                    withPadding f (fun (o, _) -> cCallG ["String"] [o])
                | 'A' -> 
                    withPadding f (function 
                        | o, Some t -> 
                            prettyPrint t o
                        | o, _ -> printfHelpers comp "prettyPrint" [o]
                    )
                | 'c' -> 
                    withPadding f (fun (s, _) -> cCallG ["String"; "fromCharCode"]  [s])   
                | 's' -> 
                    withPadding f (fun (s, _) -> printfHelpers comp "toSafe" [s])
                | 'd' | 'i' ->
                    numberToString f (fun n -> cCallG ["String"] [n])
                | 'x' ->                                           
                    numberToString f (fun n -> cCall n "toString" [cInt 16])
                | 'X' ->                                           
                    numberToString f (fun n -> cCall (cCall n "toString" [cInt 16]) "toUpperCase" [])
                | 'o' ->                                           
                    numberToString f (fun n -> cCall n "toString" [cInt 8])
                | 'e' ->
                    numberToString f (fun n -> cCall n "toExponential" []) 
                | 'E' ->
                    numberToString f (fun n -> cCall (cCall n "toExponential" []) "toUpperCase" []) 
                | 'f' | 'F' | 'M' ->
                    numberToString f (fun n ->
                        let prec =
                            if f.IsPrecisionSpecified then
                                if f.IsStarPrecision then nextVar() |> fst else cInt f.Precision
                            else cInt 6 // Default precision
                        cCall n "toFixed" [prec]
                    )
                | c -> failwithf "Failed to parse format string: '%%%c' is not supported." c
        )
        |> Seq.reduce (^+)
    
    let k = Id.New(mut = false) 
    Lambda([k],
        args |> List.rev |> List.fold (fun c (a, _) -> Lambda([a], c)) (Var k).[[inner]]
    )
  
let objty, objArrTy =
    let t = typeof<System.Object>
    let arrt = typeof<System.Object []>
    Reflection.ReadTypeDefinition t,
    Reflection.ReadTypeDefinition arrt
  
[<Sealed>]
type PrintF() =
    inherit Macro()
    override this.TranslateCtor(c) =
        let rec getFunctionArgs f =
            match f with
            | FSharpFuncType(a, r) -> 
                a :: getFunctionArgs r
            | _ -> 
                []
        match c.Arguments with
        | [I.Value (Literal.String fs)] ->
            let ts = c.DefiningType.Generics.Head |> getFunctionArgs |> List.map (fun t -> t.SubstituteGenericsToSame(NonGenericType objty))
            createPrinter c.Compilation ts fs |> MacroOk
        | _ -> MacroError "printfMacro error"

[<JavaScript>]
type private EquatableEqualityComparer<'T when 'T :> System.IEquatable<'T>>() =
    inherit System.Collections.Generic.EqualityComparer<'T>()
    override this.Equals(x, y) = (x :> System.IEquatable<_>).Equals(y)
    override this.GetHashCode(x) = (box x).GetHashCode()

[<JavaScript>]
type private BaseEqualityComparer<'T>() =
    inherit System.Collections.Generic.EqualityComparer<'T>()
    override this.Equals(x, y) = obj.Equals(box x, box y)
    override this.GetHashCode(x) = (box x).GetHashCode()

let rec isImplementing (comp: M.ICompilation) typ intf =
    comp.GetClassInfo typ
    |> Option.map (fun cls ->
        cls.Implementations |> Seq.exists (fun (KeyValue ((i, _), _)) -> i = intf)
        || cls.BaseClass |> Option.exists (fun b -> isImplementing comp b intf |> Option.exists id) 
    )

[<Sealed>]
type EqualityComparer() =
    inherit Macro()

    static let ieqTy =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.IEquatable`1"
        } 

    static member GetDefault(comp: M.ICompilation, t: Type) =
        if t.IsParameter then MacroNeedsResolvedTypeArg t else
        match t with
        | ConcreteType ct ->
            match isImplementing comp ct.Entity ieqTy with
            | Some isEquatable ->
                let td : TypeDefinitionInfo =
                    { Assembly = "WebSharper.Main"
                      FullName =
                        if isEquatable then
                            "WebSharper.MacroModule+EquatableEqualityComparer`1"
                        else
                            "WebSharper.MacroModule+BaseEqualityComparer`1" }
                Ctor(
                    {Entity = Hashed td; Generics = [t]},
                    Hashed<ConstructorInfo> { CtorParameters = [] },
                    [])
                |> MacroOk
            | _ -> MacroError ""
        | _ -> MacroError "Type form not recognized"

    override this.TranslateCall(c) =
        match c.Method.Entity.Value.MethodName with
        | "get_Default" -> EqualityComparer.GetDefault(c.Compilation, c.DefiningType.Generics.[0])
        | _ -> MacroError "Not implemented"

[<JavaScript>]
type private ComparableComparer<'T when 'T :> System.IComparable<'T>>() =
    inherit System.Collections.Generic.Comparer<'T>()
    override this.Compare(x, y) = (x :> System.IComparable<'T>).CompareTo(y)

[<JavaScript>]
type private BaseComparer<'T when 'T : comparison>() =
    inherit System.Collections.Generic.Comparer<'T>()
    override this.Compare(x, y) = compare x y

[<Sealed>]
type Comparer() =
    inherit Macro()

    static let icmpTy =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.IComparable`1"
        } 

    static member GetDefault(comp: M.ICompilation, t: Type) =
        if t.IsParameter then MacroNeedsResolvedTypeArg t else
        match t with
        | ConcreteType ct ->
            match isImplementing comp ct.Entity icmpTy with
            | Some isEquatable ->
                let td : TypeDefinitionInfo =
                    { Assembly = "WebSharper.Main"
                      FullName =
                        if isEquatable then
                            "WebSharper.MacroModule+ComparableComparer`1"
                        else
                            "WebSharper.MacroModule+BaseComparer`1" }
                Ctor(
                    {Entity = Hashed td; Generics = [t]},
                    Hashed<ConstructorInfo> { CtorParameters = [] },
                    [])
                |> MacroOk
            | _ -> MacroError ""
        | _ -> MacroError "Type form not recognized"

    override this.TranslateCall(c) =
        match c.Method.Entity.Value.MethodName with
        | "get_Default" -> Comparer.GetDefault(c.Compilation, c.DefiningType.Generics.[0])
        | _ -> MacroError "Not implemented"

/// Returns 0 for number types, undefined for others.
/// TODO: this is wrong for non-number value types!
/// TODO: also always returns undefined when called generically.
[<Sealed>]
type DefaultOf() =
    inherit Macro()

    override __.TranslateCall(c) =
        let t = c.Method.Generics.[0]
        if t.IsParameter then MacroNeedsResolvedTypeArg t else
        match t with
        | ConcreteType td when
            (td.Entity.Value.Assembly.StartsWith "mscorlib" &&
                match td.Entity.Value.FullName with
                | "System.SByte"
                | "System.Byte"
                | "System.Int16"
                | "System.UInt16"
                | "System.Int32"
                | "System.UInt32"
                | "System.Int64"
                | "System.UInt64"
                | "System.Decimal"
                | "System.Single"
                | "System.Double"
                | "System.DateTime"
                | "System.TimeSpan" -> true
                | _ -> false)
            -> MacroOk (Value (Int 0))
        | ConcreteType td -> 
            match c.Compilation.GetCustomTypeInfo td.Entity with
            | M.StructInfo ->
                let cdef = Hashed { CtorParameters = [] }
                MacroOk (Ctor(td, cdef, []))
            | _ ->
                MacroOk (Value (Null))
        | _ ->
            MacroOk (Value (Null))

[<Sealed>]
type DefaultToUndefined() =
    inherit Macro()

    static let tr =
        { new Transformer() with
            override this.TransformCall(thisObj, typ, meth, args) =
                if Option.isNone thisObj && IsDefaultValue typ.Entity meth.Entity && List.isEmpty args then
                    Undefined
                else
                    base.TransformCall(thisObj, typ, meth, args)  
        }.TransformExpression

    override __.TranslateCall(c) =
        MacroOk <| tr c.Arguments.[0]

[<Sealed>]
type TypeTest() =
    inherit Macro()

    override __.TranslateCall(c) =
        TypeCheck(c.Arguments.Head, c.Method.Generics.Head) |> MacroOk

[<Sealed>]
type InlineJS() =
    inherit Macro()

    override __.TranslateCall(c) =
        match c.Arguments.Head with
        | I.Value (String inl) ->
            let args =
                match c.Arguments with
                | [_] -> [] 
                | [_; I.NewArray args] -> args
                | _ -> failwith "InlineJS error: arguments cannot be passed as an array"
            c.Compilation.ParseJSInline(inl, args) |> MacroOk
        | _ -> failwith "InlineJS error: first argument must be a constant string"

let stringTy, lengthMeth, padLeft, padRight =
    let t = typeof<System.String>
    Reflection.ReadTypeDefinition t,
    Reflection.ReadMethod (t.GetMethod "get_Length"),
    Reflection.ReadMethod (t.GetMethod("PadLeft", [|typeof<int>|])),
    Reflection.ReadMethod (t.GetMethod("PadRight", [|typeof<int>|]))

[<Sealed>]
type StringFormat() =
    inherit Macro()

    let regExp = Regex("(?:(.*?){(0|[1-9]\d*)(?:,(-?[1-9]\d*|0))?(?::(.*?))?})|(.+)$", RegexOptions.Singleline)

    let safeToString expr =
        Conditional(
            expr ^== (Value Literal.Null),
            cString "",
            cCallG ["String"] [expr]
        )

    override __.TranslateCall(c) =
        match c.DefiningType.Entity.Value.FullName, c.Method.Entity.Value.MethodName with
        | "System.String", "Format" ->
            match c.Arguments with
            | (Value (String format)) :: args when args.Length < 4 ->
                let args =
                    match c.Method.Entity.Value.Parameters with
                    | [_; x] ->
                        try 
                            if x.TypeDefinition = objty then
                                NewArray [args.[0]]
                            else
                                args.[0]
                        with _ ->
                            // Array type has no typedef
                            args.[0]
                    | [_; x1; x2] -> NewArray [args.[0]; args.[1]]
                    | [_; x1; x2; x3] -> NewArray [args.[0]; args.[1]; args.[2]]
                    | _ -> failwith "Wrong number of arguments for String.Format"

                let warning = ref None
                let argsId = Id.New(mut = false)
               
                let parts =
                    regExp.Matches(format)
                    |> Seq.cast<Match>
                    |> Array.ofSeq

                let body =
                    if Array.isEmpty parts then cString "" else
                    parts
                    |> Seq.map (fun m ->
                        if m.Groups.[5].Value <> "" then
                            Value (Literal.String m.Groups.[5].Value)
                        else
                            let prefix = m.Groups.[1].Value
                            let prefix s = Value (Literal.String prefix) ^+ s
                            let idx = int m.Groups.[2].Value

                            let r =
                                Application(Global ["WebSharper"; "Arrays"; "get"], [Var argsId; cInt idx], Pure, Some 2)
                                |> safeToString

                            let spec = m.Groups.[4].Value
                            if spec <> "" then
                                warning := Some (sprintf "String format specifiers are not supported: %s" spec)

                            if m.Groups.[3].Value <> "" then
                                let w1 = int m.Groups.[3].Value
                                let w2 = abs w1

                                let expr =
                                    Conditional(
                                        cInt w2 ^> Call (None, NonGeneric stringTy, NonGeneric lengthMeth, [r]),
                                        Conditional(
                                            cInt w1 ^> cInt 0,
                                            Call (None, NonGeneric stringTy, NonGeneric padLeft, [r; cInt w2]),
                                            Call (None, NonGeneric stringTy, NonGeneric padRight, [r; cInt w2])
                                        ),
                                        r
                                    )
                                prefix expr
                            else prefix r
                    )
                    |> Seq.reduce (^+)

                let result = Let(argsId, args, body)

                let warningRes = !warning |> Option.map (fun w -> MacroWarning(w, MacroOk result))

                defaultArg warningRes (MacroOk result)
            
            | _ -> MacroFallback
        | _ -> MacroError "proxy is for System.String.Format"
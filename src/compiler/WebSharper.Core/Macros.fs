// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

open System.Collections.Generic
open System.Text.RegularExpressions

open WebSharper
open WebSharper.Core
open WebSharper.Core.AST

module M = WebSharper.Core.Metadata
module I = IgnoreSourcePos

let scalarTypes =
    integralTypes
    + Set [
        "System.Double"
        "System.Single"
        "System.String" 
        "System.TimeSpan"
        "System.DateTime"
    ]

let comparableTypes =
    scalarTypes
    + Set [
        "System.Char"
    ]

let isIn (s: string Set) (t: Type) = 
    match t with
    | ConcreteType t ->
        s.Contains t.Entity.Value.FullName
    | _ ->
        false

let traitCallOp (c: MacroCall) (args: Expression list) =
    let unres =
        c.Method.Generics |> List.tryPick (fun t -> if t.IsParameter then Some (MacroNeedsResolvedTypeArg t) else None)
    match unres with
    | Some u -> u 
    | _ ->
        let tc types pars ret =
            TraitCall(
                None,
                types, 
                NonGeneric (
                    Method {
                        MethodName = c.Method.Entity.Value.MethodName
                        Parameters = pars
                        ReturnType = ret 
                        Generics = 0
                    }
                ),
                args
            )
            |> MacroOk
        match c.Method.Generics with
        | [ t ] ->
            let pars =
                match c.Method.Entity.Value.MethodName with
                | "op_LeftShift" 
                | "op_RightShift" -> [ t; NonGenericType Definitions.Int ]
                | _ -> List.replicate args.Length t
            tc [ t ] pars t
        | [ t; u ] -> // for **
            tc [ t ] [ t; u ] t
        | [t; u; v] ->
            tc [ t; u ] [ t; u ] v
        | _ ->
            failwith "F# Operator value expecting 1 or 3 type arguments"
    
let utilsModule =
    TypeDefinition {
        FullName = "WebSharper.Utils"
        Assembly = "WebSharper.StdLib"
    }
let utils (comp: M.ICompilation) f args = 
    let m = comp.GetClassInfo(utilsModule).Value.Methods.Keys |> Seq.find (fun m -> m.Value.MethodName = f)
    Call(None, NonGeneric utilsModule, NonGeneric m, args)

let translateOperation (c: MacroCall) (t: Type) args leftNble rightNble op =
    match args with
    | [x; y] ->
        let a, b, lambda =
            if leftNble || rightNble then
                let a = Id.New "a"
                let b = Id.New "b"
                Var a, Var b, fun res -> CurriedLambda([a; b], None, res)
            else
                x, y, id
        let resm =
            if (op = BinaryOperator.``<<`` || op = BinaryOperator.``>>``) && isIn bigIntegralTypes t then
                Binary(a, op, Appl(Global ["BigInt"], [b], Pure, Some 1)) |> MacroOk  
            elif op = BinaryOperator.``/`` then
                if isIn smallIntegralTypes t
                then (a ^/ b) ^>> !~(Int 0) |> MacroOk
                elif isIn scalarTypes t
                then a ^/ b |> MacroOk
                else traitCallOp c [a; b]
            else
                if isIn scalarTypes t then
                    Binary(a, op, b) |> MacroOk
                else traitCallOp c [a; b]
        match leftNble, rightNble, resm with
        | true , false, MacroOk res -> utils c.Compilation "nullableOpL" [ x; y; lambda res ] |> MacroOk
        | false, true , MacroOk res -> utils c.Compilation "nullableOpR" [ x; y; lambda res ] |> MacroOk
        | true , true , MacroOk res -> utils c.Compilation "nullableOp"  [ x; y; lambda res ] |> MacroOk
        | _    , _    , res         -> res
    | _ -> MacroError "Arith macro: expecting 2 args"

[<Sealed>]
type Arith() =
    inherit Macro()
    override this.TranslateCall(c) =
        let opName = c.Method.Entity.Value.MethodName
        match c.Arguments with
        | [ a ] ->
            let op =
                match opName with
                | UnaryOpName op -> op
                | n -> failwithf "Arith macro: unrecognized operator %s" n
            match c.Method.Generics with
            | t :: _ ->
                if isIn scalarTypes t then
                    if op = UnaryOperator.``+`` then
                        a |> MacroOk
                    else
                        Unary(op, a) |> MacroOk 
                else traitCallOp c c.Arguments
            | _ -> MacroError "Arith macro: expecting a type parameter"
        | [ a; b ] ->
            let leftNble = opName.StartsWith "op_Qmark"
            let rightNble = opName.EndsWith "Qmark"
            let simpleOpName = if leftNble || rightNble then opName.Replace("Qmark", "") else opName
            let op =
                match simpleOpName with
                | BinaryOpName op -> op
                | "op_Plus" -> BinaryOperator.``+``
                | "op_Minus" -> BinaryOperator.``-``
                | "op_Divide" -> BinaryOperator.``/``
                | "op_Percent" -> BinaryOperator.``%``
                | n -> failwithf "Arith macro: unrecognized operator %s" n
            match c.Method.Generics with
            | t1 :: t2 :: _ ->
                if t1 = t2 then
                    translateOperation c t1 c.Arguments leftNble rightNble op
                else
                    traitCallOp c c.Arguments
            | t :: _ ->
                let isEnum() = 
                    match t with
                    | ConcreteType { Entity = td; Generics = [] } ->
                        match c.Compilation.GetCustomTypeInfo td with
                        | M.EnumInfo _ -> true
                        | _ -> false
                    | _ -> false
                if (op = BinaryOperator.``<<`` || op = BinaryOperator.``>>``) && isIn bigIntegralTypes t then
                    Binary(a, op, Appl(Global ["BigInt"], [b], Pure, Some 1)) |> MacroOk  
                // enums have underlying types supporting bitwise operations
                elif isEnum() || isIn scalarTypes t then
                    Binary(a, op, b) |> MacroOk
                else
                    traitCallOp c c.Arguments
            | _ -> MacroError "Arith macro: expecting a type parameters"
        | _ -> MacroError "Arith macro: expecting one or two arguments"

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

let opUncheckedTy, equalsMeth, compareMeth, hashMeth =
    match <@ Unchecked.equals 1 1 @> with
    | FSharp.Quotations.Patterns.Call (_, mi, _) ->
        let cmi = mi.DeclaringType.GetMethod("Compare")
        let hmi = mi.DeclaringType.GetMethod("Hash")
        Reflection.ReadTypeDefinition mi.DeclaringType,
        Reflection.ReadMethod mi,
        Reflection.ReadMethod cmi,
        Reflection.ReadMethod hmi
    | _ -> failwith "Expecting a Call pattern"

let UncheckedEquals x y =
    Call (None, NonGeneric opUncheckedTy, NonGeneric equalsMeth, [x; y]) 

let makeComparison cmp t x y =
    let eq x y = Call (None, NonGeneric opUncheckedTy, Generic equalsMeth [ t ], [x; y]) 
    let c b i = Binary (Call(None, NonGeneric opUncheckedTy, Generic compareMeth [ t ], [x; y]), b, Value(Int i))
    match cmp with
    | Comparison.``<``  -> c BinaryOperator.``===`` -1
    | Comparison.``<=`` -> c BinaryOperator.``<=`` 0
    | Comparison.``>``  -> c BinaryOperator.``===`` 1
    | Comparison.``>=`` -> c BinaryOperator.``>=`` 0
    | Comparison.``=``  -> eq x y
    | _                 -> Unary (UnaryOperator.``!``, eq x y)

let cInt i = Value (Int i)
let cString s = Value (Literal.String s)

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

let defMethods =
    function
    | Comparison.``<`` -> "op_LessThan"
    | Comparison.``>`` -> "op_GreaterThan"
    | Comparison.``<=`` -> "op_LessThanOrEqual"
    | Comparison.``>=`` -> "op_GreaterThanOrEqual"
    | Comparison.``=`` -> "op_Equality"
    | Comparison.``<>`` -> "op_Inequality"
    | _ -> failwith "Unexpected comparison operator"

let additionalMethods =
    function
    | Comparison.``<`` -> "op_Less"
    | Comparison.``>`` -> "op_Greater"
    | Comparison.``<=`` -> "op_LessEquals"
    | Comparison.``>=`` -> "op_GreaterEquals"
    | Comparison.``=`` -> "op_Equals"
    | Comparison.``<>`` -> "op_LessGreater"
    | _ -> failwith "Unexpected comparison operator"

let tryFindMethodFromComparison (cI: Metadata.IClassInfo option) (t: Type) (cmp: Comparison) =
    let methodInfoFromStr str =
        let mi =
            {
                MethodName = str
                Parameters = [t;t]
                ReturnType =
                    {
                        Generics = []
                        Entity = AST.Definitions.Bool 
                    } |> Type.ConcreteType
                Generics = 0
            } : MethodInfo
        Hashed mi
    match cI with
    | Some cI ->
        let defMethod = defMethods cmp |> methodInfoFromStr
        match cI.Methods.TryGetValue(defMethod) with
        | true, mem -> Some defMethod
        | false, _ ->
            let additionalMethod = additionalMethods cmp |> methodInfoFromStr
            match cI.Methods.TryGetValue(additionalMethod) with
            | true, mem -> Some additionalMethod
            | false, _ -> None
    | _ -> None

let translateComparison (c: M.ICompilation) (t: Type) args leftNble rightNble cmp =
    let classInfo =
        match t with
        | Type.ConcreteType tdef ->
            c.GetClassInfo t.TypeDefinition
        | _ -> None
    let compiledMember = tryFindMethodFromComparison classInfo t cmp
    match args with
    | [x; y] ->
        match compiledMember with
        | None -> 
            let a, b, lambda =
                if leftNble || rightNble then
                    let a = Id.New "a"
                    let b = Id.New "b"
                    Var a, Var b, fun res -> CurriedLambda([a; b], None, res)
                else
                    x, y, id
            let comp x y =
                Binary (x, toBinaryOperator cmp, y)
            let cti = 
                match t with
                | Type.ConcreteType ct -> c.GetCustomTypeInfo ct.Entity
                | _ -> M.NotCustomType
            let t =
                match cti with
                | M.EnumInfo u -> NonGenericType u            
                | _ -> t
            let res =
                if isIn comparableTypes t then
                    comp a b
                else
                    // optimization for checking against argumentless union cases 
                    let tryGetSingletonUnionCaseTag (x: Expression) =
                        match x with
                        | I.NewUnionCase(_, case, []) ->
                            match cti with
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
                    | _ -> makeComparison cmp t a b
            match leftNble, rightNble with
            | false, false -> res
            | true , false -> utils c "nullableCmpL" [ x; y; lambda res ]
            | false, true  -> utils c "nullableCmpR" [ x; y; lambda res ]
            | true , true  -> 
                match cmp with
                | Comparison.``<=`` 
                | Comparison.``>=`` 
                | Comparison.``=`` 
                    -> utils c "nullableCmpE" [ x; y; lambda res ]
                | _ -> utils c "nullableCmp"  [ x; y; lambda res ] 
            |> MacroOk
        | Some method ->
            Call(None, t.TypeDefinition |> NonGeneric, method |> NonGeneric, [x; y]) |> MacroOk
    | _ ->
        MacroError "comparisonMacro error"

let translateCompareTo (c: M.ICompilation) t l r =
    let x = Id.New "x"
    let y = Id.New "y"
    match translateComparison c t [Var x; Var y] false false Comparison.``<`` with
    | MacroOk lt ->
        match translateComparison c t [Var x; Var y] false false Comparison.``=`` with
        | MacroOk eq ->
            Let(x, l, Let(y, r, Conditional(lt, cInt -1, Conditional(eq, cInt 0, cInt 1))))
            |> MacroOk
        | r -> r
    | r -> r 

[<Sealed>]
type Comp() =
    inherit Macro()
    override this.TranslateCall(c) =
        let opName = c.Method.Entity.Value.MethodName
        let leftNble = opName.StartsWith "op_Qmark"
        let rightNble = opName.EndsWith "Qmark"
        let simpleOpName = if leftNble || rightNble then opName.Replace("Qmark", "") else opName
        let cmp =
            match simpleOpName with
            | BinaryOpName op -> toComparison op
            | "op_Equals" -> Comparison.``=``
            | "op_LessGreater" -> Comparison.``<>``
            | "op_Greater" -> Comparison.``>``
            | "op_Less" -> Comparison.``<``
            | "op_GreaterEquals" -> Comparison.``>=``
            | "op_LessEquals" -> Comparison.``<=``
            | n -> failwithf "unrecognized operator for for Comp macro: %s" n
        match c.Method.Generics with
        | t :: _ ->
            translateComparison c.Compilation t c.Arguments leftNble rightNble cmp
        | _ ->
            MacroError "comparisonMacro error"

let formatExceptionTy, formatExceptionCtor =
    match <@ new System.FormatException("") @> with
    | FSharp.Quotations.Patterns.NewObject (ci, _) ->
        Reflection.ReadTypeDefinition ci.DeclaringType,
        Reflection.ReadConstructor ci
    | _ -> failwith "Expected constructor call"

let parseInt x =
    Appl(Global ["parseInt"], [x], Pure, Some 1)
let toNumber x =
    Appl(Global ["Number"], [x], Pure, Some 1)

[<Sealed>]
type NumericMacro() =
    inherit Macro()

    let exprParse parsed tru fls =
        let id = Id.New(mut = false)
        Let (id, parsed,
            Conditional(Appl(Global ["isNaN"], [Var id], Pure, Some 1),
                tru id,
                fls id
            )
        )
    override this.TranslateCall(c) =
        let name = c.DefiningType.Entity.Value.FullName

        let ex =
            Ctor(
                NonGeneric formatExceptionTy,
                formatExceptionCtor,
                [Value (String "Input string was not in a correct format.")]
            )

        let isNble t =
            match t with
            | VoidType -> true
            | ConcreteType { Entity = td } when td.Value.FullName = "System.Nullable`1" -> true
            | _ -> false

        let mname = c.Method.Entity.Value.MethodName

        let checkCorrectType f =
            let isParamTypesOk =
                if mname.EndsWith "Shift" then
                    match c.Method.Entity.Value.Parameters with
                    | [ ConcreteType { Entity = td1 }; ConcreteType { Entity = td2 } ] ->
                        td1 = c.DefiningType.Entity && td2 = Definitions.Int32
                    | _ -> false
                else
                    c.Method.Entity.Value.Parameters 
                    |> List.forall (
                        function
                        | ConcreteType { Entity = td } when td = c.DefiningType.Entity -> true
                        | _ -> false
                    )
            if isParamTypesOk then f() else MacroError "numericMacro error"
        
        match mname with
        | BinaryOpName op when isOperation op ->
            checkCorrectType <| fun () ->
                let leftNble, rightNble =
                    match c.Method.Generics with
                    | [lt; rt] -> isNble lt, isNble rt
                    | _ -> false, false
                translateOperation c (ConcreteType c.DefiningType) c.Arguments leftNble rightNble op
        | BinaryOpName op when isComparison op ->
            checkCorrectType <| fun () ->
                let leftNble, rightNble =
                    match c.Method.Generics with
                    | [lt; rt] -> isNble lt, isNble rt
                    | _ -> false, false
                let cmp = toComparison op
                translateComparison c.Compilation (ConcreteType c.DefiningType) c.Arguments leftNble rightNble cmp
        | UnaryOpName op ->
            checkCorrectType <| fun () ->
                match c.Arguments with
                | [x] -> 
                    if op = UnaryOperator.``+`` then
                        x |> MacroOk
                    else
                        Unary (op, x) |> MacroOk
                | _ -> MacroError "numericMacro error"
        | "op_Increment" ->
            checkCorrectType <| fun () ->
                match c.Arguments with
                | [x] ->
                    MacroOk (Binary(x, BinaryOperator.``+``, Value (Int 1)))
                | _ -> MacroError "numericMacro error"
        | "op_Decrement" ->
            checkCorrectType <| fun () ->
                match c.Arguments with
                | [x] ->
                    MacroOk (Binary(x, BinaryOperator.``-``, Value (Int 1)))
                | _ -> MacroError "numericMacro error"
        | "ToString" ->
            match c.This with
            | Some self ->
                // TODO refactor to separate method
                if c.DefiningType.Entity.Value.AssemblyQualifiedName = "System.Char, netstandard" then
                    self
                else 
                    Appl(Global ["String"], [self], Pure, Some 1)
                |> MacroOk 
            | _ -> MacroError "numericMacro error"
        | "Parse" ->
            match c.Arguments with
            | [x] ->
                if name = "System.Single" || name = "System.Double" then
                    exprParse
                    <| toNumber x
                    <| fun _ -> ex
                    <| fun id -> Var id
                    |> MacroOk
                else MacroFallback
            | _ -> MacroError "numericMacro error"
        | "TryParse" ->
            match c.Arguments with
            | [x; y] ->
                if name = "System.Single" || name = "System.Double" then
                    exprParse
                    <| toNumber x
                    <| fun _ -> Value (Bool false)
                    <| fun id ->
                        Expression.Sequential [
                            SetRef y (Var id)
                            Value (Bool true)
                        ]
                    |> MacroOk
                else MacroFallback
            | _ -> MacroError "numericMacro error"
        | "Equals" ->
            translateComparison c.Compilation (ConcreteType c.DefiningType) (c.This.Value :: c.Arguments) false false Comparison.``=``
        | "CompareTo" ->
            translateCompareTo c.Compilation (ConcreteType c.DefiningType) c.This.Value c.Arguments.Head
        | "get_Zero" ->
            Value (Int 0) |> MacroOk
        | "get_One" ->
            Value (Int 1) |> MacroOk
        | _ -> MacroFallback

    override this.TranslateCtor(c) =
        match c.Arguments with
        | [] -> MacroOk (Value (Int 0))
        | _ -> MacroError "numericMacro error: contructor with arguments"

let decimalDivide =
    Method {
        MethodName = "Divide"
        Parameters = [ NonGenericType Definitions.Decimal; NonGenericType Definitions.Decimal ]
        ReturnType = NonGenericType Definitions.Decimal
        Generics = 0
    } |> NonGeneric

let decimalIntCtor =
    Constructor {
        CtorParameters = [ NonGenericType Definitions.Int ]
    }

[<Sealed>]
type DivideByIntMacro() =
    inherit Macro()

    override this.TranslateCall(c) =
        match c.Arguments with
        | [a; b] ->
            match c.Method.Generics with
            | [t] ->
                match t with
                | ConcreteType d ->
                    match d.Entity.Value.FullName with
                    | "System.Double"
                    | "System.Single" -> a ^/ b |> MacroOk
                    | "System.Decimal" ->
                        Call(None, NonGeneric Definitions.Decimal, decimalDivide, [
                            a
                            Ctor(NonGeneric Definitions.Decimal, decimalIntCtor, [ b ])
                        ])
                        |> MacroOk
                    | _ -> MacroFallback
                | t when t.IsParameter ->
                    MacroNeedsResolvedTypeArg t
                | _ -> MacroFallback
            | _ ->
                MacroError "divideByIntMacro error"
        | _ ->
            MacroError "divideByIntMacro error"

[<Sealed>]
type SumOrAverageMacro() =
    inherit Macro()

    override this.TranslateCall(c) =
        let mname = c.Method.Entity.Value.MethodName
        let asGeneric() =
            let genm = Method { c.Method.Entity.Value with MethodName = mname + "Generic" }
            let typ = TypeDefinition { c.DefiningType.Entity.Value with FullName = c.DefiningType.Entity.Value.FullName.Replace("Array", "Seq") }
            Call(None, NonGeneric typ, Generic genm c.Method.Generics, c.Arguments) |> MacroOk
        match mname with
        | "Sum"
        | "SumBy" ->
            match c.Method.Generics with
            | [ t ]
            | [ _; t ] ->
                match t with
                | ConcreteType d ->
                    // for these types it's ok to use plain JS +
                    match d.Entity.Value.FullName with
                    | "System.SByte"
                    | "System.Byte"
                    | "System.Int16"
                    | "System.UInt16"
                    | "System.Int32"
                    | "System.UInt32"
                    | "System.Int64"
                    | "System.UInt64"
                    | "System.Single"
                    | "System.Double"
                    | "System.Char" -> MacroFallback
                    | _ -> asGeneric() 
                | t when t.IsParameter ->
                    MacroNeedsResolvedTypeArg t
                | _ -> asGeneric() 
            | _ ->
                MacroError "sumOrAverageMacro error"
        | "Average"
        | "AverageBy" ->
            match c.Method.Generics with
            | [ t ]
            | [ _; t ] ->
                match t with
                | ConcreteType d ->
                    // for these types it's ok to use plain JS + and /
                    match d.Entity.Value.FullName with
                    | "System.Single"
                    | "System.Double" -> MacroFallback
                    | _ -> asGeneric() 
                | t when t.IsParameter ->
                    MacroNeedsResolvedTypeArg t
                | _ -> asGeneric() 
            | _ ->
                MacroError "sumOrAverageMacro error"
        | _ ->
            MacroError "sumOrAverageMacro error"

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
                let fromNum() = 
                    Appl(Global ["String"; "fromCharCode"], [x], Pure, Some 1)
                    |> MacroOk
                if isIn integralTypes t then fromNum() else
                    match t with
                    | ConcreteType d ->
                        match d.Entity.Value.FullName with
                        | "System.String" ->
                            Call (None, NonGeneric charTy, NonGeneric charParse, [x])
                            |> MacroOk
                        | "System.Char" -> MacroOk x
                        | "System.Double"
                        | "System.Single" -> fromNum()
                        | _ -> MacroError "charMacro error"
                    | _ ->
                        MacroError "charMacro error"
            | _ ->
                MacroError "charMacro error"
        | _ ->
            MacroError "charMacro error"

[<Sealed>]
type Range() =
    inherit Macro()
    override this.TranslateCall(c) =
        match c.Method.Generics with
        | t :: _ ->
            match t with
            | ConcreteType d ->
                match d.Entity.Value.FullName with
                | "System.Char" -> 
                    utils c.Compilation "charRange" c.Arguments |> MacroOk   
                | _ -> MacroFallback
            | _ -> MacroFallback
        | _ -> MacroError "Range macro error"

[<Sealed>]
type Conversion() =
    inherit Macro()
    override this.TranslateCall(c) =
        let isNble =
            c.DefiningType.Entity.Value.FullName = "Microsoft.FSharp.Linq.NullableModule"
        let m = c.Method
        let x = c.Arguments.Head
        let a, withNbleSupport = 
            if isNble then
                let a = Id.New "a"
                Var a, fun res -> utils c.Compilation "nullableConv" [ x; Lambda([a], None, res) ] 
            else 
                x, id
        let (|OptNbleTypeDef|_|) t =
            match t with
            | ConcreteType { Entity = tt; Generics = g } ->
                if tt.Value.FullName = "System.Nullable`1" then
                    match g.Head with
                    | ConcreteType { Entity = tt } -> Some tt
                    | _ -> None
                else Some tt
            | _ -> None
            
        match m.Generics.Head, m.Entity.Value.ReturnType with
        | OptNbleTypeDef ft, OptNbleTypeDef tt ->
            NumericConversion ft tt a |> withNbleSupport |> MacroOk
        | TypeParameter _, OptNbleTypeDef tt ->
            let tn = tt.Value.FullName
            let warnAboutChar res =
                MacroWarning ("Unsafe generic conversion for client-side, make sure input cannot be a char", MacroOk res)
            if integralTypes.Contains tn then
                parseInt a |> withNbleSupport |> warnAboutChar
            elif scalarTypes.Contains tn then
                toNumber a |> withNbleSupport |> warnAboutChar
            elif tn = "System.Char" then
                Appl(Global ["String"; "fromCharCode"], [a], Pure, Some 1) |> warnAboutChar
            else
                MacroError ("Conversion macro error: generic to " + tn)
        | f, t -> MacroError (sprintf "Conversion macro error: %O to %O" f t)

[<Sealed>]
type Op() =
    inherit Macro()
    override this.TranslateCall(c) =
        let m = c.Method
        let me = m.Entity.Value
        let t = m.Generics.Head
        if t.IsParameter then
            MacroNeedsResolvedTypeArg t
        else
            match t with
            | ConcreteType ct ->
                if scalarTypes.Contains ct.Entity.Value.FullName then
                    MacroFallback
                else
                    let meth =
                        Method {
                            MethodName = me.MethodName
                            Parameters = me.Parameters |> List.map (fun p -> p.SubstituteGenerics(Array.ofList m.Generics))
                            ReturnType = me.ReturnType.SubstituteGenerics(Array.ofList m.Generics)
                            Generics = 0      
                        }
                    TraitCall(None, [t], NonGeneric meth, c.Arguments) |> MacroOk
            | _ ->
                MacroError (sprintf "Op macro error on method %s, type not supported: %O" me.MethodName t)

module Definitions =
    let opModule =
        TypeDefinition {
            Assembly = "FSharp.Core"
            FullName = "Microsoft.FSharp.Core.Operators"
        }

    let bigIntAbsMeth =
        Method {
            MethodName = "BigIntAbs"
            Parameters = [ NonGenericType Definitions.Int64 ]
            ReturnType = NonGenericType Definitions.Int64
            Generics = 0      
        }    

    let bigIntSignMeth =
        Method {
            MethodName = "BigIntSign"
            Parameters = [ NonGenericType Definitions.Int64 ]
            ReturnType = NonGenericType Definitions.Int
            Generics = 0      
        }

[<Sealed>]
type Abs() =
    inherit Macro()
    override this.TranslateCall(c) =
        let m = c.Method
        let me = m.Entity.Value
        let t = m.Generics.Head
        if t.IsParameter then
            MacroNeedsResolvedTypeArg t
        else
            match t with
            | ConcreteType ct ->
                if bigIntegralTypes.Contains ct.Entity.Value.FullName then

                    Call(None, NonGeneric Definitions.opModule, NonGeneric Definitions.bigIntAbsMeth, c.Arguments) |> MacroOk
                elif scalarTypes.Contains ct.Entity.Value.FullName then
                    MacroFallback
                else
                    let meth =
                        Method {
                            MethodName = me.MethodName
                            Parameters = me.Parameters |> List.map (fun p -> p.SubstituteGenerics(Array.ofList m.Generics))
                            ReturnType = me.ReturnType.SubstituteGenerics(Array.ofList m.Generics)
                            Generics = 0      
                        }
                    TraitCall(None, [t], NonGeneric meth, c.Arguments) |> MacroOk
            | _ ->
                MacroError (sprintf "Op macro error on method %s, type not supported: %O" me.MethodName t)


[<Sealed>]
type Sign() =
    inherit Macro()
    override this.TranslateCall(c) =
        let m = c.Method
        let x = c.Arguments.Head
        let t = m.Generics.Head
        if t.IsParameter then
            MacroNeedsResolvedTypeArg t
        else
            match t with
            | ConcreteType ct ->
                if bigIntegralTypes.Contains ct.Entity.Value.FullName then
                    let opModule =
                        TypeDefinition {
                            Assembly = "FSharp.Core"
                            FullName = "Microsoft.FSharp.Core.Operators"
                        }
                    Call(None, NonGeneric opModule, NonGeneric Definitions.bigIntSignMeth, c.Arguments) |> MacroOk
                elif scalarTypes.Contains ct.Entity.Value.FullName then
                    MacroFallback
                else
                    let signMeth =
                        Method {
                            MethodName = "get_Sign"
                            Parameters = []
                            ReturnType = NonGenericType Definitions.Int
                            Generics = 0      
                        }
                    Call(Some x, ct, NonGeneric signMeth, []) |> MacroOk
            | _ ->
                MacroError (sprintf "Sign macro error, type not supported: %O" t)

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
                        x
                    | "System.DateTime" ->
                        Appl(ItemGet(New(Global [ "Date" ], [], [x]), Value (Literal.String "toLocaleString"), Pure), [], Pure, None)
                    | _ ->
                        Appl(Global ["String"], [x], Pure, Some 1)   
                | _ -> 
                    Appl(Global ["String"], [x], Pure, Some 1)   
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
            | NewArray [I.Value (String n); v] -> Some (n, v)
            | Call (_, td, m, [I.Value (String n); v])
                when ``is (=>)`` td.Entity m.Entity -> Some(n, v)
            | _ -> None
        let tryMap f l = 
            List.foldBack (fun i s ->
                match s with
                | None -> None
                | Some sv ->
                    match f i with
                    | Some fi -> Some (fi :: sv)
                    | None -> None
            ) l (Some [])
        match IgnoreExprSourcePos q with
        | NewUnionCase (_, _, [I.NewArray [I.Value (String n); v]; t]) ->
            getFieldsListTC ((n, v) :: l) t         
        | NewUnionCase (_, _, [I.Call (_, td, m, [I.Value (String n); v]); t])
            when ``is (=>)`` td.Entity m.Entity ->
            getFieldsListTC ((n, v) :: l) t         
        | NewUnionCase (_, _, []) -> Some (l |> List.rev) 
        | Call(None, td, m, [ I.NewArray items ]) when td.Entity = listModuleDef && m.Entity = listOfArrayDef ->
            items |> tryMap trItem
        | NewArray (items) ->
            items |> tryMap trItem
        | _ -> None
    getFieldsListTC [] q

[<Sealed>]
type New() =
    inherit Macro()

    let translate = function
        | [x] -> 
            match getFieldsList x with
            | Some xl ->
                MacroOk <| Object (xl |> List.map (fun (n, v) -> n, MemberKind.Simple, v))
            | _ -> MacroFallback
        | _ -> MacroError "New macro Error"

    override this.TranslateCall(c) = translate c.Arguments
    override this.TranslateCtor(c) = translate c.Arguments

//type FST = Reflection.FSharpType

module JSRuntime =
    let private runtimeFunc f p args = 
        Appl(GlobalAccess (Address.Runtime f), args, p, Some (List.length args))
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

let ApplItem(on, item, args) = Appl(ItemGet(on, Value (AST.String item), Pure), args, NonPure, None)

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

let stringModule = 
    TypeDefinition {
        FullName = "Microsoft.FSharp.Core.StringModule"
        Assembly = "FSharp.Core"
    }
let stringProxy (comp: M.ICompilation) f args =
    let m = comp.GetClassInfo(stringModule).Value.Methods.Keys |> Seq.find (fun m -> m.Value.MethodName = f)
    Call(None, NonGeneric stringModule, NonGeneric m, args)

let cCall e f args = Appl(ItemGet(e, !~ (Literal.String f), Pure), args, Pure, None)
let cCallG a args = Appl(Global a, args, Pure, None)

//type FST = Reflection.FSharpType

let (^+) (a: Expression) (b: Expression) =
    match a, b with
    | I.Value (String av), I.Value (String bv) -> Value (AST.String (av + bv))
    | I.Value av, I.Value bv -> Value (AST.String (av.Value.ToString() + bv.Value.ToString()))
    | _ -> a ^+ b

let createPrinter (comp: M.ICompilation) (ts: Type list) (intp: Expression list option) fs =
    let parts = FormatString.parseAll fs
    let args = ts |> List.map (fun t -> Id.New(mut = false), Some t)
    let rArgs = args |> List.map (fun (a, t) -> Var a, t) |> ref

        //match intp with
        //| None -> 
        //| Some intp -> 
            
        //    (intp, ts) ||> List.map2 (fun a t -> a, Some t) |> ref
    let nextHole() =
        match !rArgs with
        | (a, t) :: r ->
            rArgs := r
            a, t
        | _ -> failwithf "wrong number of Printer type arguments found: %d" (List.length ts)  
        
    let withPadding (f: FormatString.FormatSpecifier) t =
        if f.IsWidthSpecified then
            let width = if f.IsStarWidth then nextHole() |> fst else cInt f.Width
            let s = t (nextHole())
            if FormatString.isLeftJustify f.Flags then
                stringProxy comp "PadRight" [s; width]
            else
                if FormatString.isPadWithZeros f.Flags then
                    utils comp "padNumLeft" [s; width]
                else
                    stringProxy comp "PadLeft" [s; width]
        else t (nextHole())
        |> Some
        
    let numberToString (f: FormatString.FormatSpecifier) t =
        withPadding f (fun (n, _) ->
            if FormatString.isPlusForPositives f.Flags then utils comp "plusForPos" [n; t n]
            elif FormatString.isSpaceForPositives f.Flags then utils comp "spaceForPos" [n; t n]
            else t n
        )

    let numberToStringForIntFormmating (f: FormatString.FormatSpecifier) t =
        withPadding f (fun (n, typ) ->
            let length =
                match typ with
                | Some (ConcreteType t) when t.Entity = Definitions.SByte -> 8 
                | Some (ConcreteType t) when t.Entity = Definitions.Int16 -> 16 
                | Some (ConcreteType t) when t.Entity = Definitions.Int32 -> 32
                | _ -> 0
            let flippedN =
                if length = 0 then n 
                else utils comp "adjustSigned" [n; cInt length]
            if FormatString.isPlusForPositives f.Flags then utils comp "plusForPos" [t flippedN]
            elif FormatString.isSpaceForPositives f.Flags then utils comp "spaceForPos" [t flippedN]
            else t flippedN
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
                | 1 -> utils comp "printArray" [ Lambda([x], None, pp a (Var x)) ; o ]
                | 2 -> utils comp "printArray2D" [ Lambda([x], None, pp a (Var x)) ; o ]
                | _ -> utils comp "prettyPrint" [o]
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
                            let gtd, gm, _ = comp.NewGenerated("Printer_" + t.DisplayName)
                            comp.AddMetadataEntry(key, M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ])
                            let body = 
                                let x = Id.New(mut = false)
                                Lambda([x], None,
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
                        utils comp "printList" [ Lambda([x], None, pp ct.Generics.[0] (Var x)) ; o ]    
                    else
                        let td, m =
                            let key = M.CompositeEntry [ M.StringEntry "Printf"; M.TypeEntry t ]
                            match comp.GetMetadataEntries key with
                            | M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ] :: _ ->
                                gtd, gm
                            | _ ->
                                let gtd, gm, _ = comp.NewGenerated("Printer_" + t.DisplayName)
                                comp.AddMetadataEntry(key, M.CompositeEntry [ M.TypeDefinitionEntry gtd; M.MethodEntry gm ])
                                let gs = ct.Generics |> Array.ofList
                                let body =
                                    let x = Id.New(mut = false)
                                    Lambda([x], None,
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
                    utils comp "prettyPrint" [o]
            | _ -> utils comp "prettyPrint" [o]
        pp t o

    let inner = 
        if Array.isEmpty parts then cString "" else
        let mutable skipP = false
        let mutable skipPParens = false
        parts
        |> Seq.choose (function
            | FormatString.StringPart s -> 
                if skipPParens then
                    skipPParens <- false
                    if s.StartsWith("()") then
                        if s.Length = 2 then 
                            None
                        else
                            Some (cString (s.[2 ..]))
                    else
                        Some (cString s)
                else
                    Some (cString s)
            | FormatString.FormatPart f ->
                match f.TypeChar with
                | 'P' ->
                    skipPParens <- true
                    if skipP then
                        skipP <- false
                        None
                    else
                        withPadding f (fun (o, _) -> cCallG ["String"] [o])
                | _ ->
                skipP <- true
                match f.TypeChar with
                | 'b'
                | 'O' -> 
                    withPadding f (fun (o, _) -> cCallG ["String"] [o])
                | 'A' -> 
                    withPadding f (function 
                        | o, Some t -> 
                            prettyPrint t o
                        | o, _ -> utils comp "prettyPrint" [o]
                    )
                | 'c' -> 
                    withPadding f (fun (s, _) -> s)   
                | 's' -> 
                    withPadding f (fun (s, _) -> utils comp "toSafe" [s])
                | 'd' | 'i' ->
                    numberToString f (fun n -> cCallG ["String"] [n])
                | 'x' ->                                           
                    numberToStringForIntFormmating f (fun n -> cCall n "toString" [cInt 16])
                | 'X' ->                                           
                    numberToStringForIntFormmating f (fun n -> cCall (cCall n "toString" [cInt 16]) "toUpperCase" [])
                | 'o' ->                                           
                    numberToStringForIntFormmating f (fun n -> cCall n "toString" [cInt 8])
                | 'B' ->
                    numberToStringForIntFormmating f (fun n -> cCall n "toString" [cInt 2])
                | 'e' ->
                    numberToString f (fun n -> cCall n "toExponential" []) 
                | 'E' ->
                    numberToString f (fun n -> cCall (cCall n "toExponential" []) "toUpperCase" []) 
                | 'f' | 'F' | 'M' ->
                    numberToString f (fun n ->
                        let prec =
                            if f.IsPrecisionSpecified then
                                if f.IsStarPrecision then nextHole() |> fst else cInt f.Precision
                            else cInt 6 // Default precision
                        cCall n "toFixed" [prec]
                    )
                | c -> failwithf "Failed to parse format string: '%%%c' is not supported." c
        )
        |> Seq.reduce (^+)
    
    let k = Id.New(mut = false) 
    match intp with
    | None ->
        Lambda([k], None,
            args |> List.rev |> List.fold (fun c (a, _) -> Lambda([a], None, c)) (Var k).[[inner]]
        )
    | Some intp ->
        let cont =
            Lambda([k], None,
                (Var k).[[inner]]
            )
        List.zip args intp |> List.rev |> List.fold (fun c ((a, _), e) -> Let (a, e, c)) cont
  
let objty, objArrTy =
    let t = typeof<System.Object>
    let arrt = typeof<System.Object []>
    Reflection.ReadTypeDefinition t,
    Reflection.ReadTypeDefinition arrt
  
[<Sealed>]
type PrintF() =
    inherit Macro()
    override this.TranslateCtor(c) =
        match c.Arguments with
        | [I.Value (Literal.String fs)] ->
            let rec getFunctionArgs f =
                match f with
                | FSharpFuncType(a, r) -> 
                    a :: getFunctionArgs r
                | _ -> 
                    []
            let ts = c.DefiningType.Generics.Head |> getFunctionArgs |> List.map (fun t -> t.SubstituteGenericsToSame(NonGenericType objty))
            createPrinter c.Compilation ts None fs |> MacroOk
        | [I.Value (Literal.String fs); I.NewArray args; _] ->
            let rec getTupleArgs t =
                match t with
                | TupleType(tt, _) when args.Length > 1 -> 
                    tt
                | _ -> 
                    [t]
            let ts = c.DefiningType.Generics.[4] |> getTupleArgs |> List.map (fun t -> t.SubstituteGenericsToSame(NonGenericType objty))
            createPrinter c.Compilation ts (Some args) fs |> MacroOk
        | _ -> MacroError "printfMacro error"

[<AbstractClass>]
type StringInterpolationBase() =
    inherit Macro()

    abstract Process: stringParts: list<string> * holes: list<Expression> -> MacroResult
           
    override this.TranslateCall(c) =
        match c.Arguments with
        | [I.Value (Literal.String fs)] ->
            this.Process([fs], [])
        | [I.Call (None, etlo, pfts, [ I.Ctor (pf, _, pfArgs) ])] when 
            etlo.Entity.Value.FullName = "Microsoft.FSharp.Core.ExtraTopLevelOperators"
            && pfts.Entity.Value.MethodName = "PrintFormatToString" 
            && pf.Entity.Value.FullName = "Microsoft.FSharp.Core.PrintfFormat`5" ->
                match pfArgs with
                | [ I.Value (Literal.String fs) ] ->
                    this.Process([fs], [])
                | [I.Value (Literal.String fs); I.NewArray args; _] ->
                    this.Process(fs.Split([| "%P()" |], System.StringSplitOptions.None) |> List.ofArray, args)
                | _ ->
                    MacroError $"{this.GetType().Name} macro expects a string literal or string interpolation argument"
        | _ -> MacroError $"{this.GetType().Name} macro expects a string literal or string interpolation argument"

[<Sealed>]
type JSVerbatim() =
    inherit StringInterpolationBase()

    override this.Process(stringParts, holes) = 
        Verbatim(stringParts, holes, false) |> MacroOk

[<Sealed>]
type JSHtml() =
    inherit StringInterpolationBase()

    override this.Process(stringParts, holes) = 
        let l = stringParts.Length - 1
        let mutable useBraces = false
        let withBeginBrace (s: string) =
            useBraces <- not (s.EndsWith "<" || s.EndsWith "</")
            if useBraces then
                s + "{"
            else
                s
        let withEndBrace (s: string) =
            if useBraces then
                "}" + s
            else
                s
        let addedBraces =
            stringParts
            |> List.mapi (fun i s ->
                if i = 0 then
                    if l > 0 then 
                        s |> withBeginBrace
                    else s
                elif i = l then
                    s |> withEndBrace
                else
                    s |> withEndBrace |> withBeginBrace
            )
        Verbatim(addedBraces, holes, true) |> MacroOk

let rec isImplementing (comp: M.ICompilation) typ intf =
    comp.GetClassInfo typ
    |> Option.map (fun cls ->
        cls.Implements |> Seq.exists (fun i -> i.Entity = intf)
        || cls.BaseClass |> Option.exists (fun b -> isImplementing comp b.Entity intf |> Option.exists id) 
    )

[<Sealed>]
type EqualityComparer() =
    inherit Macro()

    static let ieqTy =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.IEquatable`1"
        }

    static member GetDefault(comp: M.ICompilation, t: Type) =
        if t.IsParameter then MacroNeedsResolvedTypeArg t else
        match t with
        | ConcreteType ct ->
            match isImplementing comp ct.Entity ieqTy with
            | Some isEquatable ->
                let td : TypeDefinitionInfo =
                    { Assembly = "WebSharper.StdLib"
                      FullName =
                        if isEquatable then
                            "WebSharper.Comparers.EquatableEqualityComparer`1"
                        else
                            "WebSharper.Comparers.BaseEqualityComparer`1" }
                Ctor (
                    { Entity = Hashed td; Generics = [t] },
                    ConstructorInfo.Default(),
                    []
                ) |> MacroOk
            | _ -> MacroError ("Class info not found for " + ct.Entity.Value.FullName)
        | _ -> MacroError "Type form not recognized"

    override this.TranslateCall(c) =
        match c.Method.Entity.Value.MethodName with
        | "get_Default" -> EqualityComparer.GetDefault(c.Compilation, c.DefiningType.Generics.[0])
        | _ -> MacroError "Not implemented"

[<Sealed>]
type Comparer() =
    inherit Macro()

    static let icmpTy =
        TypeDefinition {
            Assembly = "netstandard"
            FullName = "System.IComparable`1"
        } 

    static member GetDefault(comp: M.ICompilation, t: Type) =
        if t.IsParameter then MacroNeedsResolvedTypeArg t else
        match t with
        | ConcreteType ct ->
            match isImplementing comp ct.Entity icmpTy with
            | Some isEquatable ->
                let td : TypeDefinitionInfo =
                    { Assembly = "WebSharper.StdLib"
                      FullName =
                        if isEquatable then
                            "WebSharper.Comparers.ComparableComparer`1"
                        else
                            "WebSharper.Comparers.BaseComparer`1" }
                Ctor(
                    {Entity = Hashed td; Generics = [t]},
                    ConstructorInfo.Default(),
                    [])
                |> MacroOk
            | _ -> MacroError ("Class info not found for " + ct.Entity.Value.FullName)
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
            (td.Entity.Value.Assembly.StartsWith "netstandard" &&
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
                MacroOk (Ctor(td, ConstructorInfo.Default(), []))
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
type Unbox() =
    inherit Macro()

    override __.TranslateCall(c) =
        Coerce(c.Arguments.Head, NonGenericType Definitions.Object, c.Method.Generics.Head)
        |> MacroOk

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

//type InlineJS() =
//    inherit Macro()

//    override __.TranslateCall(c) =
//        try
//            let inl, pos =
//                match c.Arguments.Head with
//                | Value (String inl) -> inl, Unchecked.defaultof<_>
//                | ExprSourcePos(pos, Value (String inl)) -> inl, pos
//                | _ -> failwith "JS.Inline first argument must be a constant string"
//            let args =
//                match c.Arguments with
//                | [_] -> [] 
//                | [_; I.NewArray args] -> args
//                | _ -> failwith "JS.Inline arguments cannot be passed as an array"
//            c.Compilation.ParseJSInline(inl, args, pos) |> MacroOk
//        with e ->
//            MacroError e.Message

[<Sealed>]
type ImportJS() =
    inherit Macro()

    override __.TranslateCall(c) =
        match c.Method.Entity.Value.MethodName with
        | "Import" ->
            match c.Arguments with
            | [I.Value (String export); I.Value (String from)] ->
                if JavaScript.Identifier.IsValid export || export = "*" then
                    c.Compilation.AddJSImport (Some export, from) |> MacroOk
                elif export = "default" then
                    c.Compilation.AddJSImport (None, from) |> MacroOk                
                else
                    MacroError "JS.Import `export` argument must be a valid identifier"
            | _ -> 
                if c.IsInline then
                    MacroNeedsResolvedTypeArg (NonGenericType Definitions.String)
                else
                    MacroError "JS.Import arguments must be constant string"
        | "ImportDefault" ->
            match c.Arguments with
            | [I.Value (String from)] ->
                c.Compilation.AddJSImport (None, from) |> MacroOk
            | _ ->
                if c.IsInline then
                    MacroNeedsResolvedTypeArg (NonGenericType Definitions.String)
                else
                    MacroError "JS.ImportDefault arguments must be constant string"
        | "ImportAll" ->
            match c.Arguments with
            | [I.Value (String from)] ->
                c.Compilation.AddJSImport (Some "*", from) |> MacroOk
            | _ -> 
                if c.IsInline then
                    MacroNeedsResolvedTypeArg (NonGenericType Definitions.String)
                else
                    MacroError "JS.ImportAll argument must be constant string"
        | "ImportFile" ->
            match c.Arguments with
            | [I.Value (String from)] ->
                c.Compilation.AddJSImportSideEffect (from) |> MacroOk
            | _ -> 
                if c.IsInline then
                    MacroNeedsResolvedTypeArg (NonGenericType Definitions.String)
                else
                    MacroError "JS.ImportFile argument must be constant string"
        | _ ->
            failwith "Unrecognized method using ImportJS"

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
            | (I.Value (String format)) :: args when args.Length < 4 ->
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
                                ItemGet(Var argsId, cInt idx, Pure)
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
                                        (
                                            if w1 > 0 then
                                                Call (None, NonGeneric stringTy, NonGeneric padLeft, [r; cInt w2])
                                            else
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

[<Sealed>]
type Tuple() =
    inherit Macro()

    override __.TranslateCtor(c) =
        MacroFallback

    override __.TranslateCall(c) =
        let mname = c.Method.Entity.Value.MethodName
        if mname.StartsWith "get_Item" then
            MacroFallback
        else
            let t = TupleType (c.DefiningType.Generics, false)
            match mname with
            | "ToString" -> MacroOk <| Appl(Global ["String"], [c.This.Value], Pure, Some 1)
            | "GetHashCode" -> MacroOk <| Call (None, NonGeneric opUncheckedTy, Generic hashMeth [ t ], [c.This.Value]) 
            | "Equals" -> MacroOk <| Call (None, NonGeneric opUncheckedTy, Generic equalsMeth [ t ], [c.This.Value; c.Arguments.Head]) 
            | "CompareTo" -> MacroOk <| Call (None, NonGeneric opUncheckedTy, Generic compareMeth [ t ], [c.This.Value; c.Arguments.Head]) 
            | n ->  MacroFallback

[<Sealed>]
type TupleExtensions() =
    inherit Macro()

    override __.TranslateCall(c) =
        match c.Method.Entity.Value.MethodName with
        | "Deconstruct" ->
            match c.Arguments with
            | t :: args ->
                let v = Id.New "t"
                let writeOuts =
                    args |> List.mapi (fun i a ->
                        Appl(ItemGet(a, cString "set", Pure), [ ItemGet(Var v, cInt i, Pure) ], NonPure, Some 1)
                    ) |> Sequential
                MacroOk <| Let (v, t, writeOuts)
            | _ -> MacroError "Expecting a tuple argument for System.TupleExtensions.Deconstruct" 
        | "ToTuple"
        | "ToValueTuple" ->
            match c.Arguments with
            | [ x ] -> MacroOk x
            | _ -> MacroError "Expecting only a this argument for System.TupleExtensions.ToTuple/ToValueTuple"
        | n ->  MacroError ("Unrecognized method of System.TupleExtensions: " + n)
     
[<Sealed>]
type WebWorker() =
    inherit Macro()
        
    static let worker = NonGeneric <| TypeDefinition { Assembly = "WebSharper.JavaScript"; FullName = "WebSharper.JavaScript.Worker" }
    static let workerOptionsTy = NonGenericType <| TypeDefinition { Assembly = "WebSharper.JavaScript"; FullName = "WebSharper.JavaScript.WorkerOptions" }
    static let workerCtor = Constructor { CtorParameters = [NonGenericType stringTy; workerOptionsTy] }
        
    override __.TranslateCtor(c) =
        let gen name expr includeJsExports =
            let e =
                match expr with
                | Lambda([self], _, body) ->
                    Let(self, Global[], body)
                | e ->
                    Appl(e, [Global []], NonPure, Some 1)
            let filename = c.Compilation.AddBundle(name, ExprStatement e, includeJsExports).FileName
            let path = 
                Appl(
                    GlobalAccess (Address.Runtime "ScriptPath"),
                    [!~(Literal.String "workers"); !~(Literal.String (filename))],
                    NonPure, Some 2)
            let options = Object [ "type", MemberKind.Simple, !~(Literal.String "module") ]
            Ctor(worker, workerCtor, [path; options])
            |> MacroOk
        match c.Arguments with
        | [expr] -> gen "worker" expr false
        | [I.Value (String name); expr] -> gen name expr false
        | [I.Value (String name); I.Value (Bool includeJsExports); expr] -> gen name expr includeJsExports
        | [x; expr] -> MacroError (sprintf "You must use a string literal as the name of a web worker: %A" x)
        | _ -> MacroError "Invalid use of WebWorker macro"
        
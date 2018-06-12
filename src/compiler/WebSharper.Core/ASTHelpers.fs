﻿// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

[<AutoOpen>]
module WebSharper.Core.AST.ASTHelpers

/// Constructs a Concrete<'T> instance
let Generic e g =
    {
        Entity = e
        Generics = g
    }

/// Constructs a Concrete<'T> instance with empty type argument list
let NonGeneric e =
    {
        Entity = e
        Generics = []
    }

/// Constructs a ConcreteType case
let GenericType td g = ConcreteType (Generic td g)

/// Constructs a ConcreteType case with empty type argument list
let NonGenericType td = ConcreteType (NonGeneric td)

/// Constructs a ConcreteType with fully unresolved generics for the type.
let DefaultGenericType td = GenericType td (List.init td.Value.GenericLength TypeParameter)

/// Removes wrapping ExprSourcePos case if present from an AST.Expression
let IgnoreExprSourcePos expr =
    match expr with
    | ExprSourcePos (_, e) -> e
    | _ -> expr

let (|IgnoreExprSourcePos|) expr =
    match expr with
    | ExprSourcePos (_, e) -> e
    | _ -> expr

/// Copies wrapping ExprSourcePos case if present to another AST.Expression
let WithSourcePosOfExpr sourceExpr expr =
    match sourceExpr with      
    | ExprSourcePos (pos, _) -> ExprSourcePos (pos, expr)
    | _ -> expr

/// Removes wrapping ExprSourcePos case if present from an AST.Statement
let IgnoreStatementSourcePos expr =
    match expr with
    | StatementSourcePos (_, e) -> e
    | _ -> expr

/// Copies wrapping ExprSourcePos case if present to another AST.Statement
let WithSourcePosOfStatement sourceStatement statement =
    match sourceStatement with      
    | ExprSourcePos (pos, _) -> ExprSourcePos (pos, statement)
    | _ -> statement

/// Creates an AST.Literal value from an object
let ReadLiteral (value: obj) =
    match value with
    | x when obj.ReferenceEquals(x, null) -> Null      
    | :? bool   as x -> Bool   x
    | :? byte   as x -> Byte   x
    | :? char   as x -> Char   x
    | :? double as x -> Double x
    | :? int    as x -> Int    x
    | :? int16  as x -> Int16  x
    | :? int64  as x -> Int64  x
    | :? sbyte  as x -> SByte  x
    | :? single as x -> Single x
    | :? string as x -> String x
    | :? uint16 as x -> UInt16 x
    | :? uint32 as x -> UInt32 x
    | :? uint64 as x -> UInt64 x
    | :? decimal as x -> Decimal x
    | _ -> failwithf "Literal value not recognized: %A" value

let private uncheckedOps =
    TypeDefinition {
        Assembly = "FSharp.Core"
        FullName = "Microsoft.FSharp.Core.Operators+Unchecked"
    }

let private defaultOf =
    Method {
        MethodName = "DefaultOf"
        Parameters = []
        ReturnType = TypeParameter 0
        Generics = 1
    }

/// Creates a call to a macro that generates default value for type
let DefaultValueOf (typ : Type) =
    Call(None, NonGeneric uncheckedOps, Generic defaultOf [typ], [])

let IsDefaultValue td meth =
    td = uncheckedOps && meth = defaultOf

/// Combines a list of AST.Statements into a single AST.Statement
let CombineStatements statements =
    let mutable go = true
    let woEmpty =
        statements |> List.filter (fun s -> 
            match IgnoreStatementSourcePos s with
            | Empty 
            | ExprStatement Undefined -> false
            | DoNotReturn -> 
                go <- false
                false
            | Break _
            | Continue _
            | Throw _
            | Return _ ->
                if go then
                    go <- false
                    true
                else false
            | FuncDeclaration _ -> true
            | _ -> go
        )    
    match woEmpty with
    | [] -> Empty
    | [s] -> s
    | _ -> Block woEmpty

/// Combines a list of AST.Expressions into a single AST.Expression
let CombineExpressions exprs =
    let rec collect es =
        es |> Seq.collect (fun e ->  
            match IgnoreExprSourcePos e with 
            | Undefined -> Seq.empty 
            | Sequential i -> collect i
            | i -> Seq.singleton i
        )
    match collect exprs |> List.ofSeq with
    | [] -> Undefined
    | [ a ] -> a
    | res -> Sequential res

/// Creates a GlobalAccess case from an access list in normal order
let Global a = GlobalAccess (Address (List.rev a))

/// Make a proxy for a by-address value, having two functions for get/set.
let MakeRef getVal setVal =
    let value = Id.New("v", false)
    Object [
        "get", (Function ([], Return getVal))
        "set", (Function ([value], ExprStatement (setVal (Var value))))
    ]

/// Gets the value from a by-address value proxy
let GetRef r =
    Application(ItemGet (r, Value (String "get"), Pure), [], NoSideEffect, Some 0)

/// Sets the value of a by-address value proxy
let SetRef r v =
    Application(ItemGet (r, Value (String "set"), Pure), [v], NonPure, Some 1)

/// recognizes .NET names for binary operators
let (|BinaryOpName|_|) = function
    | "op_Addition" -> Some BinaryOperator.``+``
    | "op_Subtraction" -> Some BinaryOperator.``-``
    | "op_Multiply" -> Some BinaryOperator.``*``
    | "op_Division" -> Some BinaryOperator.``/``
    | "op_Modulus" -> Some BinaryOperator.``%``
    | "op_ExclusiveOr" -> Some BinaryOperator.``^``
    | "op_BitwiseAnd" -> Some BinaryOperator.``&``
    | "op_BitwiseOr" -> Some BinaryOperator.``|``
    | "op_LogicalAnd" -> Some BinaryOperator.``&&``
    | "op_LogicalOr" -> Some BinaryOperator.``||``
    | "op_LeftShift" -> Some BinaryOperator.``<<``
    | "op_RightShift" -> Some BinaryOperator.``>>``
    | "op_Equality" -> Some BinaryOperator.``==``
    | "op_Inequality" -> Some BinaryOperator.``!=``
    | "op_GreaterThan" -> Some BinaryOperator.``>``
    | "op_LessThan" -> Some BinaryOperator.``<``
    | "op_GreaterThanOrEqual" -> Some BinaryOperator.``>=``
    | "op_LessThanOrEqual" -> Some BinaryOperator.``<=``
    | _ -> None

/// recognizes .NET names for unary operators
let (|UnaryOpName|_|) = function
    | "op_UnaryPlus" -> Some UnaryOperator.``+``
    | "op_LogicalNot" -> Some UnaryOperator.``!``
    | "op_UnaryNegation" -> Some UnaryOperator.``-``
    | "op_OnesComplement" -> Some UnaryOperator.``~``
    | _ -> None

let erasedUnions = 
    System.Collections.Generic.HashSet (
        seq {
            yield TypeDefinition {
                Assembly = "WebSharper.Core"
                FullName = "WebSharper.JavaScript.Optional`1"
            }
            for i in 2 .. 7 ->
                TypeDefinition {
                    Assembly = "WebSharper.Core"
                    FullName = "WebSharper.JavaScript.Union`" + string i
                }    
        }
    )

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

let integralTypes =  smallIntegralTypes + bigIntegralTypes

let scalarTypes =
    integralTypes
    + Set [
        "System.Double"
        "System.Single"
        "System.TimeSpan"
        "System.DateTime"
    ]

let comparableTypes =
    scalarTypes
    + Set [
        "System.String"
        "System.Char"
    ]

type private NumericTypeKind =
    | SmallIntegralType
    | BigIntegralType
    | ScalarType
    | DecimalType
    | CharType
    | StringType
    | NonNumericType        

let private getNumericTypeKind n =
    if smallIntegralTypes.Contains n then SmallIntegralType
    elif bigIntegralTypes.Contains n then BigIntegralType
    elif scalarTypes.Contains n then ScalarType
    elif n = "System.Char" then CharType
    elif n = "System.String" then StringType
    elif n = "System.Decimal" then DecimalType
    else NonNumericType

let private floatCtor =
    Constructor { CtorParameters = [ NonGenericType Definitions.Float ] }

let private parseDecimal =
    Method {
        MethodName = "Parse"
        Parameters = [ NonGenericType Definitions.String ]
        ReturnType = NonGenericType Definitions.Decimal
        Generics = 0      
    } |> NonGeneric

let NumericConversion (fromTyp: TypeDefinition) (toTyp: TypeDefinition) expr =
    let toNumber expr =
        Application(Global ["Number"], [expr], Pure, Some 1)
    let toDecimal expr =
        Ctor (NonGeneric Definitions.Decimal, floatCtor, [expr])
    let charCode expr =
        Application(ItemGet(expr, Value (String "charCodeAt"), Pure), [], Pure, None)
    let fromCharCode expr =
        Application(Global ["String"; "fromCharCode"], [expr], Pure, Some 1)
    match getNumericTypeKind fromTyp.Value.FullName, getNumericTypeKind toTyp.Value.FullName with
    | SmallIntegralType, (SmallIntegralType | BigIntegralType | ScalarType)
    | (BigIntegralType | NonNumericType), (SmallIntegralType | BigIntegralType | ScalarType)
    | ScalarType, ScalarType
    | DecimalType, DecimalType
    | CharType, (CharType | StringType)
    | StringType, StringType
        -> expr
    | ScalarType, SmallIntegralType
        -> expr ^>> !~(Int 0)
    | ScalarType, BigIntegralType
        -> Application(Global ["Math"; "trunc"], [expr], Pure, Some 1)
    | (SmallIntegralType | BigIntegralType | ScalarType), CharType
        -> fromCharCode expr
    | DecimalType, CharType
        -> fromCharCode (toNumber expr)
    | DecimalType, BigIntegralType
        -> toNumber expr
    | DecimalType, SmallIntegralType
        -> toNumber expr ^>> !~(Int 0)
    | CharType, (SmallIntegralType | BigIntegralType | ScalarType)
        -> charCode expr
    | CharType, DecimalType
        -> toDecimal (charCode expr)
    | (SmallIntegralType | BigIntegralType | ScalarType | DecimalType | NonNumericType), StringType
        -> Application(Global ["String"], [expr], Pure, Some 1)
    | (StringType | DecimalType), (SmallIntegralType | BigIntegralType | ScalarType)
        -> toNumber expr
    | (SmallIntegralType | BigIntegralType | ScalarType), DecimalType
        -> toDecimal expr
    | StringType, DecimalType
        -> Call(None, NonGeneric toTyp, parseDecimal, [expr])
    | _ ->
        expr

/// Change every occurence of one Id to another
type ReplaceId(fromId, toId) =
    inherit Transformer()
    
    override this.TransformId i =
        if i = fromId then toId else i

/// Change every occurence of multiple Ids
type ReplaceIds(repl : System.Collections.Generic.IDictionary<Id, Id>) =
    inherit Transformer()
    
    override this.TransformId i =
        match repl.TryGetValue i with
        | true, j -> j
        | _ -> i

let EmbedAST<'T> (v: Expression) : FSharp.Quotations.Expr<'T> =
    FSharp.Quotations.Expr.Value(v, typeof<'T>) 
    |> FSharp.Quotations.Expr.Cast

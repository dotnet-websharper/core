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

[<AutoOpen>]
module WebSharper.Core.AST.ASTHelpers

open WebSharper.Core

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
    | :? (byte[]) as x -> ByteArray x
    | :? (uint16[]) as x -> UInt16Array x
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

let (|DefaultValueOf|_|) expr =
    match IgnoreExprSourcePos expr with
    | Call(None, { Entity = td; Generics = [] }, { Entity = meth; Generics = [ typ ] }, []) when IsDefaultValue td meth -> Some typ
    | _ -> None

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
let Global a = GlobalAccess (Address.LibAddr a)

/// Make a proxy for a by-address value, having two functions for get/set.
let MakeRef getVal setVal typ =
    let value = Id.New("v", false, ?typ = typ)
    Object [
        "get", MemberKind.Simple, (Function ([], None, typ, Return getVal))
        "set", MemberKind.Simple, (Function ([value], None, None, ExprStatement (setVal (Var value))))
    ]

/// Make a proxy for a out argument, having a setter function.
let MakeOutRef setVal typ =
    let value = Id.New("v", false)
    Object [
        "set", MemberKind.Simple, (Function ([value], None, typ, ExprStatement (setVal (Var value))))
    ]

/// Make a proxy for an in argument, having a getter function.
let MakeInRef getVal typ =
    Object [
        "get", MemberKind.Simple, (Function ([], None, typ, Return getVal))
    ]

/// Gets the value from a by-address value proxy
let GetRef r =
    Appl(ItemGet (r, Value (String "get"), Pure), [], NoSideEffect, Some 0)

/// Sets the value of a by-address value proxy
let SetRef r v =
    Appl(ItemGet (r, Value (String "set"), Pure), [v], NonPure, Some 1)

/// recognizes .NET names for binary operators
let (|BinaryOpName|_|) = function
    | "op_Addition" -> Some BinaryOperator.``+``
    | "op_Subtraction" -> Some BinaryOperator.``-``
    | "op_Multiply" -> Some BinaryOperator.``*``
    | "op_Division" -> Some BinaryOperator.``/``
    | "op_Exponentiation" -> Some BinaryOperator.``**``
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
    | "op_LogicalNot" -> Some UnaryOperator.``~``
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
            yield TypeDefinition {
                Assembly = "WebSharper.Core"
                FullName = "WebSharper.JavaScript.ItemOrArray`1"
            }
            for i in 2 .. 7 ->
                TypeDefinition {
                    Assembly = "WebSharper.Core"
                    FullName = "WebSharper.JavaScript.Union`" + string i
                }    
        }
    )

let smallIntegralTypeSizes =
    Map [
        "System.Byte", (0u, 255u)
        "System.SByte", (128u, 255u)
        "System.UInt16", (0u, 65535u)
        "System.Int16", (32768u, 65535u)
        "System.UInt32", (0u, 4294967295u)
        "System.Int32", (2147483648u, 4294967295u)
    ]

let smallIntegralTypes =
    smallIntegralTypeSizes
    |> Map.toSeq |> Seq.map fst |> Set

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
    | SmallIntegralType of uint32 * uint32
    | BigIntegralType
    | ScalarType
    | DecimalType
    | CharType
    | StringType
    | NonNumericType        

let private getNumericTypeKind n =
    match smallIntegralTypeSizes |> Map.tryFind n with
    | Some range -> SmallIntegralType range
    | _ ->
    if bigIntegralTypes.Contains n then BigIntegralType
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

let private opModule =
    TypeDefinition {
        Assembly = "FSharp.Core"
        FullName = "Microsoft.FSharp.Core.Operators"
    } |> NonGeneric

let private toIntMeth =
    Method {
        MethodName = "toInt"
        Parameters = [ NonGenericType Definitions.Float ]
        ReturnType = NonGenericType Definitions.Int
        Generics = 0      
    } |> NonGeneric

let private toUIntMeth =
    Method {
        MethodName = "toUInt"
        Parameters = [ NonGenericType Definitions.Float ]
        ReturnType = NonGenericType Definitions.Int
        Generics = 0      
    } |> NonGeneric

let MathTrunc expr =
    Appl(ItemGet(Global ["Math"], Value (String "trunc"), Pure), [expr], Pure, Some 1)

let NumericConversion (fromTyp: TypeDefinition) (toTyp: TypeDefinition) expr =
    let toNumber expr =
        Appl(Global ["Number"], [expr], Pure, Some 1)
    let toDecimal expr =
        Ctor (NonGeneric Definitions.Decimal, floatCtor, [expr])
    let toBigInt expr =
        Appl(Global ["BigInt"], [expr], Pure, Some 1)
    let charCode expr =
        Appl(ItemGet(expr, Value (String "charCodeAt"), Pure), [], Pure, None)
    let fromCharCode expr =
        Appl(Global ["String"; "fromCharCode"], [expr], Pure, Some 1)
    let toString expr =
        Appl(Global ["String"], [expr], Pure, Some 1)
    let toIntegral (neg: uint32) (mask: uint32) expr =
        if mask = 4294967295u then
            if neg = 0u then
                Call(None, opModule, toUIntMeth, [expr])
            else
                Call(None, opModule, toIntMeth, [expr])
        elif neg = 0u then
            expr ^& !~(UInt32 mask)   
        else
            ((expr ^+ !~(UInt32 neg)) ^& !~(UInt32 mask)) ^- !~(UInt32 neg) 
    let toIntegralFromBigInt (neg: uint32) (mask: uint32) expr =
        if neg = 0u then
            toNumber (expr ^& !~(UInt64 (uint64 mask)))   
        else
            toNumber (((expr ^+ !~(UInt64 (uint64 neg))) ^& !~(UInt64 (uint64 mask))) ^- !~(UInt64 (uint64 neg))) 
    match getNumericTypeKind fromTyp.Value.FullName, getNumericTypeKind toTyp.Value.FullName with
    | SmallIntegralType _, ScalarType
    | BigIntegralType, BigIntegralType
    | ScalarType, ScalarType
    | DecimalType, DecimalType
    | CharType, (CharType | StringType)
    | StringType, StringType
        -> expr
    | SmallIntegralType _, BigIntegralType -> 
        toBigInt expr
    | BigIntegralType, ScalarType ->
        toNumber expr
    | (SmallIntegralType _ | ScalarType), SmallIntegralType (neg, mask)
        -> expr |> toIntegral neg mask
    | BigIntegralType, SmallIntegralType (neg, mask)
        -> expr |> toIntegralFromBigInt neg mask
    | ScalarType, BigIntegralType
        -> toBigInt(Appl(Global ["Math"; "trunc"], [expr], Pure, Some 1))
    | (SmallIntegralType _ | ScalarType), CharType
        -> fromCharCode expr
    | (BigIntegralType | DecimalType), CharType
        -> fromCharCode (toNumber expr)
    | CharType, ScalarType
        -> charCode expr
    | CharType, BigIntegralType
        -> toBigInt (charCode expr)
    | CharType, SmallIntegralType (neg, mask) -> 
        if mask >= uint32 System.Int32.MaxValue then
            charCode expr
        else
            charCode expr |> toIntegral neg mask
    | CharType, DecimalType
        -> charCode expr |> toDecimal
    | (SmallIntegralType _ | BigIntegralType | ScalarType | DecimalType | NonNumericType), StringType
        -> toString expr
    | (StringType | DecimalType | NonNumericType), ScalarType
        -> toNumber expr
    | (StringType | NonNumericType), BigIntegralType
        -> toBigInt expr
    | DecimalType, BigIntegralType
        -> toBigInt (toString expr)
    | (StringType | DecimalType | NonNumericType), SmallIntegralType (neg, mask)
        -> toNumber expr |> toIntegral neg mask
    | (SmallIntegralType _ | ScalarType), DecimalType
        -> toDecimal expr
    | BigIntegralType, DecimalType
        -> toDecimal (toNumber expr) 
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

module TSType =
    open System

    let Basic x = TSType.Named [ x ]
    let (|Basic|_|) = function
        | TSType.Named [ x ] -> Some x
        | _ -> None
    
    let Object = Basic "object"
    let (|Object|_|) = function
        | Basic "object" -> Some ()
        | _ -> None
    let Null = Basic "null"
    let (|Null|_|) = function
        | Basic "null" -> Some ()
        | _ -> None
    let String = Basic "string"
    let (|String|_|) = function
        | Basic "string" -> Some ()
        | _ -> None
    let Number = Basic "number"
    let (|Number|_|) = function
        | Basic "number" -> Some ()
        | _ -> None
    let Boolean = Basic "boolean"
    let (|Boolean|_|) = function
        | Basic "boolean" -> Some ()
        | _ -> None
    let Never = Basic "never"
    let (|Never|_|) = function
        | Basic "never" -> Some ()
        | _ -> None
    let Void = Basic "void"
    let (|Void|_|) = function
        | Basic "void" -> Some ()
        | _ -> None
    let private Array = Basic "Array"
    let ArrayOf t = TSType.Generic(Array, [ t ])
    let (|ArrayOf|_|) t = 
        match t with
        | TSType.Generic(TSType.Named [ "Array" ], [ t ]) -> Some t
        | _ -> None
    let LambdaWithOpt (a, r) = TSType.Function(None, a, None, r)
    let Lambda (a, r) = 
        let a =
            match a with
            | [ TSType.Param _ as p ] -> [ p, true ]
            | _ -> a |> List.map (fun a -> a, false) 
        TSType.Function(None, a, None, r)

    let Parse x =
        // todo expand parser
        match x with
        | "any" -> TSType.Any
        | "$0" -> TSType.Param 0
        | "()=>$0" -> Lambda ([], TSType.Param 0)
        | "$0[]" -> ArrayOf (TSType.Param 0) 
        | "{K: $0, V: $1}" -> TSType.TypeLiteral [ "K", MemberKind.Simple, TSType.Param 0; "V", MemberKind.Simple, TSType.Param 1 ]
        | "{d: number, o: number}" -> TSType.TypeLiteral [ "d", MemberKind.Simple, Number; "o", MemberKind.Simple, Number ]
        | "{c: boolean, r: (() => void)[]}" -> TSType.TypeLiteral [ "c", MemberKind.Simple, TSType.Named [ "boolean" ]; "r", MemberKind.Simple, TSType.Named [ "(() => void)[]" ] ]
        | "(a: $wstype(../WebSharper.StdLib/WebSharper.Concurrency.AsyncBody`1)) => void" -> 
            Lambda([TSType.Importing (Address.TypeDefaultExport { Assembly = "WebSharper.StdLib"; Name = "WebSharper.Concurrency.AsyncBody`1" })], Void)            
        | _ -> 
            TSType.Named (List.ofArray (x.Split('.')))

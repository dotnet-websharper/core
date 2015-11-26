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
module WebSharper.Macro

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST

//module C = WebSharper.Core.JavaScript.Core
module A = WebSharper.Core.Attributes
//module Q = WebSharper.Core.Quotations
//module R = WebSharper.Core.Reflection

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

let isIn (s: string Set) (t: Type) = 
    match t with
    | ConcreteType t ->
        s.Contains t.Entity.Value.FullName
    | _ ->
        false

//let (|CallOrCM|_|) q =
//    match q with 
//    | Q.Call (m, l)
//    | Q.CallModule (m, l) -> Some (m, l)
//    | _ -> None

//let (|OptCoerce|) q =
//    match q with
//    | Q.Coerce (_, x)
//    | x -> x

[<Sealed>]
type Div() =
    inherit Macro()
    override this.TranslateCall(_,_,m,a,_) =
        match a with
        | [x; y] ->
            match m.Generics with
            | t :: _ ->                                                                     
                if isIn smallIntegralTypes t
                then (x ^/ y) ^>> !~(Int 0)
                elif isIn bigIntegralTypes t
                then Application(globalAccess ["Math"; "trunc"], [x ^/ y])
                else x ^/ y 
            | _ -> x ^/ y   
            |> MacroOk
        | _ -> MacroError "divisionMacro error"

[<AbstractClass>]
type Arith(name, op) =
    inherit Macro()
    override this.TranslateCall(_,_,m,a,_) =
        match a with
        | [x; y] ->
            match m.Generics with
            | t :: _ when not (isIn scalarTypes t) ->
                Application (ItemGet(x, Value (String name)), [y])
            | _ -> Binary(x, op, y)
            |> MacroOk
        | _ -> MacroError "divisionMacro error"

[<Sealed>]
type Add() = inherit Arith("add", BinaryOperator.``+``) 

[<Sealed>]
type Sub() = inherit Arith("sub", BinaryOperator.``-``) 

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

let makeComparison cmp x y =
    let eq x y = Application(globalAccess ["WebSharper"; "Unchecked"; "Equals"], [x; y])
    let c b i   = Binary (Application(globalAccess ["WebSharper"; "Unchecked"; "Compare"], [x; y]), b, Value(Int i))
    match cmp with
    | Comparison.``<``  -> c BinaryOperator.``===`` -1
    | Comparison.``<=`` -> c BinaryOperator.``<=`` 0
    | Comparison.``>``  -> c BinaryOperator.``===`` 1
    | Comparison.``>=`` -> c BinaryOperator.``>=`` 0
    | Comparison.``=``  -> eq x y
    | _                 -> Unary (UnaryOperator.``!``, eq x y)

[<AbstractClass>]
type CMP(cmp) =
    inherit Macro()
    override this.TranslateCall(_,_,m,a,_) =
        match a with
        | [x; y] ->
            match m.Generics with
            | t :: _ ->
                if isIn scalarTypes t then
                    Binary (x, toBinaryOperator cmp, y)
                else
                    makeComparison cmp x y
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

[<Sealed>]
type Char() =
    inherit Macro()
    override this.TranslateCall(_,_,m,a,_) =
        match a with
        | [x] ->
            match m.Generics with
            | t :: _ ->
                if isIn integralTypes t then MacroOk x else
                    match t with
                    | ConcreteType d ->
                        match d.Entity.Value.FullName with
                        | "System.String" ->
                            Application(globalAccess ["WebSharper"; "Char"; "Parse"], [x])
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
    override this.TranslateCall(_,_,m,a,_) =
        match a with
        | [x] ->
            match m.Generics with
            | t :: _ ->
                if t.AssemblyQualifiedName = "System.Char, mscorlib" then
                    Application(globalAccess ["String"; "fromCharCode"], [x])    
                else 
                    Application(globalAccess ["String"], [x])   
                |> MacroOk 
            | _ ->
                MacroError "stringMacro error"
        | _ ->
            MacroError "stringMacro error"

//let getFieldsList q =
//    let ``is (=>)`` (td: TypeDefinition) (m: Method) =
//        td.Value.FullName = "WebSharper.JavaScript.Pervasives"
//        && m.Value.MethodName = "op_EqualsGreater"
//    let rec getFieldsListTC l q =
//        match q with
//        | Q.NewUnionCase (_, [Q.NewTuple [Q.Value (Q.String n); v]; t]) ->
//            getFieldsListTC ((n, v) :: l) t         
//        | Q.NewUnionCase (_, [Q.CallOrCallModule (m, [Q.Value (Q.String n); v]); t])
//            when m.Entity |> ``is (=>)`` ->
//            getFieldsListTC ((n, v) :: l) t         
//        | Q.NewUnionCase (_, []) -> Some (l |> List.rev) 
//        | Q.NewArray (_,  l) ->
//            l |> List.map (
//                function 
//                | Q.NewTuple [Q.Value (Q.String n); v] -> n, v 
//                | Q.CallOrCallModule (m, [Q.Value (Q.String n); v])
//                    when m.Entity |> ``is (=>)`` -> n, v
//                | _ -> failwith "Wrong type of array passed to New"
//            ) |> Some
//        | _ -> None
//    getFieldsListTC [] q
//
//let newMacro tr q =
//    match q with
//    | Q.CallOrCallModule (_, [OptCoerce x]) ->
//        match getFieldsList x with
//        | Some xl ->
//            C.NewObject (xl |> List.map (fun (n, v) -> n, tr v))
//        | _ ->
//            cCallG ["WebSharper"; "JavaScript"; "Pervasives"] "NewFromList" [tr x]
//    | _ ->
//        failwith "newMacro error"
//
//[<Sealed>]
//type New() =
//    interface M.IMacro with
//        member this.Translate(q, tr) = newMacro tr q

//type FST = Reflection.FSharpType

[<Sealed>]
type FuncWithArgs() =
    inherit Macro()
    override this.TranslateCtor(t,_,a,_) =
        match a with
        | [func] ->
            match t.Generics.[0] with
            | TupleType _ ->
                Application(runtimeCreateFuncWithArgs, [ func ]) |> MacroOk
            | _ ->
                MacroError "Wrong type argument on FuncWithArgs: 'TArgs must be a tuple"
        | _ ->
            MacroError "funcWithArgsMacro error"

[<Sealed>]
type FuncWithArgsRest() =
    inherit Macro()
    override this.TranslateCtor(t,_,a,_) =
        match a with
        | [func] ->
            match t.Generics.[0] with
            | TupleType ts ->
                Application(runtimeCreateFuncWithArgsRest, [ Value (Int (List.length ts)) ; func ])
                |> MacroOk
            | _ ->
                MacroError "Wrong type argument on FuncWithArgsRest: 'TArgs must be a tuple"
        | _ ->
            MacroError "funcWithArgsMacro error"

[<Sealed>]
type FuncWithThis() =
    inherit Macro()
    override this.TranslateCtor(t,_,a,_) =
        match a with
        | [func] ->
            match t.Generics.[0] with
            | FSharpFuncType _ ->
                Application(runtimeCreateFuncWithThis, [ func ]) |> MacroOk
            | ConcreteType td when (
                    let n = td.Entity.Value.FullName
                    n = "WebSharper.JavaScript.Function" || n.StartsWith "WebSharper.JavaScript.FuncWith" 
                ) ->
                Application(runtimeCreateFuncWithThis, [ func ]) |> MacroOk
            | _ ->
                MacroError "Wrong type argument on FuncWithThis: 'TFunc must be an F# function or JavaScript function type"
        | _ ->
            MacroError "funcWithArgsMacro error"

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

let printfHelpers f args = Application (globalAccess ["WebSharper"; "PrintfHelpers"; f], args) 
let stringProxy f args = Application (globalAccess ["WebSharper"; "Strings"; f], args)
let cCall e f args = Application (ItemGet(e, !~ (Literal.String f)), args)
let cCallG a args = Application (globalAccess a, args)

type FST = Reflection.FSharpType

let cInt i = Value (Int i)
let cString s = Value (Literal.String s)

let createPrinter ts fs =
    let parts = FormatString.parseAll fs
    let args = ts |> List.map (fun t -> Id.New(), Some t)
        
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
                stringProxy "PadRight" [s; width]
            else
                if FormatString.isPadWithZeros f.Flags then
                    printfHelpers "padNumLeft" [s; width]
                else
                    stringProxy "PadLeft" [s; width]
        else t (nextVar())
        
    let numberToString (f: FormatString.FormatSpecifier) t =
        withPadding f (fun (n, _) ->
            if FormatString.isPlusForPositives f.Flags then printfHelpers "plusForPos" [n; t n]
            elif FormatString.isSpaceForPositives f.Flags then printfHelpers "spaceForPos" [n; t n]
            else t n
        )

    let prettyPrint t o = 
        let d = Dictionary<System.Type, Id * Expression ref>()
        let rec pp t (o: Expression) = 
            if FST.IsTuple t then
                seq {
                    yield cString "("
                    let ts = FST.GetTupleElements t
                    for i = 0 to ts.Length - 1 do 
                        yield pp ts.[i] o.[cInt i] 
                        if i < ts.Length - 1 then yield cString ", "
                    yield cString ")"
                }
                |> Seq.reduce (^+)
            elif FST.IsRecord t then
                let pi = 
                    match d.TryGetValue t with
                    | false, _ ->
                        let pi = Id.New()
                        let pr = ref Undefined // placeholder
                        d.Add(t, (pi, pr))
                        pr := (
                            let x = Id.New()
                            Lambda([x], 
                                seq {
                                    yield cString "{"
                                    let fs = FST.GetRecordFields(t, flags)
                                    for i = 0 to fs.Length - 1 do
                                        let f = fs.[i]
                                        let name = 
                                            f.GetCustomAttributesData() |> Seq.tryPick (fun a ->
                                                if a.Constructor.DeclaringType = typeof<A.NameAttribute>
                                                then Some (a.ConstructorArguments.[0].Value :?> string)
                                                else None
                                            ) |> function Some n -> n | _ -> f.Name
                                        yield cString (f.Name + " = ") ^+ pp f.PropertyType (Var x).[cString name]
                                        if i < fs.Length - 1 then yield cString "; "
                                    yield cString "}"
                                }
                                |> Seq.reduce (^+)
                            ) 
                        )
                        pi
                    | true, (pi, _) -> pi
                (Var pi).[[o]]
            elif t.IsArray then
                let r = t.GetArrayRank()
                let a = t.GetElementType()
                let x = Id.New()
                match r with 
                | 1 -> printfHelpers "printArray" [ Lambda([x], pp a (Var x)) ; o ]
                | 2 -> printfHelpers "printArray2D" [ Lambda([x], pp a (Var x)) ; o ]
                | _ -> printfHelpers "prettyPrint" [o]
            else
            let tn =
                if t.IsGenericType 
                then Some (t.GetGenericTypeDefinition().FullName)
                else None
            if tn = Some "Microsoft.FSharp.Collections.FSharpList`1" then
                let a = t.GetGenericArguments().[0]
                let x = Id.New()
                printfHelpers "printList" [ Lambda([x], pp a (Var x)) ; o ]    
            elif FST.IsUnion t then
                let pi =
                    match d.TryGetValue t with
                    | false, _ ->
                        let pi = Id.New()
                        let pr = ref Undefined // placeholder
                        d.Add(t, (pi, pr))
                        pr := (
                            let x = Id.New()
                            Lambda([x], 
                                FST.GetUnionCases(t, flags) |> Seq.map (fun c ->
                                    let fs = c.GetFields()
                                    let constant =  
                                        c.GetCustomAttributesData()
                                        |> Seq.tryPick (fun cad -> 
                                            if cad.Constructor.DeclaringType = typeof<A.ConstantAttribute> then
                                                let arg = cad.ConstructorArguments.[0]
                                                if arg.ArgumentType = typeof<int> then
                                                    cInt (unbox arg.Value)
                                                elif arg.ArgumentType = typeof<float> then
                                                    !~ (Double (unbox x))
                                                elif arg.ArgumentType = typeof<bool> then
                                                    !~ (Bool (unbox arg.Value))
                                                elif arg.ArgumentType = typeof<string> then
                                                    cString (unbox arg.Value)
                                                else failwith "Invalid ConstantAttribute."
                                                |> Some
                                            else None)
                                    match constant with
                                    | Some cVal -> Choice1Of2 (cVal, cString c.Name)
                                    | None -> 
                                        Choice2Of2(
                                            c.Tag,
                                            match fs.Length with
                                            | 0 -> cString c.Name
                                            | 1 -> 
                                                cString (c.Name + " ") ^+ pp fs.[0].PropertyType (Var x).[cString "$0"]
                                            | _ -> 
                                                seq {
                                                    yield cString (c.Name + " (")
                                                    for i = 0 to fs.Length - 1 do
                                                        yield pp fs.[i].PropertyType (Var x).[cString ("$" + string i)]
                                                        if i < fs.Length - 1 then yield cString ", "
                                                    yield cString ")"
                                                }
                                                |> Seq.reduce (^+)
                                        )
                                )
                                |> Seq.fold (fun s cInfo ->
                                    match s with
                                    | None -> match cInfo with Choice1Of2 (_, e) | Choice2Of2 (_, e) -> Some e
                                    | Some s -> 
                                        match cInfo with
                                        | Choice1Of2 (cVal, e) -> Some <| Conditional (Var x ^== cVal, e, s)
                                        | Choice2Of2 (tag, e) -> Some <| Conditional ((Var x).[cString "$"] ^== cInt tag, e, s)
                                ) None |> Option.get
                            )
                        )
                        pi
                    | true, (pi, _) -> pi
                (Var pi).[[o]]
            else printfHelpers "prettyPrint" [o]
        let inner = pp t o
        if d.Count = 0 then inner else
        LetRec (d |> Seq.map (fun (KeyValue(_, (pi, pr))) -> pi, !pr) |> List.ofSeq, inner)

    let inner = 
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
                        | o, _ -> printfHelpers "prettyPrint" [o]
                    )
                | 'c' -> 
                    withPadding f (fun (s, _) -> cCallG ["String"; "fromCharCode"]  [s])   
                | 's' -> 
                    withPadding f (fun (s, _) -> printfHelpers "toSafe" [s])
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
    
    let k = Id.New() 
    Lambda([k],
        args |> List.rev |> List.fold (fun c (a, _) -> Lambda([a], c)) (Var k).[[inner]]
    )
    
[<Sealed>]
type PrintF() =
    inherit Macro()
    override this.TranslateCtor(t,_,a,_) =
//        let rec getFunctionArgs t =
//            if FST.IsFunction t then
//                let x, y = FST.GetFunctionElements t
//                x :: getFunctionArgs y
//            else []
        let rec getFunctionArgs f =
            match f with
            | FSharpFuncType(a, r) -> 
                a :: getFunctionArgs r
            | _ -> 
                []
        match a with
        | [IgnoreExprSourcePos (Value (Literal.String fs))] ->
            let ts = //t.Generics.[0] |> Reflection.loadType |> getFunctionArgs
                t.Generics.Head |> getFunctionArgs |> List.map Reflection.loadType
            createPrinter ts fs |> MacroOk
        | _ -> MacroError "printfMacro error"

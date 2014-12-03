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
module IntelliFactory.WebSharper.Macro

open System.Collections.Generic

module C = IntelliFactory.JavaScript.Core
module A = IntelliFactory.WebSharper.Core.Attributes
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

let (|CallOrCM|_|) q =
    match q with 
    | Q.Call (m, l)
    | Q.CallModule (m, l) -> Some (m, l)
    | _ -> None

let (|OptCoerce|) q =
    match q with
    | Q.Coerce (_, x)
    | x -> x

let cString s = !~ (C.String s)
let cCall t m x = C.Call (t, cString m, x)
let cCallG l m x = cCall (C.Global l) m x
let cInt x = !~ (C.Integer (int64 x))

let divisionMacro = macro <| fun tr q ->
    match q with
    | CallOrCM (m, [x; y]) ->
        match m.Generics with
        | t :: _ -> if isIn smallIntegralTypes t
                    then (tr x / tr y) &>> cInt 0
                    elif isIn bigIntegralTypes t
                    then cCallG ["Math"] "floor" [tr x / tr y]
                    else tr x / tr y
        | _      -> tr x / tr y
    | _ ->
        failwith "divisionMacro error"

let arithMacro name def = macro <| fun tr q ->
    match q with
    | CallOrCM (m, [x; y]) ->
        match m.Generics with
        | t :: _ ->
            if isIn scalarTypes t
                then def (tr x) (tr y)
                else cCall (tr x) name [tr y]
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
    let f m x y = cCallG ["IntelliFactory"; "WebSharper"; "Unchecked"] m [x; y]
    let c b i   = C.Binary (f "Compare" x y, b, cInt i)
    match cmp with
    | Comparison.``<``  -> c B.``===`` -1
    | Comparison.``<=`` -> c B.``<=`` 0
    | Comparison.``>``  -> c B.``===`` 1
    | Comparison.``>=`` -> c B.``>=`` 0
    | Comparison.``=``  -> f "Equals" x y
    | _                 -> !!(f "Equals" x y)

let comparisonMacro cmp = macro <| fun tr q ->
    match q with
    | CallOrCM (m, [x; y]) ->
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

let charProxy = ["IntelliFactory"; "WebSharper"; "Char"]

let charMacro = macro <| fun tr q ->
    match q with
    | CallOrCM (m, [x]) ->
        match m.Generics with
        | t :: _ ->
            if isIn integralTypes t then tr x else
                match t with
                | R.Type.Concrete (d, _) ->
                    match d.FullName with
                    | "System.String" -> cCallG charProxy "Parse" [tr x]
                    | "System.Char"
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
    | CallOrCM (m, [x]) ->
        match m.Generics with
        | t :: _ ->
            match t.FullName with
            | "System.Char" -> cCallG ["String"] "fromCharCode" [tr x]
            | _             -> cCallG [] "String" [tr x]
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
    | CallOrCM (_, [OptCoerce x]) ->
        match getFieldsList x with
        | Some xl ->
            C.NewObject (xl |> List.map (fun (n, v) -> n, tr v))
        | _ ->
            cCallG ["IntelliFactory"; "WebSharper"; "Pervasives"] "NewFromList" [tr x]
    | _ ->
        failwith "newMacro error"

[<Sealed>]
type New() =
    interface M.IMacroDefinition with
        member this.Macro = newMacro

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

type FST = Reflection.FSharpType

let flags =
    System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

let printfHelpers = ["IntelliFactory"; "WebSharper"; "PrintfHelpers"] 
let stringProxy = ["IntelliFactory"; "WebSharper"; "Strings"]

let createPrinter ts fs =
    let parts = FormatString.parseAll fs
    let args =
        match ts with 
        | Some ts -> 
            ts |> Seq.map (fun t -> C.Id(), Some t) |> List.ofSeq
        | _ ->
        [
            for p in parts do
                match p with
                | FormatString.FormatPart f ->
                    yield C.Id(), None
                    if f.IsStarWidth then yield C.Id(), None
                    if f.IsStarPrecision then yield C.Id(), None
                | _ -> () 
        ]
        
    let rArgs = ref args
    let nextVar() =
        match !rArgs with
        | (a, t) :: r ->
            rArgs := r
            C.Var a, t
        | _ -> failwith "sprintfMacro error"   
        
    let withPadding (f: FormatString.FormatSpecifier) t =
        if f.IsWidthSpecified then
            let width = if f.IsStarWidth then nextVar() |> fst else cInt f.Width
            let s = t (nextVar())
            if FormatString.isLeftJustify f.Flags then
                cCallG stringProxy "PadRight" [s; width]
            else
                if FormatString.isPadWithZeros f.Flags then
                    cCallG printfHelpers "padNumLeft" [s; width]
                else
                    cCallG stringProxy "PadLeft" [s; width]
        else t (nextVar())
        
    let numberToString (f: FormatString.FormatSpecifier) t =
        withPadding f (fun (n, _) ->
            if FormatString.isPlusForPositives f.Flags then cCallG printfHelpers "plusForPos" [n; t n]
            elif FormatString.isSpaceForPositives f.Flags then cCallG printfHelpers "spaceForPos" [n; t n]
            else t n
        )

    let prettyPrint t o = 
        let d = Dictionary<System.Type, C.Id * C.Expression ref>()
        let rec pp t (o: C.Expression) = 
            if FST.IsTuple t then
                seq {
                    yield cString "("
                    let ts = FST.GetTupleElements t
                    for i = 0 to ts.Length - 1 do 
                        yield pp ts.[i] o.[cInt i] 
                        if i < ts.Length - 1 then yield cString ", "
                    yield cString ")"
                }
                |> Seq.reduce (+)
            elif FST.IsRecord t then
                let pi = 
                    match d.TryGetValue t with
                    | false, _ ->
                        let pi = C.Id()
                        let pr = ref <| C.Runtime // placeholder
                        d.Add(t, (pi, pr))
                        pr := (
                            let x = C.Id()
                            C.Lambda(None, [x], 
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
                                        yield cString (f.Name + " = ") + pp f.PropertyType (C.Var x).[cString name]
                                        if i < fs.Length - 1 then yield cString "; "
                                    yield cString "}"
                                }
                                |> Seq.reduce (+)
                            ) 
                        )
                        pi
                    | true, (pi, _) -> pi
                (C.Var pi).[[o]]
            elif t.IsArray then
                let r = t.GetArrayRank()
                let a = t.GetElementType()
                let x = C.Id()
                match r with 
                | 1 -> cCallG printfHelpers "printArray" [ C.Lambda(None, [x], pp a (C.Var x)) ; o ]
                | 2 -> cCallG printfHelpers "printArray2D" [ C.Lambda(None, [x], pp a (C.Var x)) ; o ]
                | _ -> cCallG printfHelpers "prettyPrint" [o]
            else
            let tn =
                if t.IsGenericType 
                then Some (t.GetGenericTypeDefinition().FullName)
                else None
            if tn = Some "Microsoft.FSharp.Collections.FSharpList`1" then
                let a = t.GetGenericArguments().[0]
                let x = C.Id()
                cCallG printfHelpers "printList" [ C.Lambda(None, [x], pp a (C.Var x)) ; o ]    
            elif FST.IsUnion t then
                let pi =
                    match d.TryGetValue t with
                    | false, _ ->
                        let pi = C.Id()
                        let pr = ref <| C.Runtime // placeholder
                        d.Add(t, (pi, pr))
                        pr := (
                            let x = C.Id()
                            C.Lambda(None, [x], 
                                FST.GetUnionCases(t, flags) |> Seq.map (fun c ->
                                    let fs = c.GetFields()
                                    c.Tag,
                                    match fs.Length with
                                    | 0 -> cString c.Name
                                    | 1 -> 
                                        cString (c.Name + " ") + pp fs.[0].PropertyType (C.Var x).[cString "$0"]
                                    | _ -> 
                                        seq {
                                            yield cString (c.Name + " (")
                                            for i = 0 to fs.Length - 1 do
                                                yield pp fs.[i].PropertyType (C.Var x).[cString ("$" + string i)]
                                                if i < fs.Length - 1 then yield cString ", "
                                            yield cString ")"
                                        }
                                        |> Seq.reduce (+)
                                )
                                |> Seq.fold (fun s (tag, e) ->
                                    match s with
                                    | None -> Some e
                                    | Some s -> Some <| C.IfThenElse ((C.Var x).[cString "$"] &== cInt tag, e, s)
                                ) None |> Option.get
                            )
                        )
                        pi
                    | true, (pi, _) -> pi
                (C.Var pi).[[o]]
            else cCallG printfHelpers "prettyPrint" [o]
        let inner = pp t o
        if d.Count = 0 then inner else
        C.LetRecursive (d |> Seq.map (fun (KeyValue(_, (pi, pr))) -> pi, !pr) |> List.ofSeq, inner)

    let inner = 
        parts
        |> Seq.map (function
            | FormatString.StringPart s -> cString s
            | FormatString.FormatPart f ->
                match f.TypeChar with
                | 'b'
                | 'O' -> 
                    withPadding f (fun (o, _) -> cCallG [] "String" [o])
                | 'A' -> 
                    withPadding f (function 
                        | o, Some t -> 
                            prettyPrint t o
                        | o, _ -> cCallG printfHelpers "prettyPrint" [o]
                    )
                | 'c' -> 
                    withPadding f (fun (s, _) -> cCallG ["String"] "fromCharCode" [s])   
                | 's' -> 
                    withPadding f (fun (s, _) -> cCallG printfHelpers "toSafe" [s])
                | 'd' | 'i' ->
                    numberToString f (fun n -> cCallG [] "String" [n])
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
        |> Seq.reduce (+)
    
    let k = C.Id() 
    C.Lambda(None, [k],
        args |> List.rev |> List.fold (fun c (a, _) -> C.Lambda(None, [a], c)) (C.Var k).[[inner]]
    )
    
let printfMacro = macro <| fun tr q ->
    match q with
    | Q.NewObject (c, [Q.Value (Q.String fs)]) ->
        let rec getFunctionArgs t =
            if FST.IsFunction t then
                let x, y = FST.GetFunctionElements t
                x :: getFunctionArgs y
            else []
        let ts =
            try c.Generics.[0].Load() |> getFunctionArgs |> Some
            with _ -> None
        createPrinter ts fs
    | _ ->
        failwith "printfMacro error"

[<Sealed>]
type PrintF() =
    interface M.IMacroDefinition with
        member this.Macro = printfMacro

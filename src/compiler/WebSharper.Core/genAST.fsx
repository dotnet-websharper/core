// $begin{copyright}
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

type Case =
    | Tuple of list<Case>
    | List of Case
    | Option of Case
    | Expr
    | Statement
    | Id
    | Object of string
    | Empty
    static member (*) (a, b) =
        match a with
        | Tuple at -> Tuple (at @ [b])
        | _ -> Tuple [a; b]

let rec toType c =
    match c with
    | Tuple l ->
        l |> Seq.map toType |> String.concat " * "
    | List a ->
        "list<" + toType a + ">"
    | Option a -> 
        "option<" + toType a + ">"
    | Expr -> "Expression"
    | Statement -> "Statement"
    | Id -> "Id"
    | Object o -> o
    | Empty -> "unit"

let capitalize (s: string) = s.[0 .. 0].ToUpper() + s.[1 ..]

let toFields fs =
    match fs with
    | [] -> "unit"
    | _ -> fs |> Seq.map (fun (t, n) -> capitalize n + ":" + toType t) |> String.concat " * "

let rec info c =
    match c with
    | Tuple l ->
        match l |> List.choose info with
        | [] -> None
        | cl -> Some (Tuple cl)
    | List a ->
        info a |> Option.map List
    | Option a -> 
        info a |> Option.map Option
    | Object _ -> Some c
    | Expr -> None
    | Statement -> None
    | Id -> None
    | Empty -> None

let Literal = Object "Literal"
let NonGenericTypeDefinition = Object "TypeDefinition"
let TypeDefinition = Object "Concrete<TypeDefinition>"
let Constructor = Object "Constructor"
let NonGenericMethod = Object "Method"
let Method = Object "Concrete<Method>"
let Purity = Object "Purity" 
//let Field = Object "Field"
let Str = Object "string"
let Type = Object "Type"
let Int = Object "int"
let Bool = Object "bool"
let TSType = Object "TSType"
let VarKind = Object "VarKind"
let Modifiers = Object "Modifiers"
let ApplicationInfo = Object "ApplicationInfo"
let ClassMethodInfo = Object "ClassMethodInfo"
let MemberKind = Object "MemberKind"
let ClassPropertyInfo = Object "ClassPropertyInfo"

let ExprDefs = 
    [
        "Undefined", []
            , "JavaScript `undefined` value or `void` in .NET"
        //"This", []
        //    , "The `this` value of current JavaScript function scope"
        //"Arguments", []
        //    , "The `arguments` value of current JavaScript function scope"
        "Var", [ Id, "variable" ]
            , "Gets the value of a variable"
        "Value", [ Literal, "value" ]
            , "Contains a literal value"
        "Application", [ Expr, "func" ; List Expr, "arguments"; ApplicationInfo, "info" ]
            , "Function application with extra information. \
               The `Purity` field should be true only when the function called has no side effects, so the side effects of \
               the expression is the same as evaluating `func` then the expressions in the `arguments` list. \
               The `KnownLength` field should be `Some x` only when the function is known to have `x` number of arguments \
               and does not use the `this` value."
        "Function", [ List Id, "parameters"; Option Id, "thisVar"; Option Type, "return"; Statement, "body" ]
            , "Function declaration"
        "VarSet", [ Id, "variable"; Expr, "value" ]
            , "Variable set"
        "Sequential", [ List Expr, "expressions" ]
            , "Sequential evaluation of expressions, value is taken from the last"
        "NewTuple", [ List Expr, "items"; List Type, "tupleType" ]
            , "Creating a new array"
        "Conditional", [ Expr, "condition"; Expr, "whenTrue"; Expr, "whenFalse" ]  
            , "Conditional operation"
        "ItemGet", [ Expr, "object";  Expr, "item"; Purity, "pure" ]
            , "Indexer get without side effects"
        "ItemSet", [ Expr, "object"; Expr, "item"; Expr, "value" ]
            , "Indexer set"
        "Binary", [ Expr, "left"; Object "BinaryOperator", "operator"; Expr, "right" ]
            , "Binary operation"
        "MutatingBinary", [ Expr, "left"; Object "MutatingBinaryOperator", "operator"; Expr, "right" ]
            , "Binary operation mutating right side"
        "Unary", [ Object "UnaryOperator", "operator"; Expr, "expression" ]
            , "Unary operation"
        "MutatingUnary", [ Object "MutatingUnaryOperator", "operator"; Expr, "expression" ]
            , "Unary operation mutating value"
        "ExprSourcePos", [ Object "SourcePos", "range"; Expr, "expression" ]
            , "Original source location for an expression"
        //"FuncWithThis", [ Id, "thisParam"; List Id, "parameters"; Option Type, "return"; Statement, "body" ]
        //    , "Temporary - Method of F# object expressions"
        "JSThis", []
            , "JavaScript - the this value"
        "Base", []
            , "Refers to the base class from an instance method, `super` in JavaScript"
        "Call", [ Option Expr, "thisObject"; TypeDefinition, "typeDefinition"; Method, "method"; List Expr, "arguments" ]
            , ".NET - Method call"
        "CallNeedingMoreArgs", [ Option Expr, "thisObject"; TypeDefinition, "typeDefinition"; Method, "method"; List Expr, "arguments" ]
            , "Temporary - Partial application, workaround for FCS issue #414"
        "CurriedApplication", [ Expr, "func"; List (Bool * Expr), "arguments" ]
            , "Temporary - F# function application, bool indicates if the argument has type unit"
        "OptimizedFSharpArg", [ Expr, "funcVar"; Object "FuncArgOptimization", "opt" ]
            , "Temporary - optimized curried or tupled F# function argument"
        "Ctor", [ TypeDefinition, "typeDefinition"; Constructor, "ctor"; List Expr, "arguments" ] 
            , ".NET - Constructor call"
        "ChainedCtor", [ Bool, "isBase"; TypeDefinition, "typeDefinition"; Constructor, "ctor"; List Expr, "arguments" ]
            , ".NET - Chained or base constructor call"
        "CopyCtor", [ NonGenericTypeDefinition, "typeDefinition"; Expr, "object" ]
            , ".NET - Creating an object from a plain object"
        "FieldGet", [ Option Expr, "thisObject"; TypeDefinition, "typeDefinition"; Str, "field" ]
            , ".NET - Field getter"
        "FieldSet", [ Option Expr, "thisObject"; TypeDefinition, "typeDefinition"; Str, "field"; Expr, "value" ]
            , ".NET - Field setter"
        "Let", [ Id, "identifier"; Expr, "value"; Expr, "body" ]
            , ".NET - An immutable value definition used only in expression body"
        "NewVar", [ Id, "variable"; Expr, "value" ]
            , ".NET - An expression-level variable declaration"
        "Coalesce", [ Expr, "expression"; Type, "type"; Expr, "whenNull" ]
            , ".NET - Null-coalescing"
        "TypeCheck", [ Expr, "expression"; Type, "type" ]
            , ".NET - Type check, returns bool"
        "Coerce", [ Expr, "expression"; Type, "fromType"; Type, "toType" ]
            , ".NET - Type coercion"
        "NewDelegate", [ Option Expr, "thisObject"; TypeDefinition, "typeDefinition"; Method, "method" ]
            , ".NET - Creates a new delegate"
        "StatementExpr", [ Statement, "statement"; Option Id, "result" ]
            , ".NET - Statement inside an expression. Result can be an identifier for a variable which is not explicitly defined inside the statement"
        "LetRec", [ List (Id * Expr), "bindings"; Expr, "body" ]
            , ".NET - F# let rec"
        "NewRecord", [ TypeDefinition, "typeDefinition"; List Expr, "fields" ]
            , ".NET - F# record constructor"
        "NewUnionCase", [ TypeDefinition, "typeDefinition"; Str, "unionCase"; List Expr, "fields" ]
            , ".NET - F# union case constructor"
        "UnionCaseTest", [ Expr, "expression"; TypeDefinition, "typeDefinition"; Str, "unionCase" ]
            , ".NET - F# union case test"
        "UnionCaseGet", [ Expr, "expression"; TypeDefinition, "typeDefinition"; Str, "unionCase"; Str, "field" ]
            , ".NET - F# union case field getter"
        "UnionCaseTag", [ Expr, "expression"; TypeDefinition, "typeDefinition" ]
            , ".NET - F# union case tag getter"
        "MatchSuccess", [ Int, "index"; List Expr, "captures" ]
            , ".NET - F# successful match" 
        "TraitCall", [ Option Expr, "thisObject"; List Type, "objectType"; Method, "method"; List Expr, "arguments" ]
            , ".NET - Method call"
        "Await", [ Expr, "expression" ]
            , "Temporary - C# await expression"
        "NamedParameter", [ Int, "ordinal"; Expr, "expression" ]
            , "Temporary - C# named parameter"
        "RefOrOutParameter", [ Expr, "expression" ]
            , "Temporary - C# ref or out parameter"
        "ComplexElement", [ List Expr, "items" ]
            , "Temporary - C# complex element in initializer expression"
        "Object", [ List (Str * MemberKind * Expr), "properties" ]
            , "JavaSript object"
        "GlobalAccess", [ Object "Address", "address" ]
            , "A global or imported value"
        "GlobalAccessSet", [ Object "Address", "address"; Expr, "value" ]
            , "A global or imported value setter"
        "New", [ Expr, "func"; List TSType, "param"; List Expr, "arguments" ]
            , "JavaScript 'new' call"
        "Hole", [ Int, "index" ]
            , "Temporary - A hole in an expression for inlining"
        "Cast", [ TSType, "targetType"; Expr, "expression" ]
            , "TypeScript - type cast <...>..."
        "ClassExpr", [ Option Id, "classId"; Option Expr, "baseClass"; List Statement, "members" ]
            , "JavaScript - class { ... }"
        "ObjectExpr", [ Type, "objectType"; Option Expr, "constructor"; List (Tuple [NonGenericTypeDefinition; NonGenericMethod; Expr]), "overrides" ]
            , ".NET - F# object expression"
        "Verbatim", [ List Str, "stringParts"; List Expr, "holes" ]
            , "JavaScript verbatim code"
    ]    

let StatementDefs =
    [
        "Empty", []
            , "Empty statement"
        "Break", [ Option Id, "label" ]
            , "JavaScript break statement"
        "Continue", [ Option Id, "label" ]
            , "JavaScript continue statement"
        "ExprStatement", [ Expr, "expression" ]
            , "Expression as statement"
        "Return", [ Expr, "value" ]
            , "Return a value"
        "Block", [ List Statement, "statements" ]
            , "Block of statements"
        "VarDeclaration", [ Id, "variable"; Expr, "value" ]
            , "Variable declaration"
        "FuncDeclaration", [ Id, "funcId"; List Id, "parameters"; Option Id, "thisVar"; Statement, "body"; List TSType, "generics" ]
            , "Function declaration"
        "While", [ Expr, "condition"; Statement, "body" ]
            , "'while' loop"
        "DoWhile", [ Statement, "body"; Expr, "condition" ]
            , "'do..while' loop"
        "For", [ Option Expr, "initializer"; Option Expr, "condition"; Option Expr, "step"; Statement, "body" ]
            , "'for' loop"
        "ForIn", [ Id, "variable"; Expr, "object"; Statement, "body" ]
            , "JavaScript 'for .. in' loop"
        "Switch", [ Expr, "expression"; List (Option Expr * Statement), "cases" ]
            , "JavaScript 'switch' expression"
        "If", [ Expr, "condition"; Statement, "thenStatement"; Statement, "elseStatement" ]
            , "'if' statement"
        "Throw", [ Expr, "expression" ]
            , "'throw' statement"
        "TryWith", [ Statement, "body"; Option Id, "variable"; Statement, "catchStatement" ]
            , "'try..with' statement"
        "TryFinally", [ Statement, "body"; Statement, "finallyStatement" ]
            , "'try..finally' statement"
        "Labeled", [ Id, "label"; Statement, "statement" ]
            , "Statement with a label"
        "StatementSourcePos", [ Object "SourcePos", "range"; Statement, "statement" ]
            , "Original source location for a statement"

        // C#
        "Goto", [ Id, "label" ]
            , "Temporary - C# 'goto' statement"
        "Continuation", [ Id, "label"; Expr, "expression" ]
            , "Temporary - go to next state in state-machine for iterators, async methods, or methods containing gotos"
        "Yield", [ Option Expr, "value" ]
            , "Temporary - C# 'yield return' statement"
        "CSharpSwitch", [ Expr, "expression"; List (List (Option Expr) * Statement), "cases" ]
            , "Temporary - C# 'switch' statement"
        "GotoCase", [ Option Expr, "caseExpression" ]
            , "Temporary - C# 'goto case' statement"

        // F#
        "DoNotReturn", []
            , ".NET - F# tail call position"

        // JavaScript/TypeScript
        "Import", [ Option Id, "defaultImport"; Option Id, "fullImport"; List (Str * Id), "namedImports" ; Str, "moduleName" ]
            , "JavaScript - import * as ... from ..."
        "ExportDecl", [ Bool, "isDefault"; Statement, "statement" ]
            , "JavaScript - export"
        "Declare", [ Statement, "statement" ]
            , "TypeScript - declare ..."
        "Class", [ Id, "classId"; Option Expr, "baseClass"; List TSType, "implementations"; List Statement, "members"; List TSType, "generics" ]
            , "JavaScript - class { ... }"
        "ClassMethod", [ ClassMethodInfo, "info"; Str, "name"; List Id, "parameters"; Option Id, "thisVar"; Option Statement, "body"; TSType, "signature" ]
            , "JavaScript - class method"
        "ClassConstructor", [ List (Tuple [Id; Modifiers]), "parameters"; Option Id, "thisVar"; Option Statement, "body"; TSType, "signature" ]
            , "JavaScript - class method"
        "ClassProperty", [ ClassPropertyInfo, "info"; Str, "name"; TSType, "propertyType"; Option Expr, "value" ]
            , "JavaScript - class plain property"
        "ClassStatic", [ Statement, "optional" ]
            , "JavaScript - class static block"
        "Interface", [ Id, "intfId"; List TSType, "extending"; List Statement, "members"; List TSType, "generics" ]
            , "TypeScript - interface { ... }"
        "Alias", [ Id, "alias"; List TSType, "generics"; TSType, "origType" ]
            , "TypeScript - type or import alias"
        "XmlComment", [ Str, "xml" ]
            , "TypeScript - triple-slash directive"
    ]

let binaryOps =
    [
        "!=="
        "!="     
        "%"
        "&&"
        "&"  
        "*"
        "+"    
        "-"     
        "/" 
        "<<"
        "<="
        "<"
        "==="   
        "=="
//        "="
        ">="
        ">>>" 
        ">>"
        ">"    
        "^"  
        "|"
        "||"     
    ]

let NL = System.Environment.NewLine

let letters = [| "a"; "b"; "c"; "d"; "e"; "f" |]

let code = 
    let code = ResizeArray()
    let inline cprintfn x = Printf.kprintf code.Add x 

    for t, tl in [ "and Expression =", ExprDefs; "and Statement =", StatementDefs ] do
        cprintfn "%s" t
        for n, c, comm in tl do
            let args =
                match c with
                | [] -> ""
                | _ -> " of " + toFields c
            cprintfn "    /// %s" comm
            cprintfn "    | %s%s" n args
        if t.Contains "Expression" then
            cprintfn "    with"
            for opSym in binaryOps do
                cprintfn "    static member (^%s) (a, b) = Binary (a, BinaryOperator.``%s``, b)" opSym opSym
            cprintfn "    [<System.Obsolete>] member a.Item b = ItemGet (a, b, Pure)"
            cprintfn "    [<System.Obsolete>] member a.Item b = Application (a, b, ApplicationInfo.None)"

    let ExprAndStatementDefs =
        seq {
            for n, c, comm in ExprDefs -> "Expression", n, c, comm
            for n, c, comm in StatementDefs -> "Statement", n, c, comm
        }
    
    // Transformer

    cprintfn "/// Base class for code transformers."
    cprintfn "/// Provides virtual methods for transforming each AST case separately."
    cprintfn "type Transformer() ="
    for t, n, c, comm in ExprAndStatementDefs do
        cprintfn "    /// %s" comm
        cprintfn "    abstract Transform%s : %s -> %s" n (toFields c) t
        match n with
        | "ExprSourcePos" ->
            cprintfn "    override this.TransformExprSourcePos (a, b) ="
            cprintfn "        match this.TransformExpression b with"
            cprintfn "        | ExprSourcePos (_, bt) | bt -> ExprSourcePos (a, bt)"    
        | "StatementSourcePos" ->
            cprintfn "    override this.TransformStatementSourcePos (a, b) ="
            cprintfn "        match this.TransformStatement b with"
            cprintfn "        | StatementSourcePos (_, bt) | bt -> StatementSourcePos (a, bt)"    
        | _ ->
        let args =
            match c with
            | [] -> "()"
            | [_] -> "a"
            | _ ->
                "(" + String.concat ", " (Seq.take c.Length letters) + ")"
        let rec tr c x =
            match c with
            | List Expr -> "List.map this.TransformExpression " + x
            | Option Expr -> "Option.map this.TransformExpression " + x
            | Option Statement -> "Option.map this.TransformStatement " + x
            | Expr -> "this.TransformExpression " + x 
            | Statement -> "this.TransformStatement " + x
            | Id -> "this.TransformId " + x
            | Option Id -> "Option.map this.TransformId " + x
            | List Id -> "List.map this.TransformId " + x
            | List (Tuple [Id; Expr]) -> "List.map (fun (a, b) -> this.TransformId a, this.TransformExpression b) " + x 
            | List (Tuple [Object _; Id]) -> "List.map (fun (a, b) -> a, this.TransformId b) " + x 
            | List Statement -> "List.map this.TransformStatement " + x
            | List (Tuple [Object _; Expr]) -> "List.map (fun (a, b) -> a, this.TransformExpression b) " + x
            | List (Tuple [Object _; Object _; Expr]) -> "List.map (fun (a, b, c) -> a, b, this.TransformExpression c) " + x
            | List (Tuple [Option Expr; Statement]) -> "List.map (fun (a, b) -> Option.map this.TransformExpression a, this.TransformStatement b) " + x 
            | List (Tuple [Option Expr; Expr]) -> "List.map (fun (a, b) -> Option.map this.TransformExpression a, this.TransformExpression b) " + x
            | List (Tuple [List (Option Expr); Statement]) -> "List.map (fun (a, b) -> List.map (Option.map this.TransformExpression) a, this.TransformStatement b) " + x
            | List (Tuple [Id; Object _]) -> "List.map (fun (a, b) -> this.TransformId a, b) " + x
            | Object _ -> x
            | List (Object _) -> x
            | Option (Object _) -> x
            | Empty -> ""
            | _ -> failwithf "no transformer defined for %A" c
        let trArgs = 
            match c with
            | [] -> ""
            | [c, _] -> "(" + tr c "a" + ")"
            | _ ->
                "(" + String.concat ", " (c |> Seq.mapi (fun j (a, _) -> tr a (letters.[j]))) + ")"  
        cprintfn "    override this.Transform%s %s = %s %s" n args n trArgs

    for t, tl in [ "Expression", ExprDefs; "Statement", StatementDefs ] do
        cprintfn "    abstract Transform%s : %s -> %s" t t t
        cprintfn "    override this.Transform%s x =" t
        cprintfn "        match x with"
        for n, c, _ in tl do
            let args =
                match c with
                | [] -> ""
                | [_] -> "a"
                | _ ->
                    "(" + String.concat ", " (Seq.take c.Length letters) + ")"
            let trArgs =
                match c with
                | [] -> "()"
                | [_] -> "a"
                | _ ->
                    "(" + String.concat ", " (Seq.take c.Length letters) + ")"
            cprintfn "        | %s %s -> this.Transform%s %s" n args n trArgs

    cprintfn "    /// Identifier for variable or label"    
    cprintfn "    abstract TransformId : Id -> Id"
    cprintfn "    override this.TransformId x = x"

    // Visitor

    cprintfn "/// Base class for code visitors."
    cprintfn "/// Provides virtual methods for visiting each AST case separately."
    cprintfn "type Visitor() ="
    for t, n, c, comm in ExprAndStatementDefs do
        cprintfn "    /// %s" comm
        cprintfn "    abstract Visit%s : %s -> unit" n (toFields c)
        let args =
            match c with
            | [] -> "()"
            | [_] -> "a"
            | _ ->
                "(" + String.concat ", " (Seq.take c.Length letters) + ")"
        let rec tr c x =
            match c with
            | List Expr -> "List.iter this.VisitExpression " + x
            | Option Expr -> "Option.iter this.VisitExpression " + x
            | Option Statement -> "Option.iter this.VisitStatement " + x
            | Expr -> "this.VisitExpression " + x 
            | Statement -> "this.VisitStatement " + x
            | Id -> "this.VisitId " + x
            | Option Id -> "Option.iter this.VisitId " + x
            | List Id -> "List.iter this.VisitId " + x
            | List (Tuple [Id; Expr]) -> "List.iter (fun (a, b) -> this.VisitId a; this.VisitExpression b) " + x 
            | List (Tuple [Object _; Id]) -> "List.iter (fun (a, b) -> this.VisitId b) " + x 
            | List Statement -> "List.iter this.VisitStatement " + x
            | List (Tuple [Object _; Expr]) -> "List.iter (fun (a, b) -> this.VisitExpression b) " + x
            | List (Tuple [Object _; Object _; Expr]) -> "List.iter (fun (a, b, c) -> this.VisitExpression c) " + x
            | List (Tuple [Option Expr; Statement]) -> "List.iter (fun (a, b) -> Option.iter this.VisitExpression a; this.VisitStatement b) " + x 
            | List (Tuple [Option Expr; Expr]) -> "List.iter (fun (a, b) -> Option.iter this.VisitExpression a; this.VisitExpression b) " + x 
            | List (Tuple [List (Option Expr); Statement]) -> "List.iter (fun (a, b) -> List.iter (Option.iter this.VisitExpression) a; this.VisitStatement b) " + x
            | List (Tuple [Id; Object _]) -> "List.iter (fst >> this.VisitId) " + x
            | Object _ -> "()"
            | List (Object _) -> "()"
            | Option (Object _) -> "()"
            | Empty -> ""
            | _ -> failwithf "no visitor defined for %A" c
        let trArgs = 
            match c with
            | [] -> "()"
            | [c, _] -> "(" + tr c "a" + ")"
            | _ ->
                String.concat "; " (c |> Seq.mapi (fun j (a, _) -> tr a (letters.[j])))
        cprintfn "    override this.Visit%s %s = %s" n args trArgs

    for t, tl in [ "Expression", ExprDefs; "Statement", StatementDefs ] do
        cprintfn "    abstract Visit%s : %s -> unit" t t
        cprintfn "    override this.Visit%s x =" t
        cprintfn "        match x with"
        for n, c, _ in tl do
            let args =
                match c with
                | [] -> ""
                | [_] -> "a"
                | _ ->
                    "(" + String.concat ", " (Seq.take c.Length letters) + ")"
            let trArgs =
                match c with
                | [] -> "()"
                | [_] -> "a"
                | _ ->
                    "(" + String.concat ", " (Seq.take c.Length letters) + ")"
            cprintfn "        | %s %s -> this.Visit%s %s" n args n trArgs

    cprintfn "    /// Identifier for variable or label"    
    cprintfn "    abstract VisitId : Id -> unit"
    cprintfn "    override this.VisitId x = ()"
    
    cprintfn "module IgnoreSourcePos ="

    for t, tl in [ "Expr", ExprDefs; "Statement", StatementDefs ] do
        cprintfn "    let ignore%sSourcePos expr =" t
        cprintfn "        match expr with"
        cprintfn "        | %sSourcePos (_, e) -> e" t
        cprintfn "        | _ -> expr"
        for n, c, _ in tl do
            let args =
                match c with
                | [] -> ""
                | [_] -> "a"
                | _ ->
                    "(" + String.concat ", " (Seq.take c.Length letters) + ")"
            let trArgs =
                match c with
                | [] -> "()"
                | [_] -> "a"
                | _ ->
                    "(" + String.concat ", " (Seq.take c.Length letters) + ")"
            cprintfn "    let (|%s|_|) x = match ignore%sSourcePos x with %s %s -> Some %s | _ -> None" n t n args trArgs

    cprintfn "module Debug =" 
    cprintfn "    let private PrintTypeDefinition (x:Concrete<TypeDefinition>) = x.Entity.Value.FullName + match x.Generics with [] -> \"\" | g -> (g |> List.map string |> String.concat \", \")" 
    cprintfn "    let private PrintMethod (x:Concrete<Method>) = x.Entity.Value.MethodName + match x.Generics with [] -> \"\" | g -> (g |> List.map string |> String.concat \", \")" 
    for isExrps, tl in [ true, ExprDefs; false, StatementDefs ] do
        if isExrps then 
            cprintfn "    let rec PrintExpression x =" 
        else 
            cprintfn "    and PrintStatement x =" 
        cprintfn "        match x with"
        for n, c, _ in tl do
            let args =
                match c with
                | [] -> ""
                | [_] -> "a"
                | _ ->
                    "(" + String.concat ", " (Seq.take c.Length letters) + ")"
            let rec tr c x =
                match c with
                | List Expr -> "\"[\" + String.concat \"; \" (List.map PrintExpression " + x + ") + \"]\""
                | Option Expr -> "defaultArg (Option.map PrintExpression " + x + ") \"_\""
                | Option Statement -> "defaultArg (Option.map PrintStatement " + x + ") \"\""
                | Expr -> "PrintExpression " + x 
                | Statement -> "PrintStatement " + x
                | Id -> "string " + x
                | Option Id -> "defaultArg (Option.map string " + x + ") \"_\""
                | List Id -> "\"[\" + String.concat \"; \" (List.map string " + x + ") + \"]\""
                | List (Tuple [Id; Expr]) -> "\"[\" + String.concat \"; \" (List.map (fun (a, b) -> string a + \", \" + PrintExpression b) " + x + ") + \"]\"" 
                | List (Tuple [Object _; Id]) -> "\"[\" + String.concat \"; \" (List.map (fun (a, b) -> string a + \", \" + string b) " + x + ") + \"]\"" 
                | List Statement -> "\"[\" + String.concat \"; \" (List.map PrintStatement " + x + ") + \"]\""
                | List (Tuple [Object _; Expr]) -> "\"[\" + String.concat \"; \" (List.map (fun (a, b) -> string a + \", \" + PrintExpression b) " + x + ") + \"]\""
                | List (Tuple [Object _; Object _; Expr]) -> "\"[\" + String.concat \"; \" (List.map (fun (a, b, c) -> string a + \", \" + string b + \", \" + PrintExpression c) " + x + ") + \"]\""
                | List (Tuple [Option Expr; Statement]) -> "\"[\" + String.concat \"; \" (List.map (fun (a, b) -> defaultArg (Option.map PrintExpression a) \"_\" + \", \" + PrintStatement b) " + x + ") + \"]\"" 
                | List (Tuple [Option Expr; Expr]) -> "\"[\" + String.concat \"; \" (List.map (fun (a, b) -> defaultArg (Option.map PrintExpression a) \"_\" + \", \" + PrintExpression b) " + x + ") + \"]\"" 
                | List (Tuple [List (Option Expr); Statement]) -> "\"[\" + String.concat \"; \" (List.map (fun (a, b) -> \"[\" + String.concat \"; \" (List.map (fun aa -> defaultArg (Option.map PrintExpression aa) \"_\") a) + \"], \" + PrintStatement b) " + x + ") + \"]\""
                | List (Tuple [Id; Object _]) -> "\"[\" + String.concat \"; \" (" + x + " |> List.map (fun (i, m) -> i.ToString m)) + \"]\""
                | Object "TypeDefinition" -> x + ".Value.FullName"
                | Object "Concrete<TypeDefinition>" -> "PrintTypeDefinition " + x
                | Object "Concrete<Method>" -> "PrintMethod " + x
                | Object "Constructor" -> "\".ctor\""
                | Object "Literal" -> "string " + x + ".Value"
                | Object _ -> "string " + x
                | List (Object _) -> "\"[\" + String.concat \"; \" (List.map string " + x + ") + \"]\""
                | Option (Object _) -> "defaultArg (Option.map string " + x + ") \"_\""
                | Empty -> ""
                | _ -> failwithf "no debug printer defined for %A" c
            match c with
            | [ Object "SourcePos", _; Expr, _ ] ->
                cprintfn "        | %s (_, b) -> PrintExpression b" n
            | [ Object "SourcePos", _; Statement, _ ] ->
                cprintfn "        | %s (_, b) -> PrintStatement b" n
            | _ ->
            let trArgs = 
                match c with
                | [] -> "\"\""
                | [c, _] -> "\"(\" + " + tr c "a" + " + \")\""
                | _ ->
                    "\"(\" + " + String.concat " + \", \" + " (c |> Seq.mapi (fun j (a, _) -> tr a (letters.[j]))) + " + \")\""
            cprintfn "        | %s %s -> \"%s\" + %s" n args n trArgs 
    code.ToArray()

let allCode = 
    [|
        let mutable incl = true
        for l in System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\AST.fs") do
            if incl then yield l
            if l.Contains "// {{"
            then 
                incl <- false 
                yield! code
            elif l.Contains "// }}"
            then
                incl <- true
                yield l
    |]

System.IO.File.WriteAllLines(__SOURCE_DIRECTORY__ + @"\AST.fs", allCode)
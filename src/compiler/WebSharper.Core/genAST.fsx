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

let rec shape c =
    match c with
    | Tuple l ->
        match l |> List.choose shape with
        | [] -> None
        | cl -> Some (Tuple cl)
    | List a ->
        shape a |> Option.map List
    | Id -> Some Id 
    | Option a -> 
        shape a |> Option.map Option
    | Expr -> Some Expr
    | Statement -> Some Statement
    | Object _ -> None
    | Empty -> None

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
//let Field = Object "Field"
let Str = Object "string"
let Type = Object "Type"

let ExprDefs = 
    [
        "Undefined", Empty
        "This", Empty
        "Var", Id
        "Value", Literal
        "Application", Expr * List Expr
        "Function", List Id * Statement
        "VarSet", Id * Expr
        "Sequential", List Expr
        "NewArray", List Expr
        "Conditional", Expr * Expr * Expr  
        "ItemGet", Expr * Expr
        "ItemSet", Expr * Expr * Expr
        "Binary", Expr * Object "BinaryOperator" * Expr
        "MutatingBinary", Expr * Object "MutatingBinaryOperator" * Expr
        "Unary", Object "UnaryOperator" * Expr
        "MutatingUnary", Object "MutatingUnaryOperator" * Expr
        "ExprSourcePos", Object "SourcePos" * Expr
        
        // .NET
        "FuncWithThis", Id * List Id * Statement
        "Self", Empty
        "Base", Empty
        "Call", Option Expr * TypeDefinition * Method * List Expr
        "CallNeedingMoreArgs", Option Expr * TypeDefinition * Method * List Expr
//        "CallInterface", Expr * TypeDefinition * Method * List Expr
        "Ctor", TypeDefinition * Constructor * List Expr
        "BaseCtor", Expr * TypeDefinition * Constructor * List Expr
        "CopyCtor", NonGenericTypeDefinition * Expr
        "Cctor", NonGenericTypeDefinition
//        "FieldGet", Expr * TypeDefinition * Field
//        "FieldSet", Expr * TypeDefinition * Field * Expr
        "FieldGet", Option Expr * TypeDefinition * Str
        "FieldSet", Option Expr * TypeDefinition * Str * Expr
        "Let", Id * Expr * Expr
        "NewVar", Id * Expr
        "Coalesce", Expr * Type * Expr
        "TypeCheck", Expr * Type
//        "MacroFallback", Empty
        "WithVars", List Id * Expr
        "MethodName", NonGenericTypeDefinition * NonGenericMethod
        "OverrideName", NonGenericTypeDefinition * NonGenericMethod
        "NewDelegate", Option Expr * TypeDefinition * Method

        // F#
        "LetRec", List (Id * Expr) * Expr
        "StatementExpr", Statement
        "NewRecord", TypeDefinition * List Expr
        "NewUnionCase", TypeDefinition * Str * List Expr
//        "UnionCaseTest", Expr * TypeDefinition * Str
        "UnionCaseGet", Expr * TypeDefinition * Str * Str

        // C#
        "Await", Expr
        "NamedParameter", Id * Expr
        "RefOrOutParameter", Expr

        // JavaScript
        "Object", List (Object "string" * Expr)
        "GlobalAccess", Object "Address"
        "New", Expr * List Expr
        "Hole", Object "int"
    ]    

let StatementDefs =
    [
        "Empty", Empty
        "Break", Option Id
        "Continue", Option Id
        "ExprStatement", Expr
        "Return", Expr
        "Block", List Statement
        "VarDeclaration", Id * Expr
        "While", Expr * Statement
        "DoWhile", Statement * Expr
        "For", Option Expr * Option Expr * Option Expr * Statement
        "ForIn", Id * Expr * Statement
        "Switch", Expr * List (Option Expr * Statement)
        "If", Expr * Statement * Statement
        "Throw", Expr
        "TryWith", Statement * Option Id * Statement
        "TryFinally", Statement * Statement
        "Labeled", Id * Statement
        "StatementSourcePos", Object "SourcePos" * Statement

        // C#
        "Goto", Id
        "Continuation", Id * Expr
        "Yield", Option Expr
        "CSharpSwitch", Expr * List (List (Option Expr) * Statement)
        "GotoCase", Option Expr
        "Statements", List Statement
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
        "="
        ">="
        ">>>" 
        ">>"
        ">"    
        "^"  
        "|"
        "||"     
    ]

let NL = System.Environment.NewLine

let letters = [| "a"; "b"; "c"; "d" |]

let code = 
    let code = new System.Text.StringBuilder()
    let inline cprintf x = Printf.bprintf code x 
    let inline cprintfn x = Printf.kbprintf (code.AppendLine >> ignore) code x 

    cprintfn """namespace WebSharper.Core.AST
type Literal =
    | Null
    | Bool   of bool
    | Byte   of byte
    | Char   of char
    | Double of double
    | Int    of int
    | Int16  of int16
    | Int64  of int64
    | SByte  of sbyte
    | Single of single
    | String of string
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64
    with
    static member (!~) a = Value a
"""

    for t, tl in [ "and Expression =", ExprDefs; "and Statement =", StatementDefs ] do
        cprintfn "%s" t
        for n, c in tl do
            let args =
                match c with
                | Empty -> ""
                | _ -> " of " + toType c
            cprintfn "    | %s%s" n args
        if t.Contains "Expression" then
            cprintfn "    with"
            for opSym in binaryOps do
                cprintfn "    static member (^%s) (a, b) = Binary (a, BinaryOperator.``%s``, b)" opSym opSym
            cprintfn "    member a.Item b = ItemGet (a, b)"
            cprintfn "    member a.Item b = Application (a, b)"

    let ExprAndStatementDefs =
        seq {
            for n, c in ExprDefs -> "Expression", n, c
            for n, c in StatementDefs -> "Statement", n, c
        }
    
    // Transformer

    cprintfn "type Transformer() ="
    for t, n, c in ExprAndStatementDefs do
        cprintfn "    abstract Transform%s : %s -> %s" n (toType c) t
        let args =
            match c with
            | Tuple t ->
                "(" + String.concat ", " (Seq.take t.Length letters) + ")"
            | Empty -> "()"
            | _ -> "a"
        let rec tr c x =
            match c with
            | List Expr -> "List.map this.TransformExpression " + x
            | Option Expr -> "Option.map this.TransformExpression " + x
            | Expr -> "this.TransformExpression " + x 
            | Statement -> "this.TransformStatement " + x
            | Id -> "this.TransformId " + x
            | Option Id -> "Option.map this.TransformId " + x
            | List Id -> "List.map this.TransformId " + x
            | List (Tuple [Id; Expr]) -> "List.map (fun (a, b) -> this.TransformId a, this.TransformExpression b) " + x 
            | List Statement -> "List.map this.TransformStatement " + x
            | List (Tuple [Object _; Expr]) -> "List.map (fun (a, b) -> a, this.TransformExpression b) " + x
            | List (Tuple [Option Expr; Statement]) -> "List.map (fun (a, b) -> Option.map this.TransformExpression a, this.TransformStatement b) " + x 
            | List (Tuple [List (Option Expr); Statement]) -> "List.map (fun (a, b) -> List.map (Option.map this.TransformExpression) a, this.TransformStatement b) " + x
            | Object _ -> x
            | List (Object _) -> x
            | Option (Object _) -> x
            | Empty -> ""
            | _ -> " failwith \"no transform\""
        let trArgs = 
            match c with
            | Tuple t ->
                "(" + String.concat ", " (t |> Seq.mapi (fun j a -> tr a (letters.[j]))) + ")"  
            | Empty -> ""
            | _ -> "(" + tr c "a" + ")"
        cprintfn "    override this.Transform%s %s = %s %s" n args n trArgs

    for t, tl in [ "Expression", ExprDefs; "Statement", StatementDefs ] do
        cprintfn "    abstract Transform%s : %s -> %s" t t t
        cprintfn "    override this.Transform%s x =" t
        cprintfn "        match x with"
        for n, c in tl do
            let args =
                match c with
                | Tuple t ->
                    "(" + String.concat ", " (Seq.take t.Length letters) + ")"
                | Empty -> ""
                | _ -> "a"
            let trArgs =
                match c with
                | Tuple t ->
                    "(" + String.concat ", " (Seq.take t.Length letters) + ")"
                | Empty -> "()"
                | _ -> "a"
            cprintfn "        | %s %s -> this.Transform%s %s" n args n trArgs

    cprintfn "    abstract TransformId : Id -> Id"
    cprintfn "    override this.TransformId x = x"

    // StatementTransformer

//    cprintfn "type StatementTransformer() ="
//    for n, c in StatementDefs do
//        cprintfn "    abstract Transform%s : %s -> Statement" n (toType c)
//        let args =
//            match c with
//            | Tuple t ->
//                "(" + String.concat ", " (Seq.take t.Length letters) + ")"
//            | Empty -> "()"
//            | _ -> "a"
//        let rec tr c x =
//            match c with
//            | List Expr -> x
//            | Option Expr -> x
//            | Expr -> x
//            | Statement -> "this.TransformStatement " + x
//            | Id -> x
//            | Option Id -> x
//            | List Id -> x
//            | List (Tuple [Id; Expr]) -> x
//            | List Statement -> "List.map this.TransformStatement " + x
//            | List (Tuple [Object _; Expr]) -> x
//            | List (Tuple [(Option Expr | List (Option Expr)); Statement]) -> "List.map (fun (a, b) -> a, this.TransformStatement b) " + x 
//            | Object _ -> x
//            | List (Object _) -> x
//            | Option (Object _) -> x
//            | Empty -> ""
//            | _ -> " failwith \"no transform\""
//        let trArgs = 
//            match c with
//            | Tuple t ->
//                "(" + String.concat ", " (t |> Seq.mapi (fun j a -> tr a (letters.[j]))) + ")"  
//            | Empty -> ""
//            | _ -> "(" + tr c "a" + ")"
//        cprintfn "    override this.Transform%s %s = %s %s" n args n trArgs
//
//    cprintfn "    abstract TransformStatement : Statement -> Statement"
//    cprintfn "    override this.TransformStatement x ="
//    cprintfn "        match x with"
//    for n, c in StatementDefs do
//        let args =
//            match c with
//            | Tuple t ->
//                "(" + String.concat ", " (Seq.take t.Length letters) + ")"
//            | Empty -> ""
//            | _ -> "a"
//        let trArgs =
//            match c with
//            | Tuple t ->
//                "(" + String.concat ", " (Seq.take t.Length letters) + ")"
//            | Empty -> "()"
//            | _ -> "a"
//        cprintfn "        | %s %s -> this.Transform%s %s" n args n trArgs

    // Visitor

    cprintfn "type Visitor() ="
    for t, n, c in ExprAndStatementDefs do
        cprintfn "    abstract Visit%s : %s -> unit" n (toType c)
        let args =
            match c with
            | Tuple t ->
                "(" + String.concat ", " (Seq.take t.Length letters) + ")"
            | Empty -> "()"
            | _ -> "a"
        let rec tr c x =
            match c with
            | List Expr -> "List.iter this.VisitExpression " + x
            | Option Expr -> "Option.iter this.VisitExpression " + x
            | Expr -> "this.VisitExpression " + x 
            | Statement -> "this.VisitStatement " + x
            | Id -> "this.VisitId " + x
            | Option Id -> "Option.iter this.VisitId " + x
            | List Id -> "List.iter this.VisitId " + x
            | List (Tuple [Id; Expr]) -> "List.iter (fun (a, b) -> this.VisitId a; this.VisitExpression b) " + x 
            | List Statement -> "List.iter this.VisitStatement " + x
            | List (Tuple [Object _; Expr]) -> "List.iter (fun (a, b) -> this.VisitExpression b) " + x
            | List (Tuple [Option Expr; Statement]) -> "List.iter (fun (a, b) -> Option.iter this.VisitExpression a; this.VisitStatement b) " + x 
            | List (Tuple [List (Option Expr); Statement]) -> "List.iter (fun (a, b) -> List.iter (Option.iter this.VisitExpression) a; this.VisitStatement b) " + x
            | Object _ -> "()"
            | List (Object _) -> "()"
            | Option (Object _) -> "()"
            | Empty -> ""
            | _ -> " failwith \"no visit\""
        let trArgs = 
            match c with
            | Tuple t ->
                String.concat "; " (t |> Seq.mapi (fun j a -> tr a (letters.[j])))
            | Empty -> "()"
            | _ -> "(" + tr c "a" + ")"
        cprintfn "    override this.Visit%s %s = %s" n args trArgs

    for t, tl in [ "Expression", ExprDefs; "Statement", StatementDefs ] do
        cprintfn "    abstract Visit%s : %s -> unit" t t
        cprintfn "    override this.Visit%s x =" t
        cprintfn "        match x with"
        for n, c in tl do
            let args =
                match c with
                | Tuple t ->
                    "(" + String.concat ", " (Seq.take t.Length letters) + ")"
                | Empty -> ""
                | _ -> "a"
            let trArgs =
                match c with
                | Tuple t ->
                    "(" + String.concat ", " (Seq.take t.Length letters) + ")"
                | Empty -> "()"
                | _ -> "a"
            cprintfn "        | %s %s -> this.Visit%s %s" n args n trArgs

    cprintfn "    abstract VisitId : Id -> unit"
    cprintfn "    override this.VisitId x = ()"

    // StatementVisitor

//    cprintfn "type StatementVisitor() ="
//    for n, c in StatementDefs do
//        cprintfn "    abstract Visit%s : %s -> unit" n (toType c)
//        let args =
//            match c with
//            | Tuple t ->
//                "(" + String.concat ", " (Seq.take t.Length letters) + ")"
//            | Empty -> "()"
//            | _ -> "a"
//        let rec tr c x =
//            match c with
//            | List Expr -> "()"
//            | Option Expr -> "()"
//            | Expr -> "()"
//            | Statement -> "this.VisitStatement " + x
//            | Id -> "()"
//            | Option Id -> "()"
//            | List Id -> "()"
//            | List (Tuple [Id; Expr]) -> "()"
//            | List Statement -> "List.iter this.VisitStatement " + x
//            | List (Tuple [Object _; Expr]) -> "()"
//            | List (Tuple [(Option Expr | List (Option Expr)); Statement]) -> "List.iter (fun (a, b) -> this.VisitStatement b) " + x 
//            | Object _ -> "()"
//            | List (Object _) -> "()"
//            | Option (Object _) -> "()"
//            | Empty -> ""
//            | _ -> " failwith \"no visit\""
//        let trArgs = 
//            match c with
//            | Tuple t ->
//                String.concat "; " (t |> Seq.mapi (fun j a -> tr a (letters.[j])))
//            | Empty -> "()"
//            | _ -> "(" + tr c "a" + ")"
//        cprintfn "    override this.Visit%s %s = %s" n args trArgs
//
//    cprintfn "    abstract VisitStatement : Statement -> unit" 
//    cprintfn "    override this.VisitStatement x =" 
//    cprintfn "        match x with"
//    for n, c in StatementDefs do
//        let args =
//            match c with
//            | Tuple t ->
//                "(" + String.concat ", " (Seq.take t.Length letters) + ")"
//            | Empty -> ""
//            | _ -> "a"
//        let trArgs =
//            match c with
//            | Tuple t ->
//                "(" + String.concat ", " (Seq.take t.Length letters) + ")"
//            | Empty -> "()"
//            | _ -> "a"
//        cprintfn "        | %s %s -> this.Visit%s %s" n args n trArgs

    // ExtraForms

    cprintfn """
[<AutoOpen>]
module ExtraForms =
    let Lambda (a, b) = Function (a, Return b)
    let CurriedLambda (a, b) = List.foldBack (fun a b -> Function ([a], Return b)) a b
"""

//    cprintfn """type Breaker() =
//    member this.BreakExpressionList l =
//        let bb = l |> List.map this.BreakExpression 
//        if bb |> List.forall Option.isNone then None
//        else
//            let rec bL br (accSt, accE) ol bl =
//                match ol, bl with
//                | [], _ -> accSt, accE
//                | a :: oRest, None :: bRest ->
//                    if br then
//                        let aV = Id.New()
//                        bL true (VarDeclaration (aV, a) :: accSt, Var aV :: accE) oRest bRest
//                    else
//                        bL false (accSt, a :: accE) oRest bRest
//                | _ :: oRest, Some (aSt, aE) :: bRest ->
//                    bL true (aSt @ accSt, aE :: accE) oRest bRest
//            bL false ([], []) (List.rev l) (List.rev bb) |> Some"""
//    cprintfn "    member this.BreakExpressionOpt e ="
//    cprintfn "        match e with"
//    for n, c in ExprDefs do
//        cprintf "        | %s ->" n
    
    string code

System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + @"\AST.fs", code)


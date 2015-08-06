type Case =
    | Tuple of list<Case>
    | List of Case
    | Option of Case
    | Expr
    | Statement
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
    | Empty -> None

let Id = Object "Id"
let Literal = Object "Literal"
let TypeDefinition = Object "Concrete<TypeDefinition>"
let Constructor = Object "Constructor"
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
        "Call", Option Expr * TypeDefinition * Method * List Expr
        "CallInterface", Expr * TypeDefinition * Method * List Expr
        "Ctor", TypeDefinition * Constructor * List Expr
        "CCtor", TypeDefinition
//        "FieldGet", Expr * TypeDefinition * Field
//        "FieldSet", Expr * TypeDefinition * Field * Expr
        "FieldGet", Expr * TypeDefinition * Str
        "FieldSet", Expr * TypeDefinition * Str * Expr
        "Let", Id * Expr * Expr
        "NewVar", Id * Expr
        "Coalesce", Expr * Type * Expr
        "TypeCheck", Expr * Type
        "MacroFallback", Empty

        // F#
        "LetRec", List (Id * Expr) * Expr
        "StatementExpr", Statement

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
        "Yield", Expr
        "CSharpSwitch", Expr * List (List (Option Expr) * Statement)
        "GotoCase", Option Expr
        "Statements", List Statement
    ]

let binaryOps =
    [
        "!==", "``!==``"       
        "!=", "``!=``"        
        "%", "``%``"         
        "^&&", "``&&``"        
        "^&", "``&``"         
        "*", "``*``"         
        "+", "``+``"         
//        "", "``,``"         
        "-", "``-``"         
//        "", "``.``"         
        "/", "``/``"        
        "^<<", "``<<``"        
        "^<=", "``<=``"        
        "^<", "``<``"         
        "===", "``===``"       
        "==", "``==``"        
        "^=", "``=``"         
        "^>=", "``>=``"        
        ">>>", "``>>>``"       
        "^>>", "``>>``"        
        "^>", "``>``"         
        "^^", "``^``"         
//        "", "``in``"        
//        "", "``instanceof``"
        "^|", "``|``"         
        "^||", "``||``"            
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
            for opSym, binCase in binaryOps do
                cprintfn "    static member (%s) (a, b) = Binary (a, BinaryOperator.%s, b)" opSym binCase


    let ExprAndStatementDefs =
        seq {
            for n, c in ExprDefs -> "Expression", n, c
            for n, c in StatementDefs -> "Statement", n, c
        }
    
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
            | List Statement -> "List.map this.TransformStatement " + x
            | List (Tuple [Object _; Expr]) -> "List.map (fun (a, b) -> a, this.TransformExpression b) " + x
            | List (Tuple [Option Expr; Statement]) -> "List.map (fun (a, b) -> Option.map this.TransformExpression a, this.TransformStatement b) " + x 
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


namespace WebSharper.Core.AST
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

and Expression =
    | Undefined
    | This
    | Var of Id
    | Value of Literal
    | Application of Expression * list<Expression>
    | Function of list<Id> * Statement
    | VarSet of Id * Expression
    | Sequential of list<Expression>
    | NewArray of list<Expression>
    | Conditional of Expression * Expression * Expression
    | ItemGet of Expression * Expression
    | ItemSet of Expression * Expression * Expression
    | Binary of Expression * BinaryOperator * Expression
    | MutatingBinary of Expression * MutatingBinaryOperator * Expression
    | Unary of UnaryOperator * Expression
    | MutatingUnary of MutatingUnaryOperator * Expression
    | ExprSourcePos of SourcePos * Expression
    | Call of option<Expression> * Concrete<TypeDefinition> * Concrete<Method> * list<Expression>
    | CallInterface of Expression * Concrete<TypeDefinition> * Concrete<Method> * list<Expression>
    | Ctor of Concrete<TypeDefinition> * Constructor * list<Expression>
    | CCtor of Concrete<TypeDefinition>
    | FieldGet of Expression * Concrete<TypeDefinition> * string
    | FieldSet of Expression * Concrete<TypeDefinition> * string * Expression
    | Let of Id * Expression * Expression
    | NewVar of Id * Expression
    | Coalesce of Expression * Type * Expression
    | TypeCheck of Expression * Type
    | MacroFallback
    | LetRec of list<Id * Expression> * Expression
    | StatementExpr of Statement
    | Await of Expression
    | NamedParameter of Id * Expression
    | RefOrOutParameter of Expression
    | Object of list<string * Expression>
    | GlobalAccess of Address
    | New of Expression * list<Expression>
    | Hole of int
    with
    static member (!==) (a, b) = Binary (a, BinaryOperator.``!==``, b)
    static member (!=) (a, b) = Binary (a, BinaryOperator.``!=``, b)
    static member (%) (a, b) = Binary (a, BinaryOperator.``%``, b)
    static member (^&&) (a, b) = Binary (a, BinaryOperator.``&&``, b)
    static member (^&) (a, b) = Binary (a, BinaryOperator.``&``, b)
    static member (*) (a, b) = Binary (a, BinaryOperator.``*``, b)
    static member (+) (a, b) = Binary (a, BinaryOperator.``+``, b)
    static member (-) (a, b) = Binary (a, BinaryOperator.``-``, b)
    static member (/) (a, b) = Binary (a, BinaryOperator.``/``, b)
    static member (^<<) (a, b) = Binary (a, BinaryOperator.``<<``, b)
    static member (^<=) (a, b) = Binary (a, BinaryOperator.``<=``, b)
    static member (^<) (a, b) = Binary (a, BinaryOperator.``<``, b)
    static member (===) (a, b) = Binary (a, BinaryOperator.``===``, b)
    static member (==) (a, b) = Binary (a, BinaryOperator.``==``, b)
    static member (^=) (a, b) = Binary (a, BinaryOperator.``=``, b)
    static member (^>=) (a, b) = Binary (a, BinaryOperator.``>=``, b)
    static member (>>>) (a, b) = Binary (a, BinaryOperator.``>>>``, b)
    static member (^>>) (a, b) = Binary (a, BinaryOperator.``>>``, b)
    static member (^>) (a, b) = Binary (a, BinaryOperator.``>``, b)
    static member (^^) (a, b) = Binary (a, BinaryOperator.``^``, b)
    static member (^|) (a, b) = Binary (a, BinaryOperator.``|``, b)
    static member (^||) (a, b) = Binary (a, BinaryOperator.``||``, b)
and Statement =
    | Empty
    | Break of option<Id>
    | Continue of option<Id>
    | ExprStatement of Expression
    | Return of Expression
    | Block of list<Statement>
    | VarDeclaration of Id * Expression
    | While of Expression * Statement
    | DoWhile of Statement * Expression
    | For of option<Expression> * option<Expression> * option<Expression> * Statement
    | ForIn of Id * Expression * Statement
    | Switch of Expression * list<option<Expression> * Statement>
    | If of Expression * Statement * Statement
    | Throw of Expression
    | TryWith of Statement * option<Id> * Statement
    | TryFinally of Statement * Statement
    | Labeled of Id * Statement
    | StatementSourcePos of SourcePos * Statement
    | Goto of Id
    | Yield of Expression
    | CSharpSwitch of Expression * list<list<option<Expression>> * Statement>
    | GotoCase of option<Expression>
    | Statements of list<Statement>
type Transformer() =
    abstract TransformUndefined : unit -> Expression
    override this.TransformUndefined () = Undefined 
    abstract TransformThis : unit -> Expression
    override this.TransformThis () = This 
    abstract TransformVar : Id -> Expression
    override this.TransformVar a = Var (a)
    abstract TransformValue : Literal -> Expression
    override this.TransformValue a = Value (a)
    abstract TransformApplication : Expression * list<Expression> -> Expression
    override this.TransformApplication (a, b) = Application (this.TransformExpression a, List.map this.TransformExpression b)
    abstract TransformFunction : list<Id> * Statement -> Expression
    override this.TransformFunction (a, b) = Function (a, this.TransformStatement b)
    abstract TransformVarSet : Id * Expression -> Expression
    override this.TransformVarSet (a, b) = VarSet (a, this.TransformExpression b)
    abstract TransformSequential : list<Expression> -> Expression
    override this.TransformSequential a = Sequential (List.map this.TransformExpression a)
    abstract TransformNewArray : list<Expression> -> Expression
    override this.TransformNewArray a = NewArray (List.map this.TransformExpression a)
    abstract TransformConditional : Expression * Expression * Expression -> Expression
    override this.TransformConditional (a, b, c) = Conditional (this.TransformExpression a, this.TransformExpression b, this.TransformExpression c)
    abstract TransformItemGet : Expression * Expression -> Expression
    override this.TransformItemGet (a, b) = ItemGet (this.TransformExpression a, this.TransformExpression b)
    abstract TransformItemSet : Expression * Expression * Expression -> Expression
    override this.TransformItemSet (a, b, c) = ItemSet (this.TransformExpression a, this.TransformExpression b, this.TransformExpression c)
    abstract TransformBinary : Expression * BinaryOperator * Expression -> Expression
    override this.TransformBinary (a, b, c) = Binary (this.TransformExpression a, b, this.TransformExpression c)
    abstract TransformMutatingBinary : Expression * MutatingBinaryOperator * Expression -> Expression
    override this.TransformMutatingBinary (a, b, c) = MutatingBinary (this.TransformExpression a, b, this.TransformExpression c)
    abstract TransformUnary : UnaryOperator * Expression -> Expression
    override this.TransformUnary (a, b) = Unary (a, this.TransformExpression b)
    abstract TransformMutatingUnary : MutatingUnaryOperator * Expression -> Expression
    override this.TransformMutatingUnary (a, b) = MutatingUnary (a, this.TransformExpression b)
    abstract TransformExprSourcePos : SourcePos * Expression -> Expression
    override this.TransformExprSourcePos (a, b) = ExprSourcePos (a, this.TransformExpression b)
    abstract TransformCall : option<Expression> * Concrete<TypeDefinition> * Concrete<Method> * list<Expression> -> Expression
    override this.TransformCall (a, b, c, d) = Call (Option.map this.TransformExpression a, b, c, List.map this.TransformExpression d)
    abstract TransformCallInterface : Expression * Concrete<TypeDefinition> * Concrete<Method> * list<Expression> -> Expression
    override this.TransformCallInterface (a, b, c, d) = CallInterface (this.TransformExpression a, b, c, List.map this.TransformExpression d)
    abstract TransformCtor : Concrete<TypeDefinition> * Constructor * list<Expression> -> Expression
    override this.TransformCtor (a, b, c) = Ctor (a, b, List.map this.TransformExpression c)
    abstract TransformCCtor : Concrete<TypeDefinition> -> Expression
    override this.TransformCCtor a = CCtor (a)
    abstract TransformFieldGet : Expression * Concrete<TypeDefinition> * string -> Expression
    override this.TransformFieldGet (a, b, c) = FieldGet (this.TransformExpression a, b, c)
    abstract TransformFieldSet : Expression * Concrete<TypeDefinition> * string * Expression -> Expression
    override this.TransformFieldSet (a, b, c, d) = FieldSet (this.TransformExpression a, b, c, this.TransformExpression d)
    abstract TransformLet : Id * Expression * Expression -> Expression
    override this.TransformLet (a, b, c) = Let (a, this.TransformExpression b, this.TransformExpression c)
    abstract TransformNewVar : Id * Expression -> Expression
    override this.TransformNewVar (a, b) = NewVar (a, this.TransformExpression b)
    abstract TransformCoalesce : Expression * Type * Expression -> Expression
    override this.TransformCoalesce (a, b, c) = Coalesce (this.TransformExpression a, b, this.TransformExpression c)
    abstract TransformTypeCheck : Expression * Type -> Expression
    override this.TransformTypeCheck (a, b) = TypeCheck (this.TransformExpression a, b)
    abstract TransformMacroFallback : unit -> Expression
    override this.TransformMacroFallback () = MacroFallback 
    abstract TransformLetRec : list<Id * Expression> * Expression -> Expression
    override this.TransformLetRec (a, b) = LetRec (List.map (fun (a, b) -> a, this.TransformExpression b) a, this.TransformExpression b)
    abstract TransformStatementExpr : Statement -> Expression
    override this.TransformStatementExpr a = StatementExpr (this.TransformStatement a)
    abstract TransformAwait : Expression -> Expression
    override this.TransformAwait a = Await (this.TransformExpression a)
    abstract TransformNamedParameter : Id * Expression -> Expression
    override this.TransformNamedParameter (a, b) = NamedParameter (a, this.TransformExpression b)
    abstract TransformRefOrOutParameter : Expression -> Expression
    override this.TransformRefOrOutParameter a = RefOrOutParameter (this.TransformExpression a)
    abstract TransformObject : list<string * Expression> -> Expression
    override this.TransformObject a = Object (List.map (fun (a, b) -> a, this.TransformExpression b) a)
    abstract TransformGlobalAccess : Address -> Expression
    override this.TransformGlobalAccess a = GlobalAccess (a)
    abstract TransformNew : Expression * list<Expression> -> Expression
    override this.TransformNew (a, b) = New (this.TransformExpression a, List.map this.TransformExpression b)
    abstract TransformHole : int -> Expression
    override this.TransformHole a = Hole (a)
    abstract TransformEmpty : unit -> Statement
    override this.TransformEmpty () = Empty 
    abstract TransformBreak : option<Id> -> Statement
    override this.TransformBreak a = Break (a)
    abstract TransformContinue : option<Id> -> Statement
    override this.TransformContinue a = Continue (a)
    abstract TransformExprStatement : Expression -> Statement
    override this.TransformExprStatement a = ExprStatement (this.TransformExpression a)
    abstract TransformReturn : Expression -> Statement
    override this.TransformReturn a = Return (this.TransformExpression a)
    abstract TransformBlock : list<Statement> -> Statement
    override this.TransformBlock a = Block (List.map this.TransformStatement a)
    abstract TransformVarDeclaration : Id * Expression -> Statement
    override this.TransformVarDeclaration (a, b) = VarDeclaration (a, this.TransformExpression b)
    abstract TransformWhile : Expression * Statement -> Statement
    override this.TransformWhile (a, b) = While (this.TransformExpression a, this.TransformStatement b)
    abstract TransformDoWhile : Statement * Expression -> Statement
    override this.TransformDoWhile (a, b) = DoWhile (this.TransformStatement a, this.TransformExpression b)
    abstract TransformFor : option<Expression> * option<Expression> * option<Expression> * Statement -> Statement
    override this.TransformFor (a, b, c, d) = For (Option.map this.TransformExpression a, Option.map this.TransformExpression b, Option.map this.TransformExpression c, this.TransformStatement d)
    abstract TransformForIn : Id * Expression * Statement -> Statement
    override this.TransformForIn (a, b, c) = ForIn (a, this.TransformExpression b, this.TransformStatement c)
    abstract TransformSwitch : Expression * list<option<Expression> * Statement> -> Statement
    override this.TransformSwitch (a, b) = Switch (this.TransformExpression a, List.map (fun (a, b) -> Option.map this.TransformExpression a, this.TransformStatement b) b)
    abstract TransformIf : Expression * Statement * Statement -> Statement
    override this.TransformIf (a, b, c) = If (this.TransformExpression a, this.TransformStatement b, this.TransformStatement c)
    abstract TransformThrow : Expression -> Statement
    override this.TransformThrow a = Throw (this.TransformExpression a)
    abstract TransformTryWith : Statement * option<Id> * Statement -> Statement
    override this.TransformTryWith (a, b, c) = TryWith (this.TransformStatement a, b, this.TransformStatement c)
    abstract TransformTryFinally : Statement * Statement -> Statement
    override this.TransformTryFinally (a, b) = TryFinally (this.TransformStatement a, this.TransformStatement b)
    abstract TransformLabeled : Id * Statement -> Statement
    override this.TransformLabeled (a, b) = Labeled (a, this.TransformStatement b)
    abstract TransformStatementSourcePos : SourcePos * Statement -> Statement
    override this.TransformStatementSourcePos (a, b) = StatementSourcePos (a, this.TransformStatement b)
    abstract TransformGoto : Id -> Statement
    override this.TransformGoto a = Goto (a)
    abstract TransformYield : Expression -> Statement
    override this.TransformYield a = Yield (this.TransformExpression a)
    abstract TransformCSharpSwitch : Expression * list<list<option<Expression>> * Statement> -> Statement
    override this.TransformCSharpSwitch (a, b) = CSharpSwitch (this.TransformExpression a,  failwith "no transform")
    abstract TransformGotoCase : option<Expression> -> Statement
    override this.TransformGotoCase a = GotoCase (Option.map this.TransformExpression a)
    abstract TransformStatements : list<Statement> -> Statement
    override this.TransformStatements a = Statements (List.map this.TransformStatement a)
    abstract TransformExpression : Expression -> Expression
    override this.TransformExpression x =
        match x with
        | Undefined  -> this.TransformUndefined ()
        | This  -> this.TransformThis ()
        | Var a -> this.TransformVar a
        | Value a -> this.TransformValue a
        | Application (a, b) -> this.TransformApplication (a, b)
        | Function (a, b) -> this.TransformFunction (a, b)
        | VarSet (a, b) -> this.TransformVarSet (a, b)
        | Sequential a -> this.TransformSequential a
        | NewArray a -> this.TransformNewArray a
        | Conditional (a, b, c) -> this.TransformConditional (a, b, c)
        | ItemGet (a, b) -> this.TransformItemGet (a, b)
        | ItemSet (a, b, c) -> this.TransformItemSet (a, b, c)
        | Binary (a, b, c) -> this.TransformBinary (a, b, c)
        | MutatingBinary (a, b, c) -> this.TransformMutatingBinary (a, b, c)
        | Unary (a, b) -> this.TransformUnary (a, b)
        | MutatingUnary (a, b) -> this.TransformMutatingUnary (a, b)
        | ExprSourcePos (a, b) -> this.TransformExprSourcePos (a, b)
        | Call (a, b, c, d) -> this.TransformCall (a, b, c, d)
        | CallInterface (a, b, c, d) -> this.TransformCallInterface (a, b, c, d)
        | Ctor (a, b, c) -> this.TransformCtor (a, b, c)
        | CCtor a -> this.TransformCCtor a
        | FieldGet (a, b, c) -> this.TransformFieldGet (a, b, c)
        | FieldSet (a, b, c, d) -> this.TransformFieldSet (a, b, c, d)
        | Let (a, b, c) -> this.TransformLet (a, b, c)
        | NewVar (a, b) -> this.TransformNewVar (a, b)
        | Coalesce (a, b, c) -> this.TransformCoalesce (a, b, c)
        | TypeCheck (a, b) -> this.TransformTypeCheck (a, b)
        | MacroFallback  -> this.TransformMacroFallback ()
        | LetRec (a, b) -> this.TransformLetRec (a, b)
        | StatementExpr a -> this.TransformStatementExpr a
        | Await a -> this.TransformAwait a
        | NamedParameter (a, b) -> this.TransformNamedParameter (a, b)
        | RefOrOutParameter a -> this.TransformRefOrOutParameter a
        | Object a -> this.TransformObject a
        | GlobalAccess a -> this.TransformGlobalAccess a
        | New (a, b) -> this.TransformNew (a, b)
        | Hole a -> this.TransformHole a
    abstract TransformStatement : Statement -> Statement
    override this.TransformStatement x =
        match x with
        | Empty  -> this.TransformEmpty ()
        | Break a -> this.TransformBreak a
        | Continue a -> this.TransformContinue a
        | ExprStatement a -> this.TransformExprStatement a
        | Return a -> this.TransformReturn a
        | Block a -> this.TransformBlock a
        | VarDeclaration (a, b) -> this.TransformVarDeclaration (a, b)
        | While (a, b) -> this.TransformWhile (a, b)
        | DoWhile (a, b) -> this.TransformDoWhile (a, b)
        | For (a, b, c, d) -> this.TransformFor (a, b, c, d)
        | ForIn (a, b, c) -> this.TransformForIn (a, b, c)
        | Switch (a, b) -> this.TransformSwitch (a, b)
        | If (a, b, c) -> this.TransformIf (a, b, c)
        | Throw a -> this.TransformThrow a
        | TryWith (a, b, c) -> this.TransformTryWith (a, b, c)
        | TryFinally (a, b) -> this.TransformTryFinally (a, b)
        | Labeled (a, b) -> this.TransformLabeled (a, b)
        | StatementSourcePos (a, b) -> this.TransformStatementSourcePos (a, b)
        | Goto a -> this.TransformGoto a
        | Yield a -> this.TransformYield a
        | CSharpSwitch (a, b) -> this.TransformCSharpSwitch (a, b)
        | GotoCase a -> this.TransformGotoCase a
        | Statements a -> this.TransformStatements a

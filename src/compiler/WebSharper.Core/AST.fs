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
    | FuncWithThis of Id * list<Id> * Statement
    | Self
    | Base
    | Call of option<Expression> * Concrete<TypeDefinition> * Concrete<Method> * list<Expression>
    | CallNeedingMoreArgs of option<Expression> * Concrete<TypeDefinition> * Concrete<Method> * list<Expression>
    | Ctor of Concrete<TypeDefinition> * Constructor * list<Expression>
    | BaseCtor of Expression * Concrete<TypeDefinition> * Constructor * list<Expression>
    | CopyCtor of TypeDefinition * Expression
    | Cctor of TypeDefinition
    | FieldGet of option<Expression> * Concrete<TypeDefinition> * string
    | FieldSet of option<Expression> * Concrete<TypeDefinition> * string * Expression
    | Let of Id * Expression * Expression
    | NewVar of Id * Expression
    | Coalesce of Expression * Type * Expression
    | TypeCheck of Expression * Type
    | WithVars of list<Id> * Expression
    | OverrideName of TypeDefinition * Method
    | LetRec of list<Id * Expression> * Expression
    | StatementExpr of Statement
    | NewRecord of Concrete<TypeDefinition> * list<Expression>
    | NewUnionCase of Concrete<TypeDefinition> * string * list<Expression>
    | UnionCaseGet of Expression * Concrete<TypeDefinition> * string * string
    | Await of Expression
    | NamedParameter of Id * Expression
    | RefOrOutParameter of Expression
    | Object of list<string * Expression>
    | GlobalAccess of Address
    | New of Expression * list<Expression>
    | Hole of int
    with
    static member (^!==) (a, b) = Binary (a, BinaryOperator.``!==``, b)
    static member (^!=) (a, b) = Binary (a, BinaryOperator.``!=``, b)
    static member (^%) (a, b) = Binary (a, BinaryOperator.``%``, b)
    static member (^&&) (a, b) = Binary (a, BinaryOperator.``&&``, b)
    static member (^&) (a, b) = Binary (a, BinaryOperator.``&``, b)
    static member (^*) (a, b) = Binary (a, BinaryOperator.``*``, b)
    static member (^+) (a, b) = Binary (a, BinaryOperator.``+``, b)
    static member (^-) (a, b) = Binary (a, BinaryOperator.``-``, b)
    static member (^/) (a, b) = Binary (a, BinaryOperator.``/``, b)
    static member (^<<) (a, b) = Binary (a, BinaryOperator.``<<``, b)
    static member (^<=) (a, b) = Binary (a, BinaryOperator.``<=``, b)
    static member (^<) (a, b) = Binary (a, BinaryOperator.``<``, b)
    static member (^===) (a, b) = Binary (a, BinaryOperator.``===``, b)
    static member (^==) (a, b) = Binary (a, BinaryOperator.``==``, b)
    static member (^=) (a, b) = Binary (a, BinaryOperator.``=``, b)
    static member (^>=) (a, b) = Binary (a, BinaryOperator.``>=``, b)
    static member (^>>>) (a, b) = Binary (a, BinaryOperator.``>>>``, b)
    static member (^>>) (a, b) = Binary (a, BinaryOperator.``>>``, b)
    static member (^>) (a, b) = Binary (a, BinaryOperator.``>``, b)
    static member (^^) (a, b) = Binary (a, BinaryOperator.``^``, b)
    static member (^|) (a, b) = Binary (a, BinaryOperator.``|``, b)
    static member (^||) (a, b) = Binary (a, BinaryOperator.``||``, b)
    member a.Item b = ItemGet (a, b)
    member a.Item b = Application (a, b)
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
    | Continuation of Id * Expression
    | Yield of option<Expression>
    | CSharpSwitch of Expression * list<list<option<Expression>> * Statement>
    | GotoCase of option<Expression>
    | Statements of list<Statement>
type Transformer() =
    abstract TransformUndefined : unit -> Expression
    override this.TransformUndefined () = Undefined 
    abstract TransformThis : unit -> Expression
    override this.TransformThis () = This 
    abstract TransformVar : Id -> Expression
    override this.TransformVar a = Var (this.TransformId a)
    abstract TransformValue : Literal -> Expression
    override this.TransformValue a = Value (a)
    abstract TransformApplication : Expression * list<Expression> -> Expression
    override this.TransformApplication (a, b) = Application (this.TransformExpression a, List.map this.TransformExpression b)
    abstract TransformFunction : list<Id> * Statement -> Expression
    override this.TransformFunction (a, b) = Function (List.map this.TransformId a, this.TransformStatement b)
    abstract TransformVarSet : Id * Expression -> Expression
    override this.TransformVarSet (a, b) = VarSet (this.TransformId a, this.TransformExpression b)
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
    abstract TransformFuncWithThis : Id * list<Id> * Statement -> Expression
    override this.TransformFuncWithThis (a, b, c) = FuncWithThis (this.TransformId a, List.map this.TransformId b, this.TransformStatement c)
    abstract TransformSelf : unit -> Expression
    override this.TransformSelf () = Self 
    abstract TransformBase : unit -> Expression
    override this.TransformBase () = Base 
    abstract TransformCall : option<Expression> * Concrete<TypeDefinition> * Concrete<Method> * list<Expression> -> Expression
    override this.TransformCall (a, b, c, d) = Call (Option.map this.TransformExpression a, b, c, List.map this.TransformExpression d)
    abstract TransformCallNeedingMoreArgs : option<Expression> * Concrete<TypeDefinition> * Concrete<Method> * list<Expression> -> Expression
    override this.TransformCallNeedingMoreArgs (a, b, c, d) = CallNeedingMoreArgs (Option.map this.TransformExpression a, b, c, List.map this.TransformExpression d)
    abstract TransformCtor : Concrete<TypeDefinition> * Constructor * list<Expression> -> Expression
    override this.TransformCtor (a, b, c) = Ctor (a, b, List.map this.TransformExpression c)
    abstract TransformBaseCtor : Expression * Concrete<TypeDefinition> * Constructor * list<Expression> -> Expression
    override this.TransformBaseCtor (a, b, c, d) = BaseCtor (this.TransformExpression a, b, c, List.map this.TransformExpression d)
    abstract TransformCopyCtor : TypeDefinition * Expression -> Expression
    override this.TransformCopyCtor (a, b) = CopyCtor (a, this.TransformExpression b)
    abstract TransformCctor : TypeDefinition -> Expression
    override this.TransformCctor a = Cctor (a)
    abstract TransformFieldGet : option<Expression> * Concrete<TypeDefinition> * string -> Expression
    override this.TransformFieldGet (a, b, c) = FieldGet (Option.map this.TransformExpression a, b, c)
    abstract TransformFieldSet : option<Expression> * Concrete<TypeDefinition> * string * Expression -> Expression
    override this.TransformFieldSet (a, b, c, d) = FieldSet (Option.map this.TransformExpression a, b, c, this.TransformExpression d)
    abstract TransformLet : Id * Expression * Expression -> Expression
    override this.TransformLet (a, b, c) = Let (this.TransformId a, this.TransformExpression b, this.TransformExpression c)
    abstract TransformNewVar : Id * Expression -> Expression
    override this.TransformNewVar (a, b) = NewVar (this.TransformId a, this.TransformExpression b)
    abstract TransformCoalesce : Expression * Type * Expression -> Expression
    override this.TransformCoalesce (a, b, c) = Coalesce (this.TransformExpression a, b, this.TransformExpression c)
    abstract TransformTypeCheck : Expression * Type -> Expression
    override this.TransformTypeCheck (a, b) = TypeCheck (this.TransformExpression a, b)
    abstract TransformWithVars : list<Id> * Expression -> Expression
    override this.TransformWithVars (a, b) = WithVars (List.map this.TransformId a, this.TransformExpression b)
    abstract TransformOverrideName : TypeDefinition * Method -> Expression
    override this.TransformOverrideName (a, b) = OverrideName (a, b)
    abstract TransformLetRec : list<Id * Expression> * Expression -> Expression
    override this.TransformLetRec (a, b) = LetRec (List.map (fun (a, b) -> this.TransformId a, this.TransformExpression b) a, this.TransformExpression b)
    abstract TransformStatementExpr : Statement -> Expression
    override this.TransformStatementExpr a = StatementExpr (this.TransformStatement a)
    abstract TransformNewRecord : Concrete<TypeDefinition> * list<Expression> -> Expression
    override this.TransformNewRecord (a, b) = NewRecord (a, List.map this.TransformExpression b)
    abstract TransformNewUnionCase : Concrete<TypeDefinition> * string * list<Expression> -> Expression
    override this.TransformNewUnionCase (a, b, c) = NewUnionCase (a, b, List.map this.TransformExpression c)
    abstract TransformUnionCaseGet : Expression * Concrete<TypeDefinition> * string * string -> Expression
    override this.TransformUnionCaseGet (a, b, c, d) = UnionCaseGet (this.TransformExpression a, b, c, d)
    abstract TransformAwait : Expression -> Expression
    override this.TransformAwait a = Await (this.TransformExpression a)
    abstract TransformNamedParameter : Id * Expression -> Expression
    override this.TransformNamedParameter (a, b) = NamedParameter (this.TransformId a, this.TransformExpression b)
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
    override this.TransformBreak a = Break (Option.map this.TransformId a)
    abstract TransformContinue : option<Id> -> Statement
    override this.TransformContinue a = Continue (Option.map this.TransformId a)
    abstract TransformExprStatement : Expression -> Statement
    override this.TransformExprStatement a = ExprStatement (this.TransformExpression a)
    abstract TransformReturn : Expression -> Statement
    override this.TransformReturn a = Return (this.TransformExpression a)
    abstract TransformBlock : list<Statement> -> Statement
    override this.TransformBlock a = Block (List.map this.TransformStatement a)
    abstract TransformVarDeclaration : Id * Expression -> Statement
    override this.TransformVarDeclaration (a, b) = VarDeclaration (this.TransformId a, this.TransformExpression b)
    abstract TransformWhile : Expression * Statement -> Statement
    override this.TransformWhile (a, b) = While (this.TransformExpression a, this.TransformStatement b)
    abstract TransformDoWhile : Statement * Expression -> Statement
    override this.TransformDoWhile (a, b) = DoWhile (this.TransformStatement a, this.TransformExpression b)
    abstract TransformFor : option<Expression> * option<Expression> * option<Expression> * Statement -> Statement
    override this.TransformFor (a, b, c, d) = For (Option.map this.TransformExpression a, Option.map this.TransformExpression b, Option.map this.TransformExpression c, this.TransformStatement d)
    abstract TransformForIn : Id * Expression * Statement -> Statement
    override this.TransformForIn (a, b, c) = ForIn (this.TransformId a, this.TransformExpression b, this.TransformStatement c)
    abstract TransformSwitch : Expression * list<option<Expression> * Statement> -> Statement
    override this.TransformSwitch (a, b) = Switch (this.TransformExpression a, List.map (fun (a, b) -> Option.map this.TransformExpression a, this.TransformStatement b) b)
    abstract TransformIf : Expression * Statement * Statement -> Statement
    override this.TransformIf (a, b, c) = If (this.TransformExpression a, this.TransformStatement b, this.TransformStatement c)
    abstract TransformThrow : Expression -> Statement
    override this.TransformThrow a = Throw (this.TransformExpression a)
    abstract TransformTryWith : Statement * option<Id> * Statement -> Statement
    override this.TransformTryWith (a, b, c) = TryWith (this.TransformStatement a, Option.map this.TransformId b, this.TransformStatement c)
    abstract TransformTryFinally : Statement * Statement -> Statement
    override this.TransformTryFinally (a, b) = TryFinally (this.TransformStatement a, this.TransformStatement b)
    abstract TransformLabeled : Id * Statement -> Statement
    override this.TransformLabeled (a, b) = Labeled (this.TransformId a, this.TransformStatement b)
    abstract TransformStatementSourcePos : SourcePos * Statement -> Statement
    override this.TransformStatementSourcePos (a, b) = StatementSourcePos (a, this.TransformStatement b)
    abstract TransformGoto : Id -> Statement
    override this.TransformGoto a = Goto (this.TransformId a)
    abstract TransformContinuation : Id * Expression -> Statement
    override this.TransformContinuation (a, b) = Continuation (this.TransformId a, this.TransformExpression b)
    abstract TransformYield : option<Expression> -> Statement
    override this.TransformYield a = Yield (Option.map this.TransformExpression a)
    abstract TransformCSharpSwitch : Expression * list<list<option<Expression>> * Statement> -> Statement
    override this.TransformCSharpSwitch (a, b) = CSharpSwitch (this.TransformExpression a, List.map (fun (a, b) -> List.map (Option.map this.TransformExpression) a, this.TransformStatement b) b)
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
        | FuncWithThis (a, b, c) -> this.TransformFuncWithThis (a, b, c)
        | Self  -> this.TransformSelf ()
        | Base  -> this.TransformBase ()
        | Call (a, b, c, d) -> this.TransformCall (a, b, c, d)
        | CallNeedingMoreArgs (a, b, c, d) -> this.TransformCallNeedingMoreArgs (a, b, c, d)
        | Ctor (a, b, c) -> this.TransformCtor (a, b, c)
        | BaseCtor (a, b, c, d) -> this.TransformBaseCtor (a, b, c, d)
        | CopyCtor (a, b) -> this.TransformCopyCtor (a, b)
        | Cctor a -> this.TransformCctor a
        | FieldGet (a, b, c) -> this.TransformFieldGet (a, b, c)
        | FieldSet (a, b, c, d) -> this.TransformFieldSet (a, b, c, d)
        | Let (a, b, c) -> this.TransformLet (a, b, c)
        | NewVar (a, b) -> this.TransformNewVar (a, b)
        | Coalesce (a, b, c) -> this.TransformCoalesce (a, b, c)
        | TypeCheck (a, b) -> this.TransformTypeCheck (a, b)
        | WithVars (a, b) -> this.TransformWithVars (a, b)
        | OverrideName (a, b) -> this.TransformOverrideName (a, b)
        | LetRec (a, b) -> this.TransformLetRec (a, b)
        | StatementExpr a -> this.TransformStatementExpr a
        | NewRecord (a, b) -> this.TransformNewRecord (a, b)
        | NewUnionCase (a, b, c) -> this.TransformNewUnionCase (a, b, c)
        | UnionCaseGet (a, b, c, d) -> this.TransformUnionCaseGet (a, b, c, d)
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
        | Continuation (a, b) -> this.TransformContinuation (a, b)
        | Yield a -> this.TransformYield a
        | CSharpSwitch (a, b) -> this.TransformCSharpSwitch (a, b)
        | GotoCase a -> this.TransformGotoCase a
        | Statements a -> this.TransformStatements a
    abstract TransformId : Id -> Id
    override this.TransformId x = x
type Visitor() =
    abstract VisitUndefined : unit -> unit
    override this.VisitUndefined () = ()
    abstract VisitThis : unit -> unit
    override this.VisitThis () = ()
    abstract VisitVar : Id -> unit
    override this.VisitVar a = (this.VisitId a)
    abstract VisitValue : Literal -> unit
    override this.VisitValue a = (())
    abstract VisitApplication : Expression * list<Expression> -> unit
    override this.VisitApplication (a, b) = this.VisitExpression a; List.iter this.VisitExpression b
    abstract VisitFunction : list<Id> * Statement -> unit
    override this.VisitFunction (a, b) = List.iter this.VisitId a; this.VisitStatement b
    abstract VisitVarSet : Id * Expression -> unit
    override this.VisitVarSet (a, b) = this.VisitId a; this.VisitExpression b
    abstract VisitSequential : list<Expression> -> unit
    override this.VisitSequential a = (List.iter this.VisitExpression a)
    abstract VisitNewArray : list<Expression> -> unit
    override this.VisitNewArray a = (List.iter this.VisitExpression a)
    abstract VisitConditional : Expression * Expression * Expression -> unit
    override this.VisitConditional (a, b, c) = this.VisitExpression a; this.VisitExpression b; this.VisitExpression c
    abstract VisitItemGet : Expression * Expression -> unit
    override this.VisitItemGet (a, b) = this.VisitExpression a; this.VisitExpression b
    abstract VisitItemSet : Expression * Expression * Expression -> unit
    override this.VisitItemSet (a, b, c) = this.VisitExpression a; this.VisitExpression b; this.VisitExpression c
    abstract VisitBinary : Expression * BinaryOperator * Expression -> unit
    override this.VisitBinary (a, b, c) = this.VisitExpression a; (); this.VisitExpression c
    abstract VisitMutatingBinary : Expression * MutatingBinaryOperator * Expression -> unit
    override this.VisitMutatingBinary (a, b, c) = this.VisitExpression a; (); this.VisitExpression c
    abstract VisitUnary : UnaryOperator * Expression -> unit
    override this.VisitUnary (a, b) = (); this.VisitExpression b
    abstract VisitMutatingUnary : MutatingUnaryOperator * Expression -> unit
    override this.VisitMutatingUnary (a, b) = (); this.VisitExpression b
    abstract VisitExprSourcePos : SourcePos * Expression -> unit
    override this.VisitExprSourcePos (a, b) = (); this.VisitExpression b
    abstract VisitFuncWithThis : Id * list<Id> * Statement -> unit
    override this.VisitFuncWithThis (a, b, c) = this.VisitId a; List.iter this.VisitId b; this.VisitStatement c
    abstract VisitSelf : unit -> unit
    override this.VisitSelf () = ()
    abstract VisitBase : unit -> unit
    override this.VisitBase () = ()
    abstract VisitCall : option<Expression> * Concrete<TypeDefinition> * Concrete<Method> * list<Expression> -> unit
    override this.VisitCall (a, b, c, d) = Option.iter this.VisitExpression a; (); (); List.iter this.VisitExpression d
    abstract VisitCallNeedingMoreArgs : option<Expression> * Concrete<TypeDefinition> * Concrete<Method> * list<Expression> -> unit
    override this.VisitCallNeedingMoreArgs (a, b, c, d) = Option.iter this.VisitExpression a; (); (); List.iter this.VisitExpression d
    abstract VisitCtor : Concrete<TypeDefinition> * Constructor * list<Expression> -> unit
    override this.VisitCtor (a, b, c) = (); (); List.iter this.VisitExpression c
    abstract VisitBaseCtor : Expression * Concrete<TypeDefinition> * Constructor * list<Expression> -> unit
    override this.VisitBaseCtor (a, b, c, d) = this.VisitExpression a; (); (); List.iter this.VisitExpression d
    abstract VisitCopyCtor : TypeDefinition * Expression -> unit
    override this.VisitCopyCtor (a, b) = (); this.VisitExpression b
    abstract VisitCctor : TypeDefinition -> unit
    override this.VisitCctor a = (())
    abstract VisitFieldGet : option<Expression> * Concrete<TypeDefinition> * string -> unit
    override this.VisitFieldGet (a, b, c) = Option.iter this.VisitExpression a; (); ()
    abstract VisitFieldSet : option<Expression> * Concrete<TypeDefinition> * string * Expression -> unit
    override this.VisitFieldSet (a, b, c, d) = Option.iter this.VisitExpression a; (); (); this.VisitExpression d
    abstract VisitLet : Id * Expression * Expression -> unit
    override this.VisitLet (a, b, c) = this.VisitId a; this.VisitExpression b; this.VisitExpression c
    abstract VisitNewVar : Id * Expression -> unit
    override this.VisitNewVar (a, b) = this.VisitId a; this.VisitExpression b
    abstract VisitCoalesce : Expression * Type * Expression -> unit
    override this.VisitCoalesce (a, b, c) = this.VisitExpression a; (); this.VisitExpression c
    abstract VisitTypeCheck : Expression * Type -> unit
    override this.VisitTypeCheck (a, b) = this.VisitExpression a; ()
    abstract VisitWithVars : list<Id> * Expression -> unit
    override this.VisitWithVars (a, b) = List.iter this.VisitId a; this.VisitExpression b
    abstract VisitOverrideName : TypeDefinition * Method -> unit
    override this.VisitOverrideName (a, b) = (); ()
    abstract VisitLetRec : list<Id * Expression> * Expression -> unit
    override this.VisitLetRec (a, b) = List.iter (fun (a, b) -> this.VisitId a; this.VisitExpression b) a; this.VisitExpression b
    abstract VisitStatementExpr : Statement -> unit
    override this.VisitStatementExpr a = (this.VisitStatement a)
    abstract VisitNewRecord : Concrete<TypeDefinition> * list<Expression> -> unit
    override this.VisitNewRecord (a, b) = (); List.iter this.VisitExpression b
    abstract VisitNewUnionCase : Concrete<TypeDefinition> * string * list<Expression> -> unit
    override this.VisitNewUnionCase (a, b, c) = (); (); List.iter this.VisitExpression c
    abstract VisitUnionCaseGet : Expression * Concrete<TypeDefinition> * string * string -> unit
    override this.VisitUnionCaseGet (a, b, c, d) = this.VisitExpression a; (); (); ()
    abstract VisitAwait : Expression -> unit
    override this.VisitAwait a = (this.VisitExpression a)
    abstract VisitNamedParameter : Id * Expression -> unit
    override this.VisitNamedParameter (a, b) = this.VisitId a; this.VisitExpression b
    abstract VisitRefOrOutParameter : Expression -> unit
    override this.VisitRefOrOutParameter a = (this.VisitExpression a)
    abstract VisitObject : list<string * Expression> -> unit
    override this.VisitObject a = (List.iter (fun (a, b) -> this.VisitExpression b) a)
    abstract VisitGlobalAccess : Address -> unit
    override this.VisitGlobalAccess a = (())
    abstract VisitNew : Expression * list<Expression> -> unit
    override this.VisitNew (a, b) = this.VisitExpression a; List.iter this.VisitExpression b
    abstract VisitHole : int -> unit
    override this.VisitHole a = (())
    abstract VisitEmpty : unit -> unit
    override this.VisitEmpty () = ()
    abstract VisitBreak : option<Id> -> unit
    override this.VisitBreak a = (Option.iter this.VisitId a)
    abstract VisitContinue : option<Id> -> unit
    override this.VisitContinue a = (Option.iter this.VisitId a)
    abstract VisitExprStatement : Expression -> unit
    override this.VisitExprStatement a = (this.VisitExpression a)
    abstract VisitReturn : Expression -> unit
    override this.VisitReturn a = (this.VisitExpression a)
    abstract VisitBlock : list<Statement> -> unit
    override this.VisitBlock a = (List.iter this.VisitStatement a)
    abstract VisitVarDeclaration : Id * Expression -> unit
    override this.VisitVarDeclaration (a, b) = this.VisitId a; this.VisitExpression b
    abstract VisitWhile : Expression * Statement -> unit
    override this.VisitWhile (a, b) = this.VisitExpression a; this.VisitStatement b
    abstract VisitDoWhile : Statement * Expression -> unit
    override this.VisitDoWhile (a, b) = this.VisitStatement a; this.VisitExpression b
    abstract VisitFor : option<Expression> * option<Expression> * option<Expression> * Statement -> unit
    override this.VisitFor (a, b, c, d) = Option.iter this.VisitExpression a; Option.iter this.VisitExpression b; Option.iter this.VisitExpression c; this.VisitStatement d
    abstract VisitForIn : Id * Expression * Statement -> unit
    override this.VisitForIn (a, b, c) = this.VisitId a; this.VisitExpression b; this.VisitStatement c
    abstract VisitSwitch : Expression * list<option<Expression> * Statement> -> unit
    override this.VisitSwitch (a, b) = this.VisitExpression a; List.iter (fun (a, b) -> Option.iter this.VisitExpression a; this.VisitStatement b) b
    abstract VisitIf : Expression * Statement * Statement -> unit
    override this.VisitIf (a, b, c) = this.VisitExpression a; this.VisitStatement b; this.VisitStatement c
    abstract VisitThrow : Expression -> unit
    override this.VisitThrow a = (this.VisitExpression a)
    abstract VisitTryWith : Statement * option<Id> * Statement -> unit
    override this.VisitTryWith (a, b, c) = this.VisitStatement a; Option.iter this.VisitId b; this.VisitStatement c
    abstract VisitTryFinally : Statement * Statement -> unit
    override this.VisitTryFinally (a, b) = this.VisitStatement a; this.VisitStatement b
    abstract VisitLabeled : Id * Statement -> unit
    override this.VisitLabeled (a, b) = this.VisitId a; this.VisitStatement b
    abstract VisitStatementSourcePos : SourcePos * Statement -> unit
    override this.VisitStatementSourcePos (a, b) = (); this.VisitStatement b
    abstract VisitGoto : Id -> unit
    override this.VisitGoto a = (this.VisitId a)
    abstract VisitContinuation : Id * Expression -> unit
    override this.VisitContinuation (a, b) = this.VisitId a; this.VisitExpression b
    abstract VisitYield : option<Expression> -> unit
    override this.VisitYield a = (Option.iter this.VisitExpression a)
    abstract VisitCSharpSwitch : Expression * list<list<option<Expression>> * Statement> -> unit
    override this.VisitCSharpSwitch (a, b) = this.VisitExpression a; List.iter (fun (a, b) -> List.iter (Option.iter this.VisitExpression) a; this.VisitStatement b) b
    abstract VisitGotoCase : option<Expression> -> unit
    override this.VisitGotoCase a = (Option.iter this.VisitExpression a)
    abstract VisitStatements : list<Statement> -> unit
    override this.VisitStatements a = (List.iter this.VisitStatement a)
    abstract VisitExpression : Expression -> unit
    override this.VisitExpression x =
        match x with
        | Undefined  -> this.VisitUndefined ()
        | This  -> this.VisitThis ()
        | Var a -> this.VisitVar a
        | Value a -> this.VisitValue a
        | Application (a, b) -> this.VisitApplication (a, b)
        | Function (a, b) -> this.VisitFunction (a, b)
        | VarSet (a, b) -> this.VisitVarSet (a, b)
        | Sequential a -> this.VisitSequential a
        | NewArray a -> this.VisitNewArray a
        | Conditional (a, b, c) -> this.VisitConditional (a, b, c)
        | ItemGet (a, b) -> this.VisitItemGet (a, b)
        | ItemSet (a, b, c) -> this.VisitItemSet (a, b, c)
        | Binary (a, b, c) -> this.VisitBinary (a, b, c)
        | MutatingBinary (a, b, c) -> this.VisitMutatingBinary (a, b, c)
        | Unary (a, b) -> this.VisitUnary (a, b)
        | MutatingUnary (a, b) -> this.VisitMutatingUnary (a, b)
        | ExprSourcePos (a, b) -> this.VisitExprSourcePos (a, b)
        | FuncWithThis (a, b, c) -> this.VisitFuncWithThis (a, b, c)
        | Self  -> this.VisitSelf ()
        | Base  -> this.VisitBase ()
        | Call (a, b, c, d) -> this.VisitCall (a, b, c, d)
        | CallNeedingMoreArgs (a, b, c, d) -> this.VisitCallNeedingMoreArgs (a, b, c, d)
        | Ctor (a, b, c) -> this.VisitCtor (a, b, c)
        | BaseCtor (a, b, c, d) -> this.VisitBaseCtor (a, b, c, d)
        | CopyCtor (a, b) -> this.VisitCopyCtor (a, b)
        | Cctor a -> this.VisitCctor a
        | FieldGet (a, b, c) -> this.VisitFieldGet (a, b, c)
        | FieldSet (a, b, c, d) -> this.VisitFieldSet (a, b, c, d)
        | Let (a, b, c) -> this.VisitLet (a, b, c)
        | NewVar (a, b) -> this.VisitNewVar (a, b)
        | Coalesce (a, b, c) -> this.VisitCoalesce (a, b, c)
        | TypeCheck (a, b) -> this.VisitTypeCheck (a, b)
        | WithVars (a, b) -> this.VisitWithVars (a, b)
        | OverrideName (a, b) -> this.VisitOverrideName (a, b)
        | LetRec (a, b) -> this.VisitLetRec (a, b)
        | StatementExpr a -> this.VisitStatementExpr a
        | NewRecord (a, b) -> this.VisitNewRecord (a, b)
        | NewUnionCase (a, b, c) -> this.VisitNewUnionCase (a, b, c)
        | UnionCaseGet (a, b, c, d) -> this.VisitUnionCaseGet (a, b, c, d)
        | Await a -> this.VisitAwait a
        | NamedParameter (a, b) -> this.VisitNamedParameter (a, b)
        | RefOrOutParameter a -> this.VisitRefOrOutParameter a
        | Object a -> this.VisitObject a
        | GlobalAccess a -> this.VisitGlobalAccess a
        | New (a, b) -> this.VisitNew (a, b)
        | Hole a -> this.VisitHole a
    abstract VisitStatement : Statement -> unit
    override this.VisitStatement x =
        match x with
        | Empty  -> this.VisitEmpty ()
        | Break a -> this.VisitBreak a
        | Continue a -> this.VisitContinue a
        | ExprStatement a -> this.VisitExprStatement a
        | Return a -> this.VisitReturn a
        | Block a -> this.VisitBlock a
        | VarDeclaration (a, b) -> this.VisitVarDeclaration (a, b)
        | While (a, b) -> this.VisitWhile (a, b)
        | DoWhile (a, b) -> this.VisitDoWhile (a, b)
        | For (a, b, c, d) -> this.VisitFor (a, b, c, d)
        | ForIn (a, b, c) -> this.VisitForIn (a, b, c)
        | Switch (a, b) -> this.VisitSwitch (a, b)
        | If (a, b, c) -> this.VisitIf (a, b, c)
        | Throw a -> this.VisitThrow a
        | TryWith (a, b, c) -> this.VisitTryWith (a, b, c)
        | TryFinally (a, b) -> this.VisitTryFinally (a, b)
        | Labeled (a, b) -> this.VisitLabeled (a, b)
        | StatementSourcePos (a, b) -> this.VisitStatementSourcePos (a, b)
        | Goto a -> this.VisitGoto a
        | Continuation (a, b) -> this.VisitContinuation (a, b)
        | Yield a -> this.VisitYield a
        | CSharpSwitch (a, b) -> this.VisitCSharpSwitch (a, b)
        | GotoCase a -> this.VisitGotoCase a
        | Statements a -> this.VisitStatements a
    abstract VisitId : Id -> unit
    override this.VisitId x = ()

[<AutoOpen>]
module ExtraForms =
    let Lambda (a, b) = Function (a, Return b)
    let CurriedLambda (a, b) = List.foldBack (fun a b -> Function ([a], Return b)) a b


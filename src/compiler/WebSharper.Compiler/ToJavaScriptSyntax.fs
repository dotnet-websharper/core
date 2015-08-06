module WebSharper.Compiler.Common.ToJavaScriptSyntax

module J = WebSharper.Core.JavaScript.Syntax

open WebSharper.Core
open WebSharper.Core.AST

type Environment =
    {
//        CaughtException : option<Expr<unit, unit>> // TODO
        mutable ScopeVars : Set<string>
        mutable ScopeIds : Map<Id, string>
    }
    static member New() =
        {
//            CaughtException = None
            ScopeVars = Set.empty
            ScopeIds = Map.empty
        }

    member this.Clone() =
        {
//            CaughtException = this.    
            ScopeVars = this.ScopeVars
            ScopeIds = this.ScopeIds
        }

let undef = J.Var "undefined"
let glob = J.Var "Global"

let transformId (env: Environment) (id: Id) =
    Map.find id env.ScopeIds

let defineId (env: Environment) (id: Id) =
    let vars = env.ScopeVars
    let rec add name =
        if vars |> Set.contains name then
            add (Resolve.newName name)
        else
            env.ScopeVars <- vars |> Set.add name
            env.ScopeIds <- env.ScopeIds |> Map.add id name
            name
    add (id.Name |> Option.fill "x")
       
let invalidForm() =
    failwith "invalid form at writing JavaScript"
    
let rec transformExpr (env: Environment) (expr: Expression) : J.Expression =
    let inline trE x = transformExpr env x
    let inline trI x = transformId env x
    match expr with
    | Undefined -> undef
    | This -> J.This
//        | Global -> glob
    | Var id -> J.Var (trI id)
    | Value v ->
        match v with
        | Null     -> J.Literal.Null
        | Bool   v -> if v then J.Literal.True else J.Literal.False
        | Byte   v -> J.Number (string v)
        | Char   v -> J.Number (string (int v))
        | Double v -> J.Number (string v)
        | Int    v -> J.Number (string v)
        | Int16  v -> J.Number (string v)
        | Int64  v -> J.Number (string v)
        | SByte  v -> J.Number (string v)
        | Single v -> J.Number (string v)
        | String v -> J.String v
        | UInt16 v -> J.Number (string v)
        | UInt32 v -> J.Number (string v)
        | UInt64 v -> J.Number (string v)
        |> J.Constant
    | Application (e, ps) -> J.Application (trE e, ps |> List.map trE)
    | VarSet (id, e) -> J.Binary(J.Var (trI id), J.BinaryOperator.``=``, trE e )  
    | ExprSourcePos (_, e) -> trE e // TODO
    | Function (ids, b) ->
        let innerEnv = env.Clone()
        let args = ids |> List.map (defineId innerEnv) 
        let body =
            match b |> transformStatement innerEnv with
            | J.Block b -> b |> List.map J.Action
            | b -> b |> J.Action |> List.singleton   
        J.Lambda(None, args, body)
    | ItemGet (x, y) -> (trE x).[trE y]
    | Binary (x, y, z) ->
        match y with
        | BinaryOperator.``!==``        -> J.Binary(trE x, J.BinaryOperator.``!==``       , trE z)
        | BinaryOperator.``!=``         -> J.Binary(trE x, J.BinaryOperator.``!=``        , trE z)
        | BinaryOperator.``%``          -> J.Binary(trE x, J.BinaryOperator.``%``         , trE z)
        | BinaryOperator.``&&``         -> J.Binary(trE x, J.BinaryOperator.``&&``        , trE z)
        | BinaryOperator.``&``          -> J.Binary(trE x, J.BinaryOperator.``&``         , trE z)
        | BinaryOperator.``*``          -> J.Binary(trE x, J.BinaryOperator.``*``         , trE z)
        | BinaryOperator.``+``          -> J.Binary(trE x, J.BinaryOperator.``+``         , trE z)
        | BinaryOperator.``,``          -> J.Binary(trE x, J.BinaryOperator.``,``         , trE z)
        | BinaryOperator.``-``          -> J.Binary(trE x, J.BinaryOperator.``-``         , trE z)
        | BinaryOperator.``.``          -> J.Binary(trE x, J.BinaryOperator.``.``         , trE z)
        | BinaryOperator.``/``          -> J.Binary(trE x, J.BinaryOperator.``/``         , trE z)
        | BinaryOperator.``<<``         -> J.Binary(trE x, J.BinaryOperator.``<<``        , trE z)
        | BinaryOperator.``<=``         -> J.Binary(trE x, J.BinaryOperator.``<=``        , trE z)
        | BinaryOperator.``<``          -> J.Binary(trE x, J.BinaryOperator.``<``         , trE z)
        | BinaryOperator.``===``        -> J.Binary(trE x, J.BinaryOperator.``===``       , trE z)
        | BinaryOperator.``==``         -> J.Binary(trE x, J.BinaryOperator.``==``        , trE z)
        | BinaryOperator.``=``          -> J.Binary(trE x, J.BinaryOperator.``=``         , trE z)
        | BinaryOperator.``>=``         -> J.Binary(trE x, J.BinaryOperator.``>=``        , trE z)
        | BinaryOperator.``>>>``        -> J.Binary(trE x, J.BinaryOperator.``>>>``       , trE z)
        | BinaryOperator.``>>``         -> J.Binary(trE x, J.BinaryOperator.``>>``        , trE z)
        | BinaryOperator.``>``          -> J.Binary(trE x, J.BinaryOperator.``>``         , trE z)
        | BinaryOperator.``^``          -> J.Binary(trE x, J.BinaryOperator.``^``         , trE z)
        | BinaryOperator.``in``         -> J.Binary(trE x, J.BinaryOperator.``in``        , trE z)
        | BinaryOperator.``instanceof`` -> J.Binary(trE x, J.BinaryOperator.``instanceof``, trE z)
        | BinaryOperator.``|``          -> J.Binary(trE x, J.BinaryOperator.``|``         , trE z)
        | BinaryOperator.``||``         -> J.Binary(trE x, J.BinaryOperator.``||``        , trE z)
        | _ -> failwith "invalid BinaryOperator enum value"
    | ItemSet(x, y, z) -> (trE x).[trE y] ^= trE z
    | MutatingBinary(x, y, z) ->
        match y with
        | MutatingBinaryOperator.``=``    -> J.Binary(trE x, J.BinaryOperator.``=``    , trE z)
        | MutatingBinaryOperator.``+=``   -> J.Binary(trE x, J.BinaryOperator.``+=``   , trE z)
        | MutatingBinaryOperator.``-=``   -> J.Binary(trE x, J.BinaryOperator.``-=``   , trE z)
        | MutatingBinaryOperator.``*=``   -> J.Binary(trE x, J.BinaryOperator.``*=``   , trE z)
        | MutatingBinaryOperator.``/=``   -> J.Binary(trE x, J.BinaryOperator.``/=``   , trE z)
        | MutatingBinaryOperator.``%=``   -> J.Binary(trE x, J.BinaryOperator.``%=``   , trE z)
        | MutatingBinaryOperator.``&=``   -> J.Binary(trE x, J.BinaryOperator.``&=``   , trE z)
        | MutatingBinaryOperator.``^=``   -> J.Binary(trE x, J.BinaryOperator.``^=``   , trE z)
        | MutatingBinaryOperator.``|=``   -> J.Binary(trE x, J.BinaryOperator.``|=``   , trE z)
        | MutatingBinaryOperator.``<<=``  -> J.Binary(trE x, J.BinaryOperator.``<<=``  , trE z)
        | MutatingBinaryOperator.``>>=``  -> J.Binary(trE x, J.BinaryOperator.``>>=``  , trE z)
        | MutatingBinaryOperator.``>>>=`` -> J.Binary(trE x, J.BinaryOperator.``>>>=`` , trE z)
    | Object fs -> J.NewObject (fs |> List.map (fun (k, v) -> k, trE v))
    | GlobalAccess x -> glob |> List.foldBack (fun n e -> e.[!~(J.String n)]) x.Value
    | New (x, y) -> J.New(trE x, y |> List.map trE)
    | Sequential x ->
        x |> List.map trE |> List.reduce (fun a b -> J.Binary(a, J.BinaryOperator.``,``, b))
    | Conditional (cond, then_, else_) ->
        J.Conditional(trE cond, trE then_, trE else_)
    | NewArray arr -> J.NewArray (List.map (trE >> Some) arr)
    | Unary(x, y) ->
        match x with
        | UnaryOperator.``!``    -> J.Unary(J.UnaryOperator.``!``, trE y)
        | UnaryOperator.``void`` -> J.Unary(J.UnaryOperator.``void``, trE y)
        | UnaryOperator.``+``    -> J.Unary(J.UnaryOperator.``+``, trE y)
        | UnaryOperator.``-``    -> J.Unary(J.UnaryOperator.``-``, trE y)
        | UnaryOperator.``~``    -> J.Unary(J.UnaryOperator.``~``, trE y)
        | UnaryOperator.typeof   -> J.Unary(J.UnaryOperator.typeof, trE y)
        | _ -> failwith "invalid UnaryOperator enum value"
    | MutatingUnary(x, y) ->
        match x with
        | MutatingUnaryOperator.``()++`` -> J.Postfix(trE y, J.PostfixOperator.``++``)
        | MutatingUnaryOperator.``()--`` -> J.Postfix(trE y, J.PostfixOperator.``--``)
        | MutatingUnaryOperator.``++()`` -> J.Unary(J.UnaryOperator.``++``, trE y)
        | MutatingUnaryOperator.``--()`` -> J.Unary(J.UnaryOperator.``--``, trE y)
        | MutatingUnaryOperator.delete   -> J.Unary(J.UnaryOperator.delete, trE y)
    | Hole _
    | FieldGet _
    | FieldSet _
    | Let _
    | LetRec _
    | StatementExpr _
    | Await _
    | NamedParameter _
    | RefOrOutParameter _
    | Call _
    | Ctor _ 
    | CCtor _ 
    | NewVar _ 
    | Coalesce _ 
    | LetRec _
    | TypeCheck _ -> invalidForm()

and transformStatement (env: Environment) (statement: Statement) : J.Statement =
    let inline trE x = transformExpr env x
    let inline trS x = transformStatement env x
    match statement with
    | Empty -> J.Empty
    | Break(a) -> J.Break (a |> Option.map (fun l -> l.Name.Value))
    | Continue(a) -> J.Continue (a |> Option.map (fun l -> l.Name.Value))
    | ExprStatement e -> J.Ignore(trE e)
    | Block s -> J.Block(s |> List.map trS)
    | StatementSourcePos (_, s) -> trS s // TODO  
    | If(a, b, c) -> J.If(trE a, trS b, trS c)
    | Return a -> J.Return (match a with Undefined -> None | _ -> Some (trE a))
    | VarDeclaration (id, e) ->
        J.Vars ([defineId env id, match e with Undefined -> None | _ -> Some (trE e)])
    | While(a, b) -> J.While (trE a, trS b)
    | DoWhile(a, b) -> J.Do (trS a, trE b)
    | For(a, b, c, d) -> J.For(Option.map trE a, Option.map trE b, Option.map trE c, trS d)
    | Switch(a, b) -> 
        J.Switch(trE a, b |> List.map (fun (l, s) -> match l with Some l -> J.SwitchElement.Case (trE l, [trS s]) | _ -> J.SwitchElement.Default [trS s]))
    | Throw(a) -> J.Throw (trE a)
    | Labeled(a, b) -> J.Labelled(a.Name.Value, trS b)
    | TryWith(a, b, c) -> 
        J.TryWith(trS a, defineId env (b |> Option.fallback Id), trS c, None)
    | TryFinally(a, b) ->
        J.TryFinally(trS a, trS b)
    | ForIn(a, b, c) -> J.ForVarIn(defineId env a, None, trE b, trS c)
    | Goto(_) -> failwith "Not implemented yet"
    | Yield(_) -> failwith "Not implemented yet"
    | CSharpSwitch(_, _) -> failwith "Not implemented yet"
    | GotoCase(_) -> failwith "Not implemented yet"
    | Statements a -> J.Block (a |> List.map trS) // TODO //  "Not implemented yet"
        
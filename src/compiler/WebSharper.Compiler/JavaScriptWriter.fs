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

// Transforms WebSharper.Core.AST into WebSharper.JavaScript.Syntax
// used for writing .js files
module WebSharper.Compiler.JavaScriptWriter

module J = WebSharper.Core.JavaScript.Syntax
module I = WebSharper.Core.JavaScript.Identifier

open WebSharper.Core
open WebSharper.Core.AST

type P = WebSharper.Core.JavaScript.Preferences

type Environment =
    {
        Preference : WebSharper.Core.JavaScript.Preferences
        mutable ScopeNames : Set<string>
        mutable CompactVars : int
        mutable ScopeIds : Map<Id, string>
        ScopeFuncs : ResizeArray<J.ProgramElement>
        ScopeVars : ResizeArray<J.Id>
    }
    static member New(pref) =
        {
            Preference = pref    
            ScopeNames = Set.empty
            CompactVars = 0 
            ScopeIds = Map.empty   
            ScopeFuncs = ResizeArray()
            ScopeVars = ResizeArray()
        }

    member this.NewInner() =
        {
            Preference = this.Preference    
            ScopeNames = this.ScopeNames
            CompactVars = this.CompactVars
            ScopeIds = this.ScopeIds
            ScopeFuncs = ResizeArray()
            ScopeVars = ResizeArray()
        }

    member this.Declarations =
        if this.ScopeVars.Count = 0 then [] else
            [ J.Action (J.Vars (this.ScopeVars |> Seq.map (fun v -> v, None) |> List.ofSeq)) ]
        @ List.ofSeq this.ScopeFuncs
        
let undef = J.Unary(J.UnaryOperator.``void``, J.Constant (J.Literal.Number "0"))

let transformId (env: Environment) (id: Id) =
    try Map.find id env.ScopeIds
    with _ -> 
//        "MISSINGVAR" + I.MakeValid (defaultArg id.Name "_")
        failwithf "Undefined variable during writing JavaScript: %s" (defaultArg id.Name "(noname)")

let formatter = WebSharper.Core.JavaScript.Identifier.MakeFormatter()

let defineId (env: Environment) (id: Id) =
    if env.Preference = P.Compact then
        let name = formatter env.CompactVars    
        env.CompactVars <- env.CompactVars + 1   
        env.ScopeIds <- env.ScopeIds |> Map.add id name
        env.ScopeVars.Add(name)
        name 
    else 
        let vars = env.ScopeNames
        let mutable name = (I.MakeValid (defaultArg id.Name "$1"))
        while vars |> Set.contains name do
            name <- Resolve.newName name 
        env.ScopeNames <- vars |> Set.add name
        env.ScopeIds <- env.ScopeIds |> Map.add id name
        env.ScopeVars.Add(name)
        name
       
let invalidForm c =
    failwithf "invalid form at writing JavaScript: %s" c

type CollectVariables(env: Environment) =
    inherit StatementVisitor()

    override this.VisitFuncDeclaration(f, _, _) =
        defineId env f |> ignore    

    override this.VisitVarDeclaration(v, _) =
        defineId env v |> ignore

let rec transformExpr (env: Environment) (expr: Expression) : J.Expression =
    let inline trE x = transformExpr env x
    let inline trI x = transformId env x
    match expr with
    | Undefined -> undef
    | This -> J.This
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
        | Decimal v -> J.Number (string v)
        |> J.Constant
    | Application (e, ps, _, _) -> J.Application (trE e, ps |> List.map trE)
    | VarSet (id, e) -> J.Binary(J.Var (trI id), J.BinaryOperator.``=``, trE e)   
    | ExprSourcePos (pos, e) -> 
        let jpos =
            {
                File = pos.FileName
                Line = fst pos.Start
                Column = snd pos.Start
                EndLine = fst pos.End
                EndColumn = snd pos.End
            } : J.SourcePos
        J.ExprPos (trE e, jpos)
    | Function (ids, b) ->
        let innerEnv = env.NewInner()
        let args = ids |> List.map (defineId innerEnv) 
        CollectVariables(innerEnv).VisitStatement(b)
        let body =
            match b |> transformStatement innerEnv with
            | J.Block b -> 
                match List.rev b with
                | J.Return None :: more -> List.rev more
                | _ -> b
                |> List.map J.Action
            | J.Empty
            | J.Return None -> []
            | b -> [ b |> J.Action ]
        J.Lambda(None, args, innerEnv.Declarations @ body)
    | ItemGet (x, y) 
    | ItemGetNonPure (x, y) 
        -> (trE x).[trE y]
    | Binary (x, y, z) ->
        match y with
        | BinaryOperator.``!==``        -> J.Binary(trE x, J.BinaryOperator.``!==``       , trE z)
        | BinaryOperator.``!=``         -> J.Binary(trE x, J.BinaryOperator.``!=``        , trE z)
        | BinaryOperator.``%``          -> J.Binary(trE x, J.BinaryOperator.``%``         , trE z)
        | BinaryOperator.``&&``         -> J.Binary(trE x, J.BinaryOperator.``&&``        , trE z)
        | BinaryOperator.``&``          -> J.Binary(trE x, J.BinaryOperator.``&``         , trE z)
        | BinaryOperator.``*``          -> J.Binary(trE x, J.BinaryOperator.``*``         , trE z)
        | BinaryOperator.``+``          -> J.Binary(trE x, J.BinaryOperator.``+``         , trE z)
        | BinaryOperator.``-``          -> J.Binary(trE x, J.BinaryOperator.``-``         , trE z)
        | BinaryOperator.``/``          -> J.Binary(trE x, J.BinaryOperator.``/``         , trE z)
        | BinaryOperator.``<<``         -> J.Binary(trE x, J.BinaryOperator.``<<``        , trE z)
        | BinaryOperator.``<=``         -> J.Binary(trE x, J.BinaryOperator.``<=``        , trE z)
        | BinaryOperator.``<``          -> J.Binary(trE x, J.BinaryOperator.``<``         , trE z)
        | BinaryOperator.``===``        -> J.Binary(trE x, J.BinaryOperator.``===``       , trE z)
        | BinaryOperator.``==``         -> J.Binary(trE x, J.BinaryOperator.``==``        , trE z)
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
        | _ -> failwith "invalid MutatingBinaryOperator enum value"
    | Object fs -> J.NewObject (fs |> List.map (fun (k, v) -> k, trE v))
    | New (x, y) -> J.New(trE x, y |> List.map trE)
    | Sequential x ->
        let x =
            match List.rev x with 
            | [] | [_] -> x
            | h :: t ->
                h :: (t |> List.map (function (IgnoreSourcePos.Unary(UnaryOperator.``void``, a)) | a -> a))  
                |> List.rev
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
        | _ -> failwith "invalid MutatingUnaryOperator enum value"
    | _ -> 
        failwithf "Not in JavaScript form: %A" (RemoveSourcePositions().TransformExpression(expr))
        invalidForm (GetUnionCaseName expr)

and transformStatement (env: Environment) (statement: Statement) : J.Statement =
    let inline trE x = transformExpr env x
    let inline trS x = transformStatement env x
    let sequential s effect =
        match List.rev s with
        | h :: t -> effect h :: List.map ExprStatement t |> List.rev          
        | [] -> []
    let sequentialE s =
        sequential s (function IgnoreSourcePos.Unary(UnaryOperator.``void``, e) | e -> ExprStatement e)    
    let flatten s =
        let res = ResizeArray()
        let rec add a =
            match IgnoreStatementSourcePos a with 
            | Block b -> b |> List.iter add
            | FuncDeclaration _ ->
                trS a |> ignore    
            | Empty 
            | ExprStatement IgnoreSourcePos.Undefined -> ()
            | ExprStatement (IgnoreSourcePos.Sequential s) ->
                sequentialE s |> List.iter add
            | Return (IgnoreSourcePos.Sequential s) ->
                sequential s Return |> List.iter add
            | Throw (IgnoreSourcePos.Sequential s) ->
                sequential s Throw |> List.iter add
            | _ -> 
                res.Add(trS a)
        s |> List.iter add
        List.ofSeq res    
    let flattenS s =
        match IgnoreStatementSourcePos s with
        | Block s -> flatten s
        | _ -> [ trS s ]
    match statement with
    | Empty -> J.Empty
    | Break(a) -> J.Break (a |> Option.map (fun l -> l.Name.Value))
    | Continue(a) -> J.Continue (a |> Option.map (fun l -> l.Name.Value))
    | ExprStatement (IgnoreSourcePos.Unary(UnaryOperator.``void``, (IgnoreSourcePos.Sequential s)))
    | ExprStatement (IgnoreSourcePos.Sequential s) -> J.Block (sequentialE s |> List.map trS)
    | ExprStatement (IgnoreSourcePos.Unary(UnaryOperator.``void``, e))
    | ExprStatement e -> J.Ignore(trE e)
    | Block s -> J.Block (flatten s)
    | StatementSourcePos (pos, s) -> 
        let jpos =
            {
                File = pos.FileName
                Line = fst pos.Start
                Column = snd pos.Start
                EndLine = fst pos.End
                EndColumn = snd pos.End
            } : J.SourcePos
        J.StatementPos (trS s, jpos)
    | If(a, b, c) -> J.If(trE a, trS b, trS c)
    | Return (IgnoreSourcePos.Unary(UnaryOperator.``void``, a)) -> J.Ignore(trE a)
    | Return (IgnoreSourcePos.Sequential s) -> J.Block (sequential s Return |> List.map trS)
    | Return IgnoreSourcePos.Undefined -> J.Return None
    | Return a -> J.Return (Some (trE a))
    | VarDeclaration (id, e) ->
        match e with
        | IgnoreSourcePos.Undefined -> J.Empty 
        | _ -> J.Ignore(J.Binary(J.Var (transformId env id), J.BinaryOperator.``=``, trE e))
    | FuncDeclaration (x, ids, b) ->
        let id = transformId env x
        let innerEnv = env.NewInner()
        let args = ids |> List.map (defineId innerEnv) 
        CollectVariables(innerEnv).VisitStatement(b)
        let body =
            match b |> transformStatement innerEnv with
            | J.Block b -> 
                match List.rev b with
                | J.Return None :: more -> List.rev more
                | _ -> b
                |> List.map J.Action
            | J.Empty
            | J.Return None -> []
            | b -> [ b |> J.Action ]
        J.Function(id, args, innerEnv.Declarations @ body) |> env.ScopeFuncs.Add
        J.Empty
    | While(a, b) -> J.While (trE a, trS b)
    | DoWhile(a, b) -> J.Do (trS a, trE b)
    | For(a, b, c, d) -> J.For(Option.map trE a, Option.map trE b, Option.map trE c, trS d)
    | Switch(a, b) -> 
        J.Switch(trE a, 
            b |> List.map (fun (l, s) -> 
                match l with 
                | Some l -> J.SwitchElement.Case (trE l, flattenS s) 
                | _ -> J.SwitchElement.Default (flattenS s)
            )
        )
    | Throw (IgnoreSourcePos.Sequential s) -> J.Block (sequential s Throw |> List.map trS)
    | Throw(a) -> J.Throw (trE a)
    | Labeled(a, b) -> J.Labelled(a.Name.Value, trS b)
    | TryWith(a, b, c) -> 
        J.TryWith(trS a, defineId env (match b with Some b -> b | _ -> Id.New()), trS c, None)
    | TryFinally(a, b) ->
        J.TryFinally(trS a, trS b)
    | ForIn(a, b, c) -> J.ForVarIn(defineId env a, None, trE b, trS c)
    | _ -> 
        failwithf "Not in JavaScript form: %A" (RemoveSourcePositions().TransformStatement(statement))
        invalidForm (GetUnionCaseName statement)

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
open System.Collections.Generic

type P = WebSharper.Core.JavaScript.Preferences

type Environment =
    {
        Preference : WebSharper.Core.JavaScript.Preferences
        mutable ScopeNames : Set<string>
        mutable CompactVars : int
        mutable ScopeIds : Map<Id, string>
        ScopeVars : ResizeArray<string>
        FuncDecls : ResizeArray<J.Statement>
        mutable InFuncScope : bool
        OuterScope : bool
        UsedLabels : HashSet<Id>
        Namespaces : Dictionary<string, Environment> 
    }
    static member New(pref) =
        {
            Preference = pref    
            ScopeNames = Set [ "window" ] 
            CompactVars = 0 
            ScopeIds = Map [ Id.Global(), "window" ] 
            ScopeVars = ResizeArray()
            FuncDecls = ResizeArray()
            InFuncScope = false
            OuterScope = true
            UsedLabels = HashSet()
            Namespaces = Dictionary()
        }

    member this.NewInner(isNs, ?cg) =
        {
            Preference = this.Preference    
            ScopeNames = this.ScopeNames
            CompactVars = this.CompactVars
            ScopeIds = this.ScopeIds
            ScopeVars = ResizeArray()
            FuncDecls = ResizeArray()
            InFuncScope = true
            OuterScope = isNs
            UsedLabels = HashSet()
            Namespaces = Dictionary()
        }

    member this.Declarations =
        if this.ScopeVars.Count = 0 then [] else
            [ J.Vars (this.ScopeVars |> Seq.map (fun v -> J.Id.New v, None) |> List.ofSeq) ]
        
let undef = J.Unary(J.UnaryOperator.``void``, J.Constant (J.Literal.Number "0"))

let transformId (env: Environment) (id: Id) =
    if id.HasStrongName then J.Id.New id.Name.Value else
    try 
        J.Id.New (Map.find id env.ScopeIds) 
    with _ -> 
        //J.Id.New ("MISSINGVAR" + I.MakeValid (defaultArg id.Name "_"))
        failwithf "Undefined variable during writing JavaScript: %s" (string id)

let transformLabel (env: Environment) (id: Id) =
    env.UsedLabels.Add id |> ignore
    (transformId env id).Name

let formatter = WebSharper.Core.JavaScript.Identifier.MakeFormatter()

let getCompactName (env: Environment) =
    let vars = env.ScopeNames
    let mutable name = formatter env.CompactVars   
    env.CompactVars <- env.CompactVars + 1   
    while vars |> Set.contains name do
        name <- formatter env.CompactVars   
        env.CompactVars <- env.CompactVars + 1   
    name

type IdKind =
    | DeclarationId
    | ArgumentId
    | InnerId

let defineId (env: Environment) kind (id: Id) =
    if id.HasStrongName then J.Id.New(id.Name.Value, id.IsOptional) else
    let addToDecl, isParam =
        match kind with
        | DeclarationId -> false, false
        | ArgumentId -> false, true
        | InnerId -> true, false
    let n =
        if env.Preference = P.Compact then
            let name = getCompactName env    
            env.ScopeIds <- env.ScopeIds |> Map.add id name
            if addToDecl then env.ScopeVars.Add(name)
            name 
        else 
            let vars = env.ScopeNames
            let mutable name = (I.MakeValid (defaultArg id.Name "$1"))
            while vars |> Set.contains name do
                name <- Resolve.newName name 
            env.ScopeNames <- vars |> Set.add name
            env.ScopeIds <- env.ScopeIds |> Map.add id name
            if addToDecl then env.ScopeVars.Add(name)
            name
    J.Id.New(n, id.IsOptional)
       
let invalidForm c =
    failwithf "invalid form at writing JavaScript: %s" c

type CollectStrongNames(env: Environment) =
    inherit StatementVisitor()

    let addName n =
        env.ScopeNames <- env.ScopeNames.Add n

    let addId (i: Id) =
        if i.HasStrongName then addName i.Name.Value
        
    override this.VisitFuncDeclaration(f, _, _) =
        addId f

    override this.VisitVarDeclaration(v, _) =
        addId v

    override this.VisitNamespace(n, s) =
        addName n
        s |> List.iter this.VisitStatement

    override this.VisitClass(n, _, _, _, _) =
        addName n

type CollectVariables(env: Environment) =
    inherit StatementVisitor()

    override this.VisitFuncDeclaration(f, _, _) =
        defineId env ArgumentId f |> ignore    

    override this.VisitVarDeclaration(v, _) =
        defineId env InnerId v |> ignore

    override this.VisitImportAll(n, _) =
        n |> Option.iter (defineId env DeclarationId >> ignore)

    override this.VisitNamespace(n, s) =
        let innerEnv =
            match env.Namespaces.TryGetValue n with
            | true, innerEnv -> innerEnv
            | _ ->
                let innerEnv = env.NewInner(true)
                env.Namespaces.Add(n, innerEnv)
                innerEnv
        let collect = CollectVariables(innerEnv)
        s |> List.iter collect.VisitStatement

    override this.VisitLabeled(l, s) =
        defineId env DeclarationId l |> ignore
        this.VisitStatement s

let flattenJS s =
    let res = ResizeArray()
    let rec add a =
        match J.IgnoreStatementPos a with
        | J.Block b -> b |> List.iter add
        | J.Empty -> ()
        | _ -> res.Add a
    s |> Seq.iter add
    List.ofSeq res    

let flattenFuncBody s =
    let res = ResizeArray()
    let mutable go = true
    let mutable throws = false
    let rec add a =
        if go then 
            match J.IgnoreStatementPos a with
            | J.Block b -> b |> List.iter add
            | J.Empty -> ()
            | J.Return None ->
                go <- false
            | J.Return _ ->
                go <- false
                res.Add a
            | J.Throw _ ->
                throws <- true
                go <- false
                res.Add a
            | _ -> res.Add a
    add s
    throws, List.ofSeq res    

let block s = J.Block (flattenJS s)

let rec transformExpr (env: Environment) (expr: Expression) : J.Expression =
    let inline trE x = transformExpr env x
    let inline trI x = transformId env x
    match expr with
    | Undefined -> undef
    | This -> J.This
    | Base -> J.Super
    | Arguments -> J.Var (J.Id.New "arguments")
    | Var id -> 
        if id.IsGlobal() then
            J.Cast(J.Var (J.Id.New "any"), J.Var (trI id))
        else
            J.Var (trI id)
    | Value v ->
        match v with
        | Null     -> J.Literal.Null
        | Bool   v -> if v then J.Literal.True else J.Literal.False
        | Byte   v -> J.Number (string v)
        | Char   v -> J.String (string v)
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
        | Decimal _ -> failwith "Cannot write Decimal directly to JavaScript output"
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
        J.ExprPos (J.IgnoreExprPos(trE e), jpos)
    | Function (ids, b) ->
        let innerEnv = env.NewInner(false)
        let args = ids |> List.map (defineId innerEnv ArgumentId) 
        CollectVariables(innerEnv).VisitStatement(b)
        let _, body = b |> transformStatement innerEnv |> flattenFuncBody
        let hasNoThis = HasNoThisVisitor().Check(b)
        J.Lambda(None, args, flattenJS (innerEnv.Declarations @ body), hasNoThis)
    | ItemGet (x, y, _) 
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
    | Cast (t, e) ->
        J.Cast(transformType env t, trE e)
    | _ -> 
        invalidForm (GetUnionCaseName expr)

and transformStatement (env: Environment) (statement: Statement) : J.Statement =
    let inline trE x = transformExpr env x
    let inline trS x = transformStatement env x
    let inline trT x = transformType env x
    let sequential s effect =
        match List.rev s with
        | h :: t -> effect h :: List.map ExprStatement t |> List.rev          
        | [] -> []
    let sequentialE s =
        sequential s (function IgnoreSourcePos.Unary(UnaryOperator.``void``, e) | e -> ExprStatement e)    
    let flatten s =
        let res = ResizeArray()
        let mutable go = true 
        let rec add a =
            if go then 
                match IgnoreStatementSourcePos a with 
                | Block b -> b |> List.iter add
                | Empty 
                | ExprStatement IgnoreSourcePos.Undefined -> ()
                | ExprStatement (IgnoreSourcePos.Sequential s) ->
                    sequentialE s |> List.iter add
                | Return (IgnoreSourcePos.Sequential s) ->
                    sequential s Return |> List.iter add
                    go <- false
                | Throw (IgnoreSourcePos.Sequential s) ->
                    sequential s Throw |> List.iter add
                    go <- false
                | Return _ 
                | Throw _
                | Break _
                | Continue _ ->
                    res.Add(trS a)
                    go <- false
                | _ -> 
                    res.Add(trS a)
        s |> List.iter add
        List.ofSeq res    
    // collect function declarations to be on top level of functions to satisfy strict mode
    // requirement by some JavaScript engines
    let withFuncDecls f =
        if env.InFuncScope then
            env.InFuncScope <- false
            let woFuncDecls = f()
            env.InFuncScope <- true
            if env.FuncDecls.Count > 0 then
                let res = block (Seq.append env.FuncDecls (Seq.singleton woFuncDecls))
                env.FuncDecls.Clear()
                res
            else woFuncDecls
        else 
            f()
    let varDeclaration id e t =
        let i = transformId env id
        let i = match t with Some t -> withType env t i | _ -> i
        if env.OuterScope then
            match e with
            | IgnoreSourcePos.Var o when o.HasStrongName && o.Name.Value = i.Name -> J.Empty
            | IgnoreSourcePos.Undefined -> J.Vars [ i, None ]
            | _ -> J.Vars [ i, Some (trE e) ]
        else
            match e with
            | IgnoreSourcePos.Undefined -> J.Empty 
            | _ -> J.Ignore(J.Binary(J.Var i, J.BinaryOperator.``=``, trE e))
    let funcDeclaration x ids b t =
        let innerEnv = env.NewInner(false)
        try
            let id = transformId env x
            let t, gen = 
                match t with
                | Some t -> 
                    let t, gen = getGenericParams env t
                    Some t, gen
                | _ -> None, ""
            let id =
                if gen = "" then id else
                    { id with Name = id.Name + gen }
            let id, args =
                match t with
                | Some (TSType.Lambda (ta, tr)) ->      
                        id |> withType env tr
                        , (ids, ta) ||> List.map2 (fun i t -> defineId innerEnv ArgumentId i |> withType env t) 
                | _ ->
                    id
                    , ids |> List.map (defineId innerEnv ArgumentId) 
            CollectVariables(innerEnv).VisitStatement(b)
            let throws, body = b |> transformStatement innerEnv |> flattenFuncBody
            let id = if throws then id.WithType(J.Var (J.Id.New "never")) else id
            let f = J.Function(id, args, flattenJS (innerEnv.Declarations @ body))
            if env.InFuncScope then
                f
            else
                env.FuncDecls.Add f 
                J.Empty
        with _ ->
            J.Ignore (J.Var (J.Id.New ("WRONGSIGNATURE")))
            //failwithf "incompatible signature %O(%s): %A" x (ids |> Seq.map string |> String.concat ", ") t

    match statement with
    | Empty
        -> J.Empty
    | Break(a) -> J.Break (a |> Option.map (fun l -> transformLabel env l))
    | Continue(a) -> J.Continue (a |> Option.map (fun l -> transformLabel env l))
    | ExprStatement (IgnoreSourcePos.Unary(UnaryOperator.``void``, (IgnoreSourcePos.Sequential s)))
    | ExprStatement (IgnoreSourcePos.Sequential s) -> block (sequentialE s |> List.map trS)
    | ExprStatement (IgnoreSourcePos.Unary(UnaryOperator.``void``, e))
    | ExprStatement e -> J.Ignore(trE e)
    | Block s -> block (flatten s)
    | StatementSourcePos (pos, s) -> 
        let jpos =
            {
                File = pos.FileName
                Line = fst pos.Start
                Column = snd pos.Start
                EndLine = fst pos.End
                EndColumn = snd pos.End
            } : J.SourcePos
        J.StatementPos (J.IgnoreStatementPos (trS s), jpos)
    | If(a, b, c) -> 
        withFuncDecls <| fun () -> 
            J.If(trE a, trS b, trS c)
    | Return (IgnoreSourcePos.Unary(UnaryOperator.``void``, a)) -> block [ J.Ignore(trE a); J.Return None ]
    | Return (IgnoreSourcePos.Sequential s) -> block (sequential s Return |> List.map trS)
    | Return IgnoreSourcePos.Undefined -> J.Return None
    | Return a -> J.Return (Some (trE a))
    | VarDeclaration (id, e) ->
        varDeclaration id e None
    | FuncDeclaration (x, ids, b) ->
        funcDeclaration x ids b None
    | TypedDeclaration(VarDeclaration (id, e), s) ->
        varDeclaration id e (Some s)
    | TypedDeclaration(FuncDeclaration (x, ids, b), s) ->
        funcDeclaration x ids b (Some s) 
    | While(a, b) -> 
        withFuncDecls <| fun () -> 
            J.While (trE a, trS b)
    | DoWhile(a, b) ->
        withFuncDecls <| fun () -> 
            J.Do (trS a, trE b)
    | For(a, b, c, d) -> 
        withFuncDecls <| fun () -> 
            J.For(Option.map trE a, Option.map trE b, Option.map trE c, trS d)
    | Switch(a, b) -> 
        withFuncDecls <| fun () ->
            J.Switch(trE a, 
                b |> List.map (fun (l, s) -> 
                    match l with 
                    | Some l -> J.SwitchElement.Case (trE l, flatten [ s ]) 
                    | _ -> J.SwitchElement.Default (flatten [ s ])
                )
            )
    | Throw (IgnoreSourcePos.Sequential s) -> block (sequential s Throw |> List.map trS)
    | Throw(a) -> J.Throw (trE a)
    | Labeled(a, b) -> 
        withFuncDecls <| fun () -> 
            let tB = trS b
            if env.UsedLabels.Contains a then
                J.Labelled((transformId env a).Name, tB)        
            else tB
    | TryWith(a, b, c) -> 
        withFuncDecls <| fun () ->
            J.TryWith(trS a, defineId env DeclarationId (match b with Some b -> b | _ -> Id.New()), trS c, None)
    | TryFinally(a, b) ->
        withFuncDecls <| fun () ->
            J.TryFinally(trS a, trS b)
    | ForIn(a, b, c) -> 
        withFuncDecls <| fun () ->
            J.ForVarIn(defineId env DeclarationId a, None, trE b, trS c)
    | ImportAll (a, b) ->
        J.ImportAll(a |> Option.map (transformId env), b)
    | Export a ->
        J.Export (trS a)
    | Declare (Namespace ("global", a)) ->
        J.DeclareGlobal (a |> List.map trS)
    | Declare a ->
        J.Declare (trS a)
    | Namespace (a, b) ->
        let innerEnv = env.Namespaces.[a]
        J.Namespace (J.Id.New a, List.map (transformStatement innerEnv) b)
    | Class (n, b, i, m, g) ->
        let innerEnv = env.NewInner(false, g)
        let isAbstract =
            m |> List.exists (function
                | ClassMethod (_, _, _, None, _) -> true
                | _ -> false
            )
        let gen = 
            if List.isEmpty g then "" else
                "<" + (g |> Seq.map (transformTypeName env) |> String.concat ", ") + ">"
        let n = n + gen
        J.Class(J.Id.New n, isAbstract, Option.map trT b, List.map trT i, List.map (transformMember innerEnv) m)
    | Interface (n, e, m, g) ->
        let gen = 
            if List.isEmpty g then "" else
                "<" + (g |> Seq.map (transformTypeName env) |> String.concat ", ") + ">"
        let n = n + gen
        J.Interface(J.Id.New n, List.map trT e, List.map (transformMember env) m)
    | Alias (a, t) ->
        J.TypeAlias(J.Id.New (transformTypeName env a), trT t)
    | XmlComment a ->
        J.StatementComment (J.Empty, a)
    | _ -> 
        invalidForm (GetUnionCaseName statement)

and transformTypeName (env: Environment) (typ: TSType) =
    let inline trN x = transformTypeName env x
    match typ with
    | TSType.Any -> "any"
    | TSType.Basic "Array" ->
        "any[]"
    | TSType.Basic n -> n
    | TSType.Generic (TSType.Basic "Array", [ TSType.Basic n ]) ->
        n + "[]"
    | TSType.Generic (TSType.Basic "Array", [ t ]) ->
        "(" + trN t + ")[]"
    | TSType.Generic (t, g) -> (trN t) + "<" + (g |> Seq.map (trN) |> String.concat ", ")  + ">"
    | TSType.Imported (i, addr) -> (transformId env i).Name + "." + addr
    | TSType.Lambda (a, r)  -> 
        "(" + (a |> Seq.mapi (fun i t -> string ('a' + char i) + ":" + trN t) |> String.concat ", ") + ")"
        + " => " + trN r
    | TSType.New _ -> "any" // TODO constructor signature
    | TSType.Tuple ts -> "[" + (ts |> Seq.map (trN) |> String.concat ", ") + "]"
    | TSType.Union cs -> "(" + (cs |> Seq.map (trN) |> String.concat " | ") + ")"
    | TSType.Intersection cs -> "(" + (cs |> Seq.map (trN) |> String.concat " & ") + ")"
    | TSType.Param n -> "T" + string n
    | TSType.Constraint (t, g) -> trN t + " extends " + (g |> Seq.map trN |> String.concat ", ")

and transformType (env: Environment) (typ: TSType) =
    transformTypeName env typ |> J.Id.New |> J.Var

and withType (env: Environment) (typ: TSType) (i: J.Id) =
    match typ with
    | TSType.Any -> i
    | _ -> i.WithType(transformType env typ)

and getGenericParams (env: Environment) (typ: TSType) =
    match typ with
    | TSType.Generic (t, []) -> t, ""
    | TSType.Generic (t, g) -> t, "<" + (g |> Seq.map (transformTypeName env) |> String.concat ", ") + ">"
    | _ -> typ, ""

and transformMember (env: Environment) (mem: Statement) : J.Member =
    let inline trE x = transformExpr env x
    let inline trS x = transformStatement env x
    match mem with
    | ClassMethod (s, n, p, b, t) ->
        let innerEnv = env.NewInner(false)
        let t, gen =
            match t with
            | TSType.Generic (t, g) -> t, "<" + (g |> Seq.map (transformTypeName env) |> String.concat ", ") + ">"
            | _ -> t, ""
        let args, tr =
            match t with 
            | TSType.Lambda (ta, tr) -> 
                (p, ta) ||> List.map2 (fun a t -> defineId innerEnv ArgumentId a |> withType env t) 
                , tr
            | _ ->
                p |> List.map (defineId innerEnv ArgumentId)
                , t
        let body = 
            b |> Option.map (fun b -> 
                CollectVariables(innerEnv).VisitStatement(b)
                b |> transformStatement innerEnv |> flattenFuncBody
            )
        let n = n + gen
        let id =
            match body with
            | Some (true, _) ->  J.Id.New(n, typ = J.Var (J.Id.New "never"))
            | _ -> J.Id.New(n) |> withType env tr 
        J.Method(s, id, args, body |> Option.map (fun (_, b) -> flattenJS (innerEnv.Declarations @ b)))   
    | ClassConstructor (p, b, t) ->
        let innerEnv = env.NewInner(false)
        let args =
            match t with 
            | TSType.New ta -> 
                (p, ta) ||> List.map2 (fun a t -> defineId innerEnv ArgumentId a |> withType env t) 
            | _ ->
                p |> List.map (defineId innerEnv ArgumentId)
        let body = 
            b |> Option.map (fun b -> 
                CollectVariables(innerEnv).VisitStatement(b)
                b |> transformStatement innerEnv |> flattenFuncBody |> snd
            )
        J.Constructor(args, body |> Option.map (fun b -> flattenJS (innerEnv.Declarations @ b)))   
    | ClassProperty (s, n, t) ->
        J.Property (s, J.Id.New(n) |> withType env t)
    | _ -> 
        invalidForm (GetUnionCaseName mem)

let transformProgram pref statements =
    if List.isEmpty statements then [] else
    let env = Environment.New(pref)
    let cnames = CollectStrongNames(env)
    statements |> List.iter cnames.VisitStatement
    let cvars = CollectVariables(env)
    statements |> List.iter cvars.VisitStatement
    //J.Ignore (J.Constant (J.String "use strict")) ::
    (statements |> List.map (transformStatement env) |> flattenJS)

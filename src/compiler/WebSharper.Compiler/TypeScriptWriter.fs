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
module WebSharper.Compiler.TypeScriptWriter

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
        CurrentScopeNames : HashSet<string>
        mutable CompactVars : int
        mutable ScopeIds : Map<Id, string>
        ScopeVars : ResizeArray<string>
        FuncDecls : ResizeArray<J.Statement>
        mutable InFuncScope : bool
        OuterScope : bool
        UsedLabels : HashSet<Id>
        TopNamespace : option<Environment>
        Namespaces : Dictionary<string, Environment> 
        CurrentNamespaceRev : list<string>
        CurrentNamespace : list<string>
    }
    static member New(pref) =
        {
            Preference = pref    
            ScopeNames = Set [ "window" ] 
            CurrentScopeNames = HashSet()
            CompactVars = 0 
            ScopeIds = Map [ Id.Global(), "window" ] 
            ScopeVars = ResizeArray()
            FuncDecls = ResizeArray()
            InFuncScope = true
            OuterScope = true
            UsedLabels = HashSet()
            TopNamespace = None
            Namespaces = Dictionary()
            CurrentNamespaceRev = []
            CurrentNamespace = []
        }

    member this.NewInner(?ns, ?cg) =
        let isNs = Option.isSome ns
        let nsr = match ns with Some n -> n :: this.CurrentNamespaceRev | _ -> this.CurrentNamespaceRev
        {
            Preference = this.Preference    
            ScopeNames = this.ScopeNames
            CurrentScopeNames = if isNs then HashSet() else this.CurrentScopeNames
            CompactVars = this.CompactVars
            ScopeIds = this.ScopeIds
            ScopeVars = ResizeArray()
            FuncDecls = ResizeArray()
            InFuncScope = true
            OuterScope = isNs
            UsedLabels = HashSet()
            TopNamespace = if isNs then (match this.TopNamespace with None -> Some this | t -> t) else this.TopNamespace
            Namespaces = Dictionary()
            CurrentNamespaceRev = nsr
            CurrentNamespace = if isNs then List.rev nsr else this.CurrentNamespace
        }
        
let undef = J.Unary(J.UnaryOperator.``void``, J.Constant (J.Literal.Number "0"))

let undefVar (id: Id) =
//#if DEBUG
    J.Id.New ("MISSINGVAR" + I.MakeValid (defaultArg id.Name "_"))
//#else
//    failwithf "Undefined variable during writing TypeScript: %s" (string id)
//#endif

let transformId (env: Environment) (id: Id) =
    if id.HasStrongName then J.Id.New id.Name.Value else
    try 
        J.Id.New (Map.find id env.ScopeIds) 
    with _ -> 
        undefVar id

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
       
let defineImportedId (env: Environment) (id: Id) =
    let iname = id.Name.Value
    env.ScopeNames <- env.ScopeNames |> Set.add iname
    if env.Preference = P.Compact then
        let name = getCompactName env    
        env.ScopeIds <- env.ScopeIds |> Map.add id name
    else 
        env.ScopeIds <- env.ScopeIds |> Map.add id iname
       
let invalidForm c =
    failwithf "invalid form at writing TypeScript: %s" c

type CollectStrongNames(env: Environment) =
    inherit StatementVisitor()

    let addName n =
        env.ScopeNames <- env.ScopeNames.Add n

    let addId (i: Id) =
        if i.HasStrongName then addName i.Name.Value
        
    override this.VisitFuncDeclaration(f, _, _, _) =
        addId f

    override this.VisitVarDeclaration(v, _) =
        addId v

    override this.VisitNamespace(n, s) =
        addName n
        s |> List.iter this.VisitStatement

    override this.VisitClass(n, _, _, s, _) =
        addName n
        s |> List.iter this.VisitStatement

    override this.VisitClassMethod(_, n, _, _, _) =
        addName n

    override this.VisitClassProperty(_, n, _, _) =
        addName n

    override this.VisitInterface(n, _, _, _) =
        addName n

type CollectVariables(env: Environment) =
    inherit StatementVisitor()

    override this.VisitFuncDeclaration(f, _, _, _) =
        let i = defineId env ArgumentId f
        env.CurrentScopeNames.Add i.Name |> ignore

    override this.VisitVarDeclaration(v, _) =
        let i = defineId env InnerId v
        env.CurrentScopeNames.Add i.Name |> ignore

    override this.VisitImportAll(n, _) =
        n |> Option.iter (defineId env DeclarationId >> ignore)

    override this.VisitNamespace(n, s) =
        env.CurrentScopeNames.Add n |> ignore
        let innerEnv =
            match env.Namespaces.TryGetValue n with
            | true, innerEnv -> innerEnv
            | _ ->
                let innerEnv = env.NewInner(n)
                env.Namespaces.Add(n, innerEnv)
                innerEnv
        let collect = CollectVariables(innerEnv)
        s |> List.iter collect.VisitStatement

    override this.VisitClass(n, _, _, s, _) =
        env.CurrentScopeNames.Add n |> ignore
        let innerEnv =
            match env.Namespaces.TryGetValue n with
            | true, innerEnv -> innerEnv
            | _ ->
                let innerEnv = env.NewInner(n)
                env.Namespaces.Add(n, innerEnv)
                innerEnv
        let collect = CollectVariables(innerEnv)
        s |> List.iter collect.VisitStatement

    override this.VisitClassMethod(_, n, _, _, _) =
        env.CurrentScopeNames.Add n |> ignore

    override this.VisitClassProperty(_, n, _, _) =
        env.CurrentScopeNames.Add n |> ignore

    override this.VisitInterface(n, _, _, _) =
        env.CurrentScopeNames.Add n |> ignore

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

let flattenFuncBody retTyp s =
    let isVoidOrAny = 
        match retTyp with
        | TSType.Any | TSType.Named [ "void" ] -> true
        | _ -> false
    let res = ResizeArray()
    let mutable go = true
    let mutable throws = false
    let rec add a =
        if go then 
            match J.IgnoreStatementPos a with
            | J.Block b -> b |> List.iter add
            | J.Empty -> ()
            | J.Return None when isVoidOrAny ->
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
    if res.Count = 0 && not isVoidOrAny then
        add (J.Return None)
    throws, List.ofSeq res    

let block s = J.Block (flattenJS s)

let resolveName (env: Environment) name =
    match env.TopNamespace with
    | Some top ->
        // skips namespace names until equal with the current namespace path
        let res, ns, inNS, _ =
            List.fold (fun (a, ns, i, cont) n -> 
                match ns with
                | [ h ] when cont && h = n -> a, [], i, true
                | h :: r when cont && h = n -> a, r, i.Namespaces.[n], true
                | _ -> n :: a, ns, i, false
            ) ([], env.CurrentNamespace, top, true) name
        let res, ns, inNS = 
            // if we would end up with an empty result, use last part of full name, no shadowing check needed
            match res with
            | [] -> [ List.last name ], [], None
            | t -> 
                match ns with
                | [] -> List.rev t, [], Some inNS
                | n :: r -> 
                    // step one namespace inner, so something is not seen as shadowing itself
                    List.rev t, r, inNS.Namespaces.TryFind(n)
        // checks if a resolved name would get shadowed in a namespace lower in the chain
        let rec check (env: Environment) name =
            match name with
            | [ n ] -> env.CurrentScopeNames.Contains n
            | n :: r ->
                env.CurrentScopeNames.Contains n ||
                match env.Namespaces.TryGetValue n with
                | true, innerEnv -> check innerEnv r
                | _ -> false
            | [] -> false
        // we do this check for all namespaces until arriving at current one 
        let rec isShadowed ns inNS =
            check inNS res || 
            match ns with
            | [] -> false
            | n :: r -> isShadowed r inNS.Namespaces.[n]
        if inNS |> Option.exists (isShadowed ns) then  
            name
        else        
            res
    | None -> name

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
        | Null     -> J.Literal.Null |> J.Constant
        | Bool   v -> (if v then J.Literal.True else J.Literal.False) |> J.Constant
        | Byte   v -> J.Number (string v) |> J.Constant
        | Char   v -> J.String (string v) |> J.Constant
        | Double v -> J.Number (string v) |> J.Constant
        | Int    v -> J.Number (string v) |> J.Constant
        | Int16  v -> J.Number (string v) |> J.Constant
        | Int64  v -> J.Number (string v) |> J.Constant
        | SByte  v -> J.Number (string v) |> J.Constant
        | Single v -> J.Number (string v) |> J.Constant
        | String v -> J.String v |> J.Constant
        | UInt16 v -> J.Number (string v) |> J.Constant
        | UInt32 v -> J.Number (string v) |> J.Constant
        | UInt64 v -> J.Number (string v) |> J.Constant
        | ByteArray v -> J.NewArray [ for b in v -> Some (J.Constant (J.Number (string b))) ]
        | UInt16Array v -> J.NewArray [ for b in v -> Some (J.Constant (J.Number (string b))) ]
        | Decimal _ -> failwith "Cannot write Decimal directly to TypeScript output"
    | Application (e, ps, i) ->
        J.Application (trE e, i.Params |> List.map (transformTypeName env false), ps |> List.map trE)
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
    | Function (ids, _, b) ->
        let innerEnv = env.NewInner()
        let args = ids |> List.map (defineIdTyped innerEnv ArgumentId) 
        CollectVariables(innerEnv).VisitStatement(b)
        let _, body = b |> transformStatement innerEnv |> flattenFuncBody TSType.Any
        match flattenJS body with
        | [] -> J.Lambda(None, [], [ J.Return (Some undef) ], true)
        | trB -> 
            let hasNoThis = HasNoThisVisitor().Check(b)
            J.Lambda(None, args, trB, hasNoThis)
        
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
        | BinaryOperator.``**``         -> J.Binary(trE x, J.BinaryOperator.``**``        , trE z)
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
        | BinaryOperator.``??``         -> J.Binary(trE x, J.BinaryOperator.``??``        , trE z)
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
        | MutatingBinaryOperator.``??=``  -> J.Binary(trE x, J.BinaryOperator.``??=`` , trE z)
        | _ -> failwith "invalid MutatingBinaryOperator enum value"
    | Object fs -> J.NewObject (fs |> List.map (fun (k, v) -> k, trE v))
    | New (x, ts, y) -> J.New(trE x, ts |> List.map (transformTypeName env false), y |> List.map trE)
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
    | GlobalAccess a ->
        match a.Module with
        | ImportedModule v ->
            List.foldBack (fun n e -> 
                e.[J.Constant (J.String n)]
            ) a.Address.Value (J.Var (trI v))
        | CurrentModule | JavaScriptFile _ ->
            match a.Address.Value with
            | [] -> J.Var (J.Id.New "window")
            | h :: _ as a ->
            match resolveName env (List.rev a) with
            | [] -> J.Var (J.Id.New h)
            | h :: t -> 
                List.fold (fun (e: J.Expression) n ->
                    e.[J.Constant (J.String n)]
                ) (J.Var (J.Id.New h)) t
        | _ -> 
            failwith "Addresses must be resolved to ImportedModule or CurrentModule before writing TypeScript"
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
        let i = transformId env id
        let typed() =
            match id.TSType with 
            | Some t -> i |> withTypeAny env t
            | _ ->
                match id.TSType with
                | Some t -> i |> withTypeAny env t
                | _ -> i
        match e with
        | IgnoreSourcePos.Var o when o.HasStrongName && o.Name.Value = i.Name -> 
            J.Empty
        | IgnoreSourcePos.Undefined -> 
            J.Vars ([ typed(), None ], J.VarDecl)
        | _ -> 
            J.Vars ([ typed(), Some (trE e) ], J.VarDecl)
    | FuncDeclaration (x, ids, b, gen) ->
        let innerEnv = env.NewInner()
        let id = transformId env x
        let jsgen = gen |> List.map (transformTypeName env true)
        let id = id.WithGenerics(jsgen)
        let args = ids |> List.map (fun id ->
            defineIdTyped innerEnv ArgumentId id
            |> Option.foldBack (withType innerEnv) id.TSType
        )
        let tr = defaultArg x.TSType TSType.Any
        let id = id |> withType innerEnv tr
        CollectVariables(innerEnv).VisitStatement(b)
        let throws, body = b |> transformStatement innerEnv |> flattenFuncBody tr
        let id = if throws then id.WithType(J.Var (J.Id.New "any")) else id // using "never" fixes applications on result in TS 3
        let f = J.Function(id, args, flattenJS body)
        if env.InFuncScope then
            f
        else
            env.FuncDecls.Add f 
            J.Empty
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
        let innerEnv = env.NewInner(cg = g)
        let isAbstract =
            m |> List.exists (function
                | ClassMethod (_, _, _, None, _) -> true
                | _ -> false
            )
        let jsgen = g |> List.map (transformTypeName env true)
        J.Class(J.Id.New(n, gen = jsgen), isAbstract, Option.map trT b, List.map trT i, List.map (transformMember innerEnv) m)
    | Interface (n, e, m, g) ->
        let jsgen = g |> List.map (transformTypeName env true)
        J.Interface(J.Id.New(n, gen = jsgen), List.map trT e, List.map (transformMember env) m)
    | Alias (a, t) ->
        J.TypeAlias(transformTypeName env true a, trT t)
    | XmlComment a ->
        J.StatementComment (J.Empty, a)
    | _ -> 
        invalidForm (GetUnionCaseName statement)

and transformTypeName (env: Environment) (isDeclaringParameter: bool) (typ: TSType) =
    let inline trN x = (transformTypeName env false x).Name
    match typ with
    | TSType.Any -> "any"
    | TSType.Named ["Array"] ->
        "any[]"
    | TSType.Named n -> 
        resolveName env n |> String.concat "."
    | TSType.Generic (TSType.Named ["Array"], [ TSType.Named _ as n ]) ->
        trN n + "[]"
    | TSType.Generic (TSType.Named ["Array"], [ t ]) ->
        "(" + trN t + ")[]"
    | TSType.Generic (t, g) -> (trN t) + "<" + (g |> Seq.map (trN) |> String.concat ", ")  + ">"
    | TSType.Imported (i, n) -> (transformId env i).Name + "." + String.concat "." n 
    | TSType.Importing (m, a) -> failwith "TypeScript type from an unresolved module"
    | TSType.Function (t, a, e, r)  -> 
        let this = t |> Option.map (fun t -> "this: " + trN t) 
        let args = a |> List.mapi (fun i (t, o) -> string ('a' + char i) + (if o then "?:" else ":") + trN t)
        let rest = e |> Option.map (fun t -> "...rest: (" + trN t + ")[]")  
        "((" + (Seq.concat [ Option.toList this; args; Option.toList rest ]  |> String.concat ", ") + ") => " + trN r + ")"
    | TSType.New (a, r)  -> 
        "new (" + (a |> Seq.mapi (fun i t -> string ('a' + char i) + ":" + trN t) |> String.concat ", ") + ")"
        + " => " + trN r
    | TSType.Tuple ts -> "[" + (ts |> Seq.map (trN) |> String.concat ", ") + "]"
    | TSType.Union cs -> "(" + (cs |> Seq.map (trN) |> String.concat " | ") + ")"
    | TSType.Intersection cs -> "(" + (cs |> Seq.map (trN) |> String.concat " & ") + ")"
    | TSType.Param n -> "T" + string n
    | TSType.Constraint (t, g) ->
        if isDeclaringParameter
        then trN t + " extends " + (g |> Seq.map trN |> String.concat ", ")
        else trN t
    | TSType.TypeGuard (i, t) ->
        (transformId env i).Name + " is " + trN t
    | TSType.ObjectOf t ->
        "{[a:string]:" + trN t + "}"
    |> fun n -> J.Id.New(n, typn = true)

and transformType (env: Environment) (typ: TSType) =
    transformTypeName env false typ |> J.Var

and defineIdTyped env kind id =
    let i = defineId env kind id
    match id.TSType with
    | None -> i
    | Some t -> i |> withType env t

and withType (env: Environment) (typ: TSType) (i: J.Id) : J.Id =
    match typ with
    | TSType.Any -> i
    | _ -> i.WithType(transformType env typ)

and withTypeAny (env: Environment) (typ: TSType) (i: J.Id) =
    i.WithType(transformType env typ)

and getGenericParams (env: Environment) (typ: TSType) =
    match typ with
    | TSType.Generic (t, []) -> t, []
    | TSType.Generic (t, g) -> t, g |> List.map (transformTypeName env true)
    | _ -> typ, []

and transformMember (env: Environment) (mem: Statement) : J.Member =
    let inline trE x = transformExpr env x
    let inline trS x = transformStatement env x
    match mem with
    | ClassMethod (s, n, p, b, t) ->
        let innerEnv = env.NewInner()
        let t, jsgen =
            match t with
            | TSType.Generic (t, g) -> t, g |> List.map (transformTypeName env true)
            | _ -> t, []
        let args, tr =
            match t with 
            | TSType.Function (_, ta, trest, tr) -> 
                (p, ta) ||> List.map2 (fun a (t, _) -> defineId innerEnv ArgumentId a |> withType env t) 
                , tr
            | _ ->
                p |> List.map (defineId innerEnv ArgumentId)
                , t
        let body = 
            b |> Option.map (fun b -> 
                CollectVariables(innerEnv).VisitStatement(b)
                b |> transformStatement innerEnv |> flattenFuncBody tr
            )
        let id =
            match body with
            | Some (true, _) ->  J.Id.New(n, typ = J.Var (J.Id.New "never"), gen = jsgen)
            | _ -> J.Id.New(n) |> withType env tr 
        J.Method(s, id, args, body |> Option.map (fun (_, b) -> flattenJS b))   
    | ClassConstructor (p, b, t) ->
        let innerEnv = env.NewInner()
        let args =
            match t with 
            | TSType.New (ta, _) -> 
                let ta =
                    // TODO remove workaround
                    if ta.Length < p.Length then
                        ta @ (List.replicate (p.Length - ta.Length) TSType.Any)
                    else ta
                (p, ta) ||> List.map2 (fun (a, m) t -> defineId innerEnv ArgumentId a |> withType env t, m)
            | _ ->
                p |> List.map (fun (a, m) -> defineId innerEnv ArgumentId a, m)
        let body = 
            b |> Option.map (fun b -> 
                CollectVariables(innerEnv).VisitStatement(b)
                b |> transformStatement innerEnv |> flattenFuncBody TSType.Any |> snd
            )
        J.Constructor(args, body |> Option.map (fun b -> flattenJS b))   
    | ClassProperty (s, n, t, o) ->
        J.Property (s, J.Id.New(n, opt = o) |> withType env t)
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

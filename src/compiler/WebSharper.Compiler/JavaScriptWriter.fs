// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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
        mutable VisibleGlobals : Set<string>
        mutable CompactVars : int
        mutable ScopeIds : Map<Id, string>
        //ScopeVars : ResizeArray<string>
        FuncDecls : ResizeArray<J.Statement>
        mutable InFuncScope : bool
        OuterScope : bool
    }
    static member New(pref) =
        {
            Preference = pref    
            ScopeNames = Set [ "window"; "self"; "globalThis"; "import"; "_" ]
            VisibleGlobals = Set [ "window"; "self"; "globalThis"; "import" ]
            CompactVars = 0 
            ScopeIds = Map [ Id.Global(), "globalThis"; Id.Import(), "import" ]
            //ScopeVars = ResizeArray()
            FuncDecls = ResizeArray()
            InFuncScope = true
            OuterScope = true
        }

    member this.NewInner() =
        {
            Preference = this.Preference    
            ScopeNames = this.ScopeNames
            VisibleGlobals = this.VisibleGlobals
            CompactVars = this.CompactVars
            ScopeIds = this.ScopeIds
            //ScopeVars = ResizeArray()
            FuncDecls = ResizeArray()
            InFuncScope = true
            OuterScope = false
        }
        
let undef = J.Unary(J.UnaryOperator.``void``, J.Constant (J.Literal.Number "0"))

let globalThis = J.Var (J.Id.New "globalThis")

let undefVar (id: Id) =
//#if DEBUG
    J.Id.New ("MISSINGVAR" + I.MakeValid (defaultArg id.Name "_"))
//#else
//    failwithf "Undefined variable during writing JavaScript: %s" (string id)
//#endif

let transformId (env: Environment) (id: Id) =
    if id.HasStrongName then J.Id.New id.Name.Value else
    try 
        J.Id.New (Map.find id env.ScopeIds) 
    with _ -> 
        undefVar id

let formatter = WebSharper.Core.JavaScript.Identifier.MakeFormatter()

let getCompactName (env: Environment) =
    let vars = env.ScopeNames
    let mutable name = formatter env.CompactVars   
    env.CompactVars <- env.CompactVars + 1   
    while vars |> Set.contains name do
        name <- formatter env.CompactVars   
        env.CompactVars <- env.CompactVars + 1   
    name

let defineId (env: Environment) (id: Id) =
    let name =
        if id.HasStrongName then
            let name = id.Name.Value 
            env.ScopeNames <- env.ScopeNames |> Set.add name
            env.VisibleGlobals <- env.VisibleGlobals |> Set.remove name
            name
        else
            if env.Preference = P.Compact then
                let name = getCompactName env    
                env.ScopeIds <- env.ScopeIds |> Map.add id name
                //if addToDecl then env.ScopeVars.Add(name)
                name 
            else 
                let vars = env.ScopeNames
                let mutable name = (I.MakeValid (defaultArg id.Name "_1"))
                while vars |> Set.contains name do
                    name <- Resolve.newName name 
                env.ScopeNames <- vars |> Set.add name
                env.ScopeIds <- env.ScopeIds |> Map.add id name
                //if addToDecl then env.ScopeVars.Add(name)
                name
    J.Id.New(name, rest = id.IsRest)

let defineImportedId (env: Environment) (id: Id) =
    let iname = id.Name.Value
    env.ScopeNames <- env.ScopeNames |> Set.add iname
    if env.Preference = P.Compact then
        let name = getCompactName env    
        env.ScopeIds <- env.ScopeIds |> Map.add id name
    else 
        env.ScopeIds <- env.ScopeIds |> Map.add id iname
       
let invalidForm c =
    failwithf "invalid form at writing JavaScript: %s" c

type CollectVariables(env: Environment) =
    inherit StatementVisitor()

    override this.VisitFuncDeclaration(f, _, _, _, _) =
        defineId env f |> ignore    

    override this.VisitVarDeclaration(v, _) =
        defineId env v |> ignore

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
                go <- false
                res.Add a
            | _ -> res.Add a
    add s
    List.ofSeq res    

let block s = J.Block (flattenJS s)

let getUsedArgs (args: Id list) b env = 
    if List.isEmpty args then [] else
    let unusedArgs = CollectUnusedVars(args).GetSt(b)
    let argNum =
        args |> Seq.mapi (fun i a -> if unusedArgs.Contains a then 0 else i + 1) |> Seq.max
    args |> List.take argNum |> List.map (fun a -> 
        if unusedArgs.Contains a then 
            defineId env (Id.New())
        else 
            defineId env a
    ) 

let transformMemberKind k =
    match k with
    | MemberKind.Getter -> J.Get
    | MemberKind.Setter -> J.Set
    | MemberKind.Simple -> J.Simple

let rec transformExpr (env: Environment) (expr: Expression) : J.Expression =
    let inline trE x = transformExpr env x
    let inline trI x = transformId env x
    match expr with
    | Undefined -> undef
    | Self -> J.This
    | Base -> J.Super
    | Var importId when importId = Id.Import() -> J.ImportFunc
    | Var id -> J.Var (trI id)
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
        | Decimal _ -> failwith "Cannot write Decimal directly to JavaScript output"
    | Application (e, ps, _) -> J.Application (trE e, [], ps |> List.map trE)
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
    | Function (ids, a, t, b) ->
        let innerEnv = env.NewInner()
        let args = getUsedArgs ids b innerEnv
        CollectVariables(innerEnv).VisitStatement(b)
        let body = b |> transformStatement innerEnv |> flattenFuncBody
        //let useStrict =
        //    if env.OuterScope then
        //        [ J.Ignore (J.Constant (J.String "use strict")) ]
        //    else []

        J.Lambda(None, args, flattenJS body, t.IsNone)
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
    | Object fs -> 
        J.NewObject (
            fs |> List.map (fun (k, mk, v) -> 
                match mk with
                | MemberKind.Getter 
                | MemberKind.Setter ->
                    match v with 
                    | IgnoreSourcePos.Function _ -> ()
                    | _ -> failwithf "getter/setter in object expression is not a function: %s" (Debug.PrintExpression v)
                | _ -> ()
                k, transformMemberKind mk, trE v
            )
        )
    | New (x, _, y) -> J.New(trE x, [], y |> List.map trE)
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
    | Cast (_, e) ->
        trE e
    | ClassExpr (n, b, m) ->
        let innerEnv = env.NewInner()
        let jn = n |> Option.map (defineId innerEnv)
        J.ClassExpr(jn, Option.map trE b, [], List.map (transformMember innerEnv) m)
    | GlobalAccess a ->
        match a.Module with     
        | ImportedModule g when g.IsGlobal() ->
            match List.rev a.Address.Value with
            | [] ->
                //J.Var (J.Id.New "exports")
                failwith "top scope of current module not accessible directly"
            | h :: t ->
                List.fold (fun e n ->
                    e.[J.Constant (J.String n)]
                ) (J.Var (J.Id.New h)) t
        | ImportedModule v ->
            List.foldBack (fun n e -> 
                e.[J.Constant (J.String n)]
            ) a.Address.Value (J.Var (trI v))
        | StandardLibrary | JavaScriptFile _ ->
            match List.rev a.Address.Value with
            | [] -> globalThis
            | h :: t ->
                let ha =
                    if env.VisibleGlobals.Contains(h) then
                        J.Var (J.Id.New h)
                    else
                        globalThis.[J.Constant (J.String h)]
                List.fold (fun e n ->
                    e.[J.Constant (J.String n)]
                ) ha t
        | _ -> 
            failwith "Addresses must be resolved to ImportedModule before writing JavaScript"
    | GlobalAccessSet (a, v) ->
        trE (GlobalAccess a) ^= trE v
    | _ -> 
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
            | Empty 
            | ExprStatement IgnoreSourcePos.Undefined -> ()
            | ExprStatement (IgnoreSourcePos.Sequential s) ->
                sequentialE s |> List.iter add
            | ExprStatement (IgnoreSourcePos.Conditional(a, b, c)) ->
                match b, c with
                | IgnoreSourcePos.Undefined, IgnoreSourcePos.Undefined ->
                    ()
                | IgnoreSourcePos.Undefined, _ ->
                    If(a, Empty, ExprStatement c) |> add 
                | _, IgnoreSourcePos.Undefined ->
                    If(a, ExprStatement b, Empty) |> add 
                | _ ->
                    If(a, ExprStatement b, ExprStatement c) |> add
            | Return (IgnoreSourcePos.Sequential s) ->
                sequential s Return |> List.iter add
            | Throw (IgnoreSourcePos.Sequential s) ->
                sequential s Throw |> List.iter add
            | _ -> 
                res.Add(trS a)
        s |> List.iter add
        let mutable skip = false
        res |> Seq.filter (fun s ->
            if skip then
                match J.IgnoreStatementPos s with
                | J.Function _ | J.Vars _ -> 
                    true
                | J.Labelled _ ->
                    skip <- false 
                    true
                | _ ->
                    false
            else
                match J.IgnoreStatementPos s with
                | J.Return _ | J.Throw _ | J.Break _ | J.Continue _ -> 
                    skip <- true
                | _ -> ()
                true
        ) |> List.ofSeq
    let flattenS s =
        match IgnoreStatementSourcePos s with
        | Block s -> flatten s
        | _ -> [ trS s ]
    // collect function declarations to be on top level of functions to satisfy strict mode
    // requirement by some JavaScript engines
    let withFuncDecls f =
        //if env.InFuncScope then
        //    env.InFuncScope <- false
        //    let woFuncDecls = f()
        //    env.InFuncScope <- true
        //    if env.FuncDecls.Count > 0 then
        //        let res = block (Seq.append env.FuncDecls (Seq.singleton woFuncDecls))
        //        env.FuncDecls.Clear()
        //        res
        //    else woFuncDecls
        //else 
            f()
    match statement with
    | Empty -> J.Empty
    | Break(a) -> J.Break (a |> Option.map (fun l -> l.Name.Value))
    | Continue(a) -> J.Continue (a |> Option.map (fun l -> l.Name.Value))
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
        J.StatementPos (trS s, jpos)
    | If(a, b, c) -> 
        withFuncDecls <| fun () -> 
            J.If(trE a, trS b, trS c)
    | Return (IgnoreSourcePos.Unary(UnaryOperator.``void``, a)) -> block [ J.Ignore(trE a); J.Return None ]
    | Return (IgnoreSourcePos.Sequential s) -> block (sequential s Return |> List.map trS)
    | Return IgnoreSourcePos.Undefined -> J.Return None
    | Return a -> J.Return (Some (trE a))
    | VarDeclaration (id, e) ->
        match e with
        | IgnoreSourcePos.Undefined -> 
            J.Vars([transformId env id, None], J.LetDecl)
            //J.Empty 
        //| IgnoreSourcePos.Application(Var importId, args, { Purity = NonPure; KnownLength = Some 0 }) when importId = Id.Import() ->
        //    match args with
        //    | [ Value (String from) ] ->
        //        J.Import(None, defineId env false id, from)  
        //    | [ Value (String export); Value (String from) ] ->
        //        J.Import(Some export, defineId env false id, from)  
        //    | _ -> failwith "unrecognized args for import"
        | _ -> 
            let kind = if id.IsMutable then J.LetDecl else J.ConstDecl
            J.Vars([transformId env id, Some (trE e)], kind)
            //J.Ignore(J.Binary(J.Var (transformId env id), J.BinaryOperator.``=``, trE e))
    | FuncDeclaration (x, ids, t, b, _) ->
        let id = transformId env x
        let innerEnv = env.NewInner()
        let args = getUsedArgs ids b innerEnv
        CollectVariables(innerEnv).VisitStatement(b)
        let body =
            match b |> transformStatement innerEnv with
            | J.Block b -> 
                match List.rev b with
                | J.Return None :: more -> List.rev more
                | _ -> b
            | J.Empty
            | J.Return None -> []
            | b -> [ b ]
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
            match a with 
            | Some (NewVars setters) ->
                let trV =
                    List.zip
                        (setters |> List.map (fst >> defineId env))
                        (setters |> List.map (snd >> Option.map trE))
                J.ForVars(trV, Option.map trE b, Option.map trE c, trS d)
            | _ ->
                J.For(Option.map trE a, Option.map trE b, Option.map trE c, trS d)
    | Switch(a, b) -> 
        withFuncDecls <| fun () ->
            J.Switch(trE a, 
                b |> List.map (fun (l, s) -> 
                    match l with 
                    | Some l -> J.SwitchElement.Case (trE l, flattenS s) 
                    | _ -> J.SwitchElement.Default (flattenS s)
                )
            )
    | Throw (IgnoreSourcePos.Sequential s) -> block (sequential s Throw |> List.map trS)
    | Throw(a) -> J.Throw (trE a)
    | Labeled(a, b) -> 
        withFuncDecls <| fun () -> 
            J.Labelled(a.Name.Value, trS b)
    | TryWith(a, b, c) -> 
        withFuncDecls <| fun () ->
            J.TryWith(trS a, defineId env (match b with Some b -> b | _ -> Id.New()), trS c, None)
    | TryFinally(a, b) ->
        withFuncDecls <| fun () ->
            J.TryFinally(trS a, trS b)
    | Import(None, None, namedImports, "") ->
        let jsNames = namedImports |> Seq.map fst |> Set
        env.ScopeNames <- Set.union env.ScopeNames jsNames
        env.VisibleGlobals <- Set.union env.VisibleGlobals jsNames
        J.Empty
        //J.Import(
        //    None,
        //    None,
        //    namedImports |> List.map (fun (n, _) -> n, J.Id.New n),
        //    "")
    | Import(a, b, c, d) ->
        J.Import(
            a |> Option.map (defineId env), 
            b |> Option.map (defineId env), 
            c |> List.map (fun (n, x) -> n, defineId env x),
            d)
    | ExportDecl (a, b) ->
        J.Export (a, trS b)
    | ForIn(a, b, c) -> 
        withFuncDecls <| fun () ->
            J.ForVarIn(defineId env a, None, trE b, trS c)
    | XmlComment a ->
        J.StatementComment (J.Empty, a)
    | Class (n, b, i, m, g) ->
        let jn = defineId env n
        let innerEnv = env.NewInner()
        let isAbstract =
            m |> List.exists (function
                | ClassMethod (_, _, _, _, None, _) -> true
                | _ -> false
            )
        J.Class(jn, isAbstract, Option.map trE b, [], List.map (transformMember innerEnv) m)
    | _ -> 
        invalidForm (GetUnionCaseName statement)

and transformMember (env: Environment) (mem: Statement) : J.Member =
    let inline trE x = transformExpr env x
    let inline trS x = transformStatement env x
    match mem with
    | ClassMethod (s, n, p, t, b, _) ->
        let innerEnv = env.NewInner()
        let args = 
            match b with
            | Some b -> getUsedArgs p b innerEnv
            | _-> p |> List.map (defineId innerEnv)
        let body = 
            b |> Option.map (fun b -> 
                CollectVariables(innerEnv).VisitStatement(b)
                flattenJS [ b |> transformStatement innerEnv ]
            )
        let id = J.Id.New(n)
        let acc = transformMemberKind s.Kind
        J.Method(s.IsStatic, acc, id, args, body)   
    | ClassConstructor (p, t, b, _) ->
        let innerEnv = env.NewInner()
        let args = 
            match b with
            | Some b -> getUsedArgs (p |> List.map fst) b innerEnv
            | _-> p |> List.map (fst >> defineId innerEnv)
            |> List.map (fun a -> a, Modifiers.None)
        let body = 
            b |> Option.map (fun b -> 
                CollectVariables(innerEnv).VisitStatement(b)
                flattenJS [ b |> transformStatement innerEnv ]
            )
        J.Constructor(args, body)   
    | ClassProperty (s, n, _, v) ->
        J.Property (s.IsStatic, J.Id.New(n), v |> Option.map (transformExpr env))
    | ClassStatic (b) ->
        let innerEnv = env.NewInner()
        let body = 
            CollectVariables(innerEnv).VisitStatement(b)
            flattenJS [ b |> transformStatement innerEnv ]
        J.Static body
    | _ -> 
        invalidForm (GetUnionCaseName mem)

let transformProgram pref statements =
    if List.isEmpty statements then [] else
    let env = Environment.New(pref)
    //let cnames = CollectStrongNames(env)
    //statements |> List.iter cnames.VisitStatement
    let cvars = CollectVariables(env)
    statements |> List.iter cvars.VisitStatement
    //J.Ignore (J.Constant (J.String "use strict")) ::
    (statements |> List.map (transformStatement env) |> flattenJS)

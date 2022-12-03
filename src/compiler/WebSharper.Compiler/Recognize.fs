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

module WebSharper.Compiler.Recognize
                      
open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST

module I = WebSharper.Core.JavaScript.Identifier
module S = WebSharper.Core.JavaScript.Syntax
module P = WebSharper.Core.JavaScript.Parser
type SB = WebSharper.Core.JavaScript.Syntax.BinaryOperator
module M = WebSharper.Core.Metadata

module IS = IgnoreSourcePos

let GetMutableExternals (meta: M.Info) =
    let res = HashSet()

    let registerInstanceAddresses (cls: M.ClassInfo) (baseAddr: Hashed<_>) =
        for fi, readOnly, _ in cls.Fields.Values do
            if not readOnly then
                match fi with
                | M.InstanceField n
                | M.OptionalField n ->
                    res.Add (Hashed (n :: baseAddr.Value)) |> ignore
                | _ -> () 

        let addMember (m: Method) e =
            if m.Value.MethodName.StartsWith "set_" then
                match e with
                | IS.Function(_, _, _, IS.ExprStatement(IS.ItemSet(IS.This, IS.Value (String n), _)))
                | IS.Unary(UnaryOperator.``void``, IS.ItemSet(Hole(0), IS.Value (String n), Hole(1))) ->
                    res.Add (Hashed (n :: baseAddr.Value)) |> ignore
                | _ -> ()

        for KeyValue(m, (_, _, _, e)) in cls.Methods do
            addMember m e
       
        for KeyValue((_, m), (_, e)) in cls.Implementations do
            addMember m e

    let tryRegisterInstanceAddresses typ (a: Address) =
        match typ with
        | ConcreteType ct ->
            match meta.Classes.TryGetValue ct.Entity with
            | true, (_, _, Some fcls) ->
                a.JSAddress |> Option.iter (registerInstanceAddresses fcls)
            | _ -> ()
        | _ -> ()
    
    for cls in meta.ClassInfos do
        //for fi, readOnly, ftyp in cls.Fields.Values do
        //    match fi with
        //    | M.StaticField a ->
        //        if not readOnly then
        //            a.JSAddress |> Option.iter (res.Add >> ignore)
        //        tryRegisterInstanceAddresses ftyp a
        //    | _ -> () 

        let addMember (m: Method) e =
            if m.Value.MethodName.StartsWith "set_" then
                match e with
                | IS.Function(_, _, _, IS.ExprStatement(IS.ItemSet(IS.GlobalAccess a, IS.Value (String n), _)))
                | IS.Unary(UnaryOperator.``void``, IS.ItemSet(IS.GlobalAccess a, IS.Value (String n), Hole(0))) ->
                    a.JSAddress |> Option.iter (fun a -> res.Add (Hashed (n :: a.Value)) |> ignore)
                | _ -> ()
            elif m.Value.MethodName.StartsWith "get_" then
                match e with
                | IS.Function(_, _, _, IS.Return(IS.GlobalAccess a))
                | IS.GlobalAccess a ->
                    tryRegisterInstanceAddresses m.Value.ReturnType a 
                | _ -> ()

        for KeyValue(m, (_, _, _, e)) in cls.Methods do
            addMember m e
       
        for KeyValue((_, m), (_, e)) in cls.Implementations do
            addMember m e

    res

type private Environment =
    {
        Vars : list<IDictionary<string, Expression>>
        Inputs : list<Expression>
        Labels : Map<string, Id>
        This : option<Id>
        Purity : Purity
        MutableExternals : HashSet<Hashed<list<string>>>
        FromModule : option<Module>
        ExpectedDollarVars : string[]
        UnknownArgs : HashSet<string>
    }

    static member New(thisArg, isDirect, isPure, args, ext, lib, dollarVars) =
        // TODO : add  `arguments` to scope
        let mainScope =
            Option.toList thisArg @ args
            |> Seq.mapi (fun i (a: Id) ->                    
                let isThis = Some a = thisArg
                let v =
                    if isDirect && isThis then This else Var a
                [ 
                    yield "$" + string i, v
                    match a.Name with
                    | Some n -> yield "$" + n, v
                    | _ -> ()
                    if isThis then yield "$this", v
                ] 
            ) |> Seq.concat |> dict
        {
            Vars = [ Dictionary(); mainScope ]
            Inputs = 
                if isDirect then [] 
                else (if Option.isSome thisArg then [This] else []) @ (args |> List.map Var)
            Labels = Map.empty
            This = None
            Purity = if isPure then Pure else NonPure
            MutableExternals = ext
            FromModule = lib
            ExpectedDollarVars = dollarVars
            UnknownArgs = HashSet()
        }

    static member Empty =
        {
            Vars = []
            Inputs = []
            Labels = Map.empty
            This = None
            Purity = NonPure
            MutableExternals = HashSet()
            FromModule = None
            ExpectedDollarVars = [||]
            UnknownArgs = HashSet()
        }

    member this.WithNewScope (vars) =
        { this with 
            Vars = (Dictionary(vars |> Seq.map (fun (s: S.Id, i) -> s.Name, i) |> dict) :> _) :: this.Vars 
            Purity = NonPure
        }

    member this.NewVar(id: S.Id) =
        let n = id.Name
        let v = Id.New n
        match this.Vars with
        | [] -> failwith "no scope"
        | h :: t ->
            h.Add(n, Var v)
            v

    member this.TryFindVar(id: S.Id) =
        let n = id.Name
        let rec findIn scope =
            match scope with
            | [] -> None
            | (h : IDictionary<_,_>) :: t ->
                match h.TryFind n with
                | Some _ as res -> res
                | _ -> findIn t
        findIn this.Vars

    member this.IsInput(expr) =
        this.Inputs |> List.contains expr

exception RecognitionError

/// Checks if a specific Id is mutated
type private IsImmutable(v) =
    inherit Visitor()

    let mutable scope = 0
    let mutable notMutated = true

    override this.VisitVarSet (a, b) =
        if a = v then notMutated <- false
        else this.VisitExpression b

    override this.VisitMutatingUnary (_, a) =
        match IgnoreExprSourcePos a with
        | Var av when av = v ->
            notMutated <- false
        | _ ->
            this.VisitExpression a

    override this.VisitMutatingBinary (a, _, b) =
        match IgnoreExprSourcePos a with
        | Var av when av = v ->
            notMutated <- false
        | _ ->
            this.VisitExpression a
            this.VisitExpression b

    override this.VisitExpression(a) =
        if notMutated then base.VisitExpression(a)

    member this.Check(a) =
        this.VisitExpression(a)
        notMutated

let makePossiblyImmutable expr (v: Id) =
    if IsImmutable(v).Check(expr) then 
        let vi = Id.New(?name = v.Name, mut = false)
        ReplaceId(v, vi).TransformExpression(expr)
    else expr         

let private checkNotMutating (env: Environment) a f =
    if env.IsInput a then
        failwith "arguments of inlined functions should not be mutated"
    else f a

let private setValue (env: Environment) expr value =
    match expr with
    | GlobalAccess a ->
        match a.Address.Value with
        | i :: m -> ItemSet(GlobalAccess { a with Address = Hashed m }, Value (String i), value)
        | _ -> failwith "cannot set the window object"
    | ItemGet (d, e, _) -> ItemSet(d, e, value)
    | Var d -> checkNotMutating env (Var d) (fun _ -> VarSet(d, value))
    | _ -> failwith "invalid form for setter"

let wsruntime = GlobalAccess (Address.Runtime())

let jsFunctionMembers =
    System.Collections.Generic.HashSet [
        "apply"
        "bind"
        "call"
    ]

let wsRuntimeFunctions =
    System.Collections.Generic.HashSet [
        "Create"
        "Clone"
        "Ctor"
        "Base"
        "Force"
        "Lazy"
        "PrintObject"
        "DeleteEmptyFields"
        "GetOptional"
        "SetOptional"
        "SetOrDelete"
        "Apply"
        "Bind"
        "CreateFuncWithArgs"
        "CreateFuncWithOnlyThis"
        "CreateFuncWithThis"
        "CreateFuncWithThisArgs"
        "CreateFuncWithRest"
        "CreateFuncWithArgsRest"
        "BindDelegate"
        "CreateDelegate"
        "CombineDelegates"
        "DelegateEqual"
        "ThisFunc"
        "ThisFuncOut"
        "ParamsFunc"
        "ParamsFuncOut"
        "ThisParamsFunc"
        "ThisParamsFuncOut"
        //"Curried"
        //"Curried2"
        //"Curried3"
        "UnionByType"
        "MarkResizable"
        "MarkReadOnly"
        "ScriptBasePath"
        "ScriptPath"
    ]

let (|JSGlobalAccess|_|) e =
    match e with
    | GlobalAccess a ->
        match a.JSAddress with
        | Some ja -> Some (a.Module, ja)
        | _ -> None
    | _ -> None

let inline private trI (jsId: S.Id) = Id.New(jsId.Name, opt = jsId.Optional)

let rec private transformExpression (env: Environment) (expr: S.Expression) =
    let inline trE e = transformExpression env e
    let checkNotMutating a f =
        if env.IsInput a then
            failwith "arguments of inlined functions should not be mutated"
        else f a
    let mbin a op c =
        checkNotMutating (trE a) (fun ta -> MutatingBinary(ta, op, trE c)) 
    let mun a op =
        checkNotMutating (trE a) (fun ta -> MutatingUnary(op, ta))
    match expr with
    | S.Application (a, [], b) ->
        let trA =
            match trE a with
            | ItemGet (a, b, _) -> ItemGet (a, b, Pure)
            | trA -> trA
        match trA with
        | Var t when t = Id.SourceType() ->
            match b |> List.map trE with
            | [ Value (String tn) ] ->
                GlobalAccess {
                    Module = JavaScriptModule tn
                    Address = Hashed [ "default" ]
                }   
            | _ ->
                failwithf "$type can be only used with a single string argument in inlines"
        | _->
            Appl (trA, b |> List.map trE, env.Purity, None) 
    | S.Binary (a, b, c) ->
        match b with    
        | SB.``!=``     -> Binary(trE a, BinaryOperator.``!=``, trE c)
        | SB.``!==``    -> Binary(trE a, BinaryOperator.``!==``, trE c)
        | SB.``%``      -> Binary(trE a, BinaryOperator.``%``, trE c)
        | SB.``%=``     -> mbin a MutatingBinaryOperator.``%=`` c
        | SB.``&``      -> Binary(trE a, BinaryOperator.``&``, trE c)
        | SB.``&&``     -> Binary(trE a, BinaryOperator.``&&``, trE c)
        | SB.``&=``     -> mbin a MutatingBinaryOperator.``&=`` c
        | SB.``*``      -> Binary(trE a, BinaryOperator.``*``, trE c)
        | SB.``**``     -> Binary(trE a, BinaryOperator.``**``, trE c)
        | SB.``*=``     -> mbin a MutatingBinaryOperator.``*=`` c
        | SB.``+``      -> Binary(trE a, BinaryOperator.``+``, trE c)
        | SB.``+=``     -> mbin a MutatingBinaryOperator.``+=`` c
        | SB.``,``      -> Sequential [trE a; trE c]
        | SB.``-``      -> Binary(trE a, BinaryOperator.``-``, trE c)
        | SB.``-=``     -> mbin a MutatingBinaryOperator.``-=`` c
        | SB.``.``      -> 
            let trA = trE a
            let trC = trE c
            if trA = wsruntime then
                match trC with
                | Value (String f) ->
                    if wsRuntimeFunctions.Contains f then
                        GlobalAccess (Address.Runtime f)
                    else
                        failwithf "Unrecognized WSRuntime function: %s" f
                | _ -> failwith "expected a function of WSRuntime"     
            else
                match trA, trC with
                | JSGlobalAccess (m, a), Value (String b) when not (I.IsObjectMember b || jsFunctionMembers.Contains b)  ->
                    let ga = Hashed (b :: a.Value)
                    if env.MutableExternals.Contains ga then 
                        ItemGet(trA, trC, env.Purity)
                    else
                        GlobalAccess { Module = m; Address = Hashed (b :: a.Value) }
                | _ ->
                    ItemGet(trA, trC, env.Purity) 
        | SB.``/``      -> Binary(trE a, BinaryOperator.``/``, trE c)
        | SB.``/=``     -> mbin a MutatingBinaryOperator.``/=`` c
        | SB.``<``      -> Binary(trE a, BinaryOperator.``<``, trE c)
        | SB.``<<``     -> Binary(trE a, BinaryOperator.``<<``, trE c)
        | SB.``<<=``    -> mbin a MutatingBinaryOperator.``<<=`` c
        | SB.``<=``     -> Binary(trE a, BinaryOperator.``<=``, trE c)
        | SB.``=``      -> setValue env (trE a) (trE c)
        | SB.``==``     -> Binary(trE a, BinaryOperator.``==``, trE c)
        | SB.``===``    -> Binary(trE a, BinaryOperator.``===``, trE c)
        | SB.``>``      -> Binary(trE a, BinaryOperator.``>``, trE c)
        | SB.``>>``     -> Binary(trE a, BinaryOperator.``>>``, trE c)
        | SB.``>=``     -> Binary(trE a, BinaryOperator.``>=``, trE c)
        | SB.``>>=``    -> mbin a MutatingBinaryOperator.``>>=`` c
        | SB.``>>>``    -> Binary(trE a, BinaryOperator.``>>>``, trE c)
        | SB.``>>>=``   -> mbin a MutatingBinaryOperator.``>>>=`` c
        | SB.``^``      -> Binary(trE a, BinaryOperator.``^``, trE c)
        | SB.``^=``     -> mbin a MutatingBinaryOperator.``^=`` c
        | SB.``in``     -> Binary(trE a, BinaryOperator.``in``, trE c)
        | SB.instanceof -> Binary(trE a, BinaryOperator.instanceof, trE c)
        | SB.``|``      -> Binary(trE a, BinaryOperator.``|``, trE c)
        | SB.``|=``     -> mbin a MutatingBinaryOperator.``|=`` c
        | SB.``||``     -> Binary(trE a, BinaryOperator.``||``, trE c)
        | _ -> failwith "unrecognized binary operator"
    | S.Conditional (a, b, c) ->
        Conditional (trE a, trE b, trE c)
    | S.Constant a -> 
        let rN n =
            match System.Int64.FromString n with
            | Some i -> Int64 i
            | None ->
                match System.Double.FromString n with
                | Some i -> Double i
                | None -> raise RecognitionError
        match a with
        | S.False -> Bool false
        | S.Null -> Null
        | S.Number n -> rN n
        | S.String x -> String x
        | S.True -> Bool true
        |> Value
    | S.Lambda (a, b, c, d) ->
        let vars = b |> List.map trI
        let innerEnv = env.WithNewScope(Seq.zip b (vars |> Seq.map Var))
        let body = S.Block c
        let fres =
            match a with
            | None -> Function (vars, d, None, transformStatement innerEnv body)
            | Some a -> 
                let f = env.NewVar a
                StatementExpr(FuncDeclaration(f, vars, transformStatement innerEnv body, []), Some f)
        innerEnv.Vars.Head.Values |> Seq.choose (function Var i -> Some i | _ -> None)
        |> Seq.fold makePossiblyImmutable fres
    | S.New (a, [], b) -> 
        New(trE a, [], List.map trE b)
    | S.NewArray a -> NewArray (a |> List.map (function Some i -> trE i | _ -> Undefined))
    | S.NewObject a -> Object(a |> List.map (fun (b, _, c) -> b, MemberKind.Simple, trE c))
    | S.NewRegex a -> 
        let closingSlash = a.LastIndexOf '/'
        let flags = a.[closingSlash + 1 ..] |> Seq.map (string >> String >> Value) |> List.ofSeq
        New (Global ["RegExp"], [], Value (String a.[1 .. closingSlash - 1]) :: flags)
    | S.Postfix (a, b) ->
        match b with
        | S.PostfixOperator.``++`` -> mun a MutatingUnaryOperator.``()++``
        | S.PostfixOperator.``--`` -> mun a MutatingUnaryOperator.``()--``
        | _ -> failwith "unrecognized postfix operator"
    | S.This -> This
    | S.ImportFunc -> Var (Id.Import())
    | S.Unary (a, b) ->
        match a with
        | S.UnaryOperator.``!`` -> Unary(UnaryOperator.``!``, trE b)
        | S.UnaryOperator.``+`` -> Unary(UnaryOperator.``+``, trE b)
        | S.UnaryOperator.``++`` -> mun b MutatingUnaryOperator.``++()``
        | S.UnaryOperator.``-`` -> Unary(UnaryOperator.``-``, trE b)
        | S.UnaryOperator.``--`` -> mun b MutatingUnaryOperator.``--()``
        | S.UnaryOperator.delete -> mun b MutatingUnaryOperator.delete
        | S.UnaryOperator.typeof -> Unary(UnaryOperator.typeof, trE b)
        | S.UnaryOperator.``void`` -> Unary(UnaryOperator.``void``, trE b)
        | S.UnaryOperator.``~`` -> Unary(UnaryOperator.``~``, trE b)
        | _ -> failwith "unrecognized unary operator"
    | S.Var a ->
        match a.Name with
        | "$global"
        | "window" 
        | "self"
        | "globalThis" -> Var (Id.Global())
        | "$wsruntime" -> wsruntime
        | "$type" -> Var (Id.SourceType())
        | "arguments" -> Arguments
        | "undefined" -> Undefined
        | n ->
        match env.TryFindVar a with
        | Some e -> e
        | None -> 
            if a.Name.StartsWith("$") && a.Name <> "$" && not (env.ExpectedDollarVars |> Array.contains a.Name) then
                env.UnknownArgs.Add a.Name |> ignore
            match env.FromModule with
            | Some l ->
                GlobalAccess { Module = l; Address = Hashed [ n ] }
            | _ ->
                Global [ n ]
    | e ->     
        failwithf "Failed to recognize: %A" e
//    | S.Postfix (a, b) ->
//        match b with
//        | S.PostfixOperator.``++`` -> 
                                                                       
and private transformStatement (env: Environment) (statement: S.Statement) =
    let inline trE e = transformExpression env e
    let inline trS s = transformStatement env s
    match statement with
    | S.StatementPos(a, b) -> trS a
    | S.Block a ->
        a |> List.map trS |> CombineStatements
    | S.Break a   -> Break (a |> Option.map (fun l -> env.Labels.[l]))
    | S.Continue a -> Continue (a |> Option.map (fun l -> env.Labels.[l]))
    | S.Class (a, b, c, d, e) ->
        failwith "Currently unsupported: JS class in inline"   
    | S.Declare a ->
        failwith "Currently unsupported: TS declare in inline"   
    | S.DeclareGlobal a ->
        failwith "Currently unsupported: TS declare global in inline"   
    | S.Debugger -> failwith "Currently unsupported: JS debugger"
    | S.Do (a, b) -> DoWhile (trS a, trE b)
    | S.Empty -> Empty
    | S.Export (a, b) ->
        failwith "Currently unsupported: JS export in inline"   
    | S.ExportAlias (a, b) ->
        failwith "Currently unsupported: JS export in inline"   
    | S.For (a, b, c, d) -> 
        For (Option.map trE a, Option.map trE b, Option.map trE c, trS d)
    | S.ForIn (a, b, c) ->
        let i = Id.New "i"
        ForIn(i, trE b, Block [ ExprStatement (setValue env (trE a) (Var i)); trS c ])
    | S.ForVarIn (a, b, c, d) -> 
        let v = env.NewVar a
        let bv =
            match b with
            | Some b ->  trE b
            | _ -> Undefined        
        Block [
            VarDeclaration (v, bv)
            ForIn(v, trE c, trS d)
        ]
    | S.ForVars (a, b, c, d) -> 
        let decls =
            a |> List.map (fun (i, v) ->
                let id = env.NewVar i
                NewVar(id, match v with Some v -> trE v | _ -> Undefined)
            )
        let trA =
            match decls with
            | [] -> None
            | [ x ] -> Some x
            | xs -> Some (Sequential xs)
        For (trA, Option.map trE b, Option.map trE c, trS d)
    | S.If (a, b, c) -> If (trE a, trS b, trS c)
    | S.Ignore a -> ExprStatement (trE a)    
    | S.Import (a, b, c, d) ->
        failwith "Currently unsupported: JS import in inline"   
    | S.ImportAlias (a, b) ->
        failwith "Currently unsupported: JS import in inline"   
    | S.ImportAll (a, b) ->
        failwith "Currently unsupported: JS import in inline"   
    | S.Interface (a, b, c) ->
        failwith "Currently unsupported: TS interface in inline"   
    | S.Labelled (a, b)  -> 
        failwith "Currently unsupported: JS labels"
    | S.Namespace (a, b) ->
        failwith "Currently unsupported: TS namespace in inline"   
    | S.Return a -> Return (match a with Some v -> trE v | _ -> Undefined)
    | S.Switch (a, b) -> 
        Switch(trE a, b |> List.map (function 
            | S.Case (c, cs) -> Some (trE c), Block (cs |> List.map trS) 
            | S.Default d -> None, Block (d |> List.map trS)
        ))
    | S.Throw a -> Throw (trE a)
    | S.TryFinally (a, b) -> TryFinally (trS a, trS b)
    | S.TryWith (a, b, c, d) -> 
        let tw = TryWith (trS a, Some (env.NewVar b), trS c)
        match d with
        | None -> tw
        | Some d -> TryFinally (tw, trS d)
    | S.TypeAlias (a, b) ->
        failwith "Currently unsupported: TS type alias in inline"   
    | S.Vars (a, _) ->
        match a with
        | [var, value] -> 
            VarDeclaration (env.NewVar var, match value with Some v -> trE v | None -> Undefined)
        | _ -> 
            Block (a |> List.map (fun (var, value) -> VarDeclaration (env.NewVar var, match value with Some v -> trE v | None -> Undefined)))
    | S.While (a, b) -> While (trE a, trS b)
    | S.With (a, b) -> failwith "Unsupported: JS with statement"
    | S.Function (a, b, c) ->
        let f = env.NewVar a
        let vars = b |> List.map trI
        let innerEnv = env.WithNewScope(Seq.zip b (vars |> Seq.map Var))
        let body = S.Block c
        FuncDeclaration(f, vars, transformStatement innerEnv body, [])
    | S.StatementComment _ -> failwith "impossible, comments are not parsed"

type InlinedStatementsTransformer() =
    inherit StatementTransformer()
    
    let mutable returnVar = None

    override this.TransformReturn(expr) =
        let rv =
            match returnVar with
            | None -> 
                let rv = Id.New("r")
                returnVar <- Some rv
                rv
            | Some rv -> rv
        
        VarSetStatement(rv, expr)

    override this.TransformFuncDeclaration(a, b, c, d) = FuncDeclaration(a, b, c, d)
    
    member this.Run(st) =
        let res = this.TransformStatement(st)
        StatementExpr(res, returnVar)
                
type ParseResult =
    {
        Expr: Expression
        Warnings: string list
    }

let createInline ext thisArg args isPure fromLib dollarVars inlineString =        
    let parsed = 
        try inlineString |> P.Source.FromString |> P.ParseExpression |> Choice1Of2
        with _ -> inlineString |> P.Source.FromString |> P.ParseProgram |> Choice2Of2 
    let env = Environment.New(thisArg, false, isPure, args, ext, fromLib, dollarVars)
    let b =
        match parsed with
        | Choice1Of2 e ->
            e |> transformExpression env
        | Choice2Of2 p ->
            p
            |> S.Block
            |> transformStatement env
            |> InlinedStatementsTransformer().Run
    {
        Expr = makeExprInline (Option.toList thisArg @ args) b
        Warnings =
            [
                if env.UnknownArgs.Count > 0 then
                    yield "Dollar-prefixed variable(s) in inline JavaScript don't correspond to an argument: "
                        + String.concat ", " env.UnknownArgs
            ]
    }

let parseDirect ext thisArg args dollarVars jsString =
    let parsed = 
        try jsString |> P.Source.FromString |> P.ParseExpression |> Choice1Of2
        with _ -> jsString |> P.Source.FromString |> P.ParseProgram |> Choice2Of2 
    let env = Environment.New(thisArg, true, false, args, ext, None, dollarVars)
    let body =
        match parsed with
        | Choice1Of2 e ->
            e |> transformExpression env |> Return 
        | Choice2Of2 p ->
            p
            |> S.Block
            |> transformStatement env
    {
        Expr = Function(args, false, None, body)
        Warnings =
            [
                if env.UnknownArgs.Count > 0 then
                    yield "Dollar-prefixed variable(s) in direct JavaScript don't correspond to an argument: "
                        + String.concat ", " env.UnknownArgs
            ]
    }

let parseGeneratedJavaScript e =
    e |> transformExpression Environment.Empty

let parseGeneratedString s =
    s |> P.Source.FromString
    |> P.ParseExpression
    |> parseGeneratedJavaScript


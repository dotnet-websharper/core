// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
        for { CompiledForm = fi; ReadOnly = readOnly } in cls.Fields.Values do
            if not readOnly then
                match fi with
                | M.InstanceField n
                | M.OptionalField n ->
                    res.Add (Hashed (n :: baseAddr.Value)) |> ignore
                | _ -> () 

        let addMember (m: Method) e =
            if m.Value.MethodName.StartsWith "set_" then
                match e with
                | IS.Function(_, Some t, _, IS.ExprStatement(IS.ItemSet(IS.Var t2, IS.Value (String n), _))) when t = t2 ->
                    res.Add (Hashed (n :: baseAddr.Value)) |> ignore
                | IS.Unary(UnaryOperator.``void``, IS.ItemSet(Hole(0), IS.Value (String n), Hole(1))) ->
                    res.Add (Hashed (n :: baseAddr.Value)) |> ignore
                | _ -> ()

        for KeyValue(m, { Expression = e }) in cls.Methods do
            addMember m e
       
        for KeyValue((_, m), { Expression = e }) in cls.Implementations do
            addMember m e

    let tryRegisterInstanceAddresses typ (a: Address) =
        match typ with
        | ConcreteType ct ->
            match meta.Classes.TryGetValue ct.Entity with
            | true, (_, _, Some fcls) ->
                a.JSAddress |> Option.iter (Hashed >> registerInstanceAddresses fcls)
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
                    a.JSAddress |> Option.iter (fun a -> res.Add (Hashed (n :: a)) |> ignore)
                | _ -> ()
            elif m.Value.MethodName.StartsWith "get_" then
                match e with
                | IS.Function(_, _, _, IS.Return(IS.GlobalAccess a))
                | IS.GlobalAccess a ->
                    tryRegisterInstanceAddresses m.Value.ReturnType a 
                | _ -> ()

        for KeyValue(m, { Expression = e }) in cls.Methods do
            addMember m e
       
        for KeyValue((_, m), { Expression = e }) in cls.Implementations do
            addMember m e

    res

type private Environment =
    {
        Vars : list<IDictionary<string, Expression>>
        Inputs : list<Expression>
        Labels : Map<string, Id>
        This : ref<option<Id>>
        Purity : Purity
        MutableExternals : HashSet<Hashed<list<string>>>
        Import : option<Address>
        ExpectedDollarVars : string[]
        UnknownArgs : HashSet<string>
        AssemblyName : string
    }

    static member New(thisArg, isDirect, isPure, args, ext, import, dollarVars, asm) =
        // TODO : add  `arguments` to scope
        let mainScope =
            Option.toList thisArg @ args
            |> Seq.mapi (fun i (a: Id) ->                    
                let isThis = Some a = thisArg
                let v = Var a
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
                else (Option.toList thisArg @ args) |> List.map Var
            Labels = Map.empty
            This = ref thisArg
            Purity = if isPure then Pure else NonPure
            MutableExternals = ext
            Import = import
            ExpectedDollarVars = dollarVars
            UnknownArgs = HashSet()
            AssemblyName = asm
        }

    static member Empty =
        {
            Vars = []
            Inputs = []
            Labels = Map.empty
            This = ref None
            Purity = NonPure
            MutableExternals = HashSet()
            Import = None
            ExpectedDollarVars = [||]
            UnknownArgs = HashSet()
            AssemblyName = ""
        }

    member this.WithNewScope (vars, isArrow) =
        { this with 
            Vars = (Dictionary(vars |> Seq.map (fun (s: S.Id, i) -> s.Name, i) |> dict) :> _) :: this.Vars 
            Purity = NonPure
            This = if isArrow then this.This else ref None
        }

    member this.NewVar(id: S.Id, ?mut) =
        let n = id.Name
        let v = Id.New(n, mut = defaultArg mut true)
        match this.Vars with
        | [] -> failwith "no scope"
        | h :: t ->
            h[n] <- Var v
            //h.Add(n, Var v)
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

    member this.ThisVar =
        match this.This.Value with
        | Some t -> Var t
        | _ -> JSThis

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
        let vi = Id.New(?name = v.Name, mut = false, opt = v.IsOptional, rest = v.IsRest)
        ReplaceId(v, vi).TransformExpression(expr)
    else expr         

let private checkNotMutating (env: Environment) a f =
    if env.IsInput a then
        failwith "arguments of inlined functions should not be mutated"
    else f a

let private setValue (env: Environment) expr value =
    match expr with
    | GlobalAccess a ->
        match a.Address with
        | i :: m -> ItemSet(GlobalAccess { a with Address = m }, Value (String i), value)
        | _ -> failwith "cannot set the window object"
    | ItemGet (d, e, _) -> ItemSet(d, e, value)
    | Var d -> checkNotMutating env (Var d) (fun _ -> VarSet(d, value))
    | _ -> failwith "invalid form for setter"

let wsruntime = GlobalAccess (Address.RuntimeAddr [])

let jsFunctionMembers =
    System.Collections.Generic.HashSet [
        "apply"
        "bind"
        "call"
    ]

let wsRuntimeFunctions =
    System.Collections.Generic.HashSet [
        "Create"
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
        "CompileError"
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

let inline private trI (jsId: S.Id) = Id.New(jsId.Name, opt = jsId.Optional, rest = jsId.Rest)

let private trFuncKind (k: S.FuncKind) =
    match k with
    | S.Normal -> FuncKind.Normal
    | S.Generator -> FuncKind.Generator
    | S.Async -> FuncKind.Async
    | S.AsyncGenerator -> FuncKind.AsyncGenerator

/// An optional chain split at its outermost `?.` link
type private OptionalSplit =
    {
        /// The value checked for null or undefined
        Object : S.Expression
        /// Stands for the value of Object in Chain
        Placeholder : S.Id
        /// The rest of the chain, evaluated when Object is not null or undefined
        Chain : S.Expression
        /// For a method call `x.m?.(args)`, `x` is evaluated once and passed as `this`
        ThisBinding : option<S.Id * S.Expression>
    }

let mutable private optionalCounter = 0

let rec private splitOptional (e: S.Expression) : option<OptionalSplit> =
    let fresh () =
        optionalCounter <- optionalCounter + 1
        S.Id.New ("$opt" + string optionalCounter)
    match e with
    | S.ExprPos (x, _) -> splitOptional x
    | S.Binary (x, SB.``?.``, m) ->
        let v = fresh ()
        Some { Object = x; Placeholder = v; Chain = S.Binary (S.Var v, SB.``.``, m); ThisBinding = None }
    | S.OptionalApplication (S.IgnoreEPos (S.Binary (x, SB.``.``, m)), args) ->
        let xv = fresh ()
        let v = fresh ()
        Some {
            Object = S.Binary (S.Var xv, SB.``.``, m)
            Placeholder = v
            Chain = S.Application (S.Binary (S.Var v, SB.``.``, S.Constant (S.String "call")), [], S.Var xv :: args)
            ThisBinding = Some (xv, x)
        }
    | S.OptionalApplication (x, args) ->
        let v = fresh ()
        Some { Object = x; Placeholder = v; Chain = S.Application (S.Var v, [], args); ThisBinding = None }
    | S.Binary (x, SB.``.``, m) ->
        splitOptional x |> Option.map (fun s -> { s with Chain = S.Binary (s.Chain, SB.``.``, m) })
    | S.Application (x, ts, args) ->
        splitOptional x |> Option.map (fun s -> { s with Chain = S.Application (s.Chain, ts, args) })
    | S.TemplateLiteral (Some x, p, h) ->
        splitOptional x |> Option.map (fun s -> { s with Chain = S.TemplateLiteral (Some s.Chain, p, h) })
    | _ -> None

/// Checks if `arguments` is used
type private UsesArguments() =
    inherit Visitor()
    let mutable found = false
    member this.Found = found
    override this.VisitGlobalAccess a =
        if a.Address = [ "arguments" ] then found <- true

/// The `this` variable of a non-arrow function if its body uses `this` or `arguments`.
/// Otherwise the function can be written as an arrow function.
let private usedThis (env: Environment) (body: Statement) =
    match env.This.Value with
    | Some t ->
        let usesArgs = UsesArguments()
        usesArgs.VisitStatement body
        if usesArgs.Found || CountVarOccurence(t).GetForStatement(body) > 0 then Some t else None
    | None -> None

/// Statements setting default values of parameters at the start of a function body
let private defaultValueStatements (env: Environment) (pars: S.Id list) (vars: Id list) trE =
    List.zip pars vars |> List.choose (fun (p, v) ->
        p.DefaultValue |> Option.map (fun d ->
            If (Binary (Var v, BinaryOperator.``===``, Undefined), ExprStatement (VarSet (v, trE d)), Empty)
        )
    )

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
    // a op= c, where op is a logical operator: a op (a = c), evaluating the target once
    let logicalAssign a op c =
        match trE a with
        | ItemGet (o, k, p) ->
            let ov = Id.New(mut = false)
            let kv = Id.New(mut = false)
            Let (ov, o, Let (kv, k, Binary (ItemGet (Var ov, Var kv, p), op, ItemSet (Var ov, Var kv, trE c))))
        | ta ->
            checkNotMutating ta (fun ta -> Binary (ta, op, setValue env ta (trE c)))
    let optionalSplit =
        match expr with
        | S.Binary (_, (SB.``.`` | SB.``?.``), _)
        | S.Application _
        | S.OptionalApplication _
        | S.TemplateLiteral (Some _, _, _) -> splitOptional expr
        | _ -> None
    match optionalSplit with
    | Some s ->
        let scope = env.Vars.Head
        let thisBinding =
            s.ThisBinding |> Option.map (fun (xv, x) ->
                let tx = trE x
                let v = Id.New(mut = false)
                scope.[xv.Name] <- Var v
                v, tx
            )
        let trO = trE s.Object
        let pv = Id.New(mut = false)
        scope.[s.Placeholder.Name] <- Var pv
        let res = OptionalChain (trO, pv, trE s.Chain)
        match thisBinding with
        | Some (v, tx) -> Let (v, tx, res)
        | None -> res
    | None ->
    match expr with
    | S.Yield (a, d) -> YieldExpr (Option.map trE a, d)
    | S.Await a -> Await (trE a)
    | S.Spread a -> Spread (trE a)
    | S.TemplateLiteral (tag, parts, holes) -> TemplateString (Option.map trE tag, parts, List.map trE holes)
    | S.ComputedMember _ -> failwith "Computed member name outside of object literal"
    | S.Application (a, [], b) ->
        let trA =
            match trE a with
            | ItemGet (a, b, _) -> ItemGet (a, b, Pure)
            | trA -> trA
        match trA with
        | Var t when t = Id.SourceType() ->
            match b |> List.map trE with
            | [ Value (String n) ] ->
                GlobalAccess (Address.TypeDefaultExport { Assembly = env.AssemblyName; TypeName = n})
            | [ Value (String a); Value (String n) ] ->
                GlobalAccess (Address.TypeDefaultExport { Assembly = a; TypeName = n})
            | _ ->
                failwithf "$type in inlines can be only used with one or two string arguments, specifying type name in current assembly or assembly name and type name"
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
                    let ga = Hashed (b :: a)
                    if env.MutableExternals.Contains ga then 
                        ItemGet(trA, trC, env.Purity)
                    else
                        GlobalAccess { Module = m; Address = b :: a }
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
        | SB.``??``     -> Binary(trE a, BinaryOperator.``??``, trE c)
        | SB.``??=``    -> mbin a MutatingBinaryOperator.``??=`` c
        | SB.``&&=``    -> logicalAssign a BinaryOperator.``&&`` c
        | SB.``||=``    -> logicalAssign a BinaryOperator.``||`` c
        | SB.``**=``    ->
            match trE a with
            | ItemGet (o, k, p) ->
                let ov = Id.New(mut = false)
                let kv = Id.New(mut = false)
                Let (ov, o, Let (kv, k, ItemSet (Var ov, Var kv, Binary (ItemGet (Var ov, Var kv, p), BinaryOperator.``**``, trE c))))
            | ta -> checkNotMutating ta (fun ta -> setValue env ta (Binary (ta, BinaryOperator.``**``, trE c)))
        | _ -> failwith "unrecognized binary operator"
    | S.Conditional (a, b, c) ->
        Conditional (trE a, trE b, trE c)
    | S.Constant a -> 
        match a with
        | S.False -> Bool false
        | S.Null -> Null
        | S.Number n -> JSNumber n
        | S.String x -> String x
        | S.True -> Bool true
        |> Value
    | S.Lambda (a, b, c, d, kind) ->
        let vars = b |> List.map trI
        let innerEnv = env.WithNewScope(Seq.zip b (vars |> Seq.map Var), d)
        // a non-arrow function has its own `this`, and stays a non-arrow function
        if not d then innerEnv.This.Value <- Some (Id.NewThis())
        let body = S.Block c
        let trBodyWithDefaults () =
            let defaults = defaultValueStatements innerEnv b vars (transformExpression innerEnv)
            let trBody = transformStatement innerEnv body
            CombineStatements (defaults @ [ trBody ])
        let thisOf b = if d then None else usedThis innerEnv b
        let fres =
            match a, kind with
            | None, S.Normal -> 
                let trBody = trBodyWithDefaults ()
                Function (vars, thisOf trBody, None, trBody)
            | None, _ -> 
                let trBody = trBodyWithDefaults ()
                FuncWithKind (trFuncKind kind, Function (vars, thisOf trBody, None, trBody))
            | Some a, S.Normal -> 
                let f = innerEnv.NewVar a
                let trBody = trBodyWithDefaults ()
                StatementExpr(FuncDeclaration(f, vars, thisOf trBody, trBody, []), Some f)
            | Some a, _ -> 
                let f = innerEnv.NewVar a
                let trBody = trBodyWithDefaults ()
                let func = FuncWithKind (trFuncKind kind, Function (vars, thisOf trBody, None, trBody))
                StatementExpr(VarDeclaration(f, func), Some f)
        innerEnv.Vars.Head.Values |> Seq.choose (function Var i -> Some i | _ -> None)
        |> Seq.fold makePossiblyImmutable fres
    | S.New (a, [], b) -> 
        New(trE a, [], List.map trE b)
    | S.NewArray a -> NewArray (a |> List.map (function Some i -> trE i | _ -> Undefined))
    | S.NewObject a when a |> List.exists (fun (_, _, v) -> match S.IgnoreExprPos v with S.Spread _ | S.ComputedMember _ -> true | _ -> false) ->
        // { a: 1, ...b, [c]: 2 } -> Object.assign({ a: 1 }, b), o[c] = 2
        let isSpecial (_, _, v) = match S.IgnoreExprPos v with S.Spread _ | S.ComputedMember _ -> true | _ -> false
        let first = a |> List.takeWhile (isSpecial >> not)
        let rest = a |> List.skipWhile (isSpecial >> not)
        let o = Id.New("o", mut = false)
        let ops =
            rest |> List.map (fun (n, k, v) ->
                match S.IgnoreExprPos v, k with
                | S.Spread x, _ ->
                    Appl (ItemGet (Global [ "Object" ], Value (String "assign"), Pure), [ Var o; trE x ], NonPure, None)
                | S.ComputedMember (key, value), S.Simple -> ItemSet (Var o, trE key, trE value)
                | S.ComputedMember _, _ -> failwith "Currently unsupported: getter or setter with a computed name in inline object literal"
                | _, S.Simple -> ItemSet (Var o, Value (String n), trE v)
                | _ -> failwith "Currently unsupported: getter or setter after a spread or computed member in inline object literal"
            )
        Let (o, trE (S.NewObject first), Sequential (ops @ [ Var o ]))
    | S.NewObject a ->
        let trKind k =
            match k with
            | S.Accessor.Get -> MemberKind.Getter
            | S.Accessor.Set -> MemberKind.Setter
            | S.Accessor.Simple -> MemberKind.Simple
        Object(a |> List.map (fun (b, k, c) -> b, trKind k, trE c))
    | S.NewRegex a -> 
        let closingSlash = a.LastIndexOf '/'
        let flags = a.[closingSlash + 1 ..] |> Seq.map (string >> String >> Value) |> List.ofSeq
        New (Global ["RegExp"], [], Value (String a.[1 .. closingSlash - 1]) :: flags)
    | S.Postfix (a, b) ->
        match b with
        | S.PostfixOperator.``++`` -> mun a MutatingUnaryOperator.``()++``
        | S.PostfixOperator.``--`` -> mun a MutatingUnaryOperator.``()--``
        | _ -> failwith "unrecognized postfix operator"
    | S.This -> env.ThisVar
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
        | "$import" ->
            match env.Import with
            | Some i ->
                GlobalAccess i
            | None ->
                failwith "Import attribute not found for use of $import in Inline."
        | "$importFile" ->
            match env.Import with
            | Some i ->
                SideeffectingImport i
            | None ->
                failwith "Import attribute not found for use of $importFile in Inline."
        //| "arguments" -> Arguments
        | "undefined" -> Undefined
        | n ->
        match env.TryFindVar a with
        | Some e -> e
        | None -> 
            if a.Name.StartsWith("$") && a.Name <> "$" && not (env.ExpectedDollarVars |> Array.contains a.Name) then
                env.UnknownArgs.Add a.Name |> ignore
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
    | S.Debugger -> Debugger
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
        let l = Id.New a
        Labeled (l, transformStatement { env with Labels = env.Labels.Add(a, l) } b)
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
    | S.Vars (a, kind) ->
        let decl (var, value) =
            let value = match value with Some v -> trE v | None -> Undefined
            VarDeclaration (env.NewVar(var, kind <> S.ConstDecl), value)
        match a with
        | [ d ] -> decl d
        | _ -> Block (a |> List.map decl)
    | S.While (a, b) -> While (trE a, trS b)
    | S.With (a, b) -> failwith "Unsupported: JS with statement"
    | S.ForOf (a, b, c) ->
        let v = Id.New "x"
        ForOf (v, trE b, Block [ ExprStatement (setValue env (trE a) (Var v)); trS c ])
    | S.ForDeclOf (kind, a, b, c) ->
        let trB = trE b
        let v = env.NewVar(a, kind <> S.ConstDecl)
        ForOf (v, trB, trS c)
    | S.ForDeclIn (kind, a, b, c) ->
        let trB = trE b
        let v = env.NewVar(a, kind <> S.ConstDecl)
        ForIn (v, trB, trS c)
    | S.Function (a, b, c, kind) ->
        let f = env.NewVar a
        let vars = b |> List.map trI
        let innerEnv = env.WithNewScope(Seq.zip b (vars |> Seq.map Var), false)
        innerEnv.This.Value <- Some (Id.NewThis())
        let trBody =
            match c with
            | Some c ->
                let defaults = defaultValueStatements innerEnv b vars (transformExpression innerEnv)
                CombineStatements (defaults @ [ transformStatement innerEnv (S.Block c) ])
            | _ -> Empty
        match kind with
        | S.Normal ->
            FuncDeclaration(f, vars, usedThis innerEnv trBody, trBody, [])
        | _ ->
            VarDeclaration(f, FuncWithKind (trFuncKind kind, Function (vars, usedThis innerEnv trBody, None, trBody)))
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

    override this.TransformFuncDeclaration(a, b, c, d, e) = FuncDeclaration(a, b, c, d, e)
    
    member this.Run(st) =
        let res = this.TransformStatement(st)
        StatementExpr(res, returnVar)
                
type ParseResult =
    {
        Expr: Expression
        Warnings: string list
    }

let createInline ext thisArg args isPure import dollarVars assemblyName inlineString =        
    let parsed = 
        try inlineString |> P.Source.FromString |> P.ParseExpression |> Choice1Of2
        with _ -> inlineString |> P.Source.FromString |> P.ParseProgram |> Choice2Of2 
    let env = Environment.New(thisArg, false, isPure, args, ext, import, dollarVars, assemblyName)
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

let parseDirect ext thisArg args dollarVars assemblyName jsString =
    let parsed = 
        try jsString |> P.Source.FromString |> P.ParseExpression |> Choice1Of2
        with _ -> jsString |> P.Source.FromString |> P.ParseProgram |> Choice2Of2 
    let env = Environment.New(thisArg, true, false, args, ext, None, dollarVars, assemblyName)
    let body =
        match parsed with
        | Choice1Of2 e ->
            e |> transformExpression env |> Return 
        | Choice2Of2 p ->
            p
            |> S.Block
            |> transformStatement env
    {
        Expr = Function(args, thisArg, None, body)
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


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

module WebSharper.Compiler.FSharp.TailCalls

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata
open WebSharper.Compiler
open WebSharper.Compiler.NotResolved
module I = IgnoreSourcePos

type TailPos =
    | TailPos
    | UnsureTailPos 
    | NotTailPos
    | MemberRoot

type Environment =
    {
        TailCalls : HashSet<Id>
        ScopeCalls : list<Dictionary<Id, int * bool>>
        mutable TailPos : TailPos 
        SelfTailCall : ref<option<bool>>
        CurrentMethod : option<TypeDefinition * Method>
        mutable ThisAlias : option<Id>
        Inlines : HashSet<Method>
    }

    static member New(m, inl) =
        {
            TailCalls = HashSet()
            ScopeCalls = []
            TailPos = MemberRoot
            SelfTailCall = ref None
            CurrentMethod = m
            ThisAlias = None
            Inlines = inl
        }

    member this.WithScope(sc) =
        { this with
            ScopeCalls = sc :: this.ScopeCalls   
        }

type TailCallAnalyzer(env) =
    inherit Visitor()

    let nextTailPos() =
        match env.TailPos with
        | NotTailPos -> NotTailPos
        | _ -> TailPos

    let hasInScope x n =
        match env.ScopeCalls with
        | [] -> false
        | current :: _ ->
            match current.TryGetValue(x) with
            | true, (appl, isUsed) when appl = n -> 
                if env.TailPos = NotTailPos then
                    current.Remove x |> ignore
                    false
                else
                    if not isUsed then
                        current.[x] <- (n, true)
                    true
            | _ -> false

    override this.VisitExpression(expr) =
        match env.TailPos with
        | TailPos -> env.TailPos <- UnsureTailPos
        | UnsureTailPos -> env.TailPos <- NotTailPos
        | _ -> ()
        base.VisitExpression(expr)

    override this.VisitExprSourcePos(_, expr) =
        base.VisitExpression expr
    
    override this.VisitStatementSourcePos(_, st) =
        base.VisitStatement st
    
    override this.VisitApplication(f, args, _, _) =
        match f with
        | I.Var f when hasInScope f 1 ->
            ()
        | _ ->
            this.VisitExpression f    
        args |> List.iter this.VisitExpression

    override this.VisitCurriedApplication(f, args) =
        match f with
        | I.Var f when hasInScope f args.Length ->
            ()
        | _ ->
            this.VisitExpression f    
        args |> List.iter (snd >> this.VisitExpression)

    override this.VisitConditional(c, a, b) =
        let p = nextTailPos()
        env.TailPos <- NotTailPos
        this.VisitExpression c
        env.TailPos <- p
        this.VisitExpression a
        env.TailPos <- p
        this.VisitExpression b
      
    override this.VisitFunction(args, body) =
        match env.TailPos with
        | MemberRoot when Option.isSome env.CurrentMethod ->
            let scope = Dictionary()
            env.SelfTailCall := Some false
            let inner = TailCallAnalyzer(env.WithScope scope)
            inner.VisitStatement body  
        | _ ->
            let scope = Dictionary()
            let inner = TailCallAnalyzer(env.WithScope scope)
            inner.VisitStatement body      
    
    override this.VisitCall(obj, td, meth, args) =
        let sameType, sameMethod = 
            if Option.isNone !env.SelfTailCall then 
                false, false 
            else
                match env.CurrentMethod with
                | Some (ct, cm) -> (ct = td.Entity), (cm = meth.Entity)
                | None -> false, false
        let isOtherMethodCall = 
            sameType && not sameMethod &&
            not (env.Inlines.Contains meth.Entity)
        if isOtherMethodCall then
            env.SelfTailCall := None
        if sameType && sameMethod then
            let isTailSelfCall =
                env.TailPos <> NotTailPos &&   
                (
                    match env.ScopeCalls with
                    | [ _ ] -> true
                    | _ -> false // self call from a closure
                )
                && (
                    match obj with
                    | None 
                    | Some I.This -> true
                    | Some (I.Var t) ->
                        match env.ThisAlias with
                        | Some thisVar -> t = thisVar
                        | _ -> false
                    | _ -> false
                )
            env.SelfTailCall := if isTailSelfCall then Some true else None 
        env.TailPos <- NotTailPos
        obj |> Option.iter this.VisitExpression 
        args |> List.iter this.VisitExpression
        
    override this.VisitLet(var, value, body) =
        match value with
        | I.This -> env.ThisAlias <- Some var
        | _ -> ()
        let p = nextTailPos()
        env.TailPos <- NotTailPos
        this.VisitExpression value
        env.TailPos <- p        
        this.VisitExpression body
          
    override this.VisitLetRec(bindings, body) =
        let scope = Dictionary()
        let valueBodies = ResizeArray()
        for var, value in bindings do
            match value with
            | CurriedLambda (args, body, _) ->
                scope.Add(var, (args.Length, false))
                valueBodies.Add (body, true) 
            | Lambda (_, body, _) ->
                scope.Add(var, (1, false))
                valueBodies.Add (body, true) 
            | _ ->
                valueBodies.Add (value, false)    
        let innerEnv = env.WithScope scope
        let inner = TailCallAnalyzer(innerEnv)
        for body, isFunc in valueBodies do
            if isFunc then 
                innerEnv.TailPos <- TailPos
                inner.VisitExpression(body)
            else
                this.VisitExpression body
        scope |> Seq.choose (function
            | (KeyValue (k, (_, true))) -> Some k
            | _ -> None)
        |> env.TailCalls.UnionWith
        this.VisitExpression(body)

    override this.VisitId x =
        match env.ScopeCalls with
        | current :: outer ->
            if env.TailPos = NotTailPos then
                current.Remove(x) |> ignore
            for sc in outer do
                sc.Remove(x) |> ignore
        | _ -> ()
       
    override this.VisitSequential xs =
        match List.rev xs with
        | [] -> ()
        | h :: t ->
            let p = nextTailPos()
            env.TailPos <- NotTailPos
            t |> List.iter this.VisitExpression
            env.TailPos <- p        
            this.VisitExpression h

type AddCapturing(vars : seq<Id>) =
    inherit Transformer()

    let defined = HashSet(vars)
    let captured = HashSet()
    let mutable scope = 0

    override this.TransformNewVar(var, value) =
        if scope = 0 then
            defined.Add var |> ignore
        base.TransformNewVar(var, value)

    override this.TransformVarDeclaration(var, value) =
        if scope = 0 then
            defined.Add var |> ignore
        base.TransformVarDeclaration(var, value)

    override this.TransformLet(var, value, body) =
        if scope = 0 then
            defined.Add var |> ignore
        base.TransformLet(var, value, body)

    override this.TransformLetRec(defs, body) = 
        if scope = 0 then
            for var, _ in defs do
                defined.Add var |> ignore
        base.TransformLetRec(defs, body)
    
    override this.TransformId i =
        if scope > 0 && defined.Contains i then 
            captured.Add i |> ignore
        i

    override this.TransformFunction(args, body) =
        scope <- scope + 1
        let res = 
            if scope = 1 then
                captured.Clear()
                let f = base.TransformFunction(args, body)
                if captured.Count > 0 then
                    let cVars = captured |> List.ofSeq
                    let cArgs = cVars |> List.map (fun v -> Id.New(?name = v.Name, mut = false))
                    Application(
                        Function(cArgs, Return (ReplaceIds(Seq.zip cVars cArgs |> dict).TransformExpression f)), 
                        cVars |> List.map Var, NonPure, None) 
                else f
            else
                base.TransformFunction(args, body)
        scope <- scope - 1
        res

type ContainsMappedVar(mapping, v) =
    inherit Visitor()

    let mutable go = true

    override this.VisitExpression(e) =
        if go then
            base.VisitExpression(e)

    override this.VisitId(x) =
        if mapping x = v then
            go <- false    

    member this.Check(e) =
        this.VisitExpression(e)
        not go

let containsMappedVar mapping v expr =
    ContainsMappedVar(mapping, v).Check(expr)

type TailCallTransformer(env) =
    inherit Transformer()

    // key: function id
    // value: transformed args for mutual recursion, original args, indexing in mutual recursion 
    let transforming = Dictionary<Id, list<Id> * list<Id> * option<Id * int>>()
    let transformIds = Dictionary<Id, Id>()
    let argCopies = Dictionary<Id, Id>()
    let copying = HashSet<Id>()
    let mutable selfCallArgs = None
    let mutable currentIndex = None

    let withCopiedArgs args b =
        let copiedArgs =
            args |> Seq.choose (fun a ->
                match argCopies.TryGetValue a with
                | true, c -> Some c 
                | _ -> None
            ) |> List.ofSeq    
        if List.isEmpty copiedArgs then
            b
        else
            Block [
                for a in copiedArgs -> VarDeclaration(a, Undefined)
                yield b  
            ]
        |> AddCapturing(args).TransformStatement

    member this.Recurse(fArgs, origArgs: list<_>, args: list<_>, index) =
        Sequential [
            // if recurring with multiple arguments,
            // current values sometimes need copying
            if args.Length > 1 then
                let nowCopying = HashSet()
                let recArgs =
                    fArgs |> Seq.mapi (fun i a ->
                        let oa = origArgs.[i]
                        match args.[i] with
                        | I.Var v when v = oa -> None
                        | ar ->
                            let needsCopy =
                                args |> Seq.skip (i + 1) |> Seq.exists (fun b ->
                                    containsMappedVar this.TransformId a b
                                )                       
                            if needsCopy then 
                                nowCopying.Add a |> ignore
                                if not (argCopies.ContainsKey a) then
                                    argCopies.Add(a, Id.New(?name = a.Name, mut = false)) 
                            Some (a, ar)
                    )
                    |> Seq.choose id |> List.ofSeq
                copying.UnionWith(nowCopying)
                for a in nowCopying do
                    yield VarSet(argCopies.[a], Var a)    
                match index with
                | Some (iv, i) when currentIndex <> Some i -> 
                    yield VarSet(iv, Value (Int i))  
                | _ -> ()
                for x, v in recArgs do
                    yield VarSet(x, this.TransformExpression v)
                copying.ExceptWith(nowCopying)
                yield StatementExpr (DoNotReturn, None)
            else
                for x, v in Seq.zip fArgs args do
                    yield VarSet(x, this.TransformExpression v)
                match index with
                | Some (iv, i) when currentIndex <> Some i ->
                    yield VarSet(iv, Value (Int i))  
                | _ -> ()
                yield StatementExpr (DoNotReturn, None)
        ]

    override this.TransformApplication(f, args, p, l) =
        match f with
        | I.Var f when transforming.ContainsKey f ->
            let fArgs, origArgs, index = transforming.[f]
            this.Recurse(fArgs, origArgs, args, index)
        | _ ->
            base.TransformApplication(f, args, p, l)

    override this.TransformCurriedApplication(f, args) =
        match f with
        | I.Var f when transforming.ContainsKey f ->
            failwith "CurriedApplication should have been optimized away"
        | _ ->
            base.TransformCurriedApplication(f, args)

    override this.TransformId i =
        let j = 
            match transformIds.TryGetValue i with
            | true, j -> j
            | _ -> i
        if copying.Contains j then
            argCopies.[j]
        else j

    override this.TransformLetRec(bindings, body) =
        // optimize tupled and curried
        let bindings = Array.ofList bindings
        let mutable body = body
        for bi = 0 to bindings.Length - 1 do
            let var, value = bindings.[bi]
            match value with
            | CurriedFunction(fArgs, fBody) ->
                let f = Function(fArgs, fBody)
                bindings.[bi] <- var, f
                let tr = OptimizeLocalCurriedFunc(var, List.length fArgs)
                for bj = 0 to bindings.Length - 1 do
                    let v, c = bindings.[bj]                              
                    bindings.[bj] <- v, tr.TransformExpression(c)   
                body <- tr.TransformExpression(body)    
            | TupledLambda(fArgs, fBody, isReturn) ->
                bindings.[bi] <- var, Function(fArgs, if isReturn then Return fBody else ExprStatement fBody) 
                let tr = OptimizeLocalTupledFunc(var, List.length fArgs)
                for bj = 0 to bindings.Length - 1 do
                    let v, c = bindings.[bj]
                    bindings.[bj] <- v, tr.TransformExpression(c)   
                body <- tr.TransformExpression(body)    
            | _ -> ()
        // optimize tail calls
        let matchedBindings = ResizeArray() 
        let mutable funcCount = 0
        let mutable numArgs = 0
        let mutable minArgs = System.Int32.MaxValue
        for var, value in bindings do
            if env.TailCalls.Contains(var) then
                match value with
                | Lambda(args, fbody, isReturn) ->
                    let args = 
                        args |> List.map (fun a -> 
                            let am = a.ToMutable()
                            transformIds.Add(a, am)
                            am
                        )
                    matchedBindings.Add(var, Choice1Of2 (args, fbody))
                    numArgs <- max numArgs args.Length 
                    minArgs <- min minArgs args.Length 
                    funcCount <- funcCount + 1
                | _ -> matchedBindings.Add(var, Choice2Of2 value)
            else matchedBindings.Add(var, Choice2Of2 value)
        match funcCount with
        | 0 -> base.TransformLetRec(List.ofArray bindings, body) 
        | 1 ->
            let trBindings = ResizeArray() 
            for var, value in matchedBindings do  
                match value with     
                | Choice1Of2 (args, fbody) ->
                    transforming.Add(var, (args, args, None))
                    let trFBody = 
                        While (Value (Bool true), 
                            Return (this.TransformExpression(fbody)))             
                        |> withCopiedArgs args
                    trBindings.Add(var, Function(args, trFBody))
                    transforming.Remove var |> ignore
                | Choice2Of2 value ->
                    trBindings.Add(var, value)
            match List.ofSeq trBindings with
            | [ var, value ] ->
                Let(var, value, base.TransformExpression body) 
            | trBindings ->
                LetRec(trBindings, base.TransformExpression body) 
        | _ ->
            let indexVar = Id.New "recI"
            let recFunc = Id.New("recF", mut = false)
            let mutable i = 0
            let newArgs = Array.init numArgs (fun i -> Id.New(opt = (i >= minArgs)))
            for var, value in matchedBindings do  
                match value with     
                | Choice1Of2 (args, _) ->
                    let aargs = List.init args.Length (fun j -> newArgs.[j])
                    args |> List.iteri (fun j a -> transformIds.[a] <- newArgs.[j])
                    transforming.Add(var, (aargs, args, Some(indexVar, i)))
                    i <- i + 1
                | _ -> ()
            let trBodies = ResizeArray() 
            let trBindings = ResizeArray()
            i <- 0
            for var, value in matchedBindings do  
                match value with     
                | Choice1Of2 (args, fbody) ->
                    let ci = currentIndex
                    currentIndex <- Some i
                    let trFBody = this.TransformExpression(fbody)    
                    currentIndex <- ci
                    trBodies.Add(Some (Value (Int i)), Block [ Return trFBody; Break None; ] )
                    let recArgs = Value (Int i) :: List.map Var args
                    trBindings.Add(var, 
                        Function(args, 
                            Return (Application (Var recFunc, recArgs, NonPure, Some (numArgs + 1)))))                    
                    i <- i + 1
                | Choice2Of2 value ->
                    trBindings.Add(var, value)
            for var, value in matchedBindings do  
                match value with     
                | Choice1Of2 _ ->
                    transforming.Remove var |> ignore
                | _ -> ()
            let mainFunc =
                recFunc, Function(indexVar :: List.ofSeq newArgs,
                    While (Value (Bool true), 
                        Switch (Var indexVar, List.ofSeq trBodies))   
                    |> withCopiedArgs newArgs   
                )    
            LetRec(mainFunc :: List.ofSeq trBindings, base.TransformExpression body) 

    override this.TransformFunction(args, body) =
        let isTailRecMethodFunc =
            match !env.SelfTailCall with
            | Some isUsed -> 
                env.SelfTailCall := None
                isUsed
            | None -> false
        match isTailRecMethodFunc, body with
        | true, (I.Return b | I.ExprStatement b) ->
            selfCallArgs <- Some args
            let args = 
                args |> List.map (fun a -> 
                    let am = a.ToMutable()
                    transformIds.Add(a, am)
                    am
                )
            Function(args,
                While (Value (Bool true), 
                    Return (this.TransformExpression(b)))       
                |> withCopiedArgs args
            )             
        | _ ->
            base.TransformFunction(args, body)

    override this.TransformCall(obj, td, meth, args) =
        match env.CurrentMethod, selfCallArgs with
        | Some (ct, cm), Some fArgs
            when td.Entity = ct && meth.Entity = cm ->
                this.Recurse(fArgs, fArgs, args, None)
        | _ ->
            base.TransformCall(obj, td, meth, args)       

let optimize methOpt inlines expr =
    let env = Environment.New(methOpt, inlines) 
    TailCallAnalyzer(env).VisitExpression(expr)  
    TailCallTransformer(env).TransformExpression(expr)

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

type RecCall =
    | MethodCall of TypeDefinition * Method
    | LocalFunction of Id

type TailPos =
    | TailPos
    | UnsureTailPos 
    | NotTailPos

type Environment =
    {
        TailCalls : HashSet<RecCall>
        ScopeCalls : list<Dictionary<RecCall, int * bool>>
        mutable TailPos : TailPos 
    }

    static member New() =
        {
            TailCalls = HashSet()
            ScopeCalls = []
            TailPos = TailPos
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
        env.TailPos <> NotTailPos &&
        match env.ScopeCalls with
        | [] -> false
        | hs :: _ ->
            match hs.TryGetValue(x) with
            | true, (appl, isUsed) when appl = n -> 
                if not isUsed then
                    hs.[x] <- (n, true)
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
        | I.Var f when hasInScope (LocalFunction f) 1 ->
            ()
        | _ ->
            this.VisitExpression f    
        args |> List.iter this.VisitExpression

    override this.VisitCurriedApplication(f, args) =
        env.TailPos <- NotTailPos
        match f with
        | I.Var f when hasInScope (LocalFunction f) args.Length ->
            ()
        | _ ->
            this.VisitExpression f    
        args |> List.iter this.VisitExpression

    override this.VisitConditional(c, a, b) =
        let p = nextTailPos()
        this.VisitExpression c
        env.TailPos <- p
        this.VisitExpression a
        env.TailPos <- p
        this.VisitExpression b
      
    override this.VisitFunction(args, body) =
        let scope = Dictionary()
        let inner = TailCallAnalyzer(env.WithScope scope)
        inner.VisitStatement body      
        
    override this.VisitLet(var, value, body) =
        let p = nextTailPos()
        this.VisitExpression value
        env.TailPos <- p        
        this.VisitExpression body
          
    override this.VisitLetRec(bindings, body) =
        let scope = Dictionary()
        let valueBodies = ResizeArray()
        for var, value in bindings do
            match value with
            | CurriedLambda (args, body, _) ->
                scope.Add(LocalFunction var, (args.Length, false))
                valueBodies.Add (body, true) 
            | Lambda (_, body, _) ->
                scope.Add(LocalFunction var, (1, false))
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
                current.Remove(LocalFunction x) |> ignore
            for sc in outer do
                sc.Remove(LocalFunction x) |> ignore
        | _ -> ()
       
    override this.VisitSequential xs =
        match List.rev xs with
        | [] -> ()
        | h :: t ->
            let p = nextTailPos()
            t |> List.iter this.VisitExpression
            env.TailPos <- p        
            this.VisitExpression h

type TailCallTransformer(tailcalls: HashSet<RecCall>) =
    inherit Transformer()

    // key: function id
    // value: label, transformed args, original args, indexing in mutual recursion 
    let transforming = Dictionary<Id, Id * list<Id> * list<Id> * option<Id * int>>()
    let transformIds = Dictionary<Id, Id>()
    let argCopies = Dictionary<Id, Id>()
    let copying = HashSet<Id>()

    override this.TransformApplication(f, args, p, l) =
        match f with
        | I.Var f when transforming.ContainsKey f ->
            let label, fArgs, origArgs, index = transforming.[f]
            Sequential [
                // if recurring with multiple arguments,
                // current values sometimes need copying
                if args.Length > 1 then
                    let nowCopying = HashSet()
                    fArgs |> Seq.iteri (fun i a ->
                        let checker = VarsNotUsed(Seq.take (i + 1) origArgs)
                        let needsCopy =
                            args |> Seq.skip (i + 1) |> Seq.exists (fun b ->
                                not (checker.Get(b))
                            )                       
                        if needsCopy then 
                            nowCopying.Add a |> ignore
                            if not (argCopies.ContainsKey a) then
                                argCopies.Add(a, Id.New(?name = a.Name)) 
                    )
                    copying.UnionWith(nowCopying)
                    for a in nowCopying do
                        yield VarSet(argCopies.[a], Var a)    
                    match index with
                    | Some (iv, i) -> yield VarSet(iv, Value (Int i))  
                    | None -> ()
                    for x, v in Seq.zip fArgs args do
                        yield VarSet(x, this.TransformExpression v)
                    copying.ExceptWith(nowCopying)
                    yield StatementExpr(Continue (Some label), None)
                else
                    for x, v in Seq.zip fArgs args do
                        yield VarSet(x, this.TransformExpression v)
                    match index with
                    | Some (iv, i) -> yield VarSet(iv, Value (Int i))  
                    | None -> ()
                    yield StatementExpr(Continue (Some label), None)
            ]
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
                bindings.[bi] <- var, Function(fArgs, fBody) 
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
        let label = Id.New "rec"
        for var, value in bindings do
            if tailcalls.Contains(LocalFunction var) then
                match value with
                | Lambda(args, fbody, isReturn) ->
                    matchedBindings.Add(var, Choice1Of2 (args, fbody))
                    numArgs <- max numArgs args.Length 
                    funcCount <- funcCount + 1
                | _ -> matchedBindings.Add(var, Choice2Of2 value)
            else matchedBindings.Add(var, Choice2Of2 value)
        match funcCount with
        | 0 -> base.TransformLetRec(List.ofArray bindings, body) 
        | 1 ->
            let trBindings = ResizeArray() 
            let mutable fArgs = []
            for var, value in matchedBindings do  
                match value with     
                | Choice1Of2 (args, fbody) ->
                    transforming.Add(var, (label, args, args, None))
                    fArgs <- args
                    let trFBody = this.TransformExpression(fbody)    
                    let trFBody =
                        Labeled(label, 
                            While (Value (Bool true), 
                                Return trFBody))             
                    trBindings.Add(var, Function(args, trFBody))
                    transforming.Remove var |> ignore
                | Choice2Of2 value ->
                    trBindings.Add(var, value)
            let res = LetRec(List.ofSeq trBindings, base.TransformExpression body) 
            let copiedArgs =
                fArgs |> Seq.choose (fun a ->
                    match argCopies.TryGetValue a with
                    | true, c -> Some c 
                    | _ -> None
                ) |> List.ofSeq    
            if List.isEmpty copiedArgs then res else
                Sequential [
                    for a in copiedArgs -> NewVar(a, Undefined)
                    yield res  
                ]
        | _ ->
            let indexVar = Id.New "recI"
            let recFunc = Id.New("recF", mut = false)
            let mutable i = 0
            let newArgs = Array.init numArgs (fun _ -> Id.New())
            for var, value in matchedBindings do  
                match value with     
                | Choice1Of2 (args, _) ->
                    let aargs = List.init args.Length (fun j -> newArgs.[j])
                    args |> List.iteri (fun j a -> transformIds.Add(a, newArgs.[j]))
                    transforming.Add(var, (label, aargs, args, Some(indexVar, i)))
                    i <- i + 1
                | _ -> ()
            let trBodies = ResizeArray() 
            let trBindings = ResizeArray()
            i <- 0
            for var, value in matchedBindings do  
                match value with     
                | Choice1Of2 (args, fbody) ->
                    let trFBody = this.TransformExpression(fbody)    
                    trBodies.Add(Some (Value (Int i)), Return trFBody)
                    let recArgs = Value (Int i) :: List.map Var args
                    trBindings.Add(var, 
                        Function(args, 
                            Return (Application (Var recFunc, recArgs, false, Some (numArgs + 1)))))                    
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
                    Labeled(label, 
                        While (Value (Bool true), 
                            Switch (Var indexVar, List.ofSeq trBodies)))             
                )    
            let res = LetRec(mainFunc :: List.ofSeq trBindings, base.TransformExpression body) 
            let copiedArgs =
                newArgs |> Seq.choose (fun a ->
                    match argCopies.TryGetValue a with
                    | true, c -> Some c 
                    | _ -> None
                ) |> List.ofSeq    
            if List.isEmpty copiedArgs then res else
                Sequential [
                    for a in copiedArgs -> NewVar(a, Undefined)
                    yield res  
                ]

let optimize (expr) =
    let env = Environment.New() 
    TailCallAnalyzer(env).VisitExpression(expr)  
    TailCallTransformer(env.TailCalls).TransformExpression(expr)

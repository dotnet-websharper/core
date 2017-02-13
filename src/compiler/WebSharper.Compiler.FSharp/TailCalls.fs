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
                    printfn "tail call analyzer found possible tail call: %A" x
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

    let transforming = Dictionary<Id, Id * list<Id>>()

    override this.TransformApplication(f, args, p, l) =
        match f with
        | I.Var f when transforming.ContainsKey f ->
            let label, fArgs = transforming.[f]
            Sequential [
                for x, v in Seq.zip fArgs args do
                    yield VarSet(x, v)
                yield StatementExpr(Continue (Some label), None)
            ]
        | _ ->
            base.TransformApplication(f, args, p, l)

    override this.TransformCurriedApplication(f, args) =
        match f with
        | I.Var f when transforming.ContainsKey f ->
            let label, fArgs = transforming.[f]
            Sequential [
                for x, v in Seq.zip fArgs args do
                    yield VarSet(x, v)
                yield StatementExpr(Continue (Some label), None)
            ]
        | _ ->
            base.TransformCurriedApplication(f, args)

    override this.TransformLetRec(bindings, body) =
        match bindings with 
        | [ var, func ] when tailcalls.Contains(LocalFunction var) ->
            match func with
            | CurriedLambda(args, fbody, isReturn)
            | Lambda(args, fbody, isReturn) ->
                let label = Id.New "rec"
                transforming.Add(var, (label, args))
                let trFBody = this.TransformExpression(fbody)
                let trFBody =
                    Labeled(label, 
                        While (Value (Bool true), 
                            if isReturn then Return trFBody else ExprStatement trFBody))
                transforming.Remove var |> ignore
                let res = Let(var, Function(args, trFBody), base.TransformExpression body) 
                res
            | _ ->     
                base.TransformLetRec(bindings, body)                         
        | _ ->
            base.TransformLetRec(bindings, body)    

let optimize (expr) =
    let env = Environment.New() 
    TailCallAnalyzer(env).VisitExpression(expr)  
    if env.TailCalls.Count = 0 then expr else
        printfn "tail call on functions : %A" (List.ofSeq env.TailCalls)
        TailCallTransformer(env.TailCalls).TransformExpression(expr)

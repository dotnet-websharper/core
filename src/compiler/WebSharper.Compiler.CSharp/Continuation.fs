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

module internal WebSharper.Compiler.CSharp.Continuation
 
open WebSharper.Core.AST
open WebSharper.Compiler

module TaskMethod =
    let Start =  
        Method {
            MethodName = "Start"
            Parameters = []
            ReturnType = VoidType
            Generics = 0
        } |> NonGeneric 

    // This is a method on the Task proxy only
    let OnCompleted =
        Method {
            MethodName = "OnCompleted"
            Parameters = [FSharpFuncType (VoidType, VoidType)]
            ReturnType = VoidType
            Generics = 0
        } |> NonGeneric 
        
    // This is a method on the Task proxy only
    let RunContinuations =
        Method {
            MethodName = "RunContinuations"
            Parameters = []
            ReturnType = VoidType
            Generics = 0
        } |> NonGeneric 
    
type CollectLabels() =
    inherit StatementVisitor()

    let mutable labels = ResizeArray()

    member private this.Labels = List.ofSeq labels

    override this.VisitLabeled (a, b) =
        labels.Add a
        this.VisitStatement b

    static member Collect (s: Statement) =
        let h = CollectLabels()
        h.VisitStatement s
        h.Labels

type AwaitTransformer() =
    inherit Transformer()

    override this.TransformAwait(a) =
        let awaited = Id.New "$await"
        let doneLabel = Id.New "$done"
        let setStatus s = ItemSet(Var awaited, Value (String "exc"), !~(Int s))
        let start = Call(Some (Var awaited), NonGeneric Definitions.Task, TaskMethod.Start, [])
        let exc = ItemGet(Var awaited, Value (String "exc"), NoSideEffect)
        Sequential [
            NewVar(awaited, this.TransformExpression a)
            Conditional (setStatus 0, start, Undefined)
            IgnoredStatementExpr <| Continuation(doneLabel, Var awaited)
            IgnoredStatementExpr <| Labeled(doneLabel, Empty)
            IgnoredStatementExpr <| If (exc, Throw exc, Empty)
            ItemGet(Var awaited, Value (String "result"), NoSideEffect)                 
        ]

type HasGotos() =
    inherit StatementVisitor()
    let mutable found = false

    member private this.Found = found

    override this.VisitGoto(_) = 
        found <- true
    
    override this.VisitContinue(a) =
        if Option.isSome a then found <- true

    override this.VisitContinuation(_,_) =
        found <- true

    override this.VisitYield(_) =
        found <- true

    override this.VisitStatement(s) =
        if not found then base.VisitStatement(s)

    override this.VisitExpression(e) =
        if not found then base.VisitExpression(e)
        
    static member Check s =
        let vis = HasGotos()
        vis.VisitStatement s
        vis.Found    

type CountLabels() =
    inherit StatementVisitor()
    let mutable count = 0

    member private this.Count = count

    override this.VisitLabeled(_, a) =
        count <- count + 1
        this.VisitStatement a

    static member Get a =
        let c = CountLabels()
        c.VisitStatement a
        c.Count

type FreeNestedGotos(?loopStart, ?loopEnd, ?increment) =
    inherit StatementTransformer()

    member this.TransformFor(init, cond, incr, body, currentLabel) =
        let forStart = 
            match currentLabel with
            | Some l -> l
            | _ -> Id.New "$for"
        let forEnd = Id.New "$endfor"
        Block [
            match init with Some i -> yield ExprStatement i | _ -> ()
            yield Labeled (forStart, Empty)
            match cond with Some c -> yield If (c, Empty, Goto forEnd) | _ -> ()    
            yield FreeNestedGotos(forStart, forEnd, ?increment = incr).TransformStatement body
            match incr with Some i -> yield ExprStatement i | _ -> ()
            yield Goto forStart 
            yield Labeled (forEnd, Empty)
        ]

    override this.TransformFor(init, cond, incr, body) =
        let trBody = this.TransformStatement body
        if HasGotos.Check trBody then
            this.TransformFor(init, cond, incr, body, None)
        else For(init, cond, incr, trBody)

    override this.TransformLabeled(label, body) =
        match IgnoreStatementSourcePos body with
        | For (init, cond, incr, body) -> this.TransformFor(init, cond, incr, body, Some label)
        | _ -> base.TransformLabeled(label, body)  

    override this.TransformContinue _ =
        match increment with
        | Some incr ->
            Block [
                ExprStatement incr
                Goto loopStart.Value
            ]
        | _ ->
        Goto loopStart.Value

    override this.TransformBreak _ =
        Goto loopEnd.Value

    // TODO : current label check for while, dowhile
    override this.TransformWhile(cond, body) =
        if HasGotos.Check body then
            let whileStart = Id.New "$while"
            let whileEnd = Id.New "$endwhile"
            Block [
                Labeled (whileStart, Empty)
                If (cond, Empty, Goto whileEnd)
                FreeNestedGotos(whileStart, whileEnd).TransformStatement body
                Goto whileStart
                Labeled(whileEnd, Empty)
            ]
        else While(cond, body)

    override this.TransformDoWhile(body, cond) =
        if HasGotos.Check body then
            let whileStart = Id.New "$dowhile"
            let whileEnd = Id.New "$enddowile"
            Block [
                Labeled (whileStart, Empty)
                FreeNestedGotos(whileStart, whileEnd).TransformStatement body
                If (cond, Goto whileStart, Empty)
            ] 
        else DoWhile(body, cond)

    override this.TransformIf(cond, sThen, sElse) =
        if HasGotos.Check sThen || HasGotos.Check sElse then
            let elseStart = Id.New "$else"
            let endIf = Id.New "$endif"
            Block [
                If (cond, Empty, Goto elseStart)
                this.TransformStatement sThen
                Goto endIf
                Labeled(elseStart, this.TransformStatement sElse)
                Labeled(endIf, Empty)
            ]   
        else If(cond, sThen, sElse)

    override this.TransformYield(expr) =
        match expr with
        | Some e ->
            let yieldId = Id.New "$yield"
            Block [
                Continuation (yieldId, e)
                Labeled (yieldId, Empty)
            ]     
        | _ -> Yield None

    override this.TransformTryWith(body, e, catch) =
        if HasGotos.Check body || HasGotos.Check catch then
            let tryStart = Id.New "$try"
            Labeled(tryStart, TryWith(this.TransformStatement body, e, this.TransformStatement catch))    
        else TryWith(body, e, catch)

    override this.TransformTryFinally(body, final) =
        if HasGotos.Check body || HasGotos.Check final then
            let tryStart = Id.New "$try"
//            let finallyStart = Id.New "$finally"
//            let caught = Id.New "$exc"
//            let e = Id.New "e"
//            Block [
//                VarDeclaration (caught, Undefined)
//                TryWith(this.TransformStatement body, Some e, VarSetStatement(caught, e))
//            ]
            Labeled(tryStart, 
                TryFinally(this.TransformStatement body, this.TransformStatement final)
//                TryFinally(this.TransformStatement body, Labeled(finallyStart, this.TransformStatement final))
            )
        else TryFinally(body, final)

//        if Option.isSome (HasGotos.Check trBody) || Option.isSome (HasGotos.Check trCatch) then
//            
//            P
//        else TryWith(trBody, e, trCatch)

type ExtractVarDeclarations() =
    inherit Transformer() 
    
    let vars = ResizeArray()

    member this.Vars = vars :> _ seq

    override this.TransformVarDeclaration(i, v) =
        vars.Add i
        ExprStatement (VarSet(i, this.TransformExpression v))

    override this.TransformLet(i, v, b) =
        vars.Add i
        Sequential [ VarSet(i, this.TransformExpression v); this.TransformExpression b ]

    override this.TransformFunction(a, arr, ret, b) =
        Function(a, arr, ret, b)
     
type State =
    | SingleState of ResizeArray<Statement>
    | Catch of option<Id> * Statement
    | Finally of Statement
    | MultState of ResizeArray<State>        

[<AbstractClass>]
type ContinuationTransformer(labels) =
    inherit StatementTransformer() 

    let labelLookup = dict (labels |> Seq.mapi (fun i l -> l, i + 1)) 
    let stateVar = Id.New "$state"
    let topLabel = Id.New "$top"
    let localFunctions = ResizeArray()
    let mutable tryScope = 0

//    let mutable currentFinallyIndex = None
//    let mutable hasFinally = false
//    let pendingStateVar = Id.New "$stateAfterFinally"

    let gotoIndex i =
        let setState =
//            match currentFinallyIndex with
//            | Some fi ->
//                hasFinally <- true
//                ExprStatement <| Sequential [VarSet(pendingStateVar, Value (Int i)); VarSet(stateVar, Value (Int fi))]
//            | None -> 
            ExprStatement <| VarSet(stateVar, Value (Int i))
        Block [
            setState
            Continue (Some topLabel)
        ]

    abstract Yield : Expression -> Statement

    member this.StateVar = stateVar
    
    member this.LocalFunctions = localFunctions :> _ seq

    member this.TryScope = tryScope

    override this.TransformGoto(a) =
        gotoIndex labelLookup.[a]

    override this.TransformContinuation(a, b) =
        Block [
            ExprStatement <| VarSet(stateVar, Value (Int labelLookup.[a]))
            this.Yield b
        ]

    override this.TransformFuncDeclaration(f, args, thisVar, body, gen) =
        localFunctions.Add(FuncDeclaration(f, args, thisVar, body, gen))
        Empty
            
    member this.TransformMethodBodyInner(s: Statement) =
        let states = ResizeArray()
        let mutable nextIndex = 0
        let mutable lastState = ResizeArray()
        states.Add (SingleState lastState)
        let mutable currentScope = states
        let newState() =
            nextIndex <- nextIndex + 1
            lastState.Add (gotoIndex nextIndex)
            lastState <- ResizeArray() 
        let multScope inner =
            newState()
            let newScope = ResizeArray()
            newScope.Add(SingleState lastState)
            currentScope.Add (MultState newScope)
            let prevScope = currentScope
            currentScope <- newScope
            inner()
            currentScope <- prevScope
        let rec addStatements s =
            match IgnoreStatementSourcePos s with
            | Block ss ->
                for i in ss do
                    addStatements i 
            | Labeled(_, TryWith(body, e, catch)) ->
                multScope <| fun () ->
                    tryScope <- tryScope + 1
                    addStatements body
                    tryScope <- tryScope - 1
                    currentScope.Add (Catch (e, CombineStatements [ this.TransformStatement catch; gotoIndex (nextIndex + 1) ] ))
                newState()
                currentScope.Add (SingleState lastState)
            | Labeled(_, TryFinally(body, final)) ->
                multScope <| fun () ->
                    tryScope <- tryScope + 1
                    addStatements body
                    tryScope <- tryScope - 1
                    currentScope.Add (Finally (this.TransformStatement final))
            | Labeled (_, ls) ->
                newState()
                currentScope.Add (SingleState lastState)
                addStatements ls
            | _ -> 
                lastState.Add (this.TransformStatement s)
        addStatements s
        
        let startAndLast l =
            let rec sl acc r =
                match r with
                | [ x ] -> List.rev acc, x  
                | h :: t -> sl (h :: acc) t
                | _ -> failwith "list is empty"
            sl [] l
             
        let mutable i = -1
        let rec getSwitchCase st =
            match st with
            | SingleState s ->
                i <- i + 1
                [ Value (Int i) ], CombineStatements (List.ofSeq s)
            | MultState sl ->
                let last = sl.[sl.Count - 1]   
                let cases = [ for i in 0 .. sl.Count - 2 -> sl.[i] ]    
                match last with
                | Catch (e, catch) ->
                    let cases = cases |> List.map getSwitchCase  
                    (cases |> List.collect fst),
                    TryWith(
                        match cases with
                        | [_, b] -> b
                        | _ ->
                            Switch (Var stateVar, 
                                cases |> List.collect (fun (a, b) ->
                                    let s, l = startAndLast a
                                    (s |> List.map (fun v -> Some v, Empty)) @ [ Some l, b ]
                                )
                            )
                        , e, catch
                    )
                | Finally final ->
                    let cases = cases |> List.map getSwitchCase  
                    (cases |> List.collect fst),
                    TryFinally(
                        match cases with
                        | [_, b] -> b
                        | _ ->
                            Switch (Var stateVar, 
                                cases |> List.collect (fun (a, b) ->
                                    let s, l = startAndLast a
                                    (s |> List.map (fun v -> Some v, Empty)) @ [ Some l, b ]
                                )
                            )
                        , final
                    )
                | _ -> failwith "impossible"
            | Catch _ -> failwith "unexpected: Catch case at root in continuation transformation" 
            | Finally _ -> failwith "unexpected: Finally case at root in continuation transformation" 
                    
        Labeled (topLabel,
            While (Value (Bool true), 
                Switch (Var stateVar, 
                    states |> List.ofSeq |> List.map getSwitchCase |> List.collect (fun (a, b) ->
                                let s, l = startAndLast a
                                (s |> List.map (fun v -> Some v, Empty)) @ [ Some l, b ]
                            )
                )
            )
        )

let enumeratorTy =
    TypeDefinition {
        Assembly = "WebSharper.StdLib"
        FullName = "WebSharper.Enumerator+T`2"
    }

type GeneratorTransformer(labels) =
    inherit ContinuationTransformer(labels)

    let en = Id.New "$enum"

    override this.Yield(value) =
        Block [
            ExprStatement <| ItemSet(Var en, Value (String "c"), value)
            Return (Value (Bool true))
        ]

    override this.TransformYield(_) =
        Return (Value (Bool false))

    member this.TransformMethodBody(s: Statement) =
        let extract = ExtractVarDeclarations()
        let inner =
            this.TransformMethodBodyInner s |> extract.TransformStatement

        Return <| Object [ 
            "GetEnumerator", MemberKind.Simple,
                Function ([], None, None,
                    Block [
                        yield VarDeclaration(en, CopyCtor(enumeratorTy, Object ["d", MemberKind.Simple, Function ([], None, None, Empty)])) // TODO: disposing iterators
                        yield VarDeclaration(this.StateVar, Value (Int 0))
                        yield! this.LocalFunctions
                        for v in extract.Vars do
                            yield VarDeclaration(v, Undefined)
                        yield ExprStatement <| ItemSet(Var en, Value (String "n"), Function ([], None, None, inner))
                        yield Return (Var en)
                    ]
                )
        ]

type TaskReturn =
    | ReturnsVoid
    | ReturnsTask
    | ReturnsResultTask

let addLastReturnIfNeeded v s =
    let rec endsWithReturn s =
        match IgnoreStatementSourcePos s with
        | Return _ -> true
        | Block ss -> endsWithReturn (List.last ss) 
        | _ -> false
    if endsWithReturn s then s else
        CombineStatements [ s; Return v ]

type AsyncTransformer(labels, returns) =
    inherit ContinuationTransformer(labels)

    let task = Id.New "$task"
    let run = Id.New "$run"

    override this.Yield(v) =
        Block [
            ExprStatement <| Call(Some v, NonGeneric Definitions.Task, TaskMethod.OnCompleted, [ Var run ])
            Return Undefined        
        ]

    override this.TransformReturn(a) =
        Block [
            if IgnoreExprSourcePos a <> Undefined then
                yield ExprStatement <| ItemSet(Var task, Value (String "result"), a)
            yield ExprStatement <| ItemSet(Var task, Value (String "status"), Value (Int (int System.Threading.Tasks.TaskStatus.RanToCompletion)))
            yield ExprStatement <| Call(Some (Var task), NonGeneric Definitions.Task, TaskMethod.RunContinuations, [])
            yield Return Undefined   
        ]

    override this.TransformThrow(a) =
        if this.TryScope = 0 then
            Block [
                if IgnoreExprSourcePos a <> Undefined then
                    yield ExprStatement <| ItemSet(Var task, Value (String "exc"), a)
                yield ExprStatement <| ItemSet(Var task, Value (String "status"), Value (Int (int System.Threading.Tasks.TaskStatus.Faulted)))
                yield ExprStatement <| Call(Some (Var task), NonGeneric Definitions.Task, TaskMethod.RunContinuations, [])
                yield Return Undefined
            ]
        else
            Throw(a)

    member this.TransformMethodBody(s: Statement) =
        let extract = ExtractVarDeclarations()
        let inner =
            this.TransformMethodBodyInner s |> extract.TransformStatement

        Block [
            yield VarDeclaration(task, 
                CopyCtor ((if returns = ReturnsResultTask then Definitions.Task1 else Definitions.Task), 
                    Object [
                        "status", MemberKind.Simple, Value (Int (int System.Threading.Tasks.TaskStatus.Running))
                        "continuations", MemberKind.Simple, NewArray []
                    ]))
            yield VarDeclaration(run, Undefined)
            yield VarDeclaration(this.StateVar, Value (Int 0))
            for v in extract.Vars do
                yield VarDeclaration(v, Undefined)
            yield! this.LocalFunctions
            yield ExprStatement <| VarSet(run, Function ([], None, None, inner))
            yield ExprStatement <| Appl (Var run, [], NonPure, Some 0)
            if returns <> ReturnsVoid then 
                yield Return (Var task)
        ]

type GotoTransformer(labels) =
    inherit ContinuationTransformer(labels)

    override this.Yield(_) = failwith "GotoTransformer: unexpected yield"

    member this.TransformMethodBody(s: Statement) =
        let extract = ExtractVarDeclarations()
        let inner =
            this.TransformMethodBodyInner s |> extract.TransformStatement

        Block [
            yield VarDeclaration(this.StateVar, Value (Int 0))
            for v in extract.Vars do
                yield VarDeclaration(v, Undefined)
            yield inner
        ]

open WebSharper.Compiler

let eliminateGotos s =
    if HasGotos.Check s then 
        let g =
            s |> BreakStatement
            |> addLastReturnIfNeeded Undefined
            |> FreeNestedGotos().TransformStatement
        g |> GotoTransformer(CollectLabels.Collect g).TransformMethodBody
    else
        s

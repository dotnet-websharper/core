module internal WebSharper.Compiler.CSharp.Continuation
 
open WebSharper.Core.AST

type CollectLabels() =
    inherit StatementVisitor()

//    let mutable hasGoto = false
    let mutable labels = ResizeArray()

    member private this.Labels = List.ofSeq labels

//    member private this.Result = 
//        if hasGoto then Some labels else None

//    override this.VisitGoto _ = hasGoto <- true

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
        let exc = ItemGet(Var awaited, Value (String "exc"))
        Sequential [
            NewVar(awaited, this.TransformExpression a)
            StatementExpr <| Continuation(doneLabel, Var awaited)
            StatementExpr <| Labeled(doneLabel, Empty)
            StatementExpr <| If (exc, Throw exc, Empty)
            ItemGet(Var awaited, Value (String "result"))                 
        ]

type FreeNestedGotos(?loopStart, ?loopEnd) =
    inherit StatementTransformer()

//    let mutable currentLoopStart = None
//    let mutable currentLoopEnd = None

//    let loopLabels = Dictionary()

    member this.TransformFor(init, cond, incr, body, currentLabel) =
        let forStart = 
            match currentLabel with
            | Some l -> l
            | _ -> Id.New "$for"
        let forEnd = Id.New "$endfor"
        Statements [
            match init with Some i -> yield ExprStatement i | _ -> ()
            yield Labeled (forStart, Empty)
            match cond with Some c -> yield If (c, Empty, Goto forEnd) | _ -> () // If (c, inLoop, Empty) | _ -> inLoop         
            yield FreeNestedGotos(forStart, forEnd).TransformStatement body
            match incr with Some i -> yield ExprStatement i | _ -> ()
            yield Goto forStart 
            yield Labeled (forEnd, Empty)
        ]

    override this.TransformFor(init, cond, incr, body) =
        let trBody = this.TransformStatement body
//        if HasGotos.Check trBody |> Option.isSome then
        this.TransformFor(init, cond, incr, body, None)
//        else For(init, cond, incr, trBody)

    override this.TransformLabeled(label, body) =
        match ignoreStatementSourcePos body with
        | For (init, cond, incr, body) -> this.TransformFor(init, cond, incr, body, Some label)
        | _ -> base.TransformLabeled(label, body)  

    override this.TransformContinue _ =
        Goto loopStart.Value

    override this.TransformBreak _ =
        Goto loopEnd.Value

    // TODO : current label check for while, dowhile
    override this.TransformWhile(cond, body) =
        let whileStart = Id.New "$while"
        let whileEnd = Id.New "$endwhile"
        Statements [
            Labeled (whileStart, Empty)
            If (cond, Empty, Goto whileEnd)
            FreeNestedGotos(whileStart, whileEnd).TransformStatement body
            Goto whileStart
            Labeled(whileEnd, Empty)
        ]

    override this.TransformDoWhile(body, cond) =
        let whileStart = Id.New "$dowhile"
        let whileEnd = Id.New "$enddowile"
        Statements [
            Labeled (whileStart, Empty)
            FreeNestedGotos(whileStart, whileEnd).TransformStatement body
            If (cond, Goto whileStart, Empty)
        ] 

    override this.TransformIf(cond, sThen, sElse) =
        let elseStart = Id.New "$else"
        let endIf = Id.New "$endif"
        Statements [
            If (cond, Empty, Goto elseStart)
            this.TransformStatement sThen
            Goto endIf
            Labeled(elseStart, this.TransformStatement sElse)
            Labeled(endIf, Empty)
        ]   

    override this.TransformYield(expr) =
        match expr with
        | Some e ->
            let yieldId = Id.New "$yield"
            Statements [
                Continuation (yieldId, e)
                Labeled (yieldId, Empty)
            ]     
        | _ -> Yield None

//    override this.TransformTryWith(body, e, catch) =
//        let trBody = this.TransformStatement body
//        let trCatch = this.TransformStatement catch
//        if Option.isSome (HasGotos.Check trBody) || Option.isSome (HasGotos.Check trCatch) then
//            
//            P
//        else TryWith(trBody, e, trCatch)

//    override this.TransformYield(expr) =
//        let yieldId = Id.New "$yield"
//        Statements [
//            
//            Labeled (yieldId, Empty)
//        ]     

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

    override this.TransformNewVar(i, v) =
        vars.Add i        
        VarSet(i, this.TransformExpression v)

    override this.TransformFunction(a, b) =
        Function(a, b)

[<AbstractClass>]
type ContinuationTransformer(labels) =
    inherit StatementTransformer() 

    let labelLookup = dict (labels |> Seq.mapi (fun i l -> l, i + 1)) 
    let stateVar = Id.New "$state"
    let topLabel = Id.New "$top"
    
    let gotoIndex i =
        Statements [
            ExprStatement <| VarSet(stateVar, Value (Int i))
            Continue (Some topLabel)
        ]
    abstract Yield : Expression -> Statement

    member this.StateVar = stateVar

    override this.TransformGoto(a) =
        gotoIndex labelLookup.[a]

    override this.TransformContinuation(a, b) =
        Statements [
            ExprStatement <| VarSet(stateVar, Value (Int labelLookup.[a]))
            this.Yield b
        ]
            
    member this.TransformMethodBodyInner(s: Statement) =
        let cases = ResizeArray()
        let mutable lastCase = ResizeArray()
        cases.Add lastCase
        let rec addStatements s =
            match ignoreStatementSourcePos s with
            | Block ss
            | Statements ss ->
                for i in ss do
                    addStatements i 
            | Labeled (_, ls) ->
                lastCase.Add (gotoIndex (cases.Count))
                lastCase <- ResizeArray() 
                cases.Add lastCase
                addStatements ls
            | _ -> 
                lastCase.Add (this.TransformStatement s)
        addStatements s
                    
        Labeled (topLabel,
            While (Value (Bool true), 
                Switch (Var stateVar, 
                    cases |> Seq.mapi (fun i c ->
                        Some (Value (Int i)), Statements (List.ofSeq c)
                    ) |> List.ofSeq
                )
            )
        )

let enumeratorTy =
    TypeDefinition {
        Assembly = "WebSharper.Main"
        FullName = "WebSharper.Enumerator+T`2"
    }


type GeneratorTransformer(labels) =
    inherit ContinuationTransformer(labels)

    let en = Id.New "$enum"

    override this.Yield(value) =
        Statements [
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
            "GetEnumerator", 
                Function ([],
                    Block [
                        yield VarDeclaration(en, CopyCtor(enumeratorTy, Object ["d", Function ([], Empty)])) // TODO: disposing iterators
                        yield VarDeclaration(this.StateVar, Value (Int 0))
                        for v in extract.Vars do
                            yield VarDeclaration(v, Undefined)
                        yield ExprStatement <| ItemSet(Var en, Value (String "n"), Function ([], inner))
                        yield Return (Var en)
                    ]
                )
        ]

let taskTy =
//    Reflection.getTypeDefinition(typeof<System.Threading.Tasks.Task<_>>)
    TypeDefinition {
        Assembly = "mscorlib"
        FullName = "System.Threading.Tasks.Task`1"
    }

type AsyncTransformer(labels) =
    inherit ContinuationTransformer(labels)

    let task = Id.New "$task"
    let run = Id.New "$run"

    override this.Yield(v) =
        Statements [
            ExprStatement <| Application(ItemGet(v, Value (String "OnCompleted")), [ Var run ])
            Return (Value (Bool true))         
        ]

    override this.TransformReturn(a) =
        match ignoreExprSourcePos a with
        | Undefined -> Return (Value (Bool false))
        | _ ->
            Statements [
                ExprStatement <| ItemSet(Var task, Value (String "result"), a)
                ExprStatement <| ItemSet(Var task, Value (String "status"), Value (Int (int System.Threading.Tasks.TaskStatus.RanToCompletion)))
                ExprStatement <| Application(ItemGet(Var task, Value (String "RunContinuations")), [])
                Return (Value (Bool false))         
            ]

    member this.TransformMethodBody(s: Statement) =
        let extract = ExtractVarDeclarations()
        let inner =
            this.TransformMethodBodyInner s |> extract.TransformStatement

        Block [
            yield VarDeclaration(task, 
                CopyCtor (taskTy, 
                    Object [
                        "status", Value (Int (int System.Threading.Tasks.TaskStatus.Running))
                        "continuations", NewArray []
                    ]))
            yield VarDeclaration(run, Undefined)
            yield VarDeclaration(this.StateVar, Value (Int 0))
            for v in extract.Vars do
                yield VarDeclaration(v, Undefined)
            yield ExprStatement <| VarSet(run, Function ([], inner))
            yield ExprStatement <| Application (Var run, [])
            yield Return (Var task)
        ]

//        match v with
//        | Var awaited ->
//        | _ -> failwith "AsyncTransformer expecting Var"

type ToDotNetAST() =
    inherit Transformer()
    
    let mutable iteratorVars = None : option<Id * Id * Id> 
    let mutable labels = Map.empty : Map<Id, (Id * Id) * int> 
    let tryBlocks = ResizeArray()
    let states = ResizeArray()
    
    override this.TransformGoto id =
        let (gotoVar, gotoLabel), i = labels.[id]
        Block [
            ExprStatement(VarSet(gotoVar, Value (Int i)))
            Break (Some gotoLabel)
        ]

    override this.TransformCSharpSwitch (switchExpr, cases) =
        let rec hasGotoCase statement = 
            false
    //        statement |> childrenCSharpStatement |> List.exists (fun s ->
    //            match s with
    //            | OtherStatement (GotoCase _) -> true
    //            | OtherStatement (CSharpSwitch _) -> false
    //            | _ -> hasGotoCase s
    //        )   
        if cases |> List.exists (snd >> hasGotoCase) then
            Block [] // TODO : has GotoCase
        else
            Switch (switchExpr |> this.TransformExpression,
                cases |> List.collect (fun (labels, statement) ->
                    let rec sepLabels acc ls =
                        match ls with
                        | [l] -> 
                            (l |> Option.map this.TransformExpression, statement |> this.TransformStatement) :: acc
                            |> List.rev
                        | l :: rest -> sepLabels ((l |> Option.map this.TransformExpression, Empty) :: acc) rest
                        | _ -> failwith "transformCSharpSwitch: no labels - impossible" 
                    sepLabels [] labels
                )
            )

    override this.TransformStatements st =
        match st with
        | [] -> Empty
        | [s] -> s |> this.TransformStatement
        | _ -> Block (st |> List.map this.TransformStatement) 
       
    override this.TransformBlock st =    
        let blockLabels =
            st |> List.choose (
                function
                | IgnoreStatementSourcePos (Labeled (l, _)) -> Some l 
                | _ -> None
            )
        if List.isEmpty blockLabels then
            Block (st |> List.map this.TransformStatement)           
        else     
            let blockLabels =
                match st.Head with
                | IgnoreStatementSourcePos (Labeled _) -> blockLabels
                | _ -> Id.New "Start" :: blockLabels 
            let rec getSegments acc statements =
                match statements with
                | [] -> List.rev acc
                | s :: rest ->
                    match s with
                    | IgnoreStatementSourcePos (Labeled (_, s)) ->
                        getSegments ([s] :: acc) rest
                    | _ ->
                        getSegments ((s :: acc.Head) :: acc.Tail) rest
            let gotoVar = Id.New "goto"
            let gotoLabel = Id.New "GotoLoop"
            let innerLabels =
                let gotoVarAndLabel = gotoVar, gotoLabel

                blockLabels |> List.fold (fun (m, i) l ->
                    m |> Map.add l (gotoVarAndLabel, i), i + 1
                ) (labels, 1) |> fst
               
//            let envWithNewLabels =
//                let gotoVarAndLabel = gotoVar, gotoLabel
//                
//                { env with 
//                    Labels =
//                        labels |> List.fold (fun (m, i) l ->
//                            m |> Map.add l (gotoVarAndLabel, i), i + 1
//                        ) (env.Labels, 1) |> fst
//                }
            Block [
                VarDeclaration (gotoVar, Value (Int 1))
                Labeled (gotoLabel,
                    Switch(Var gotoVar,
                        getSegments [] st |> List.mapi (fun i s ->
                            Some (Value (Int (i + 1))),
                            s |> List.map this.TransformStatement |> Block  
                        )
                    )
                )
            ]
//
//type Environment =
//    {
//        IsAsync : bool
//        IteratorVars : option<Id * Id * Id> // current, next, dispose
//        GetMethodParamNames : Method -> list<string>
//        Labels : Map<Id, (Id * Id) * int>
//    }
//    static member New() =
//        {
//            IsAsync = false
//            IteratorVars = None
//            GetMethodParamNames = fun _ -> [] 
//            Labels = Map.empty
//        }
//
//let rec hasYield (statement: Statement) =
//    false
////    childrenCSharpStatement statement |> List.exists (fun s ->
////        match s with
////        | OtherStatement (Yield _) -> true
////        | _ -> hasYield s   
////    )  
//
//let rec transformExpr (env: Environment) (expr: Expr) : DotNetExpr =
//    expr |> recurDotNetExpr (transformExpr env) fail (transformStatement env)
//
//and transformStatement (env: Environment) (statement: Statement) : DotNetStatement =
//    let inline trS x = recurStatement (transformExpr env) (transformStatement env) fail x
//    match statement with
//    | Block a -> transformBlock env a
//    | OtherStatement statement ->
//        match statement with
//        | Goto id -> 
//            let (gotoVar, gotoLabel), i = env.Labels.[id]
//            Block [
//                ExprStatement(VarSet(gotoVar, Value (Int i)))
//                Break (Some gotoLabel)
//            ]
//        | CSharpSwitch (switchExpr, cases) ->
//            transformCSharpSwitch env switchExpr cases
//        | Statements statements ->
//            match statements with
//            | [] -> Empty
//            | [s] -> s |> trS
//            | _ -> Block (statements |> List.map trS)    
//    | _ -> trS statement
//
//and transformBlock (env: Environment) (statements: list<Statement>) =
//    let labels =
//        statements |> List.choose (
//            function
//            | IgnoreStatementSourcePos (Labeled (l, _)) -> Some l 
//            | _ -> None
//        )
//    if List.isEmpty labels then
//        Block (statements |> List.map (transformStatement env))           
//    else     
//        let labels =
//            match statements.Head with
//            | IgnoreStatementSourcePos (Labeled _) -> labels
//            | _ -> Id.New("Start") :: labels 
//        let rec getSegments acc statements =
//            match statements with
//            | [] -> List.rev acc
//            | s :: rest ->
//                match s with
//                | IgnoreStatementSourcePos (Labeled (_, s)) ->
//                    getSegments ([s] :: acc) rest
//                | _ ->
//                    getSegments ((s :: acc.Head) :: acc.Tail) rest
//        let gotoVar = Id.New "goto"
//        let gotoLabel = Id.New "GotoLoop"
//        let envWithNewLabels =
//            let gotoVarAndLabel = gotoVar, gotoLabel
//            { env with 
//                Labels =
//                    labels |> List.fold (fun (m, i) l ->
//                        m |> Map.add l (gotoVarAndLabel, i), i + 1
//                    ) (env.Labels, 1) |> fst
//            }
//        Block [
//            VarDeclaration (gotoVar, Value (Int 1))
//            Labeled (gotoLabel,
//                Switch(Var gotoVar,
//                    getSegments [] statements |> List.mapi (fun i s ->
//                        Some (Value (Int (i + 1))),
//                        s |> List.map (
//                            recurStatement (transformExpr env) 
//                                (transformStatement envWithNewLabels) fail
//                        ) |> Block  
//                    )
//                )
//            )
//        ]
//       
//and transformCSharpSwitch (env: Environment) (switchExpr: Expr) (cases: list<list<option<Expr>> * Statement>) : DotNetStatement =
//    let rec hasGotoCase statement = 
//        false
////        statement |> childrenCSharpStatement |> List.exists (fun s ->
////            match s with
////            | OtherStatement (GotoCase _) -> true
////            | OtherStatement (CSharpSwitch _) -> false
////            | _ -> hasGotoCase s
////        )   
//    if cases |> List.exists (snd >> hasGotoCase) then
//        Block [] // TODO : has GotoCase
//    else
//        Switch (switchExpr |> transformExpr env,
//            cases |> List.collect (fun (labels, statement) ->
//                let rec sepLabels acc ls =
//                    match ls with
//                    | [l] -> 
//                        (l |> Option.map (transformExpr env), statement |> transformStatement env) :: acc
//                        |> List.rev
//                    | l :: rest -> sepLabels ((l |> Option.map (transformExpr env), Empty) :: acc) rest
//                    | _ -> failwith "transformCSharpSwitch: no labels - impossible" 
//                sepLabels [] labels
//            )
//        )
//                    
////let rec exprToDotNet (expr: Expr) : DotNetExpr =
////    match expr with
////    | OtherExpr expr ->
////        match expr with
////        | OtherDotNetExpr expr ->
////            match expr with
////            | Await e -> CreateAsync (Return (exprToDotNet e))
////
////        | Call (this, meth, arguments) ->
////    | ExprSourcePos (pos, e) -> ExprSourcePos (pos, exprToDotNet e)  
////
////and statementToDotNet (statement: Statement) : DotNetStatement =
////    match statement with
////    | OtherStatement statement ->
////        | Labeled
////        
////            
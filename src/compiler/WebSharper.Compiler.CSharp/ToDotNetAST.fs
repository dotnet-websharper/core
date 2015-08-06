module internal RoslynTest.ToDotNetAST
 
open WebSharper.Core.AST

let fail _ = failwith "Transform error: C# to .NET common"

type ToDotNetAST() =
    inherit Transformer()
    
    let mutable labels = Map.empty : Map<Id, (Id * Id) * int> 
    
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
                | _ -> Id "Start" :: blockLabels 
            let rec getSegments acc statements =
                match statements with
                | [] -> List.rev acc
                | s :: rest ->
                    match s with
                    | IgnoreStatementSourcePos (Labeled (_, s)) ->
                        getSegments ([s] :: acc) rest
                    | _ ->
                        getSegments ((s :: acc.Head) :: acc.Tail) rest
            let gotoVar = Id "goto"
            let gotoLabel = Id "GotoLoop"
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
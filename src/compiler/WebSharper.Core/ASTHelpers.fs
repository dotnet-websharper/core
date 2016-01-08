[<AutoOpen>]
module WebSharper.Core.AST.ASTHelpers

let concrete (e, g) =
    {
        Entity = e
        Generics = g
    }

let concreteType (td, g) = ConcreteType (concrete (td, g))

let ignoreExprSourcePos expr =
    match expr with
    | ExprSourcePos (_, e) -> e
    | _ -> expr

let (|IgnoreExprSourcePos|) expr = ignoreExprSourcePos expr

let withSourcePosOfExpr sourceExpr expr =
    match sourceExpr with      
    | ExprSourcePos (pos, _) -> ExprSourcePos (pos, expr)
    | _ -> expr

let ignoreStatementSourcePos expr =
    match expr with
    | StatementSourcePos (_, e) -> e
    | _ -> expr

let (|IgnoreStatementSourcePos|) expr = ignoreStatementSourcePos expr

let withSourcePosOfStatement sourceStatement statement =
    match sourceStatement with      
    | ExprSourcePos (pos, _) -> ExprSourcePos (pos, statement)
    | _ -> statement
    
let tryGetExprSourcePos expr =
    match expr with
    | ExprSourcePos (p, _) -> Some p
    | _ -> None

let tryGetStatementSourcePos expr =
    match expr with
    | StatementSourcePos (p, _) -> Some p
    | _ -> None

let getConstantValue (value: obj) =
    match value with
    | x when obj.ReferenceEquals(x, null) -> Null      
    | :? bool   as x -> Bool   x
    | :? byte   as x -> Byte   x
    | :? char   as x -> Char   x
    | :? double as x -> Double x
    | :? int    as x -> Int    x
    | :? int16  as x -> Int16  x
    | :? int64  as x -> Int64  x
    | :? sbyte  as x -> SByte  x
    | :? single as x -> Single x
    | :? string as x -> String x
    | :? uint16 as x -> UInt16 x
    | :? uint32 as x -> UInt32 x
    | :? uint64 as x -> UInt64 x
    | _ -> failwith "F# constant value not recognized: %A" value
    |> Value
 
type ReplaceId(fromId, toId) =
    inherit Transformer()
    
    override this.TransformId i =
        if i = fromId then toId else i

let breakTODO s = failwith ("TODO: break for " + s)

// has no side effect
let rec isPureExpr expr =
    match expr with
    | Undefined
    | This
    | Base
    | Var _
    | Value _
    | Function _ 
    | FuncWithThis _
    | GlobalAccess _
    | Self
        -> true
    | Sequential a 
    | NewArray a 
        -> List.forall isPureExpr a 
    | Conditional (a, b, c) 
        -> isPureExpr a && isPureExpr b && isPureExpr c
    | ItemGet(a, b) // TODO: be more careful, in JS it can have side effects
    | Binary (a, _, b)
    | Let (_, a, b)
    | Coalesce(a, _, b) 
        -> isPureExpr a && isPureExpr b 
    | Unary (_, a) 
    | ExprSourcePos (_, a)
    | TypeCheck(a, _)
        -> isPureExpr a     
    | Object a 
        -> List.forall (snd >> isPureExpr) a 
    | LetRec (a, b) 
        -> List.forall (snd >> isPureExpr) a && isPureExpr b
    | _ -> false

// has no side effect and value does not depend on execution order
let rec isStronglyPureExpr expr =
    match expr with
    | Undefined
    | This
    | Base
//    | Var _
    | Value _
//    | Function _ 
//    | FuncWithThis _
    | GlobalAccess _
    | Self
        -> true
    | Sequential a 
        -> 
        match List.rev a with
        | [] -> true
        | h :: t -> isStronglyPureExpr h && List.forall isPureExpr t
    | NewArray a 
        -> List.forall isStronglyPureExpr a 
    | Conditional (a, b, c) 
        -> isStronglyPureExpr a && isStronglyPureExpr b && isStronglyPureExpr c
//    | ItemGet(a, b)
    | Binary (a, _, b)
    | Let (_, a, b)
    | Coalesce(a, _, b) 
        -> isStronglyPureExpr a && isStronglyPureExpr b 
    | Unary (op, a)
        ->
        match op with
        | UnaryOperator.``void`` -> isPureExpr a
        | _ -> isStronglyPureExpr a 
    | ExprSourcePos (_, a)
    | TypeCheck(a, _)
        -> isStronglyPureExpr a     
    | Object a 
        -> List.forall (snd >> isStronglyPureExpr) a 
    | LetRec (a, b) 
        -> List.forall (snd >> isStronglyPureExpr) a && isStronglyPureExpr b
    | _ -> false

type NotMutatedOrCaptured(v) =
    inherit Visitor()

    let mutable scope = 0
    let mutable ok = true

    override this.VisitVarSet (a, b) =
        if a = v then ok <- false
        else this.VisitExpression b

    override this.VisitMutatingUnary (_, a) =
        match ignoreExprSourcePos a with
        | Var av when av = v ->
            ok <- false
        | _ ->
            this.VisitExpression a

    override this.VisitMutatingBinary (a, _, b) =
        match ignoreExprSourcePos a with
        | Var av when av = v ->
            ok <- false
        | _ ->
            this.VisitExpression a
            this.VisitExpression b

    override this.VisitFunction(a, b) =
        scope <- scope + 1
        base.VisitFunction(a, b)
        scope <- scope - 1

    override this.VisitFuncWithThis(a, b, c) =
        scope <- scope + 1
        base.VisitFuncWithThis(a, b, c)
        scope <- scope - 1

    override this.VisitId a =
        if scope > 0 then ok <- false

    member this.Check(a) =
        this.VisitExpression(a)
        ok

let varEvalOrder (vars : Id list) expr =
    let watchedVars = System.Collections.Generic.HashSet vars
    let mutable vars = vars
    let mutable ok = true 

    let fail () =
        vars <- []
        ok <- false
    
    let rec eval e =
        if ok then
            match e with
            | Undefined
            | This
            | Base
            | Value _
            | Self
                -> ()
            | Sequential a
            | NewArray a ->
                Seq.iter eval a
            | Conditional (a, b, c) ->
                eval a
                let aVars = vars
                eval b
                if ok then
                    let bVars = vars
                    vars <- aVars
                    eval c
                    if ok && (bVars <> vars) then fail()
            | ItemGet(a, b) 
            | Binary (a, _, b)
            | Let (_, a, b)
                ->
                eval a
                eval b
            | Unary (_, a) 
            | ExprSourcePos (_, a)
            | TypeCheck(a, _)
                -> eval a
            | Object a 
                -> List.iter (snd >> eval) a 
            | Var v ->
                if watchedVars.Contains v then
                    match vars with
                    | [] -> fail()
                    | hv :: tv ->
                        if v = hv then
                            vars <- tv
                        else fail() 

            | _ -> fail()
               
    eval expr
    ok     

let mkSequential e =
    match List.rev e with
    | [] -> Undefined
    | [ s ] -> s
    | h :: t -> Sequential (List.rev (h :: (t |> List.filter (isPureExpr >> not)))) 

let combineStatements statements =
    match statements with
    | [] -> Empty
    | [s] -> s
    | _ -> Block statements

type CountVarOccurence(v) =
    inherit Visitor()

    let mutable occ = 0

    override this.VisitId(a) =
        if a = v then 
            occ <- occ + 1

    member this.Get(e) =
        this.VisitExpression(e) 
        occ

type SubstituteVar(v, e) =
    inherit Transformer()

    override this.TransformVar(a) = if a = v then e else Var a

type SubstituteVars(sub : System.Collections.Generic.IDictionary<Id, Expression>) =
    inherit Transformer()

    override this.TransformVar(a) = 
        match sub.TryGetValue(a) with
        | true, e -> e
        | _ -> Var a 

type Broken<'a> =
    {
        Body : 'a
        Statements : Statement list
        Variables : Id list
    }

let broken b =
    {
        Body = b
        Statements = []
        Variables = []
    }

let mapBroken f b =
    {
        Body = f b.Body
        Statements = b.Statements
        Variables = b.Variables
    }

let toStatements f b =
    seq {
        for v in b.Variables -> VarDeclaration(v, Undefined)
        yield! b.Statements 
        yield f b.Body
    }

let toStatementsL f b =
    seq {
        for v in b.Variables -> VarDeclaration(v, Undefined)
        yield! b.Statements 
        yield! f b.Body
    }

let hasNoStatements b = List.isEmpty b.Statements
let (|HasNoStatements|_|) b = if hasNoStatements b then Some b else None

let rec breakExpr expr : Broken<Expression> =
    let inline br x = breakExpr x

    let brL l : Broken<Expression list> =
        let bb = l |> List.map br
        if bb |> List.forall hasNoStatements then
            {
                Body = bb |> List.map (fun b -> b.Body)
                Statements = bb |> List.collect (fun b -> b.Statements)
                Variables = bb |> List.collect (fun b -> b.Variables)
            }
        else
            let rec bL br (accVar, accSt, accE) bl =
                match bl with
                | [] -> accVar, accSt, accE
                | HasNoStatements b :: bRest ->
                    if br then
                        if isStronglyPureExpr b.Body then
                            bL true (b.Variables @ accVar, accSt, b.Body :: accE) bRest
                        else
                            let v = Id.New "$x"
                            bL true (b.Variables @ accVar, VarDeclaration (v, b.Body) :: accSt, Var v :: accE) bRest
                    else
                        bL false (b.Variables @ accVar, accSt, b.Body :: accE) bRest
                | b :: bRest ->
                    if isStronglyPureExpr b.Body then
                        bL true (b.Variables @ accVar, b.Statements @ accSt, b.Body :: accE) bRest
                    else
                        let v = Id.New "$x"
                        bL true (v :: b.Variables @ accVar, b.Statements @ VarDeclaration (v, b.Body) :: accSt, Var v :: accE) bRest
                | _ -> failwith "impossible"
            let vars, st, e = bL false ([], [], []) (List.rev bb)
            {
                Body = e
                Statements = st
                Variables = vars
            }

    let comb2 f a b : Broken<Expression> =
        brL [a; b] |> mapBroken (fun [a; b] -> f(a, b))
    let comb3 f a b c : Broken<Expression> =
        brL [a; b; c] |> mapBroken (fun [a; b; c] -> f(a, b, c))
    
    match expr with
    | Undefined
    | This
    | Base
    | Var _
    | Value _ 
    | Function _ 
    | Self
    | GlobalAccess _
    | Hole _
        -> broken expr 
    // generated for disposing iterators
    | Application (ItemGet(Let (x, Var y, Var x2), i), b) when x = x2 ->
        broken (Application(ItemGet(Var y, i), b))
    | Application (ItemGet(a, b), c) ->
        brL (a :: b :: c)
        |> mapBroken (fun (aE :: bE :: cE) -> Application (ItemGet(aE, bE), cE))
    | Application (a, b) -> 
        brL (a :: b)
        |> mapBroken (fun (aE :: bE) -> Application (aE, bE))
    | VarSet (a, b) ->
        br b
        |> mapBroken (fun bE -> VarSet (a, bE))
    | Sequential a ->
        brL a |> mapBroken mkSequential
    | NewArray a ->
        brL a |> mapBroken NewArray
    | Conditional (a, b, c) ->
        let brA = br a 
        let brB = br b
        let brC = br c
        let vOpt, st, e =
            if hasNoStatements brB && hasNoStatements brC then   
                None, brA.Statements, Conditional (brA.Body, brB.Body, brC.Body)
            else
                let res = Id.New "$i"
                let setRes x =
                    if hasNoStatements x then ExprStatement(VarSet(res, x.Body))
                    else Block (x.Statements @ [ ExprStatement(VarSet(res, x.Body)) ]) 
                Some res, brA.Statements @ [If (brA.Body, setRes brB, setRes brC)], Var res        
        {
            Body = e
            Statements = st
            Variables = Option.toList vOpt @ brA.Variables @ brB.Variables @ brC.Variables
        }
    | ItemGet (a, b) ->
        comb2 ItemGet a b
    | ItemSet (a, b, c) ->
        comb3 ItemSet a b c
    | Binary (a, b, c) ->
        match b with
        | BinaryOperator.``&&`` ->
            Conditional (a, c, Value (Bool false)) |> br
        | BinaryOperator.``||`` ->
            Conditional (a, Value (Bool true), c) |> br
        | _ ->
            comb2 (fun (aE, cE) -> Binary(aE, b, cE)) a c
    | MutatingBinary (a, b, c) -> 
        comb2 (fun (aE, cE) -> MutatingBinary(aE, b, cE)) a c
    | Unary (a, b) ->
        br b
        |> mapBroken (fun bE -> Unary (a, bE))
    | MutatingUnary (a, b) ->
        br b
        |> mapBroken (fun bE -> MutatingUnary (a, bE))
    | ExprSourcePos (a, b) -> 
        br b 
        |> mapBroken (fun bB -> ExprSourcePos(a, bB))
    | StatementExpr st ->
        {
            Body = Undefined
            Statements = [ st ]
            Variables = [] // TODO : vars from statemen 
        }
    | Call(a, b, c, d) ->
        brL (Option.toList a @ d)
        |> mapBroken (fun l ->
            if Option.isSome a then
                Call (Some l.Head, b, c, l.Tail)
            else Call (None, b, c, l)
        )
    | Ctor(a, b, c) ->
        brL c
        |> mapBroken (fun l -> Ctor(a, b, l))
    | CopyCtor (a, b) ->
        br b |> mapBroken (fun bE -> CopyCtor (a, bE))
    | FieldGet(_, _, _) -> breakTODO "FieldGet"
    | FieldSet(_, _, _, _) -> breakTODO "FieldSet"
    | Let(a, IgnoreExprSourcePos(Var b), c) when NotMutatedOrCaptured(a).Check(c) && NotMutatedOrCaptured(a).Check(c) -> // TODO: maybe weaker check is enough
        ReplaceId(a, b).TransformExpression(c) |> br    
    | Let(a, b, c) ->
        let brB = br b
        if hasNoStatements brB then
            let inlined =
                if isStronglyPureExpr brB.Body && NotMutatedOrCaptured(a).Check(c) then
                    match CountVarOccurence(a).Get(c) with
                    | 0 -> Some c
                    | 1 -> Some (SubstituteVar(a, brB.Body).TransformExpression(c))
                    | _ -> None
                else None
            match inlined with
            | Some i -> br i
            | _ ->
                let brC = br c 
                {
                    Body = brC.Body
                    Statements = VarDeclaration(a, brB.Body) :: brC.Statements 
                    Variables = brB.Variables @ brC.Variables
                }
        else
            let brC = br c 
            {
                Body = brC.Body
                Statements = brB.Statements @ VarDeclaration(a, brB.Body) :: brC.Statements 
                Variables = brB.Variables @ brC.Variables
            }
    | NewVar(a, b) ->
        let brB = br b
        { brB with
            Body = VarSet (a, brB.Body)
            Variables = a :: brB.Variables    
        }
    | WithVars (a, b) ->
        let brB = br b
        { brB with
            Variables = a @ brB.Variables    
        }
    | Object a ->
        let names, values = List.unzip a
        brL values
        |> mapBroken (fun l -> Object (List.zip names l)) 
    | Coalesce(_, _, _) -> breakTODO "Coalesce"
    | TypeCheck(a, b) ->
        br a
        |> mapBroken (fun aE -> TypeCheck (aE, b))
    | LetRec (a, b) -> // better support for mutually recursive functions and values
        let brAs = a |> List.map (fun (i, v) -> i, br v)
        let brB = br b
        {
            Body = brB.Body
            Statements =
                [
                    for i, _ in a do 
                        yield VarDeclaration(i, Undefined)
                    for i, v in brAs do 
                        yield! v.Statements
                        yield ExprStatement <| VarSet(i, v.Body)
                    yield! brB.Statements
                ]
            Variables = brB.Variables 
        }
//        Some (
//            [
//                for i, _ in a do 
//                    yield VarDeclaration(i, Undefined)
//                for i, v in a do 
//                    match br v with
//                    | Some (vSt, vE) -> 
//                        yield! vSt
//                        yield ExprStatement <| VarSet(i, vE)
//                    | _ -> yield ExprStatement <| VarSet(i, v)
//                match brB with
//                | Some (bSt, _) -> yield! bSt
//                | _ -> ()
//            ], 
//            match brB with
//            | Some (_, bE) -> bE
//            | _ -> b
//        )
//
//        breakTODO "LetRec"
//        None // TODO
    | Await(_) -> breakTODO "Await"
    | New(a, b) -> 
        brL (a :: b)
        |> mapBroken (fun (aE :: bE) -> New (aE, bE))
    | NamedParameter(_, _) -> breakTODO "NamedParameter"
    | RefOrOutParameter(_) -> breakTODO "RefOrOutParameter"
    | BaseCtor _ -> breakTODO "BaseCtor"
    | FuncWithThis(_, _, _) -> breakTODO "FuncWithThis"
    | CallNeedingMoreArgs(_, _, _, _) -> breakTODO "CallNeedingMoreArgs"
    | Cctor(_) -> breakTODO "Cctor"
    | OverrideName(_, _) -> breakTODO "OverrideName"
    | NewRecord(_, _) -> breakTODO "NewRecord"
    | NewUnionCase(_, _, _) -> breakTODO "NewUnionCase"
    | UnionCaseGet(_, _, _, _) -> breakTODO "UnionCaseGet"

and private breakSt statement : Statement seq =
    let inline brE x = breakExpr x
    let inline brS x = breakSt x
    let inline combine x = x |> List.ofSeq |> combineStatements
//    let inline combineOpt x xB = match xB with Some xB -> combine xB | _ -> x
//    let comb2 f a b =
//        match brS a, brS b with
//        | Some aB, Some bB -> f (combine aB) (combine bB) |> Seq.singleton |> Some
//        | Some aB, None -> f (combine aB) b |> Seq.singleton |> Some
//        | None, Some bB -> f a (combine bB) |> Seq.singleton |> Some
//        | _ -> None
    match statement with
    | Empty
    | Break _ 
    | Continue _ 
    | Yield _ 
//    | Yield(a) -> 
//        brE a |> Option.map (fun (st, aB) -> Seq.append st (Seq.singleton (Yield aB))) // TODO yield breakup
    | Goto _ //failwith "Not implemented yet"
    | CSharpSwitch _ // failwith "Not implemented yet"
    | GotoCase _ //failwith "Not implemented yet"
        -> Seq.singleton statement
    | ExprStatement a ->
        brE a |> toStatements ExprStatement
    | Return a ->
        brE a |> toStatements Return
    | Block a ->
        Seq.collect brS a |> List.ofSeq |> Block |> Seq.singleton
    | Labeled (a, b) ->
        Seq.singleton (Labeled (a, combine (brS b))) 
    | VarDeclaration (a, b) ->
        brE b |> toStatements (fun bE -> VarDeclaration (a, bE))
    | While (a, b) ->
        let brA = brE a
        if hasNoStatements brA then
            brA |> toStatements (fun aE -> While (aE, combine(brS b)))
        else
            let ok = Id.New "$w"
            brA |> toStatementsL (fun aE -> 
                [
                    VarDeclaration (ok, aE)
                    While (Var ok, 
                        Block [
                            yield combine (brS b) 
                            for s in brA.Statements do
                                match s with
                                | VarDeclaration (v, x) -> yield ExprStatement(VarSet(v, x))
                                | _ -> yield s 
                            yield ExprStatement(VarSet(ok, aE))
                        ]
                    ) 
                ]
            )
    | StatementSourcePos (a, b) ->
        Seq.singleton (StatementSourcePos (a, combine (brS b)))
    | DoWhile(a, b) -> 
        failwith "TODO: break for DoWhile"
        // this is wrong because of VarDeclarations repeated
//        match brE b with
//        | Some (st, bB) ->
//            DoWhile (
//                match brS a with
//                | None -> combine (Seq.append [a] st)
//                | Some aB -> combine (Seq.append aB st)
//                , bB    
//            ) |> Seq.singleton |> Some
//        | None ->
//            brS a |> Option.map (fun aB -> DoWhile (combine aB, b) |> Seq.singleton)
    | For(a, b, c, d) ->       
        let withoutInit =
            While (
                match b with Some b -> b | _ -> Value (Bool true) 
                ,
                match c with Some c -> Statements [d; ExprStatement c] | _ -> d
            )
        match a with
        | Some a -> Statements [ExprStatement a; withoutInit]
        | _ -> withoutInit
        |> brS
//        let aB = Option.map brE a
//        let bB = Option.map brE b
//        let cB = Option.map brE c
//        let dB = brS d
//        match aB, bB, cB, dB with
//        | None, None, None, None -> None
//        | _ ->
//            None
        // TODO
        //failwith "Not implemented yet"
    | Switch(a, b) -> 
        let brA = brE a
        let brCases = b |> List.map (fun (c, d) -> Option.map brE c, brS d)
        if brCases |> List.forall (fst >> Option.forall hasNoStatements) then
            let cases = List.map (fun (cB, cS) -> cB |> Option.map (fun x -> x.Body), combine cS) brCases
            { brA with
                Variables =
                    [
                        yield! brA.Variables
                        for (cB, _) in brCases do
                            match cB with
                            | Some cB -> yield! cB.Variables
                            | _ -> ()
                    ]
            } |> toStatements (fun aE -> Switch(aE, cases))
        else
            breakTODO "Switch with breaking case"

//        if Option.isNone brA && brCases |> List.forall (fun (c, d) -> Option.isNone c && Option.isNone d) then 
//            None
//        else
//            if brCases |> List.exists (fst >> Option.isSome) then
//                 breakTODO "Switch with breaking case"
//            else
//                let cases = List.map2 (fun (c, d) (_, dB) -> c, combineOpt d dB) b brCases
//                match brA with
//                | Some (st, aE) ->
//                    Seq.append st (Seq.singleton (Switch(aE, cases)))
//                | _ ->
//                    Seq.singleton (Switch(a, cases))
//                |> Some 
    | If(a, b, c) ->
        brE a |> toStatements (fun aE -> If (aE, combine (brS b), combine (brS c)))
    | Throw(a) -> 
        brE a |> toStatements Throw
    | TryWith (a, b, c) ->
        Seq.singleton (TryWith (combine (brS a), b, combine (brS c)))
    | TryFinally (a, b) ->           
        Seq.singleton (TryFinally (combine (brS a), combine (brS b)))
    | Statements a ->
        List.map brS a |> Seq.concat
    | ForIn(a, b, c) -> 
        brE b |> toStatements (fun bE -> ForIn (a, bE, combine (brS c)))
    | Continuation(a, b) ->
        brE b |> toStatements (fun bE -> Continuation(a, bE))
        
    //breakTODO "Continuation"

let breakStatement statement =
    match breakSt statement |> List.ofSeq with
    | [ s ] -> s
    | st -> Block st
      
open WebSharper.Core

type Substitution(args, ?thisObj) =
    inherit Transformer()
    
//    let mutable replaceThis = true
    let args = 
        Array.ofList (Option.toList thisObj @ if List.isEmpty args then [ Undefined ] else args)
    let refresh = System.Collections.Generic.Dictionary()

    override this.TransformHole i = args.[i]

//    override this.TransformThis () = 
//        if replaceThis then thisObj.Value else This

    override this.TransformFunction(args, body) =
//        replaceThis <- false
        let res = base.TransformFunction(args, body)
//        replaceThis <- true
        res

    override this.TransformId i =
        match refresh.TryFind i with
        | Some n -> n
        | _ ->
            let n = Id.New (?name = i.Name)
            refresh.Add(i, n)
            n
   
type FixThisScope() =
    inherit Transformer()
    let mutable scope = 0
    let mutable thisVar = None
    let mutable thisArgs = System.Collections.Generic.Dictionary<Id, int * bool ref>()

    override this.TransformFunction(args, body) =
        scope <- scope + 1
        let res = base.TransformFunction(args, body)
        scope <- scope - 1
        res
     
    override this.TransformFuncWithThis (thisArg, args, body) =
        scope <- scope + 1
        let used = ref false
        thisArgs.Add(thisArg, (scope, used))
        let trBody = this.TransformStatement body
        scope <- scope - 1
        if !used then
            Function(args, Statements [ VarDeclaration(thisArg, This); trBody ])
        else
            Function(args, trBody)
    
    member this.Fix(expr) =
        let b = this.TransformExpression(expr)
        match thisVar with
        | Some t -> Let (t, This, b)
        | _ -> b

    member this.Fix(statement) =
        let b = this.TransformStatement(statement)
        match thisVar with
        | Some t -> combineStatements [ VarDeclaration(t, This); b ]
        | _ -> b
                
    override this.TransformThis () =
        if scope > 0 then
            match thisVar with
            | Some t -> Var t
            | None ->
                let t = Id.New "$this"
                thisVar <- Some t
                Var t
        else This

    override this.TransformVar v =
        match thisArgs.TryFind v with
        | Some (funcScope, used) ->
            if scope > funcScope then
                used := true
                Var v
            else This
        | _ -> Var v

type ReplaceThisWithVar(v) =
    inherit Transformer()

    override this.TransformThis () = Var v
    override this.TransformBase () = failwith "base calls not allowed inside inlined members"

let makeExprInline (vars: Id list) expr =
    if varEvalOrder vars expr then
        SubstituteVars(vars |> Seq.mapi (fun i a -> a, Hole i) |> dict).TransformExpression(expr)
    else
        List.foldBack (fun (v, h) body ->
            Let (v, h, body)    
        ) (vars |> List.mapi (fun i a -> a, Hole i)) expr

type StatementTransformer() =
    inherit Transformer()

    override this.TransformExpression(a) = a

type StatementVisitor() =
    inherit Visitor()

    override this.VisitExpression(_) = ()

open WebSharper.Core

let globalAccess a = GlobalAccess (Address (List.rev a))
  
let consAddress n (a: Address) = Address(n :: a.Value)

let private runtime = ["Runtime"; "IntelliFactory"]
let private runtimeFunc f = GlobalAccess (Address (f :: runtime))
let runtimeClass =  runtimeFunc "Class"
let runtimeCtor =   runtimeFunc "Ctor"
let runtimeCctor =  runtimeFunc "Cctor"
let runtimeGetOptional = runtimeFunc "GetOptional"
let runtimeSetOptional = runtimeFunc "SetOptional"
let runtimeDeleteEmptyFields = runtimeFunc "DeleteEmptyFields"
let runtimeCreateFuncWithArgs = runtimeFunc "CreateFuncWithArgs"
let runtimeCreateFuncWithArgsRest = runtimeFunc "CreateFuncWithArgsRest"
let runtimeCreateFuncWithThis = runtimeFunc "CreateFuncWithThis"
let runtimeInvokeDelegate = runtimeFunc "InvokeDelegate"

//let runtimeDefine = runtimeFunc "Define"

let sysObjDef =
    TypeDefinition {
        Assembly = "mscorlib"
        FullName = "System.Object"    
    }

let iResourceDef =
    TypeDefinition {
        Assembly = "WebSharper.Core"
        FullName = "WebSharper.Core.Resources+IResource"    
    }
    
let ignoreSystemObject td =
    if td = sysObjDef then None else Some td

module Resolve =
    open System.Collections.Generic

    let newName (name: string) =
        match name.LastIndexOf '$' with
        | -1 -> name + "$1"
        | i -> 
            match System.Int32.TryParse name.[i + 1 ..] with
            | true, n -> name.[.. i - 1] + "$" + string (n + 1)
            | _ -> name + "$1"

    type private ResolveNode =
        | Module
        | Class
        | Member
    
    type Resolver() =
        let statics = Dictionary<Address, ResolveNode>()
        let prototypes = Dictionary<TypeDefinition, HashSet<string>>()

        let rec getSubAddress (root: list<string>) (name: string) node =
            let tryAddr = Address (name :: root)
            match statics.TryFind tryAddr, node with
            | Some _, Member
            | Some Member, _ 
            | Some Class, Class -> getSubAddress root (newName name) node
            | Some (Class | Module), Module -> tryAddr
            | _ -> 
                statics.[tryAddr] <- node
                tryAddr

        let getExactSubAddress (root: list<string>) (name: string) node =
            let tryAddr = Address (name :: root)
            match statics.TryFind tryAddr, node with
            | Some (Class | Module), Module -> true
            | Some Module, Class
            | None, _ -> 
                statics.[tryAddr] <- node
                true
            | _ -> false

        let rec getFullAddress (address: list<string>) node =
            match address with
            | [] -> failwith "Empty address"
            | [ x ] -> getSubAddress [] x node
            | h :: r -> getSubAddress ((getFullAddress r Module).Value) h node

        let rec getExactFullAddress (address: list<string>) node =
            match address with
            | [] -> failwith "Empty address"
            | [ x ] -> getExactSubAddress [] x node
            | h :: r -> 
                getExactFullAddress r Module && getExactSubAddress r h node

        member this.LookupPrototype typ =
            match prototypes.TryFind typ with
            | Some p -> p
            | _ ->
                let p = HashSet()
                prototypes.Add(typ, p)
                p

        member this.ExactClassAddress(addr: list<string>, hasPrototype) =
            getExactFullAddress addr (if hasPrototype then Class else Module)
            && if hasPrototype then getExactSubAddress addr "prototype" Member else true 
        
        member this.ClassAddress(addr: list<string>, hasPrototype) =
            let res = getFullAddress addr (if hasPrototype then Class else Module)
            if hasPrototype then
                getExactSubAddress addr "prototype" Member |> ignore    
            res
                    
        member this.ExactStaticAddress addr =
            getExactFullAddress addr Member 

        member this.StaticAddress addr =
            getFullAddress addr Member 
                  


//    type ResolvedNames = 
//        {
//            Statics : Dictionary<Address, bool> 
//            Prototypes : Dictionary<TypeDefinition, HashSet<string>>
//        }
//
//        static member Empty = 
//            {
//                Statics = Dictionary()
//                Prototypes = Dictionary()
//            }
//
//        member this.LookupPrototype typ =
//            match this.Prototypes.TryFind typ with
//            | Some p -> p
//            | _ ->
//                let p = HashSet()
//                this.Prototypes.Add(typ, p)
//                p

//    let rec getSubAddress (root: list<string>) (name: string) isLeaf (d: Dictionary<Address, bool>) =
//        let tryAddr = Address (name :: root)
//        match d.TryGetValue tryAddr with
//        | true, true -> getSubAddress root (newName name) isLeaf d
//        | true, false when not isLeaf -> tryAddr
//        | _ -> 
//            d.Add(tryAddr, isLeaf)
//            tryAddr
//
//    let getExactSubAddress (root: list<string>) (name: string) isLeaf (d: Dictionary<Address, bool>) =
//        let tryAddr = Address (name :: root)
//        match d.TryGetValue tryAddr with
//        | true, true -> failwith "Name resolving error"
//        | true, false when not isLeaf -> tryAddr
//        | _ -> 
//            d.Add(tryAddr, isLeaf)
//            tryAddr
//
//    let rec getFullAddress (address: list<string>) isLeaf (d: Dictionary<Address, bool>) =
//        match address with
//        | [] -> failwith "empty address"
//        | [ x ] -> getSubAddress [] x isLeaf d
//        | h :: r -> getSubAddress ((getFullAddress r false d).Value) h isLeaf d
//    
//    let getPrototype (address: list<string>) (d: Dictionary<Address, bool>) =
//        getExactSubAddress address "prototype" false d
//
//    let getCctor (address: list<string>) (d: Dictionary<Address, bool>) =
//        getExactSubAddress address "$cctor" true d

//    let rec getExactFullAddress (address: list<string>) isLeaf (d: Dictionary<Address, bool>) =
//        match address with
//        | [] -> failwith "empty address"
//        | [ x ] -> getExactSubAddress [] x isLeaf d
//        | h :: r -> getExactSubAddress ((getExactFullAddress r false d).Value) h isLeaf d
   
    let rec getRenamed name (s: HashSet<string>) =
        if s.Add name then name else getRenamed (newName name) s
   
     
//    type Package =
//        | Module of Dictionary<string, option<Package>>
//
//    let rec getModule name (d: IDictionary<string, _>) =
//        match d.TryGetValue name with
//        | true, Some (Module m) -> name, m
//        | true, _ -> getModule (newName name) d  
//        | _ -> 
//            let m = Dictionary()
//            d.Add(name, Some (Module m))
//            name, m
//
//    let classAddress root address =
//        match List.rev address with
//        | c :: rest ->
//            let restAddr, restD = 
//                ([], root) |> List.foldBack (fun name (a, d) -> 
//                    let n, o = getModule name d
//                    n :: a, o
//                ) rest
//            let n, o = getModule c restD 
//            Address (n :: restAddr), o
//        | _ ->
//            failwith "empty address"
//
//    let rec getRenamed name (s: ISet<string>) =
//        if s.Add name then name else getRenamed (newName name) s
//
//    let rec addChild name child (d: IDictionary<string, _>) =
//        match d.TryGetValue name with
//        | true, _ -> addChild (newName name) child d
//        | _ -> 
//            d.Add(name, child)
//            name 
//
//    let addStatic root address =
//        match List.rev address with
//        | c :: rest ->
//            let restAddr, restD = 
//                ([], root) |> List.foldBack (fun name (a, d) -> 
//                    let n, o = getModule nam- d
//                    n :: a, o
//                ) rest
//            let n = addChild c None restD
//            Address (n :: restAddr)
//        | _ ->
//            failwith "empty address"
//
//    let addPrototype (d: IDictionary<_, _>) =
//        let m = Dictionary() 
//        d.Add("prototype", Some (Module m))
//        m
    
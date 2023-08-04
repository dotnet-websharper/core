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

// Various helpers for compilation
[<AutoOpen>]
module WebSharper.Compiler.Breaker

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Compiler
open System.Collections.Generic

module I = IgnoreSourcePos

type BreakResult =
    | ResultVar of Id
    | ResultExpr of Expression

let isStronglyPureOrResultVar x =
    match x with 
    | ResultVar v -> true
    | ResultExpr e -> isStronglyPureExpr e

type Broken<'a> =
    {
        Body : 'a
        Statements : Statement list
        Variables : Id list
    }

let debugPrintBroken (a: Broken<BreakResult>) =
    sprintf "{ Body = %s; Statements = [%s]; Variables = [%s] }"
        (
            match a.Body with
            | ResultVar a -> sprintf "ResultVar %A" a  
            | ResultExpr e -> Debug.PrintExpression e
        )
        (a.Statements |> List.map Debug.PrintStatement |> String.concat "; ")
        (
            a.Variables |> List.map (sprintf "%A") |> String.concat "; "
        )

let debugPrintBrokenList (a: Broken<list<BreakResult>>) =
    sprintf "{ Body = %s; Statements = [%s]; Variables = [%s] }"
        (
            "[ " + (
                a.Body |> List.map (function
                    | ResultVar a -> sprintf "ResultVar %A" a  
                    | ResultExpr e -> Debug.PrintExpression e
                ) |> String.concat "; "
            ) + " ]"
        )
        (a.Statements |> List.map Debug.PrintStatement |> String.concat "; ")
        (
            a.Variables |> List.map (sprintf "%A") |> String.concat "; "
        )

let broken b =
    {
        Body = ResultExpr b
        Statements = []
        Variables = []
    }

let mapBroken f b =
    {
        Body = ResultExpr (f b.Body)
        Statements = b.Statements
        Variables = b.Variables
    }

let bindBroken f b =
    {
        Body = f b.Body
        Statements = b.Statements
        Variables = b.Variables
    }

let private getExpr b = match b with ResultVar v -> Var v | ResultExpr e -> e

let toBrExpr b =
    {
        Body = getExpr b.Body
        Statements = b.Statements
        Variables = b.Variables
    }

let toBrExprList b =
    {
        Body = List.map getExpr b.Body
        Statements = b.Statements
        Variables = b.Variables
    }

let mapBroken2L f b =
    b |> mapBroken (function a :: b -> f a b | _ -> failwith "mapBroken2L: list too short")

let mapBroken3L f b =
    b |> mapBroken (function a :: b :: c -> f a b c | _ -> failwith "mapBroken3L: list too short")

type TransformVarSets(v, tr) =
    inherit Transformer()

    override this.TransformVarSet(a, b) =
        if a = v then
             tr (this.TransformExpression b)    
        else base.TransformVarSet(a, b)    

    override this.TransformVarDeclaration(a, b) =
        if a = v then
             ExprStatement(tr (this.TransformExpression b))
        else base.TransformVarDeclaration(a, b)    

type TransformMoreVarSets(vs, tr) =
    inherit Transformer()

    override this.TransformVarSet(a, b) =
        if vs |> List.contains a then
             tr (this.TransformExpression b)    
        else base.TransformVarSet(a, b)    

    override this.TransformVarDeclaration(a, b) =
        if vs |> List.contains a then
             ExprStatement(tr (this.TransformExpression b))
        else base.TransformVarDeclaration(a, b)    

type MarkApplicationsPure(v, purity) =
    inherit Transformer()

    override this.TransformApplication(func, args, info) =
        match func with
        | I.Var x when x = v ->
            Application(func, args |> List.map this.TransformExpression, info)    
        | _ -> base.TransformApplication(func, args, info)

    override this.TransformLet(id, value, body) =
        match value with
        | I.Var x when x = v ->
            ReplaceId(id, v).TransformExpression(body) |> this.TransformExpression        
        | _ -> base.TransformLet(id, value, body)

type ContainsIds (ids : Id list) =
    inherit Visitor()

    let searched = HashSet(ids)
    let found = HashSet()

    member this.Searched = searched
    member this.All = Seq.append searched found

    override this.VisitId i =        
        if searched.Remove(i) then 
            found.Add i |> ignore

let toDecls (vars: Id list) (statements: Statement list) = 
    if List.isEmpty vars then
        Seq.ofList statements
    else
        let c = ContainsIds(vars)
        let trStatements =
            statements |> List.map (fun s ->
                match s with
                | I.ExprStatement (I.VarSet(a, b)) when c.Searched.Contains a ->
                    c.Searched.Remove a |> ignore
                    c.VisitExpression(b)
                    VarDeclaration(a, b)
                | _ ->
                    c.VisitStatement(s)
                    s
            )
        Seq.append 
            (c.All |> Seq.map (fun v -> VarDeclaration(v, Undefined)))
            trStatements
    
let toStatements f b =
    let b = toBrExpr b
    seq {
        yield! toDecls b.Variables b.Statements 
        yield f b.Body
    }

let toStatementsL f (b: Broken<BreakResult>)=
    let b = toBrExpr b
    seq {
        yield! toDecls b.Variables b.Statements 
        yield! f b.Body
    }

let toStatementExpr b =
    match b.Body with
    | ResultVar v ->
        toDecls b.Variables (b.Statements |> List.map (TransformVarSets(v, id).TransformStatement))
    | ResultExpr e ->
        seq {
            yield! toDecls b.Variables b.Statements 
            if not (isPureExpr e) then
                yield ExprStatement (removePureParts e)
        }

let hasNoStatements b = List.isEmpty b.Statements

let rec (|PropSet|_|) expr =
    match IgnoreExprSourcePos expr with
    | Unary (UnaryOperator.``void``, I.ItemSet (I.Var objVar, I.Value (String field), value))
    | ItemSet (I.Var objVar, I.Value (String field), value) 
        when CountVarOccurence(objVar).Get(value) = 0 ->
        Some (objVar, (field, value))
    | Let (var, value, PropSet ((objVar, (field, I.Var v)))) 
        when v = var && CountVarOccurence(objVar).Get(value) = 0 ->
        Some (objVar, (field, value))
    | _ -> None

let (|ObjWithPropSetters|_|) expr =
    let rec getSetters objVar acc p =
        match p with
        | [] -> List.rev acc, Undefined
        | [e] ->
            match IgnoreExprSourcePos e with
            | PropSet (v, (n, o)) when v = objVar -> 
                List.rev ((n, MemberKind.Simple, o) :: acc), Undefined
            | _ -> List.rev acc, e 
        | e :: r -> 
            match IgnoreExprSourcePos e with
            | e when isPureExpr e -> getSetters objVar acc r
            | PropSet (v, (n, o)) when v = objVar -> 
                getSetters objVar ((n, MemberKind.Simple, o) :: acc) r
            | _ -> 
                List.rev acc, CombineExpressions p
    match expr with 
    | Let (objVar, I.Object objFields, I.Sequential p) ->
        match getSetters objVar [] p with
        | [], _ -> None
        | setters, Var v when v = objVar -> Some (Object (objFields @ setters))
        | setters, Undefined -> Some (CombineExpressions (List.map (fun (_, _, v) -> v) (objFields @ setters)))
        | setters, res -> Some (Let (objVar, Object (objFields @ setters), res))
    | _ -> None

let (|Lets|) expr =
    let rec getLets acc expr =
        match expr with
        | I.Let (v, e, body) -> getLets ((v, e) :: acc) body
        | _ -> List.rev acc, expr
    getLets [] expr

let bind key value body = Let (key, value, body)

let globalId = Address.Runtime "id"

let rec removeLets expr =
    let func vars ret body isReturn =
        if isReturn then Lambda(vars, ret, body) else Function(vars, None, ret, ExprStatement body)
    match expr with
    | Application (I.Let (var, value, body), xs, info) ->
        Let (var, value, Application (body, xs, info)) |> removeLets
    | Application (I.GlobalAccess g, [x], _) when g = globalId ->
        x
    | Application (I.Function (args, None, _, (I.ExprStatement body | I.Return body)), xs, { KnownLength = Some _ }) 
        when List.length args <= List.length xs && not (needsScoping args xs body) ->
        let xs, more = xs |> List.splitAt args.Length
        match more with
        | [] -> List.foldBack2 bind args xs body
        | _ -> List.foldBack2 bind args xs (CombineExpressions (more @ [ body ]))
    | Application (I.Function (_, None, _, (I.Empty | I.Block [])), xs, _) ->
        Sequential xs
    | Let (a, CurriedFunction (bArgs, ret, bBody), c) ->
        Let(a, Function(bArgs, None, ret, bBody), OptimizeLocalCurriedFunc(a, List.length bArgs).TransformExpression(c))
        |> removeLets
    | Let (a, TupledLambda (bArgs, ret, bBody, isReturn), c) ->
        Let(a, func bArgs ret bBody isReturn, OptimizeLocalTupledFunc(a, List.length bArgs).TransformExpression(c))
        |> removeLets
    // this pattern is generated by Async functions
    | Let (var, value, I.Function ([x], None, _, (I.ExprStatement(I.Application (I.Var f, [I.Var y], _)) | I.Return(I.Application (I.Var f, [I.Var y], _)))))
        when f = var && x = y ->
            value
    | Let(a, b, I.Var c) when a = c ->
        b
    | Let(a, I.Var b, c) 
        when (not b.IsMutable) || (notMutatedOrCaptured a c && notMutatedOrCaptured b c) -> // TODO: maybe weaker check is enough
            ReplaceId(a, b).TransformExpression(c)            
    | Let(a, I.GlobalAccess b, c) when not a.IsMutable ->
        SubstituteVar(a, GlobalAccess b).TransformExpression(c)
    | Let(var, value, I.Application(func, [I.Var v], info))
        when v = var && isStronglyPureExpr func && CountVarOccurence(var).Get(func) = 0 ->
            Application(func, [value], info)
    | Let(var, value, I.Application(I.Var v, args, info)) 
        when v = var && CountVarOccurence(var).Get(Sequential args) = 0 ->
            Application(value, args, info)
    | ObjWithPropSetters res ->
        res
    | Let(a, b, c) ->
        let optimizeTupled  =
            match b with
            | I.NewArray items ->
                match c with
                | AlwaysTupleGet a items.Length (_, (|TupleGet|_|)) ->
                    let vars = List.init items.Length (fun _ -> Id.New(mut = false))
                    List.foldBack2 bind vars items (c |> BottomUp (function TupleGet i -> Var vars.[i] | e -> e)) |> Some
                | _ -> None
            | _ -> None
        match optimizeTupled with
        | Some o -> o
        | _ ->
        match CountVarOccurence(a).Get(c) with
        | 0 ->
            if isPureExpr b then c else Sequential [ b; c ]
        | 1 -> 
            let notmut() =
                match b with
                | I.Function _ -> notMutatedOrCapturedExceptAppl a c
                | _ -> notMutatedOrCaptured a c
            if isStronglyPureExpr b && (isTrivialValue b || notmut()) then 
                SubstituteVar(a, b).TransformExpression(c)
            else
                let accVars = HashSet [a]
                let rec collectVars acc expr =
                    match expr with
                    | I.Let (a, b, c) when VarsNotUsed(accVars).Get(b) ->
                        accVars.Add a |> ignore
                        collectVars ((a, b) :: acc) c
                    | _ -> List.rev acc, expr
                let varsAndVals, innerExpr = collectVars [a, b] c 
                if varEvalOrder (varsAndVals |> List.map fst) innerExpr then
                    SubstituteVars(varsAndVals |> dict).TransformExpression(innerExpr)
                else 
                    expr
        | _ -> 
            expr
    | _ -> expr

let optimize expr =
    match expr with
    // eta-reduction
    | Function (vars, None, _, I.Return (I.Application (f, args, { KnownLength = Some i })))
        when List.length args = i && sameVars vars args && isPureExpr f && VarsNotUsed(vars).Get(f) ->
        f
    // created for sprintf
    | Application(CurriedLambda(i :: vars, ret, I.Application (Var j, [expr], _), true), [ I.Function([x], None, _, I.Return (Var y))], _) when i = j && x = y ->
        CurriedLambda(vars, ret, expr)
    // created for F# string interpolation
    | Application(Lets(bindings, I.Function ([i], None, _, I.Return (I.Application (Var j, [expr], _)))), [ I.Function([x], None, _, I.Return (Var y))], _) when i = j && x = y ->
        expr |> List.foldBack (fun (v, e) b -> Let(v, e, b)) bindings
    | CurriedApplicationSeparate (CurriedLambda(vars, ret, body, isReturn), args) when not (needsScoping vars (List.map snd args) body) ->
        let moreArgsCount = args.Length - vars.Length
        let bind key (isUnit, value) body = 
            if isUnit then
                Sequential [value; body]
            else
                Let (key, value, body)
        if moreArgsCount = 0 then
            if isReturn then
                List.foldBack2 bind vars args body
            else 
                List.foldBack2 bind vars args (Sequential [body; Value Null])
        elif moreArgsCount > 0 then
            let args, moreArgs = args |> List.splitAt vars.Length
            let f =
                if isReturn then
                    List.foldBack2 bind vars args body
                else 
                    List.foldBack2 bind vars args (Sequential [body; Value Null])
            curriedApplication f moreArgs
        else 
            let vars, moreVars = vars |> List.splitAt args.Length
            List.foldBack2 bind vars args (CurriedLambda(moreVars, ret, body)) 
    | Application(TupledLambda(vars, _, body, isReturn), [ I.NewArray args ], { KnownLength = Some _ })
        when vars.Length = args.Length && not (needsScoping vars args body) ->
        if isReturn then
            List.foldBack2 bind vars args body
        else 
            List.foldBack2 bind vars args (Sequential [body; Value Null])
    | Application(I.ItemGet(I.Function (vars, None, _, I.Return body), I.Value (String "apply"), _), [ I.Value Null; argArr ], _) ->
        List.foldBack2 bind vars (List.init vars.Length (fun i -> ItemGet(argArr, Value (Int i), Pure))) body                   
    | Application (I.Function (args, None, _, (I.ExprStatement body | I.Return body)), xs, { KnownLength = Some _ }) 
        when List.length args <= List.length xs && not (needsScoping args xs body) ->
        let xs, more = xs |> List.splitAt args.Length
        match more with
        | [] -> List.foldBack2 bind args xs body
        | _ -> List.foldBack2 bind args xs (CombineExpressions (more @ [ body ]))
    | Application (I.Function (_, None, _, (I.Empty | I.Block [])), xs, _) ->
        Sequential xs
    | Sequential [a] ->
        a
    | Application (I.Sequential (_ :: _ :: _ as a), b, c) ->
        let ar = List.rev a
        Sequential (List.rev (Application (ar.Head, b, c) :: List.tail ar))
    // generated for disposing iterators
    | Application (ItemGet(Let (x, Var y, Var x2), i, gp), b, o) when x = x2 ->
        Application(ItemGet(Var y, i, gp), b, o)
    | StatementExpr (I.ExprStatement a, None) ->
        Void a   
    | _ ->
        expr

type Optimizer() =
    inherit Transformer()

    override this.TransformExpression (a) =
        let mutable i = 0
        let mutable a = base.TransformExpression a
        let mutable b = a |> removeLets |> optimize |> Optimizations.cleanRuntime false
#if DEBUG        
        if logTransformations then
            printfn "optimizer first: %s -> %s" (Debug.PrintExpression a) (Debug.PrintExpression b)
#endif      
        while i < 10 && not (obj.ReferenceEquals(a, b) || a = b) do
            i <- i + 1
            a <- base.TransformExpression b
            b <- a |> removeLets |> optimize |> Optimizations.cleanRuntime false
#if DEBUG        
            if logTransformations then
                printfn "optimizer: %s -> %s" (Debug.PrintExpression a) (Debug.PrintExpression b)
#endif      
        b

let optimizer = Optimizer() :> Transformer

let funcVariablesDoNotUse a br =
    br.Statements |> List.forall(fun f -> 
        match f with
        | I.FuncDeclaration _ ->
            CountVarOccurence(a).GetForStatement(f) = 0
        | _ -> true
    )    

let negate expr =
    match IgnoreExprSourcePos expr with
    | Unary(UnaryOperator.Not, a) -> a
    | Binary(a, op, b) ->
        match op with
        | BinaryOperator.``===`` -> Binary(a, BinaryOperator.``!==`` , b)
        | BinaryOperator.``!==`` -> Binary(a, BinaryOperator.``===`` , b) 
                                                                     
        | BinaryOperator.``==``  -> Binary(a, BinaryOperator.``!=``  , b)
        | BinaryOperator.``!=``  -> Binary(a, BinaryOperator.``==``  , b)

        | BinaryOperator.``<=``  -> Binary(a, BinaryOperator.``>``   , b)
        | BinaryOperator.``>``   -> Binary(a, BinaryOperator.``<=``  , b)

        | BinaryOperator.``>=``  -> Binary(a, BinaryOperator.``<``   , b)
        | BinaryOperator.``<``   -> Binary(a, BinaryOperator.``>=``  , b)

        | _ -> Unary(UnaryOperator.Not, expr)
    | _ -> Unary(UnaryOperator.Not, expr)

let rec breakExpr expr : Broken<BreakResult> =
    let inline br x = 
        breakExpr x
        //let res = breakExpr x
        //printfn "Breaker: %s ---> %s" (Debug.PrintExpression x) (debugPrintBroken res)
        //res

    let brLR l : Broken<BreakResult list> =
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
                | b :: bRest ->
                    if hasNoStatements b then
                        if br then
                            if isStronglyPureOrResultVar b.Body then
                                bL true (b.Variables @ accVar, accSt, b.Body :: accE) bRest
                            else
                                let v = Id.New()
                                bL true (b.Variables @ accVar, VarDeclaration (v, getExpr b.Body) :: accSt, ResultVar v :: accE) bRest
                        else
                            bL false (b.Variables @ accVar, accSt, b.Body :: accE) bRest
                    else
                        if isStronglyPureOrResultVar b.Body then
                            bL true (b.Variables @ accVar, b.Statements @ accSt, b.Body :: accE) bRest
                        else
                            let v = Id.New()
                            bL true (b.Variables @ accVar, b.Statements @ VarDeclaration (v, getExpr b.Body) :: accSt, ResultVar v :: accE) bRest
            let vars, st, e = bL false ([], [], []) (List.rev bb)
            {
                Body = e
                Statements = st
                Variables = vars
            }

    let brL l = brLR l |> toBrExprList

    let comb2 f a b : Broken<BreakResult> =
        brL [a; b] |> mapBroken (function [a; b] -> f(a, b) | _ -> failwith "impossible")
    let comb3 f a b c : Broken<BreakResult> =
        brL [a; b; c] |> mapBroken (function [a; b; c] -> f(a, b, c) | _ -> failwith "impossible")
    
    let cond brA brB brC =
        if hasNoStatements brB && hasNoStatements brC then   
            let brB = brB |> toBrExpr
            let brC = brC |> toBrExpr
            {
                Body = ResultExpr (Conditional (brA.Body, brB.Body, brC.Body))
                Statements = brA.Statements
                Variables = brA.Variables @ brB.Variables @ brC.Variables
            }
        else
            let res = Id.New()
            let setRes x =
                match x.Body with
                | ResultExpr e ->
                    CombineStatements (x.Statements @ [ VarSetStatement(res, e) ])
                | ResultVar v ->
                    CombineStatements (x.Statements |> List.map (TransformVarSets(v, fun e -> VarSet(res, e)).TransformStatement))                      
            {
                Body = ResultVar res
                Statements = brA.Statements @ [If (brA.Body, setRes brB, setRes brC)]
                Variables = brA.Variables @ brB.Variables @ brC.Variables @ [res]
            }

    let boolOp a b c =
        let brA = br a |> toBrExpr
        let brC = br c
        if hasNoStatements brC then
            let brC = toBrExpr brC 
            {
                Body = ResultExpr (Binary (brA.Body, b, brC.Body))
                Statements = brA.Statements
                Variables = brA.Variables @ brC.Variables
            }
        elif b = BinaryOperator.``&&`` then
            cond brA brC (broken (Value (Bool false)))
        else
            cond brA (broken (Value (Bool true))) brC

//    match optimize expr with
    match expr with
    | Undefined
    | Base
    | Var _
    | Value _ 
    | JSThis
    | GlobalAccess _
    | SideeffectingImport _
    | Hole _
        -> broken expr 
    | Function (args, isArr, typ, body) ->
        // do not remove unused functions for now until we can fix up call points too.
        //let args =
        //    args |> List.rev |> List.skipWhile (fun a -> CountVarOccurence(a).GetForStatement(body) = 0) |> List.rev
        broken (Function (args, isArr, typ, BreakStatement body)) 
    | CurriedApplication (f, xs) ->
        xs |> List.fold applyFSharpArg f |> br  
    | Application (ItemGet(a, b, p), c, d) ->
        brL (a :: b :: c)
        |> mapBroken3L (fun aE bE cE -> Application (ItemGet(aE, bE, p), cE, d))
    | Application (a, b, c) -> 
        brL (a :: b)
        |> mapBroken2L (fun aE bE -> Application (aE, bE, c))
    | VarSet (a, b) ->
        let brB = br b
        match brB.Body with
        | ResultVar v ->
            {
                Body = ResultExpr(Var a)
                Statements = brB.Statements |> List.map (TransformVarSets(v, fun e -> VarSet(a, e)).TransformStatement)
                Variables = brB.Variables
            }
        | ResultExpr e ->
            {
                Body = ResultExpr (VarSet (a, e))
                Statements = brB.Statements
                Variables = brB.Variables
            }
    | GlobalAccessSet (a, b) ->
        br b |> toBrExpr
        |> mapBroken (fun bE -> GlobalAccessSet (a, bE))
    | Sequential a ->
        let rec collect a =
            a |> List.collect (
                function 
                | I.Sequential i -> collect i
                | i -> [i]
            )
        let a = collect a
        let brA = brLR a 
        if hasNoStatements brA then
            brA |> toBrExprList |> mapBroken (fun s ->
                match List.rev s with
                | [] -> Undefined
                | [ s ] -> s
                | h :: t -> h :: (t |> List.map removePureParts) |> List.rev |> CombineExpressions
            )
        else 
            let b, extraExprs, removeVars =
                match List.rev brA.Body with
                | [] -> ResultExpr Undefined, [], []
                | [ s ] -> s, [], []
                | h :: t -> 
                    let notUsed v =
                        match h with
                        | ResultVar hv -> hv <> v
                        | ResultExpr e -> CountVarOccurence(v).Get(e) = 0
                    h, 
                    t |> List.choose (function ResultExpr e when not (isPureExpr e) -> Some (removePureParts e) | _ -> None) |> List.rev,
                    t |> List.choose (function ResultVar v -> (if notUsed v then Some v else None) | _ -> None)
            {
                Body = b
                Statements = 
                    if List.isEmpty removeVars then
                        brA.Statements
                    else
                        brA.Statements |> List.map (TransformMoreVarSets(removeVars, id).TransformStatement)
                    @ (extraExprs |> List.map ExprStatement)
                Variables = brA.Variables |> List.filter (fun v -> removeVars |> List.contains v |> not)
            }
    | NewTuple ([ a ], b) ->
        br a |> toBrExpr |> mapBroken (fun a -> NewTuple ([ a ], b))
    | NewTuple (a, b) ->
        brL a |> mapBroken (fun a -> NewTuple(a, b))
    | Conditional (I.Sequential a, b, c) ->
        match List.rev a with
        | [] -> br c
        | t :: r -> Sequential (List.rev (Conditional (t, b, c) :: r)) |> br
    | Conditional (I.Value (Bool a), b, c) ->
        if a then br b else br c    
    | Conditional (a, I.Value (Bool true), I.Value (Bool false)) ->
        br a
    | Conditional (a, I.Value (Bool false), I.Value (Bool true)) ->
        negate a |> br
    | Conditional (a, b, I.Value (Bool false)) ->
        boolOp a BinaryOperator.``&&`` b
    | Conditional (a, I.Value (Bool true), c) ->
        boolOp a BinaryOperator.``||`` c
    | Conditional (a, b, c) ->
        cond (br a |> toBrExpr) (br b) (br c)
    | ItemGet (a, b, p) ->
        comb2 (fun (a, b) -> ItemGet(a, b, p)) a b
    | ItemSet (a, b, c) ->
        comb3 ItemSet a b c
    | Binary ((I.Value _ as a), b, (I.Value _ as c)) ->
        Binary(Cast(TSType.Any, a), b, c) |> br  
    | Binary (a, b, c) ->
        match b with
        | BinaryOperator.``&&``
        | BinaryOperator.``||`` ->
            boolOp a b c
        | _ ->
            comb2 (fun (aE, cE) -> Binary(aE, b, cE)) a c
    | Cast(a, b) ->
        br b |> mapBroken (fun bB -> Cast(a, getExpr bB))
    | Coerce(a, b, c) ->
        br a |> mapBroken (fun bA -> Coerce(getExpr bA, b, c))
    | MutatingBinary (a, b, c) -> 
        comb2 (fun (aE, cE) -> MutatingBinary(aE, b, cE)) a c
    | Unary (a, b) ->
        match a with
        | UnaryOperator.``void`` ->
            let brB = br b
            match brB.Body with
            | ResultVar v ->
                { brB with
                    Body = ResultExpr Undefined
                    Statements = brB.Statements |> List.map (TransformVarSets(v, id).TransformStatement)
                }
            | ResultExpr e ->
                { brB with
                    Body = ResultExpr (if isPureExpr e then Undefined else Unary (a, e))
                }
        | _ ->
            br b |> toBrExpr
            |> mapBroken (fun bE -> Unary (a, bE))
    | MutatingUnary (a, b) ->
        br b |> toBrExpr
        |> mapBroken (fun bE -> MutatingUnary (a, bE))
    | ExprSourcePos (a, b) -> 
        br b
        |> bindBroken (function ResultVar v -> ResultVar v | ResultExpr e -> ResultExpr (ExprSourcePos(a, IgnoreExprSourcePos e)))
    | StatementExpr (I.VarDeclaration(a, b), None) ->
        let brB = br b
        {
            Body = ResultExpr Undefined
            Statements = brB.Statements @ [ VarDeclaration(a, getExpr brB.Body) ]
            Variables = brB.Variables
        }
    | StatementExpr (I.ExprStatement a, Some b) ->
        let brA = br a
        match brA.Body with
        | ResultVar _ ->
            {
                Body = ResultVar b
                Statements = brA.Statements
                Variables = brA.Variables @ [ b ]
            }
        | ResultExpr ae ->
            {
                Body = ResultExpr (Sequential [ae; Var b])
                Statements = brA.Statements 
                Variables = brA.Variables @ [ b ]
            }
    | StatementExpr (st, Some v) ->
        {
            Body = ResultVar v
            Statements = st |> breakSt |> List.ofSeq
            Variables = [ v ]
        }
    | StatementExpr (st, None) ->
        {
            Body = ResultExpr Undefined
            Statements = st |> breakSt |> List.ofSeq
            Variables = []
        }
    | Call(a, b, c, d) ->
        brL (Option.toList a @ d)
        |> mapBroken (fun l ->
            if Option.isSome a then
                Call (Some l.Head, b, c, l.Tail)
            else Call (None, b, c, l)
        )
    | TraitCall(a, b, c, d) ->
        brL (Option.toList a @ d)
        |> mapBroken (fun l ->
            if Option.isSome a then
                TraitCall (Some l.Head, b, c, l.Tail)
            else TraitCall (None, b, c, l)
        )
    | Ctor(a, b, c) ->
        brL c
        |> mapBroken (fun l -> Ctor(a, b, l))
    | CopyCtor (a, b) ->
        br b |> toBrExpr |> mapBroken (fun bE -> CopyCtor (a, bE))
    | FieldGet(a, b, c) ->
        match a with
        | Some a ->
            br a |> toBrExpr |> mapBroken (fun aE -> FieldGet (Some aE, b, c))
        | None -> broken expr
    | FieldSet(a, b, c, d) ->
        match a with
        | Some a ->
            comb2 (fun (aE, dE) -> FieldSet (Some aE, b, c, dE)) a d
        | None ->
            br d |> toBrExpr |> mapBroken (fun dE -> FieldSet (None, b, c, dE))            
    //| Let(var, (SimpleFunction _ as f), c) ->
    //    SubstituteVar(var, f).TransformExpression c |> br
    | Let(var, (I.Function(args, thisVar, ret, body) as f), c) ->
        match CountVarOccurence(var).Get(c) with
        | 0 ->
            br c
        | 1 when notMutatedOrCapturedExceptAppl var c ->
            br (SubstituteVar(var, f).TransformExpression(c))
        | _ ->
            let brC = br c
            {
                Body = brC.Body
                Statements = funcFromLambda(var.WithType(ret), thisVar, false, args, BreakStatement body, []) :: brC.Statements
                Variables = brC.Variables
            }
    | Let(a, b, c) ->
        let brB = br b
        let aUses = CountVarOccurence(a).Get(c)
        match brB.Body with
        | ResultExpr _ ->
            let brB = toBrExpr brB
            let declA = if aUses = 0 then ExprStatement(brB.Body) else VarDeclaration(a, brB.Body)
            if hasNoStatements brB then
                let c = 
                    match getFunctionPurity brB.Body with
                    | NonPure -> c
                    | p ->
                        MarkApplicationsPure(a, p).TransformExpression(c)   
                let inlined =
                    if isStronglyPureExpr brB.Body then
                        match aUses with
                        | 0 -> Some c
                        | 1 -> 
                            if isTrivialValue brB.Body || notMutatedOrCaptured a c then
                                Some (SubstituteVar(a, brB.Body).TransformExpression(c))
                            else None
                        | _ -> None
                    elif isPureExpr brB.Body && aUses = 0 then
                        Some c    
                    else None
                match inlined with
                | Some i -> 
                    br i
                | _ ->
                    let brC = br c 
                    if hasNoStatements brC then
                        let brC = toBrExpr brC
                        if funcVariablesDoNotUse a brC && varEvalOrder [a] brC.Body then 
                            {
                                Body = ResultExpr(SubstituteVar(a, brB.Body).TransformExpression(brC.Body))
                                Statements = []
                                Variables = brB.Variables @ brC.Variables
                            }
                        else
                            //{
                            //    Body = ResultExpr(Sequential [VarSet (a, brB.Body); brC.Body ])
                            //    Statements = []
                            //    Variables = (a, None) :: brB.Variables @ brC.Variables
                            //}
                            {
                                Body = ResultExpr(brC.Body)
                                Statements = [ declA ]
                                Variables = brB.Variables @ brC.Variables
                            }
                    else
                        {
                            Body = brC.Body
                            Statements = declA :: brC.Statements 
                            Variables = brB.Variables @ brC.Variables
                        }
            else
                let brC = br c 
                if hasNoStatements brC then
                    let brC = toBrExpr brC
                    {
                        Body = ResultExpr(brC.Body)
                        Statements = brB.Statements @ [ declA ]
                        Variables = brB.Variables @ brC.Variables
                    }
                else
                    {
                        Body = brC.Body
                        Statements = brB.Statements @ declA :: brC.Statements 
                        Variables = brB.Variables @ brC.Variables
                    }
        | ResultVar bv ->
            let brC = br c 
            if CountVarOccurence(a).Get(c) = 0 then
                {
                    Body = brC.Body
                    Statements = 
                        (brB.Statements |> List.map (TransformVarSets(bv, id).TransformStatement)) 
                        @ brC.Statements
                    Variables = brB.Variables @ brC.Variables
                }
            else
            {
                Body = brC.Body
                Statements = 
                        (brB.Statements |> List.map (TransformVarSets(bv, fun e -> VarSet(a, e)).TransformStatement)) 
                        @ brC.Statements
                Variables = brB.Variables @ brC.Variables @ [ a ]
            }
    | NewVar(a, Undefined) ->
        {
            Body = ResultExpr(Undefined)
            Statements = []
            Variables = [ a ]
        }
    | NewVar(a, b) ->
        let brB = br b
        match brB.Body with
        | ResultExpr e ->
            if hasNoStatements brB then
                { 
                    Body = ResultExpr(VarSet (a, e))
                    Statements = brB.Statements
                    Variables = brB.Variables @ [ a ]       
                }
            else
                { 
                    Body = ResultVar a
                    Statements = brB.Statements @ [ VarDeclaration(a, e) ]
                    Variables = brB.Variables    
                }
        | ResultVar v ->
            { 
                Body = ResultVar a
                Statements = brB.Statements |> List.map (TransformVarSets(v, fun e -> VarSet(a, e)).TransformStatement)
                Variables = brB.Variables @ [ a ]   
            }
    | Object [n, k, a] ->
        br a |> mapBroken (fun a -> Object [n, k, getExpr a])
    | Object a ->
        let names, kinds, values = List.unzip3 a
        brL values
        |> mapBroken (fun l -> Object (List.zip3 names kinds l)) 
    | Coalesce(a, b, c) ->
        let brA = br a
        let brC = br c
        if hasNoStatements brC then
            {
                Body = ResultExpr (Coalesce (getExpr brA.Body, b, getExpr brC.Body))
                Statements = brA.Statements
                Variables = brA.Variables @ brC.Variables
            }
        else
            // TODO: without additional recursion
            let v = Id.New(mut = false)
            Let (v, a, Conditional (TypeCheck(Var v, b), Var v, c)) |> br
    | TypeCheck(a, b) ->
        br a |> toBrExpr
        |> mapBroken (fun aE -> TypeCheck (aE, b))
    | LetRec (a, b) ->
        let brAs = 
            a |> List.map (fun (i, v) -> 
                i, 
                let brA = br v |> toBrExpr
                match brA.Body with
                | I.Function(args, thisVar, ret, body) when not i.IsMutable ->
                    { 
                        Body = None 
                        Statements = brA.Statements @ [ funcFromLambda(i.WithType(ret), thisVar, true, args, body, []) ]
                        Variables = brA.Variables
                    }
                | _ -> 
                    { 
                        Body = Some brA.Body 
                        Statements = brA.Statements
                        Variables = brA.Variables
                    }
            )
        let brB = br b |> toBrExpr
        {
            Body = ResultExpr brB.Body
            Statements =
                [
                    for i, v in brAs do 
                        yield! v.Statements
                        match v.Body with
                        | Some b ->
                            yield VarDeclaration(i, b)
                        | _ -> ()
                    yield! brB.Statements
                ]
            Variables = (brAs |> List.collect (fun (_, ba) -> ba.Variables)) @ brB.Variables 
        }
    | New(a, ts, b) -> 
        brL (a :: b)
        |> mapBroken2L (fun aE bE -> New (aE, ts, bE))
    | ObjectExpr(typ, ctor, ovr) ->
        Option.toList ctor @ (ovr |> List.map (fun (_, _, e) -> e))
        |> brL |> mapBroken (fun m ->
            match ctor, m with
            | Some _, brCtor :: brOvr ->
                let brMem = List.map2 (fun (t, m, _) e -> t, m, e) ovr brOvr
                ObjectExpr (typ, Some brCtor, brMem)
            | _, brOvr ->
                let brMem = List.map2 (fun (t, m, _) e -> t, m, e) ovr brOvr
                ObjectExpr (typ, None, brMem)

        )
    | ClassExpr(name, baseCls, mem) ->
        ClassExpr(name, baseCls, mem |> List.map BreakStatement) |> broken
    | Verbatim(stringParts, holes, isJSX) ->
        let brHoles =
            holes 
            |> List.map (fun e ->
                let brE = breakExpr e
                if hasNoStatements brE then
                    getExpr brE.Body
                else
                    Application(Function([], None, None, brE |> toStatementsSpec Return |> List.ofSeq |> CombineStatements), [], ApplicationInfo.None)
            )
        Verbatim(stringParts, brHoles, isJSX) |> broken
    | e ->
        failwithf "Break expression error, not handled: %s" (Debug.PrintExpression e)

/// break expression to statements, if result would be a temporary var,
/// use `f` (Return or Throw) to transform it to a statement
and toStatementsSpec f b =
    match b.Body with
    | ResultExpr _ -> 
        toStatements f b
    | ResultVar v -> 
        toDecls b.Variables
            (b.Statements |> Seq.collect (TransformVarSets(v, fun a -> StatementExpr(f a, None)).TransformStatement >> breakSt) |> List.ofSeq)

and private breakSt statement : Statement seq =
    let inline brE x = 
        breakExpr x
        //let res = breakExpr x
        //printfn "Breaker: %s ---> %s" (Debug.PrintExpression x) (debugPrintBroken res)
        //res
    let inline brS x = 
        breakSt x
        //let res = breakSt x
        //printfn "Breaker: %s ===> %s" (Debug.PrintStatement x) (res |> Seq.map Debug.PrintStatement |> String.concat "; ")
        //res
    let combine x = 
        x |> List.ofSeq |> CombineStatements
    match statement with
    | Empty
    | Break _ 
    | Continue _ 
    | DoNotReturn
    | Yield _ 
    | Goto _ 
    | ClassProperty _
    | ClassStatic _
        -> Seq.singleton statement
    | GotoCase a -> 
        match a with
        | Some a ->
            brE a |> toStatements (Some >> GotoCase)
        | _ -> Seq.singleton statement
    | ExprStatement (I.Value _ | I.Undefined) ->
        Seq.empty
    | ExprStatement (I.Conditional(a, b, c)) ->
        If(a, ExprStatement b, ExprStatement c)|> brS
    | If(a, I.Empty, I.Empty)
    | ExprStatement a ->
        brE (removePureParts a) |> toStatementExpr
    | Return a ->
        let brA = brE a
        // if we would apply a function in return positions, expand it
        match brA.Body with
        | ResultExpr (I.Application (I.Function (args, _, _, body), xs, _))
            when List.length args = List.length xs ->
                let inlined, notInlined =
                    List.zip args xs |> List.partition (function (a, I.Var _) when not a.IsMutable -> true | _ -> false)   
                [
                    for var, value in notInlined do
                        match value with
                        | I.Function (args, thisVar, ret, body) ->    
                            yield funcFromLambda(var.WithType(ret), thisVar, false, args, body, [])
                        | _ ->
                            yield VarDeclaration(var, value)
                    yield! toDecls brA.Variables brA.Statements 
                    if List.isEmpty inlined then
                        yield body
                    else
                        let d =
                            inlined |> Seq.map (function 
                                | v, I.Var i -> v, i
                                | _ -> failwith "impossible"
                            ) |> dict
                        yield ReplaceIds(d).TransformStatement(body)
                ]
                |> Seq.ofList
        | _ ->
            brA |> toStatementsSpec Return |> List.ofSeq |> CombineStatements |> Seq.singleton
    | Block a ->
        if a |> List.forall (function I.ExprStatement _ -> true | _ -> false) then
            a |> List.map (function I.ExprStatement e -> e | _ -> failwith "impossible")
            |> Sequential |> brE |> toStatementExpr
        else 
            Seq.collect brS a |> List.ofSeq |> Block |> Seq.singleton
    | Labeled (a, b) ->
        Seq.singleton (Labeled (a, combine (brS b))) 
    | VarDeclaration (a, b) ->
        brE b |> toStatements (fun bE -> VarDeclaration (a, bE))
    | FuncDeclaration (a, b, c, d, e) ->
        Seq.singleton (FuncDeclaration (a, b, c, combine (brS d), e))
    | While (a, b) ->
        let brA = brE a
        if hasNoStatements brA then
            brA |> toStatements (fun aE -> While (aE, combine(brS b)))
        else
            let ok = Id.New()
            brA |> toStatementsL (fun aE -> 
                [
                    VarDeclaration (ok, aE)
                    While (Var ok, 
                        Block [
                            yield combine (brS b) 
                            for s in brA.Statements do
                                match s with
                                | VarDeclaration (v, x) -> yield VarSetStatement(v, x)
                                | _ -> yield s 
                            yield VarSetStatement(ok, aE)
                        ]
                    ) 
                ]
            )
    | StatementSourcePos (a, b) ->
        Seq.singleton (StatementSourcePos (a, combine (brS b)))
    | DoWhile(a, b) -> 
        let brB = brE b 
        if hasNoStatements brB then
            brB |> toStatements (fun bE -> DoWhile (combine(brS a), bE))
        else
            let brB = brB |> toBrExpr
            toDecls brB.Variables [ DoWhile (combine (Seq.append (brS a) brB.Statements), brB.Body) ]
    //| For(Some (NewVars setters) as a, b, c, d) ->
    //    let brA = Option.map brE a
    //    let brB = Option.map brE b
    //    let brC = Option.map brE c
    //    let get x = x |> Option.map (fun y -> y.Body |> getExpr)
    //    if Option.forall hasNoStatements brB && Option.forall hasNoStatements brC then
    //        [
    //            match brA with
    //            | Some brA -> 
    //                yield! brA.Variables
    //            | _ -> ()
    //            match brB with
    //            | Some brB -> 
    //                yield! brB.Variables
    //            | _ -> ()
    //            match brC with
    //            | Some brC -> 
    //                yield! brC.Variables
    //            | _ -> ()
    //            match brA with
    //            | Some brA -> 
    //                yield! brA.Statements
    //            | _ -> ()
    //            yield For(get brA, get brB, get brC, combine (brS d))
    //        ]            
    //        |> Seq.ofList
    //    else
    //        let withoutInit =
    //            While (
    //                match b with Some b -> b | _ -> Value (Bool true) 
    //                ,
    //                match c with Some c -> combine [ContinueTransformer(c).TransformStatement(d); ExprStatement c] | _ -> d
    //            )
    //        match a with
    //        | Some a -> combine [ExprStatement a; withoutInit]
    //        | _ -> withoutInit
    //        |> brS
    | For(a, b, c, d) ->       
        let brA = Option.map brE a
        let brB = Option.map brE b
        let brC = Option.map brE c
        let get x = x |> Option.map (fun y -> y.Body |> getExpr)
        if Option.forall hasNoStatements brB && Option.forall hasNoStatements brC then
            match a with 
            | Some (NewVars setters) ->
                let brSetters = 
                    setters |> List.map (fun (i, v) -> i, v |> Option.map brE)
                let brAS =
                    brSetters |> List.map (fun (i, brV) -> 
                        NewVar(i, get brV |> Option.defaultValue Undefined)
                    )
                    |> Sequential |> Some
                let vars =
                    [
                        for (_, brS) in brSetters do
                            match brS with
                            | Some brS ->
                                yield! brS.Variables
                            | _ -> ()
                        match brB with
                        | Some brB -> 
                            yield! brB.Variables
                        | _ -> ()
                        match brC with
                        | Some brC -> 
                            yield! brC.Variables
                        | _ -> ()
                    ]

                let st =
                    [
                        for (_, brS) in brSetters do
                            match brS with
                            | Some brS ->
                                yield! brS.Statements
                            | _ -> ()
                    ]
                [
                    yield! toDecls vars st
                    yield For(brAS, get brB, get brC, combine (brS d))
                ]            
                |> Seq.ofList
            | _ ->
                let vars = 
                    [
                        match brA with
                        | Some brA -> 
                            yield! brA.Variables
                        | _ -> ()
                        match brB with
                        | Some brB -> 
                            yield! brB.Variables
                        | _ -> ()
                        match brC with
                        | Some brC -> 
                            yield! brC.Variables
                        | _ -> ()
                    ]  
                let st = 
                    [
                        match brA with
                        | Some brA -> 
                            yield! brA.Statements
                        | _ -> ()
                    ]
                [
                    yield! toDecls vars st
                    yield For(get brA, get brB, get brC, combine (brS d))
                ]            
                |> Seq.ofList
        else
            let withoutInit =
                While (
                    match b with Some b -> b | _ -> Value (Bool true) 
                    ,
                    match c with Some c -> combine [ContinueTransformer(c).TransformStatement(d); ExprStatement c] | _ -> d
                )
            match a with
            | Some a -> combine [ExprStatement a; withoutInit]
            | _ -> withoutInit
            |> brS
    | Switch(a, b) -> 
        let brA = brE a
        let brCases = b |> List.map (fun (c, d) -> c, combine(brS d))
        brA |> toStatements (fun aE -> Switch(aE, brCases))
    | CSharpSwitch (a, b) ->
        Switch (
            a, 
            [
                for (ls, s) in b do
                    match List.rev ls with
                    | last :: restRev ->
                        let rest = List.rev restRev
                        for r in rest -> r, Empty
                        yield last, s
                    | _ -> failwith "CSharpSwitch case with no labels"    
            ]
        )
        |> brS
    | If(I.Value (Bool a), b, c) ->
        if a then brS b else brS c
    | If(a, I.Empty, c) ->
        If(negate a, c, Empty) |> brS
    | If(a, b, c) ->
        let lastVarSet x =
            let exprRestAndLast =
                match IgnoreStatementSourcePos x with
                | ExprStatement (I.Sequential s) -> 
                    match List.rev s with
                    | [] -> None
                    | h :: t -> Some (List.rev t, h) 
                | ExprStatement e -> Some ([], e)
                | _ -> None 
            match exprRestAndLast with
            | Some (rest, I.VarSet(a, e)) -> Some (rest, a, e)
            | _ -> None
        let append rest e =
            match rest with
            | [] -> e
            | _ -> Sequential (rest @ [ e ]) 
        match lastVarSet b, lastVarSet c with
        | Some (br, bv, be), Some (cr, cv, ce) when bv = cv ->
            VarSet(bv, Conditional(a, append br be, append cr ce)) |> brE 
            |> toStatementExpr
        | _ ->
            brE a |> toStatements (fun aE -> If (aE, combine (brS b), combine (brS c)))
    | Throw(a) -> 
        brE a |> toStatementsSpec Throw
    | TryWith (a, b, c) ->
        Seq.singleton (TryWith (combine (brS a), b, combine (brS c)))
    | TryFinally (a, b) ->           
        Seq.singleton (TryFinally (combine (brS a), combine (brS b)))
    | ForIn(a, b, c) -> 
        brE b |> toStatements (fun bE -> ForIn (a, bE, combine (brS c)))
    | Continuation(a, b) ->
        brE b |> toStatements (fun bE -> Continuation(a, bE))
    | ClassMethod(isStatic, name, pars, thisVar, body, sgn) ->
        ClassMethod(isStatic, name, pars, thisVar, body |> Option.map BreakStatement, sgn) |> Seq.singleton
    | ClassConstructor(pars, thisVar, body, sgn) ->
        ClassConstructor(pars, thisVar, body |> Option.map BreakStatement, sgn) |> Seq.singleton
    | e ->
        failwithf "Break statement error, not handled: %s" (Debug.PrintStatement e)

and BreakStatement statement =
    match breakSt statement |> List.ofSeq with
    | [ s ] -> s
    | st -> CombineStatements st

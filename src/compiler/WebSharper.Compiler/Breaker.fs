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

open WebSharper.Core.AST

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
        Variables : (Id * Statement option) list
    }

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
let private getVarList b = match b with ResultVar v -> [ v, None ] | ResultExpr e -> []

let toBrExpr b =
    {
        Body = getExpr b.Body
        Statements = b.Statements
        Variables = getVarList b.Body @ b.Variables
    }

let toBrExprList b =
    {
        Body = List.map getExpr b.Body
        Statements = b.Statements
        Variables = List.collect getVarList b.Body @ b.Variables
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

type TransformMoreVarSets(vs, tr) =
    inherit Transformer()

    override this.TransformVarSet(a, b) =
        if vs |> List.contains a then
             tr (this.TransformExpression b)    
        else base.TransformVarSet(a, b)    

type MarkApplicationsPure(v) =
    inherit Transformer()

    override this.TransformApplication(func, args, isPure, length) =
        match func with
        | I.Var x when x = v ->
            Application(func, args |> List.map this.TransformExpression, true, length)    
        | _ -> base.TransformApplication(func, args, isPure, length)

    override this.TransformLet(id, value, body) =
        match value with
        | I.Var x when x = v ->
            ReplaceId(id, v).TransformExpression(body) |> this.TransformExpression        
        | _ -> base.TransformLet(id, value, body)

let toDecls (vars: _ list) = 
    seq {
        for vv in vars do
            match vv with
            | v, None -> yield VarDeclaration(v, Undefined)
            | _ -> ()
        for vv in vars do
            match vv with
            | _, Some s -> yield s
            | _ -> ()
    }

let toStatements f b =
    let b = toBrExpr b
    seq {
        yield! toDecls b.Variables
        yield! b.Statements 
        yield f b.Body
    }

let toStatementsL f (b: Broken<BreakResult>)=
    let b = toBrExpr b
    seq {
        yield! toDecls b.Variables
        yield! b.Statements 
        yield! f b.Body
    }

let ignoreVoid e =
    match e with 
    | I.Unary(UnaryOperator.``void``, e)
    | e -> e

let toStatementExpr b =
    match b.Body with
    | ResultVar v ->
        seq {
            yield! toDecls b.Variables
            for st in b.Statements ->
                TransformVarSets(v, id).TransformStatement(st)    
        }
    | ResultExpr e ->
        seq {
            yield! toDecls b.Variables
            yield! b.Statements 
            if not (isPureExpr e) then
                yield ExprStatement (ignoreVoid e)
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

let (|PropSetters|_|) p =
    match List.rev p with
    | (I.Var objVar) :: setters ->
        List.rev setters |> List.fold (fun acc e ->
            match acc with
            | None -> None
            | Some accv ->
            match IgnoreExprSourcePos e with
            | e when isPureExpr e -> acc
            | PropSet (v, fv) when v = objVar -> 
                Some (fv :: accv)
            | _ -> 
                None
        ) (Some []) |> Option.map (fun setters -> setters, objVar)
    | _ -> None

let rec breakExpr expr : Broken<BreakResult> =
    let inline br x = breakExpr x

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
                                bL true ((v, None) :: b.Variables @ accVar, VarSetStatement (v, getExpr b.Body) :: accSt, ResultVar v :: accE) bRest
                        else
                            bL false (b.Variables @ accVar, accSt, b.Body :: accE) bRest
                    else
                        if isStronglyPureOrResultVar b.Body then
                            bL true (b.Variables @ accVar, b.Statements @ accSt, b.Body :: accE) bRest
                        else
                            let v = Id.New()
                            bL true ((v, None) :: b.Variables @ accVar, b.Statements @ VarSetStatement (v, getExpr b.Body) :: accSt, ResultVar v :: accE) bRest
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
    
    let sameVars vars args =
        args |> List.forall (function I.Var _ -> true | _ -> false)
        && vars = (args |> List.map (function I.Var v -> v | _ -> failwith "impossible")) 

    match expr with
    | Undefined
    | This
    | Base
    | Var _
    | Value _ 
    | Self
    | GlobalAccess _
    | Hole _
        -> broken expr 
    | Function (([] | [_]), I.Empty) ->
        broken (Global [ "ignore" ])
    | Function ([x], I.Return (I.Var y)) when x = y ->
        broken (Global [ "id" ])
    | Function (vars, I.Return (I.Application (f, args, _, Some i)))
        when List.length args = i && sameVars vars args && VarsNotUsed(vars).Get(f) ->
            br f
    | Function (args, body) ->
        let args =
            args |> List.rev |> List.skipWhile (fun a -> CountVarOccurence(a).GetForStatement(body) = 0) |> List.rev
        broken (Function (args, BreakStatement body)) 
    | Application (I.Function (args, I.Return body), xs, _, _) 
        when List.length args = List.length xs && not (needsScoping args body) ->
        let bind key value body = Let (key, value, body)
        List.foldBack2 bind args xs body |> br
    | Application (I.Function (args, I.ExprStatement body), xs, _, _) 
        when List.length args = List.length xs && not (needsScoping args body) ->
        let bind key value body = Let (key, value, body)
        List.foldBack2 bind args xs body |> br
    | Application (I.Function (_, (I.Empty | I.Block [])), xs, _, _) ->
        Sequential xs |> br
    | Application (I.Let (var, value, body), xs, p, l) ->
        Let (var, value, Application (body, xs, p, l)) |> br
    // generated for disposing iterators
    | Application (ItemGet(Let (x, Var y, Var x2), i), b, p, l) when x = x2 ->
        Application(ItemGet(Var y, i), b, p, l) |> br
    | Application (ItemGet(a, b), c, d, e) ->
        brL (a :: b :: c)
        |> mapBroken3L (fun aE bE cE -> Application (ItemGet(aE, bE), cE, d, e))
    | Application (a, b, c, d) -> 
        brL (a :: b)
        |> mapBroken2L (fun aE bE -> Application (aE, bE, c, d))
    | VarSet (a, b) ->
        br b |> toBrExpr
        |> mapBroken (fun bE -> VarSet (a, bE))
    | Sequential [a] ->
        br a
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
                | h :: t -> h :: (t |> List.filter (isPureExpr >> not)) |> List.rev |> Sequential
            )
        else 
            let b, extraExprs, removeVars =
                match List.rev brA.Body with
                | [] -> ResultExpr Undefined, [], []
                | [ s ] -> s, [], []
                | h :: t -> 
                    h, 
                    t |> List.choose (function ResultExpr e when not (isPureExpr e) -> Some (ignoreVoid e) | _ -> None) |> List.rev,
                    t |> List.choose (function ResultVar v -> Some v | _ -> None)
            {
                Body = b
                Statements = 
                    if List.isEmpty removeVars then
                        brA.Statements
                    else
                        brA.Statements |> List.map (TransformMoreVarSets(removeVars, id).TransformStatement)
                    @ (extraExprs |> List.map ExprStatement)
                Variables = brA.Variables |> List.filter (fun (v, _) -> removeVars |> List.contains v |> not)
            }
    | NewArray [ a ] ->
        br a |> mapBroken (fun a -> NewArray [getExpr a])
    | NewArray a ->
        brL a |> mapBroken NewArray
    | Conditional (a, b, c) ->
        let brA = br a |> toBrExpr 
        let brB = br b
        let brC = br c
        match brB.Body, brC.Body with
        | ResultVar bv, ResultVar cv ->
            let r = Id.New()
            let setRes x =
                x.Statements |> List.map (TransformMoreVarSets([bv; cv], fun e -> VarSet(r, e)).TransformStatement) |> Block
            {
                Body = ResultVar r
                Statements = brA.Statements @ [If (brA.Body, setRes brB, setRes brC) ] 
                Variables = brA.Variables @ brB.Variables @ brC.Variables
            }
        | _ ->
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
                Variables = brA.Variables @ brB.Variables @ brC.Variables
            }
    | ItemGet (a, b) ->
        comb2 ItemGet a b
    | ItemGetNonPure (a, b) ->
        comb2 ItemGetNonPure a b
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
        match a with
        | UnaryOperator.``void`` ->
            let brB = br b
            match brB.Body with
            | ResultVar v ->
                { 
                    Body = ResultExpr Undefined
                    Statements = brB.Statements |> List.map (TransformVarSets(v, id).TransformStatement)
                    Variables = brB.Variables    
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
        match brB.Body with
        | ResultVar v ->
            {
                Body = ResultVar a
                Statements = brB.Statements |> List.map (TransformVarSets(v, fun e -> VarSet(a, e)).TransformStatement)
                Variables = brB.Variables
            }
        | ResultExpr e ->
            {
                Body = ResultExpr (Void (VarSet(a, e)))
                Statements = brB.Statements
                Variables = [ a, None ] @ brB.Variables
            }
    | StatementExpr (I.ExprStatement a, None) ->
        br a   
    | StatementExpr (I.ExprStatement a, Some b) ->
        let brA = br a
        match brA.Body with
        | ResultVar _ ->
            {
                Body = ResultVar b
                Statements = brA.Statements
                Variables = brA.Variables
            }
        | ResultExpr ae ->
            {
                Body = ResultExpr (Sequential [ae; Var b])
                Statements = brA.Statements
                Variables = (b, None) :: brA.Variables
            }
    | StatementExpr (st, v) ->
        {
            Body = match v with Some v -> ResultVar v | _ -> ResultExpr Undefined
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
        brL (a :: d)
        |> mapBroken (fun l ->
            TraitCall (l.Head, b, c, l.Tail)
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
    // this pattern is generated by Async functions
    | Let (var, value, I.Function ([x], (I.ExprStatement(I.Application (I.Var f, [I.Var y], _, _)) | I.Return(I.Application (I.Var f, [I.Var y], _, _)))))
        when f = var && x = y ->
            br value
    | Let(a, b, I.Var c) when a = c ->
        br b
    | Let(a, I.Var b, c) 
        when (not b.IsMutable) || (notMutatedOrCaptured a c && notMutatedOrCaptured b c) -> // TODO: maybe weaker check is enough
            ReplaceId(a, b).TransformExpression(c) |> br            
    | Let(var, value, I.Application(func, [I.Var v], p, l))
        when v = var && isStronglyPureExpr func && CountVarOccurence(var).Get(func) = 0 ->
            Application(func, [value], p, l) |> br
    | Let (objVar, I.Object objFields, I.Sequential (PropSetters (setters, v))) when v = objVar ->
        objFields @ setters |> Object |> br
    | Let(var, I.Function(args, body), c) 
        when notMutatedOrCaptured var c && CountVarOccurence(var).Get(c) >= 2 ->
            let brC = br c
            {
                Body = brC.Body
                Statements = brC.Statements
                Variables = (var, Some (FuncDeclaration(var, args, BreakStatement body))) :: brC.Variables
            }
    | Let(a, b, c) ->
        let brB = br b
        match brB.Body with
        | ResultExpr _ ->
            let brB = toBrExpr brB
            if hasNoStatements brB then
                let c = 
                    if isPureFunction brB.Body then
                        MarkApplicationsPure(a).TransformExpression(c)   
                    else c
                let inlined =
                    if isStronglyPureExpr brB.Body then
                        match CountVarOccurence(a).Get(c) with
                        | 0 -> Some c
                        | 1 -> 
                            if isTrivialValue brB.Body || notMutatedOrCaptured a c then
                                Some (SubstituteVar(a, brB.Body).TransformExpression(c))
                            else None
                        | _ -> None
                    elif isPureExpr brB.Body && CountVarOccurence(a).Get(c) = 0 then
                        Some c    
                    else None
                match inlined with
                | Some i -> br i
                | _ ->
                    let brC = br c 
                    if hasNoStatements brC then
                        let brC = toBrExpr brC
                        if varEvalOrder [a] brC.Body then 
                            {
                                Body = ResultExpr(SubstituteVar(a, brB.Body).TransformExpression(brC.Body))
                                Statements = []
                                Variables = brB.Variables @ brC.Variables
                            }
                        else
                            {
                                Body = ResultExpr(Sequential [VarSet (a, brB.Body); brC.Body ])
                                Statements = []
                                Variables = (a, None) :: brB.Variables @ brC.Variables
                            }
                    else
                        {
                            Body = brC.Body
                            Statements = VarDeclaration(a, brB.Body) :: brC.Statements 
                            Variables = brB.Variables @ brC.Variables
                        }
            else
                let brC = br c 
                if hasNoStatements brC then
                    let brC = toBrExpr brC
                    {
                        Body = ResultExpr(Sequential [VarSet (a, brB.Body); brC.Body ])
                        Statements = brB.Statements
                        Variables = (a, None) :: brB.Variables @ brC.Variables
                    }
                else
                    {
                        Body = brC.Body
                        Statements = brB.Statements @ VarDeclaration(a, brB.Body) :: brC.Statements 
                        Variables = brB.Variables @ brC.Variables
                    }
        | ResultVar bv ->
            let brC = br c 
            {
                Body = brC.Body
                Statements = 
                    (brB.Statements |> List.map (TransformVarSets(bv, fun e -> VarSet(a, e)).TransformStatement)) 
                    @ brC.Statements 
                Variables = (a, None) :: brB.Variables @ brC.Variables
            }
    | NewVar(a, Undefined) ->
        {
            Body = ResultExpr(Undefined)
            Statements = []
            Variables = [ a, None ]
        }
    | NewVar(a, b) ->
        let brB = br b
        match brB.Body with
        | ResultExpr e ->
            { 
                Body = ResultExpr(VarSet (a, e))
                Statements = brB.Statements
                Variables = (a, None) :: brB.Variables    
            }
        | ResultVar v ->
            { 
                Body = ResultVar a
                Statements = brB.Statements |> List.map (TransformVarSets(v, fun e -> VarSet(a, e)).TransformStatement)
                Variables = brB.Variables    
            }
    | Object [n, a] ->
        br a |> mapBroken (fun a -> Object [n, getExpr a])
    | Object a ->
        let names, values = List.unzip a
        brL values
        |> mapBroken (fun l -> Object (List.zip names l)) 
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
    | LetRec (a, b) -> // better support for mutually recursive functions and values
        let brAs = a |> List.map (fun (i, v) -> i, br v |> toBrExpr)
        let brB = br b |> toBrExpr
        {
            Body = ResultExpr brB.Body
            Statements =
                [
                    for i, v in brAs do 
                        yield! v.Statements
                        yield VarSetStatement(i, v.Body)
                    yield! brB.Statements
                ]
            Variables = (brAs |> List.collect (fun (v, ba) -> (v, None) :: ba.Variables)) @ brB.Variables 
        }
    | New(a, b) -> 
        brL (a :: b)
        |> mapBroken2L (fun aE bE -> New (aE, bE))
    | e ->
        failwithf "Break expression error: %A" e

and private breakSt statement : Statement seq =
    let inline brE x = breakExpr x
    let inline brS x = breakSt x
    let combine x = 
        x |> List.ofSeq |> CombineStatements
    match statement with
    | Empty
    | Break _ 
    | Continue _ 
    | Yield _ 
    | Goto _ -> Seq.singleton statement
    | GotoCase a -> 
        match a with
        | Some a ->
            brE a |> toStatements (Some >> GotoCase)
        | _ -> Seq.singleton statement
    | ExprStatement a ->
        brE a |> toStatementExpr
    | Return a ->
        brE a |> toStatements Return
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
    | FuncDeclaration (a, b, c) ->
        Seq.singleton (FuncDeclaration (a, b, combine (brS c)))
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
            [
                yield! toDecls brB.Variables
                yield DoWhile (combine (Seq.append (brS a) brB.Statements), brB.Body)   
            ]            
            |> Seq.ofList
    | For(a, b, c, d) ->       
        let brA = Option.map brE a
        let brB = Option.map brE b
        let brC = Option.map brE c
        let get x = x |> Option.map (fun y -> y.Body |> getExpr)
        if Option.forall hasNoStatements brB && Option.forall hasNoStatements brC then
            [
                match brA with
                | Some brA -> 
                    yield! toDecls brA.Variables
                | _ -> ()
                match brB with
                | Some brB -> 
                    yield! toDecls brB.Variables
                | _ -> ()
                match brC with
                | Some brC -> 
                    yield! toDecls brC.Variables
                | _ -> ()
                match brA with
                | Some brA -> 
                    yield! brA.Statements
                | _ -> ()
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
        brE a |> toStatements Throw
    | TryWith (a, b, c) ->
        Seq.singleton (TryWith (combine (brS a), b, combine (brS c)))
    | TryFinally (a, b) ->           
        Seq.singleton (TryFinally (combine (brS a), combine (brS b)))
    | ForIn(a, b, c) -> 
        brE b |> toStatements (fun bE -> ForIn (a, bE, combine (brS c)))
    | Continuation(a, b) ->
        brE b |> toStatements (fun bE -> Continuation(a, bE))

and BreakStatement statement =
    match breakSt statement |> List.ofSeq with
    | [ s ] -> s
    | st -> Block st

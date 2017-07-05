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

module WebSharper.Compiler.FSharp.ArgCurrying

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata
open WebSharper.Compiler
open WebSharper.Compiler.NotResolved
module I = IgnoreSourcePos

type Member =
    | Method of TypeDefinition * Method
    | Constructor of TypeDefinition * Constructor

/// Examines applications of function typed arguments.
/// If always used with a certain number of fixed arguments, 
/// curried functions can be optimized to flat function in translation.
type FuncArgVisitor(opts: FuncArgOptimization list, margs: Id list) =
    inherit Visitor()

    let cargs =
        (opts, margs) ||> Seq.map2 (fun c a ->
            match c with
            | CurriedFuncArg _ 
            | TupledFuncArg _ -> Some a
            | _ -> None
        ) |> Seq.choose id |> HashSet
    let iargs = margs |> Seq.mapi (fun i a -> a, i) |> dict |> Dictionary

    let appl =
        opts |> Seq.map (fun c ->
            match c with
            | CurriedFuncArg a -> a
            | TupledFuncArg _ -> 1
            | _ -> 0
        ) |> Array.ofSeq
    let calls = Array.init iargs.Count ResizeArray

    let setAppl i value =
        let i = iargs.[i]
        let a = appl.[i]
        if a > value then
            appl.[i] <- value

    let rec (|ArgIndex|_|) e =
        match e with
        | Var v ->
            if cargs.Contains v then Some iargs.[v] else None
        | Hole i ->
            if cargs.Contains margs.[i] then Some i else None
        | Call(None, typ, meth, [ a ]) 
            when typ.Entity.Value.FullName = "WebSharper.JavaScript.Pervasives" && meth.Entity.Value.MethodName = "As" ->
                (|ArgIndex|_|) (IgnoreExprSourcePos a)
        | _ -> None
                
    member this.Results =
        let res =
            (appl, opts) ||> Seq.map2(fun i c -> 
                match c with     
                | CurriedFuncArg _ -> 
                    if i > 1 then CurriedFuncArg i else NotOptimizedFuncArg
                | TupledFuncArg _ ->
                    if i > 0 then c else NotOptimizedFuncArg
                | _ -> c
            ) |> Array.ofSeq
        Array.zip res calls    

    override this.VisitId i =
        if cargs.Contains i then
            setAppl i 0

    override this.VisitHole i =
        this.VisitId(margs.[i])

    override this.VisitFunction(args, body) =
        this.VisitStatement body

    override this.VisitLet(var, value, body) =
        match IgnoreExprSourcePos value with
        | Hole _ when iargs.ContainsKey var -> this.VisitExpression(body)
        | ArgIndex i -> 
            cargs.Add var |> ignore
            iargs.Add(var, i)  
            this.VisitExpression(body)
        | _ -> base.VisitLet(var, value, body)

    override this.VisitCall(thisOpt, typ, meth, args) =
        thisOpt |> Option.iter this.VisitExpression
        args |> List.iteri (fun i a ->
            match IgnoreExprSourcePos a with
            | ArgIndex j ->
                calls.[j].Add(Method (typ.Entity, meth.Entity), i)
            | a -> this.VisitExpression a     
        )

    override this.VisitCtor(typ, ctor, args) =
        args |> List.iteri (fun i a ->
            match IgnoreExprSourcePos a with
            | ArgIndex j ->
                calls.[j].Add(Constructor (typ.Entity, ctor), i)
            | a -> this.VisitExpression a     
        )

    override this.VisitBaseCtor(expr, typ, ctor, args) =
        args |> List.iteri (fun i a ->
            match IgnoreExprSourcePos a with
            | ArgIndex j ->
                calls.[j].Add(Constructor (typ.Entity, ctor), i)
            | a -> this.VisitExpression a     
        )
        this.VisitExpression expr

    override this.VisitCurriedApplication(f, args) =
        match IgnoreExprSourcePos f with
        | ArgIndex i ->
            setAppl margs.[i] (List.length args)
        | f -> this.VisitExpression f
        args |> List.iter this.VisitExpression            

    override this.VisitApplication(f, args, _, _) =
        match IgnoreExprSourcePos f with
        | ArgIndex i ->
            setAppl margs.[i] 1
        | f -> this.VisitExpression f
        args |> List.iter this.VisitExpression            

type FuncArgTransformer(al: list<Id * FuncArgOptimization>, isInstance) =
    inherit Transformer()

    let cargs = dict al
    let mutable noHoleLets = true
         
    override this.TransformVar(v) =
        match cargs.TryGetValue v with
        | true, (CurriedFuncArg _ | TupledFuncArg _ as opt) ->
            OptimizedFSharpArg(Var v, opt)
        | _ -> Var v
    
    override this.TransformHole(i) =
        // only want this for holes that was optimized so that there is no let
        if noHoleLets then
            // only real arguments of instance methods was analyzed
            let j = if isInstance then i - 1 else i
            if j = -1 then Hole 0 else
            match al.[j] with
            | _, (CurriedFuncArg _ | TupledFuncArg _ as opt) ->
                OptimizedFSharpArg(Hole i, opt)
            | _ -> Hole i
        else Hole i

    override this.TransformCurriedApplication(func, args: Expression list) =
        match func with
        | I.Var f ->
            match cargs.TryGetValue f with
            | true, CurriedFuncArg a ->
                let ucArgs, restArgs = args |> List.map this.TransformExpression |> List.splitAt a
                let inner = Application(Var f, ucArgs, NonPure, Some a)
                curriedApplication inner restArgs
            | true, TupledFuncArg a ->
                match args with
                | t :: rArgs ->
                    curriedApplication (this.TransformApplication(func, [t], NonPure, Some 1))
                        (List.map this.TransformExpression rArgs)
                | _ -> failwith "tupled func must have arguments"
            | _ -> base.TransformCurriedApplication(func, args)
        | _ -> base.TransformCurriedApplication(func, args)

    override this.TransformApplication(func, args, p, l) =
        let normal() =
            Application(this.TransformExpression func, List.map this.TransformExpression args, p, l)
        match func with
        | I.Var f ->
            match cargs.TryGetValue f with
            | true, TupledFuncArg a ->
                match args with
                | [ I.NewArray es ] ->
                    Application(Var f, List.map this.TransformExpression es, p, Some (List.length es))
                | _ ->
                    failwith "tupled function applied with multiple arguments"    
            | _ -> normal()    
        | _ -> normal()

    member this.TransformBody e =
        match e with
        | Let (_, Hole _, _) -> noHoleLets <- false 
        | _ -> ()
        this.TransformExpression e
    
type ResolveFuncArgs(comp: Compilation) =
    let members = Dictionary<Member, NotResolvedMethod * Id list * bool>()
    let resolved = HashSet<Member>()
    let rArgs = Dictionary<Member * int, FuncArgOptimization>()
    let callsTo = Dictionary<Member * int, list<Member * int>>()

    let printMem mem = 
        match mem with
        | Method(typ, meth) -> sprintf "method %s.%s" typ.Value.FullName meth.Value.MethodName
        | Constructor(typ, ctor) -> sprintf "constructor %s" typ.Value.FullName

    let getRArgs mi =
        match rArgs.TryGetValue(mi) with
        | true, v -> v
        | _ -> NotOptimizedFuncArg
         
    let rec setRArgs mi value =
        let mem, i = mi
        match rArgs.TryGetValue(mi) with
        | true, v -> 
            if value <> v then
                rArgs.[mi] <- value
                match callsTo.TryGetValue(mi) with
                | true, ct ->
                    for c in ct do
                        setRArgs c value
                | _ -> ()
        | _ ->
            rArgs.[mi] <- value
            rArgs.[mi] <- value

    member this.AddMember(mem, nr, args, isInstance) =
        members.Add(mem, (nr, args, isInstance)) |> ignore    

    member this.GetCompiled(mem, i) =
        match mem with
        | Method(typ, meth) ->
            comp.TryLookupClassInfo(typ) |> Option.map (fun cls -> cls.Methods.[meth])
        | Constructor(typ, ctor) ->
            comp.TryLookupClassInfo(typ) |> Option.map (fun cls -> cls.Constructors.[ctor])
        |> Option.bind (fun (_, opts, _) ->
            opts.FuncArgs |> Option.map (fun ca -> ca.[i])
        )

    member this.ResolveMember(mem) =
        if resolved.Add(mem) then
            match members.TryGetValue mem with
            | true, (nr, args, _) -> 
                let nr, args, _ = members.[mem] 
                let cv = FuncArgVisitor(nr.FuncArgs.Value, args)
                cv.VisitExpression(nr.Body)
                for i, (c, calls) in cv.Results |> Seq.indexed do
                    match c with
                    | CurriedFuncArg _ -> 
                        calls |> Seq.fold (fun v call -> 
                            match this.GetCompiled(call) with
                            | Some n -> 
                                Dict.addToMulti callsTo call (mem, i)
                                min n v
                            | _ -> 
                                this.ResolveMember(fst call)
                                min (getRArgs call) v
                        ) c |> setRArgs (mem, i)
                    | TupledFuncArg _ -> 
                        calls |> Seq.fold (fun v call -> 
                            match this.GetCompiled(call) with
                            | Some n -> 
                                Dict.addToMulti callsTo call (mem, i)
                                min n v
                            | _ -> 
                                this.ResolveMember(fst call)
                                min (getRArgs call) v
                        ) c |> setRArgs (mem, i)
                    | _ ->
                        setRArgs (mem, i) c
            | _ -> ()

    member this.ResolveAll() =
        for mem in members.Keys do
            this.ResolveMember(mem)

        let rArgs =
            rArgs
            |> Seq.map (fun (KeyValue((mem, i), c)) -> mem, (i, c))
            |> Seq.groupBy fst
            |> Seq.map (fun (mem, curr) ->
                let _, ocs, _ = members.[mem] 
                let cs = Array.zeroCreate (List.length ocs)
                for _, (i, c) in curr do
                    cs.[i] <- c
                mem, List.ofArray cs
            )
            |> dict
        
        for (KeyValue(mem, (nr, args, isInstance))) in members do
            let cs = rArgs.[mem]
            nr.FuncArgs <- Some cs
            let tr = FuncArgTransformer(List.zip args cs, isInstance)
            nr.Body <- tr.TransformBody(nr.Body)

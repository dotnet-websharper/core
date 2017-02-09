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

// Main translation module from .NET forms to JavaScript
module WebSharper.Compiler.Translator
 
open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Compiler

module M = WebSharper.Core.Metadata


/// Debug-only checker for invalid forms after transformation to have localized error.
/// Otherwise error is thrown when writing to JavaScript after packaging.
type CheckNoInvalidJSForms(comp: Compilation, isInline) as this =
    inherit TransformerWithSourcePos(comp)

    let invalidForm f = 
        this.Error("Invalid form after JS tranlation: " + f)

    override this.TransformSelf () = invalidForm "Self"
    override this.TransformBase () = invalidForm "Base"
    override this.TransformHole a = if isInline then base.TransformHole(a) else invalidForm "Hole"
    override this.TransformFieldGet (_,_,_) = invalidForm "FieldGet"
    override this.TransformFieldSet (_,_,_,_) = invalidForm "FieldSet"
    override this.TransformLet (a, b, c) = if isInline then base.TransformLet(a, b, c) else invalidForm "Let" 
    override this.TransformLetRec (_,_) = invalidForm "LetRec"
    override this.TransformStatementExpr (a, b) = if isInline then base.TransformStatementExpr(a, b) else invalidForm "StatementExpr"
    override this.TransformAwait _  = invalidForm "Await"
    override this.TransformNamedParameter (_,_) = invalidForm "NamedParameter"
    override this.TransformRefOrOutParameter _ = invalidForm "RefOrOutParamete"
    override this.TransformCtor (a, b, c) = if isInline then base.TransformCtor(a, b, c) else invalidForm "Ctor"
    override this.TransformCoalesce (_,_,_) = invalidForm "Coalesce"
    override this.TransformTypeCheck (_,_) = invalidForm "TypeCheck"
    override this.TransformCall (a, b, c, d) = if isInline then base.TransformCall(a, b, c, d) else invalidForm "Call"

type RemoveLets() =
    inherit Transformer()
    
    override this.TransformExpression (a) =
        base.TransformExpression(removeLets a)
                                
let removeLetsTr = RemoveLets()

type RuntimeCleaner() =
    inherit Transformer()
    
    override this.TransformExpression (a) =
        base.TransformExpression(Optimizations.cleanRuntime a)

let private runtimeCleaner = RuntimeCleaner()

type Breaker(isInline) =
    inherit Transformer()

    override this.TransformStatement (a) =
#if DEBUG
        if logTransformations then
            printfn "breaker start: %s" (Debug.PrintStatement a)
#endif
        let opt = a |> optimizer.TransformStatement
        let res = if isInline then opt else BreakStatement opt
#if DEBUG
        if logTransformations then
            printfn "breaker result: %s" (Debug.PrintStatement res)
#endif
        res
let private breaker = Breaker(false)
let private inlineOptimizer = Breaker(true)

type CollectCurried() =
    inherit Transformer()

    override this.TransformFunction(args, body) =
        match Function(args, body) with
        | CurriedFunction(a, b) ->
            match a.Length with
            | 2 -> JSRuntime.Curried2 (base.TransformFunction(a, b)) 
            | 3 -> JSRuntime.Curried3 (base.TransformFunction(a, b)) 
            | n -> JSRuntime.Curried (base.TransformFunction(a, b)) n
        | _ -> base.TransformFunction(args, body)   
   
let collectCurried = CollectCurried() 

let defaultRemotingProvider =
    TypeDefinition {
        Assembly = "WebSharper.Main"
        FullName =  "WebSharper.Remoting+AjaxRemotingProvider"
    }
    
let emptyConstructor = Hashed { CtorParameters = [] }

let inline private getItem n x = ItemGet(x, Value (String n))
let inline private getIndex n x = ItemGet(x, Value (Int n))

type GenericInlineResolver (generics) =
    inherit Transformer()

    let gs = Array.ofSeq generics 

    let subs (t: Type) = t.SubstituteGenerics(gs)

    override this.TransformCall (thisObj, typ, meth, args) =
        Call (
            thisObj |> Option.map this.TransformExpression, 
            Generic typ.Entity (typ.Generics |> List.map subs),
            Generic meth.Entity (meth.Generics |> List.map subs), 
            args |> List.map this.TransformExpression
        )
        
    override this.TransformCtor(typ, ctor, args) =
        Ctor (
            Generic typ.Entity (typ.Generics |> List.map subs),
            ctor, 
            args |> List.map this.TransformExpression
        )

    override this.TransformTraitCall(thisObj, typ, meth, args) =
        TraitCall (
            thisObj |> this.TransformExpression, 
            typ |> subs,
            Generic meth.Entity (meth.Generics |> List.map subs), 
            args |> List.map this.TransformExpression
        )

let private objTy = NonGenericType Definitions.Obj

let rpcMethodNode name ret =
    M.AbstractMethodNode (Definitions.IRemotingProvider, Method {
        MethodName = name
        Parameters = [ NonGenericType Definitions.String; ArrayType (objTy, 1) ]
        ReturnType = ret
        Generics = 0       
    })

let private syncRpcMethodNode = rpcMethodNode "Sync" objTy
let private asyncRpcMethodNode = rpcMethodNode "Async" (GenericType Definitions.Async [objTy])
let private taskRpcMethodNode = rpcMethodNode "Task" (GenericType Definitions.Task1 [objTy])
let private sendRpcMethodNode = rpcMethodNode "Send" VoidType

type DotNetToJavaScript private (comp: Compilation, ?inProgress) =
    inherit TransformerWithSourcePos(comp)

    let inProgress = defaultArg inProgress []
    let mutable selfAddress = None
    let mutable currentNode = M.AssemblyNode ("", false) // placeholder
    let mutable currentIsInline = false
    let mutable hasDelayedTransform = false
    let mutable currentFuncArgs = None

    let removeSourcePosFromInlines expr =
        if currentIsInline then removeSourcePos.TransformExpression expr else expr

    let modifyDelayedInlineInfo (info: M.CompiledMember) =
        if hasDelayedTransform then 
            let rec m info =
                match info with 
                | M.Macro (t, p, fb) -> M.Macro(t, p, fb |> Option.map m)
                | _ -> M.NotCompiledInline 
            m info
        else info

    let isInline info =
        let rec ii m =
            match m with 
            | M.Inline -> true
            | M.Macro(_, _, Some f) -> ii f
            | _ -> false
        match info with        
        | NotCompiled (m, _, _) 
        | NotGenerated (_, _, m, _) -> ii m

    let breakExpr e = 
        if currentIsInline then
            e 
            |> removeLetsTr.TransformExpression
            |> runtimeCleaner.TransformExpression
            |> inlineOptimizer.TransformExpression
        else
            e 
            |> removeLetsTr.TransformExpression
            |> runtimeCleaner.TransformExpression
            |> breaker.TransformExpression
            |> collectCurried.TransformExpression

    member this.CheckResult (res) =
#if DEBUG
        if hasDelayedTransform then res else
            CheckNoInvalidJSForms(comp, currentIsInline).TransformExpression res
#else
        res
#endif
     
    member this.Generate(g, p, m) =
        match comp.GetGeneratorInstance(g) with
        | Some gen ->
            let genResult = 
                try
                    gen.Generate {
                        Member = m
                        Parameter = p
                        Compilation = comp
                    }
                with e -> GeneratorError e.Message
            let verifyFunction gres =
                match IgnoreExprSourcePos gres with 
                | Function _
                | FuncWithThis _ -> gres
                | _ -> this.Error(sprintf "Generator not returning a function: %s" g.Value.FullName)
            let rec getExpr gres = 
                match gres with
                | GeneratedQuotation q -> 
                    QuotationReader.transformExpression (QuotationReader.Environment.New(comp)) q
                    |> verifyFunction |> this.TransformExpression |> breakExpr
                | GeneratedAST resExpr -> resExpr |> verifyFunction |> this.TransformExpression |> breakExpr
                | GeneratedString s -> Recognize.parseGeneratedString s
                | GeneratedJavaScript js -> Recognize.parseGeneratedJavaScript js
                | GeneratorError msg ->
                    this.Error(sprintf "Generator error in %s: %s" g.Value.FullName msg)
                | GeneratorWarning (msg, gres) ->
                    this.Warning (sprintf "Generator warning in %s: %s" g.Value.FullName msg)
                    getExpr gres
            getExpr genResult
        | None ->
            if comp.UseLocalMacros then
                this.Error("Getting generator failed")
            else
                this.Warning("Could not run generator in code service.")
                Undefined       

    member this.GetCustomTypeConstructorInline (i : M.CustomTypeInfo, ctor: Constructor) =
        match i with
        | M.FSharpRecordInfo fields ->
            let obj = 
                fields
                |> Seq.mapi (fun i f -> 
                    f.JSName,
                        if f.Optional then
                            let id = Id.New(mut = false)
                            Let(id, Hole i,
                                Conditional(Var id, ItemGet(Var id, Value (String "$0")), Undefined))
                        else Hole i)
                |> List.ofSeq |> Object
            let optFields = 
                fields |> List.choose (fun f -> 
                    if f.Optional then Some (Value (String f.JSName)) else None)
            if List.isEmpty optFields then obj
            else JSRuntime.DeleteEmptyFields obj optFields
        | _ -> this.Error("Unhandled F# compiler generated constructor")
    
    member this.GetCustomTypeMethodInline (typ : Concrete<TypeDefinition>, i : M.CustomTypeInfo, meth: Concrete<Method>) =
        let me = meth.Entity.Value
        match i with
        | M.DelegateInfo di ->
            match me.MethodName with
            | "Invoke" ->
                // TODO: optional arguments
                let args = di.DelegateArgs |> List.mapi (fun i _ -> Hole (i + 1)) //|> NewArray
                
                Application(Hole 0, args, false, Some args.Length)
                //Application(JSRuntime.InvokeDelegate, [Hole 0; args])
            | "op_Addition" -> JSRuntime.CombineDelegates (NewArray [Hole 0; Hole 1])
            | "op_Equality" -> JSRuntime.DelegateEqual (Hole 0) (Hole 1)
            | "ToString" -> Value (String typ.Entity.Value.FullName)
            | mn -> this.Error("Unrecognized delegate method: " + mn)
        | M.FSharpRecordInfo fields ->
            match me.MethodName.[.. 2] with
            | "get" ->
                let fn = me.MethodName.[4 ..]
                let resOpt =
                    fields |> List.tryPick (fun f ->
                        if f.Name = fn then
                            if f.Optional then 
                                JSRuntime.GetOptional(ItemGet(Hole 0, Value (String f.JSName)))
                            else
                                ItemGet(Hole 0, Value (String f.JSName))
                            |> Some
                        else None
                    )
                match resOpt with
                | Some res -> res
                | _ -> this.Error(sprintf "Could not find property of F# record type: %s.%s" typ.Entity.Value.FullName fn)
            | "set" -> 
                let fn = me.MethodName.[4 ..]
                let resOpt =
                    fields |> List.tryPick (fun f ->
                        if f.Name = fn then
                            if f.Optional then
                                JSRuntime.SetOptional (Hole 0) (Value (String f.JSName)) (Hole 1)
                            else
                                ItemSet(Hole 0, Value (String f.JSName), Hole 1)                                
                            |> Some
                        else None
                    )
                match resOpt with
                | Some res -> res
                | _ -> this.Error(sprintf "Could not find property of F# record type: %s.%s" typ.Entity.Value.FullName fn)
            | _ -> 
                match me.MethodName with
                | "ToString" -> Value (String typ.Entity.Value.FullName)
                | _ -> this.Error("Unrecognized member of F# record type")         
        | M.FSharpUnionInfo u ->
            let mN = me.MethodName
            if mN.StartsWith "get_Is" then
                let cN = mN.[6 ..]
                let i, c = u.Cases |> Seq.indexed |> Seq.find (fun (i, c) -> c.Name = cN)
                match c.Kind with 
                | M.ConstantFSharpUnionCase v -> Hole 0 ^== Value v
                | _ ->                 
                    if u.HasNull then 
                        let v = Id.New(mut = false)
                        Let (v, Hole 0, (Var v ^!= Value Null) ^&& (Var v).[Value (String "$")] ^== Value (Int i)) 
                    else
                        if u.Cases.Length = 2 then Hole 0 ^!= Value Null
                        else (Hole 0).[Value (String "$")] ^== Value (Int i)
            elif mN = "get_Tag" then
                if u.Cases |> List.forall (function { Kind = M.NormalFSharpUnionCase _ } -> true | _ -> false) then
                    (Hole 0).[Value (String "$")]
                else
                    let v = Id.New(mut = false)
                    let afterNullCheck = 
                        if u.Cases |> List.forall (function { Kind = M.NormalFSharpUnionCase _ | M.ConstantFSharpUnionCase Null } -> true | _ -> false) then
                            (Var v).[Value (String "$")]
                        else    
                            u.Cases |> List.indexed 
                            |> List.choose (function (i, { Kind = M.ConstantFSharpUnionCase v }) -> (if v <> Null then Some (i, v) else None) | _ -> None)  
                            |> List.fold (fun rest (i, c) -> Conditional (Var v ^== Value c, Value (Int i), rest)) ((Var v).[Value (String "$")])  
                    if u.HasNull then 
                        let ui = u.Cases |> List.findIndex (function { Kind = M.ConstantFSharpUnionCase Null } -> true | _ -> false)
                        Let (v, Hole 0, Conditional((Var v ^!= Value Null), Value (Int ui), afterNullCheck))
                    else
                        Let (v, Hole 0, afterNullCheck)
            elif mN.StartsWith "New" then 
                let cN = mN.[3 ..]
                let i, c = u.Cases |> Seq.indexed |> Seq.find (fun (_, c) -> c.Name = cN)

                match c.Kind with
                | M.ConstantFSharpUnionCase v -> Value v
                | M.NormalFSharpUnionCase fields -> 
                    let args = fields |> List.mapi (fun i _ -> Hole i)
                    let objExpr =
                        Object (
                            ("$", Value (Int i)) ::
                            (args |> List.mapi (fun j e -> "$" + string j, this.TransformExpression e)) 
                        )
                    this.TransformCopyCtor(typ.Entity, objExpr)
            elif mN.StartsWith "get_" then
                let cN = mN.[4 ..]
                let i, c = u.Cases |> Seq.indexed |> Seq.find (fun (_, c) -> c.Name = cN)

                match c.Kind with
                | M.ConstantFSharpUnionCase v -> Value v
                | M.NormalFSharpUnionCase [] -> 
                    this.TransformCopyCtor(typ.Entity, Object [ "$", Value (Int i) ])
                | _ -> failwith "A union case with a property getter should not have fields"
            else
                match mN with
                | "ToString" -> Value (String typ.Entity.Value.FullName)
                | _ -> this.Error("Unrecognized F# compiler generated method for union: " + mN)                 
        | M.FSharpUnionCaseInfo c ->
            let mN = me.MethodName
            if mN.StartsWith "get_" then
                let fN = mN.[4 ..]
                match c.Kind with
                | M.ConstantFSharpUnionCase _ ->
                    this.Error("Getting item of Constant union case: " + me.MethodName) 
                | M.NormalFSharpUnionCase fields -> 
                    match fields |> List.tryFindIndex (fun f -> f.Name = fN) with
                    | Some i ->
                        ItemGet(Hole 0, Value (String ("$" + string i)))
                    | _ ->
                        this.Error("Could not find item of union case: " + fN)        
            else 
                this.Error("Unrecognized F# compiler generated method for union case: " + me.MethodName)    
        | _ -> this.Error("Unrecognized F# compiler generated method: " + me.MethodName)
     
    member this.CompileMethod(info, expr, typ, meth) =
        currentNode <- M.MethodNode(typ, meth) 
#if DEBUG
        if meth.Value.MethodName.StartsWith "DebugCompiler" then
            printfn "Logging transformations: %s" meth.Value.MethodName
            logTransformations <- true
#endif      
        if inProgress |> List.contains currentNode then
            let msg = sprintf "Inline loop found at method %s.%s" typ.Value.FullName meth.Value.MethodName
            comp.AddError(None, SourceError msg)
            comp.FailedCompiledMethod(typ, meth)
        else
        // for C# static auto-properties
        selfAddress <- 
            comp.TryLookupClassInfo(typ) |> Option.bind (fun cls ->
                cls.StaticConstructor |> Option.map (fun (a, _) -> Address (List.tail a.Value))    
            )
        currentIsInline <- isInline info
        match info with
        | NotCompiled (i, notVirtual, funcArgs) ->
            currentFuncArgs <- funcArgs
            let res = this.TransformExpression expr |> removeSourcePosFromInlines |> breakExpr
            let res = this.CheckResult(res)
            let opts =
                {
                    IsPure = notVirtual && isPureFunction res
                    FuncArgs = funcArgs
                } : M.Optimizations
            comp.AddCompiledMethod(typ, meth, modifyDelayedInlineInfo i, opts, res)
        | NotGenerated (g, p, i, notVirtual) ->
            let m = GeneratedMethod(typ, meth)
            let res = this.Generate (g, p, m)
            let res = this.CheckResult(res)
            let opts =
                {
                    IsPure = notVirtual && isPureFunction res
                    FuncArgs = None
                } : M.Optimizations
            comp.AddCompiledMethod(typ, meth, modifyDelayedInlineInfo i, opts, res)
#if DEBUG
        logTransformations <- false
#endif

    member this.CompileImplementation(info, expr, typ, intf, meth) =
        currentNode <- M.ImplementationNode(typ, intf, meth)
        currentIsInline <- isInline info // TODO: implementations should not be inlined
        match info with
        | NotCompiled (i, _, _) -> 
            let res = this.TransformExpression expr |> breakExpr
            let res = this.CheckResult(res)
            comp.AddCompiledImplementation(typ, intf, meth, i, res)
        | NotGenerated (g, p, i, _) ->
            let m = GeneratedImplementation(typ, intf, meth)
            let res = this.Generate (g, p, m)
            let res = this.CheckResult(res)
            comp.AddCompiledImplementation(typ, intf, meth, i, res)

    member this.CompileConstructor(info, expr, typ, ctor) =
        currentNode <- M.ConstructorNode(typ, ctor)
        if inProgress |> List.contains currentNode then
            let msg = sprintf "inline loop found at constructor of %s" typ.Value.FullName
            comp.AddError(None, SourceError msg)
            comp.FailedCompiledConstructor(typ, ctor)
        else
        currentIsInline <- isInline info
        match info with
        | NotCompiled (i, _, funcArgs) -> 
            currentFuncArgs <- funcArgs
            let res = this.TransformExpression expr |> removeSourcePosFromInlines |> breakExpr
            let res = this.CheckResult(res)
            let opts =
                {
                    IsPure = isPureFunction res
                    FuncArgs = funcArgs
                } : M.Optimizations
            comp.AddCompiledConstructor(typ, ctor, modifyDelayedInlineInfo i, opts, res)
        | NotGenerated (g, p, i, _) ->
            let m = GeneratedConstructor(typ, ctor)
            let res = this.Generate (g, p, m)
            let res = this.CheckResult(res)
            let opts =
                {
                    IsPure = isPureFunction res
                    FuncArgs = None
                } : M.Optimizations
            comp.AddCompiledConstructor(typ, ctor, modifyDelayedInlineInfo i, opts, res)

    member this.CompileStaticConstructor(addr, expr, typ) =
        currentNode <- M.TypeNode typ
        selfAddress <- 
            let cls = comp.TryLookupClassInfo(typ).Value
            let addr = fst cls.StaticConstructor.Value 
            Some (Address (List.tail addr.Value))
        let res = this.TransformExpression expr |> breakExpr
        let res = this.CheckResult(res)
        comp.AddCompiledStaticConstructor(typ, addr, res)

    static member CompileFull(comp: Compilation) =
        for t, c, i, e in comp.GetCompilingConstructors() do
            let toJS = DotNetToJavaScript(comp)
            toJS.CompileConstructor(i, e, t, c)

        for t, a, e in comp.GetCompilingStaticConstructors() do
            let toJS = DotNetToJavaScript(comp)
            toJS.CompileStaticConstructor(a, e, t)

        for t, it, m, i, e in comp.GetCompilingImplementations() do
            let toJS = DotNetToJavaScript(comp)
            toJS.CompileImplementation(i, e, t, it, m)

        match comp.EntryPoint with
        | Some ep ->
            let toJS = DotNetToJavaScript(comp)
            comp.EntryPoint <- Some (toJS.TransformStatement(ep))
        | _ -> ()

        let compileMethods() =
            while comp.CompilingMethods.Count > 0 do
                let toJS = DotNetToJavaScript(comp)
                let (KeyValue((t, m), (i, e))) =  Seq.head comp.CompilingMethods
                toJS.CompileMethod(i, e, t, m)

        compileMethods()
        comp.CloseMacros()
        compileMethods()

    static member CompileExpression (comp, expr) =
        DotNetToJavaScript(comp).TransformExpression(expr)

    member this.AnotherNode() = DotNetToJavaScript(comp, currentNode :: inProgress)    

    member this.AddDependency(dep: M.Node) =
        comp.Graph.AddEdge(currentNode, dep)

    member this.AddTypeDependency(typ) =
        let typ = comp.FindProxied typ
        if comp.HasType typ then
            comp.Graph.AddEdge(currentNode, M.TypeNode typ)

    member this.AddConstructorDependency(typ, ctor) =
        let typ = comp.FindProxied typ
        if comp.ConstructorExistsInMetadata (typ, ctor) then
            comp.Graph.AddEdge(currentNode, M.ConstructorNode (typ, ctor))
        else
            this.AddTypeDependency(typ)

    member this.AddMethodDependency(typ, meth) =
        let typ = comp.FindProxied typ
        if comp.MethodExistsInMetadata (typ, meth) then
            if comp.IsInterface typ then
                comp.Graph.AddEdge(currentNode, M.AbstractMethodNode (typ, meth))
            else
                comp.Graph.AddEdge(currentNode, M.MethodNode (typ, meth))
        else
            if comp.HasType typ then
                comp.Graph.AddEdge(currentNode, M.TypeNode typ)

    member this.Error(err) =
        comp.AddError(this.CurrentSourcePos, err)
        errorPlaceholder

    member this.OptimizeArg (opt, expr) =
        match opt with
        | NotOptimizedFuncArg -> expr
        | CurriedFuncArg currying ->
            match IgnoreExprSourcePos expr with 
            | OptimizedFSharpArg(f, CurriedFuncArg arity) when arity = currying ->
                f
            | _ ->
                let cargs = List.init currying (fun _ -> Id.New(mut = false))
                Lambda(cargs, CurriedApplication(expr, cargs |> List.map Var))  
        | TupledFuncArg tupling -> 
            match expr with
            | TupledLambda (args, body, _) ->
                Lambda(List.ofSeq args, body)
            | _ ->
                match IgnoreExprSourcePos expr with
                | OptimizedFSharpArg(f, TupledFuncArg arity) when arity = tupling -> 
                    f
                | _ ->
                    let args = List.init tupling (fun _ -> Id.New(mut = false))
                    Lambda(args, Application(expr, [NewArray(args |> List.map Var)], false, Some 1))

    override this.TransformOptimizedFSharpArg(f, opt) =
        match opt with
        | CurriedFuncArg arity ->
            let rec c args a =
                if a = 0 then
                    Application (f, List.rev args, false, Some arity)
                else
                    let x = Id.New(mut = false)
                    Lambda ([x], c (Var x :: args) (a - 1))
            c [] arity
//            match arity with
//            | 2 -> JSRuntime.Curried2 f
//            | 3 -> JSRuntime.Curried3 f
//            | n -> JSRuntime.Curried f n
        | TupledFuncArg arity ->
            let x = Id.New(mut = false)
            let args =
                List.init arity (fun i -> (Var x).[Value (Int i)])
            Lambda ([x], Application (f, args, false, Some arity))
        | _ ->
            this.TransformExpression(f)

    member this.TransformArgument(expr) =
        match IgnoreExprSourcePos expr with
        | OptimizedFSharpArg(_, (CurriedFuncArg _ | TupledFuncArg _)) -> expr
        | _ -> this.TransformExpression expr

//    override this.TransformCurriedFuncVar(v, currying) =
//        let args = ResizeArray()
//        let rec c currying =
//            match currying with
//            | h :: t ->
//                match h with
//                | 0 ->
//                    Lambda ([], c t)
//                | 1 ->
//                    let v = Id.New(mut = false)
//                    args.Add (Var v)
//                    Lambda ([v], c t)
//                | _ ->
//                    let v = Id.New(mut = false)
//                    for i = 0 to h - 1 do
//                        args.Add ((Var v).[Value (Int i)])
//                    Lambda ([v], c t)
//            | [] -> Application (Var v, List.ofSeq args, false, Some args.Count)
//        let res = c currying
//        printfn "curried %A: %s" currying (Debug.PrintExpression res)
//        res 

    member this.CompileCall (info, opts: M.Optimizations, expr, thisObj, typ, meth, args, ?baseCall) =
        match thisObj with
        | Some (IgnoreSourcePos.Base as tv) ->
            this.CompileCall (info, opts, expr, Some (This |> WithSourcePosOfExpr tv), typ, meth, args, true)
        | _ ->
        if comp.HasGraph then
            this.AddMethodDependency(typ.Entity, meth.Entity)
        let trThisObj = thisObj |> Option.map this.TransformExpression
        let trArgs() = 
            let ta = args
            match opts.FuncArgs with
            | Some ca ->
                (ca, ta) ||> Seq.map2 (fun ao expr ->
                    this.OptimizeArg(ao, expr) |> this.TransformArgument
                )
                |> List.ofSeq   
            | _ -> ta |> List.map this.TransformArgument
                        
        match info with
        | M.Instance name ->
            match baseCall with
            | Some true ->
                let ba = comp.TryLookupClassInfo(typ.Entity).Value.Address.Value
                Application(
                    GlobalAccess ba |> getItem "prototype" |> getItem name |> getItem "call",
                    This :: (trArgs()), opts.IsPure, None)
            | _ ->
                Application(
                    this.TransformExpression thisObj.Value |> getItem name,
                    trArgs(), opts.IsPure, None) 
        | M.Static address ->
            Application(GlobalAccess address, trArgs(), opts.IsPure, Some meth.Entity.Value.Parameters.Length)
        | M.Inline ->
            let res = Substitution(trArgs(), ?thisObj = trThisObj).TransformExpression(expr)
//            if opts.FuncArgs.IsSome then
//                printfn "func arg inline: %s" (Debug.PrintExpression expr)
//                printfn "func arg inline result: %s" (Debug.PrintExpression res)
            res
        | M.NotCompiledInline ->
            let ge =
                if not (List.isEmpty typ.Generics && List.isEmpty meth.Generics) then
                    try GenericInlineResolver(typ.Generics @ meth.Generics).TransformExpression expr
                    with e -> this.Error (sprintf "Failed to resolve generics: %s" e.Message)
                else expr
            Substitution(trArgs(), ?thisObj = trThisObj).TransformExpression(ge)
            |> this.TransformExpression
        | M.Macro (macro, parameter, fallback) ->
            let macroResult = 
                match comp.GetMacroInstance(macro) with
                | Some m ->
                    try 
                        m.TranslateCall {
                                This = thisObj
                                DefiningType = typ
                                Method = meth
                                Arguments = args
                                Parameter = parameter |> Option.map M.ParameterObject.ToObj
                                IsInline = currentIsInline
                                Compilation = comp
                        }
                    with e -> MacroError e.Message 
                | _ -> 
                    if comp.UseLocalMacros then
                        MacroError "Macro type failed to load"
                    else
                        MacroWarning(
                            "Cannot run macro in code service, consider moving it to another assembly.",
                            MacroOk Undefined
                        )
            let rec getExpr mres =
                match mres with
                | MacroOk resExpr -> this.TransformExpression resExpr
                | MacroWarning (msg, mres) ->
                    this.Warning (sprintf "Macro warning in %s.TranslateCall: %s" macro.Value.FullName msg)
                    getExpr mres
                | MacroError msg ->
                    this.Error(sprintf "Macro error in %s.TranslateCall: %s" macro.Value.FullName msg)
                | MacroDependencies (nodes, mres) ->
                    if comp.HasGraph then
                        nodes |> List.iter this.AddDependency
                    getExpr mres
                | MacroFallback ->
                    match fallback with
                    | None -> this.Error(sprintf "No macro fallback found for '%s'" macro.Value.FullName)
                    | Some f -> this.CompileCall (f, opts, expr, thisObj, typ, meth, args)      
                | MacroNeedsResolvedTypeArg -> 
                    if currentIsInline then
                        hasDelayedTransform <- true
                        let typ = Generic (comp.FindProxied typ.Entity) typ.Generics
                        Call(trThisObj, typ, meth, args |> List.map this.TransformExpression)
                    else 
                        this.Error(sprintf "Macro '%s' requires a resolved type argument." macro.Value.FullName)
            getExpr macroResult
        | M.Remote (kind, handle, rh) ->
            let name, mnode =
                match kind with
                | M.RemoteAsync -> "Async", asyncRpcMethodNode
                | M.RemoteTask -> "Task", taskRpcMethodNode
                | M.RemoteSend -> "Send", sendRpcMethodNode
                | M.RemoteSync -> "Sync", syncRpcMethodNode
            let remotingProvider =
                let td =
                    match rh with
                    | Some (rp, _) -> rp
                    | _ -> defaultRemotingProvider   
                this.TransformCtor(NonGeneric td, emptyConstructor, []) 
            if comp.HasGraph then
                this.AddDependency(mnode)
                let rec addTypeDeps (t: Type) =
                    match t with
                    | ConcreteType c ->
                        this.AddDependency(M.TypeNode c.Entity)
                        c.Generics |> List.iter addTypeDeps
                    | ArrayType(t, _) -> addTypeDeps t
                    | TupleType ts -> ts |> List.iter addTypeDeps
                    | _ -> ()
                addTypeDeps meth.Entity.Value.ReturnType
            Application (remotingProvider |> getItem name, [ Value (String (handle.Pack())); NewArray (trArgs()) ], opts.IsPure, Some 2)
        | M.Constructor _ -> failwith "Not a valid method info: Constructor"

    override this.TransformCall (thisObj, typ, meth, args) =
        if typ.Entity = Definitions.Dynamic then
            match meth.Entity.Value.MethodName with
            | BinaryOpName op ->
                if args.Length <> 2 then
                    this.Error("Dynamic binary operator expecting 2 arguments")
                else
                    Binary (this.TransformExpression args.[0], op, this.TransformExpression args.[1])
            | UnaryOpName op ->
                if args.Length <> 2 then
                    this.Error("Dynamic binary operator expecting 1 argument")
                else
                    Unary (op, this.TransformExpression args.[0])
            | "op_Decrement" ->
                if args.Length <> 1 then
                    this.Error("Dynamic decrement operator expecting 1 argument")
                else
                    Binary (this.TransformExpression args.[0], BinaryOperator.``-``, Value (Int 1))
            | "op_Increment" ->
                if args.Length <> 1 then
                    this.Error("Dynamic increment operator expecting 1 argument")
                else
                    Binary (this.TransformExpression args.[0], BinaryOperator.``+``, Value (Int 1))
            | n ->
                match thisObj with
                | Some o ->
                    Application(ItemGet(this.TransformExpression o, Value (String n)), args |> List.map this.TransformExpression, false, None) 
                | _ ->
                    this.Error("Static method on dynamic object not tranlated: " + n)
        else
        match comp.LookupMethodInfo(typ.Entity, meth.Entity) with
        | Compiled (info, opts, expr) ->
            this.CompileCall(info, opts, expr, thisObj, typ, meth, args)
        | Compiling (info, expr) ->
            if isInline info then
                this.AnotherNode().CompileMethod(info, expr, typ.Entity, meth.Entity)
                this.TransformCall (thisObj, typ, meth, args)
            else
                match info with
                | NotCompiled (info, _, funcArgs) ->
                    let opts =
                        {
                            IsPure = false
                            FuncArgs = funcArgs
                        } : M.Optimizations
                    this.CompileCall(info, opts, expr, thisObj, typ, meth, args)
                | NotGenerated (_, _, info, _) ->
                    this.CompileCall(info, M.Optimizations.None, expr, thisObj, typ, meth, args)
        | CustomTypeMember ct ->  
            try
                let inl = this.GetCustomTypeMethodInline(typ, ct, meth)
                Substitution(args |> List.map this.TransformExpression, ?thisObj = (thisObj |> Option.map this.TransformExpression)).TransformExpression(inl)
            with _ ->
                this.Error("Failed to translate compiler generated method: " + meth.Entity.Value.MethodName)
        | LookupMemberError err ->
            comp.AddError (this.CurrentSourcePos, err)
            match thisObj with 
            | Some thisObj ->
                Application(ItemGet(this.TransformExpression thisObj, errorPlaceholder), args |> List.map this.TransformExpression, false, None) 
            | _ ->
                Application(errorPlaceholder, args |> List.map this.TransformExpression, false, None)

    override this.TransformTraitCall(thisObj, typ, meth, args) =
        match typ with
        | ConcreteType ct ->
            let tr (methods: seq<Method>) =
                let mi = meth.Entity.Value
                let mName = mi.MethodName
                let ms =                    
                    methods |> Seq.choose (fun m ->
                        // TODO: check compatility with signature better
                        if m.Value.MethodName = mName then Some m else None
                    ) 
                    |> List.ofSeq
                match ms with
                | [ m ] ->
                    this.TransformCall(Some thisObj, ct, Generic m meth.Generics, args)
                | [] -> this.Error(sprintf "Could not find method for trait call: %s" mName)
                | _ -> this.Error(sprintf "Ambiguity at translating trait call: %s" mName)
            match comp.TryLookupClassInfo ct.Entity with
            | None -> 
                match comp.TryLookupInterfaceInfo ct.Entity with
                | Some intf -> tr intf.Methods.Keys
                | None -> this.Error (TypeNotFound ct.Entity)
            | Some cls -> tr cls.Methods.Keys
        | _ ->
            if currentIsInline then
                hasDelayedTransform <- true
                TraitCall(this.TransformExpression thisObj, typ, meth, args |> List.map this.TransformExpression)
            else 
                this.Error("Using a trait call requires the Inline attribute.")
    
    override this.TransformNewDelegate(thisObj, typ, meth) =
        // TODO: CustomTypeMember
        if comp.HasGraph then
            this.AddMethodDependency(typ.Entity, meth.Entity)
        let inlined() =
            let args = meth.Entity.Value.Parameters |> List.map (fun _ -> Id.New(mut = false))
            let call = 
                Lambda(args, Call(thisObj, typ, meth, args |> List.map Var))
                |> this.TransformExpression
            this.Warning("Creating delegate from inlined call, equality may not work.")
            call        
        match comp.LookupMethodInfo(typ.Entity, meth.Entity) with
        | Compiled (info, _, _)
        | Compiling ((NotCompiled (info, _, _) | NotGenerated (_, _, info, _)), _) ->
            match info with 
            | M.Static address -> 
                GlobalAccess address
            | M.Instance name -> 
                match comp.TryLookupClassInfo typ.Entity with
                | Some { Address = Some addr } ->
                    let func = GlobalAccess addr |> getItem "prototype" |> getItem name
                    JSRuntime.BindDelegate func (this.TransformExpression thisObj.Value) 
                | _ -> this.Error ("Cannot look up prototype for delegate creating")
            | M.NotCompiledInline
            | M.Inline _ 
            | M.Macro _ 
            | M.Remote _ -> inlined()
            | M.Constructor _ -> failwith "impossible"
        | CustomTypeMember _ -> inlined()
        | LookupMemberError err -> this.Error err

    member this.CompileCtor(info, opts: M.Optimizations, expr, typ, ctor, args) =
        if comp.HasGraph then
            this.AddConstructorDependency(typ.Entity, ctor)
        let trArgs() = 
            match opts.FuncArgs with
            | Some ca ->
                (ca, args) ||> Seq.map2 (fun ao expr ->
                    this.OptimizeArg(ao, expr) |> this.TransformArgument
                )
                |> List.ofSeq   
            | _ -> args |> List.map this.TransformArgument
        match info with
        | M.Constructor address ->
            New(GlobalAccess address, trArgs())
        | M.Static address ->
            Application(GlobalAccess address, trArgs(), opts.IsPure, Some ctor.Value.CtorParameters.Length)
        | M.Inline -> 
            Substitution(trArgs()).TransformExpression(expr)
        | M.NotCompiledInline -> 
            let ge =
                if not (List.isEmpty typ.Generics) then
                    try GenericInlineResolver(typ.Generics).TransformExpression expr
                    with e -> this.Error(sprintf "Failed to resolve generics: %s" e.Message)
                else expr
            Substitution(trArgs()).TransformExpression(ge)
            |> this.TransformExpression
        | M.Macro (macro, parameter, fallback) ->
            let macroResult = 
                match comp.GetMacroInstance(macro) with
                | Some m ->
                    try
                        m.TranslateCtor {
                             DefiningType = typ
                             Constructor = ctor
                             Arguments = args
                             Parameter = parameter |> Option.map M.ParameterObject.ToObj
                             IsInline = currentIsInline
                             Compilation = comp
                        }
                    with e -> MacroError e.Message 
                | _ -> MacroError "Macro type failed to load"
            let rec getExpr mres =
                match mres with
                | MacroOk resExpr -> this.TransformExpression resExpr
                | MacroWarning (msg, mres) ->
                    this.Warning (sprintf "Macro warning in %s.TranslateCall: %s" macro.Value.FullName msg)
                    getExpr mres
                | MacroError msg ->
                    this.Error(sprintf "Macro error in %s.TranslateCall: %s" macro.Value.FullName msg)
                | MacroDependencies (nodes, mres) ->
                    if comp.HasGraph then
                        nodes |> List.iter this.AddDependency
                    getExpr mres
                | MacroFallback ->
                    match fallback with
                    | None -> this.Error(sprintf "No macro fallback found for '%s'" macro.Value.FullName)
                    | Some f -> this.CompileCtor (f, opts, expr, typ, ctor, args)      
                | MacroNeedsResolvedTypeArg -> 
                    if currentIsInline then
                        hasDelayedTransform <- true
                        let typ = Generic (comp.FindProxied typ.Entity) typ.Generics
                        Ctor(typ, ctor, trArgs())
                    else 
                        this.Error(sprintf "Macro '%s' requires a resolved type argument." macro.Value.FullName)
            getExpr macroResult
        | _ -> this.Error("Invalid metadata for constructor.")

    override this.TransformCopyCtor(typ, objExpr) =
        match comp.TryLookupClassInfo typ |> Option.bind (fun c -> if c.HasWSPrototype then c.Address else None) with
        | Some a ->
            if comp.HasGraph then
                this.AddTypeDependency typ
            New (GlobalAccess a, [ this.TransformExpression objExpr ])
        | _ -> this.TransformExpression objExpr

    override this.TransformNewRecord(typ, fields) =
        match comp.TryGetRecordConstructor typ.Entity with
        | Some rctor ->
            if comp.HasGraph then
                this.AddDependency(M.ConstructorNode (comp.FindProxied typ.Entity, rctor))
            this.TransformCtor(typ, rctor, fields)
        | _ ->
            try 
                let inl = this.GetCustomTypeConstructorInline(comp.GetCustomType typ.Entity, emptyConstructor)
                Substitution(fields |> List.map this.TransformExpression).TransformExpression(inl)
            with _ -> this.Error("Failed to translate F# record creation.")

    override this.TransformNewUnionCase(typ, case, args) = 
        let t = typ.Entity
        match comp.GetCustomType typ.Entity with
        | M.FSharpUnionInfo u ->
            let i, c = u.Cases |> Seq.indexed |> Seq.find (fun (i, c) -> c.Name = case)
            match c.Kind with
            | M.ConstantFSharpUnionCase v ->
                Value v
            | _ ->
                let objExpr =
                    Object (
                        ("$", Value (Int i)) ::
                        (args |> List.mapi (fun j e -> "$" + string j, e)) 
                    )
                this.TransformCopyCtor(typ.Entity, objExpr)
        | _ -> this.Error("Failed to translate union case creation.")

    override this.TransformUnionCaseTest(expr, typ, case) = 
        match comp.GetCustomType typ.Entity with
        | M.FSharpUnionInfo u ->
            let i, c = u.Cases |> Seq.indexed |> Seq.find (fun (i, c) -> c.Name = case)
            match c.Kind with
            | M.ConstantFSharpUnionCase v ->
                this.TransformExpression expr ^== Value v
            | _ -> 
                if u.HasNull then
                    let v = Id.New(mut = false)
                    Let (v, this.TransformExpression expr, 
                        (Var v ^!= Value Null) ^&& (ItemGet(Var v, Value (String "$")) ^== Value (Int i)) 
                    )
                else
                    ItemGet(this.TransformExpression expr, Value (String "$")) ^== Value (Int i)    
        | _ -> this.Error("Failed to translate union case test.")

    override this.TransformUnionCaseTag(expr, typ) = 
        match comp.GetCustomType typ.Entity with
        | M.FSharpUnionInfo u ->
            let constantCases = 
                u.Cases |> List.indexed |> List.filter (function (_, { Kind = M.ConstantFSharpUnionCase _ }) -> true | _ -> false)
            if List.isEmpty constantCases then                 
                ItemGet(this.TransformExpression expr, Value (String "$"))
            else 
                // TODO: no default tag when all cases are constant valued
                let ev = Id.New (mut = false)
                let b = 
                    (constantCases, ItemGet(Var ev, Value (String "$")))
                    ||> List.foldBack (fun (i, c) e -> 
                        match c.Kind with
                        | M.ConstantFSharpUnionCase v ->
                            Conditional(Var ev ^== Value v, Value (Int i), e)
                        | _ -> failwith "impossible"
                    )   
                Let (ev, this.TransformExpression expr, b)
        | _ -> this.Error("Failed to translate union case tag.")

    override this.TransformCtor(typ, ctor, args) =
        let node = comp.LookupConstructorInfo(typ.Entity, ctor)
        match node with
        | Compiled (info, opts, expr) -> 
            this.CompileCtor(info, opts, expr, typ, ctor, args)
        | Compiling (info, expr) ->
            if isInline info then
                this.AnotherNode().CompileConstructor(info, expr, typ.Entity, ctor)
                this.TransformCtor(typ, ctor, args)
            else 
                match info with
                | NotCompiled (info, _, funcArgs) -> 
                    let opts =
                        {
                            IsPure = false
                            FuncArgs = funcArgs
                        } : M.Optimizations
                    this.CompileCtor(info, opts, expr, typ, ctor, args)
                | NotGenerated (_, _, info, _) ->
                    this.CompileCtor(info, M.Optimizations.None, expr, typ, ctor, args)
        | CustomTypeMember ct ->  
            try
                let inl = this.GetCustomTypeConstructorInline(ct, ctor)
                Substitution(args |> List.map this.TransformExpression).TransformExpression(inl)
            with _ ->
                this.Error("Failed to translate compiler generated constructor")
        | LookupMemberError err ->
            comp.AddError (this.CurrentSourcePos, err)
            Application(errorPlaceholder, args |> List.map this.TransformExpression, false, None)
                  
    override this.TransformBaseCtor(expr, typ, ctor, args) =
        let norm = this.TransformCtor(typ, ctor, args)
        match norm with
        | New (func, a) ->
            Application(func |> getItem "call", expr :: a, false, None)
        // This is allowing simple inlines
        | Let (i1, a1, New(func, [Var v1])) when i1 = v1 ->
            Application(func |> getItem "call", expr :: [a1], false, None)
        | _ ->
            comp.AddError (this.CurrentSourcePos, SourceError "base class constructor is not regular")
            Application(errorPlaceholder, args |> List.map this.TransformExpression, false, None)

    override this.TransformCctor(typ) =
        if comp.HasGraph then
            this.AddTypeDependency typ
        Application(GlobalAccess (comp.LookupStaticConstructorAddress typ), [], false, Some 0)

    override this.TransformOverrideName(typ, meth) =
        match comp.LookupMethodInfo(typ, meth) with
        | Compiled (M.Instance name, _, _) 
        | Compiling ((NotCompiled ((M.Instance name), _, _) | NotGenerated (_,_,M.Instance name, _)), _) ->
            Value (String name)
        | LookupMemberError err ->
            this.Error err
        | _ -> 
            this.Error ("Could not get name of abstract method")

    override this.TransformSelf () = 
        match selfAddress with
        | Some self -> GlobalAccess self
        | _ -> this.Error ("Self address missing")

    override this.TransformFieldGet (expr, typ, field) =
        if comp.HasGraph then
            this.AddTypeDependency typ.Entity
        match comp.LookupFieldInfo (typ.Entity, field) with
        | CompiledField f ->
            match f with
            | M.InstanceField fname ->
                this.TransformExpression expr.Value |> getItem fname
            | M.StaticField faddr ->
                match comp.TryLookupStaticConstructorAddress typ.Entity with
                | Some cctorAddr ->
                    Sequential [
                        Application(GlobalAccess cctorAddr, [], false, Some 0)
                        GlobalAccess faddr
                    ]
                | _ ->    
                    GlobalAccess faddr   
            | M.OptionalField fname -> 
                JSRuntime.GetOptional (this.TransformExpression expr.Value |> getItem fname)
            | M.IndexedField i ->
                this.TransformExpression expr.Value |> getIndex i
        | CustomTypeField ct ->
            match ct with
            | M.FSharpUnionCaseInfo case ->
                match case.Kind with
                | M.NormalFSharpUnionCase fields ->
                    let fName = "$" + string (fields |> List.findIndex (fun f -> f.Name = field))
                    ItemGet(this.TransformExpression expr.Value, Value (String fName))
                | _ -> failwith "Constant union case should not have fields" 
            | M.FSharpRecordInfo fields ->
                match fields |> List.tryPick (fun f -> if f.Name = field then Some (f.JSName, f.Optional) else None) with
                | Some (name, isOpt) ->
                    if isOpt then
                        JSRuntime.GetOptional (this.TransformExpression expr.Value |> getItem name)
                    else
                        this.TransformExpression expr.Value |> getItem name 
                | _ -> this.Error(sprintf "Could not find field of F# record type: %s.%s" typ.Entity.Value.FullName field)
            | M.FSharpUnionInfo _ -> failwith "Union base type should not have fields"   
            | _ -> failwith "CustomTypeField error"          
        | PropertyField (getter, _) ->
            match getter with
            | Some m -> 
                this.TransformCall (expr, typ, NonGeneric m, [])   
            | _ -> this.Error(sprintf "Could not getter of F# field: %s.%s" typ.Entity.Value.FullName field)
        | LookupFieldError err ->
            this.Error err

    override this.TransformFieldSet (expr, typ, field, value) =
        if comp.HasGraph then
            this.AddTypeDependency typ.Entity
        match comp.LookupFieldInfo (typ.Entity, field) with
        | CompiledField f ->
            match f with
            | M.InstanceField fname ->
                ItemSet(this.TransformExpression expr.Value, Value (String fname), this.TransformExpression value) 
            | M.StaticField faddr ->
                let f, a = List.head faddr.Value, List.tail faddr.Value
                match comp.TryLookupStaticConstructorAddress typ.Entity with
                | Some cctorAddr ->
                    Sequential [
                        Application(GlobalAccess cctorAddr, [], false, Some 0)
                        ItemSet(GlobalAccess (Hashed a), Value (String f), this.TransformExpression value)
                    ]
                | _ ->    
                    ItemSet(GlobalAccess (Hashed a), Value (String f), this.TransformExpression value)
            | M.OptionalField fname -> 
                JSRuntime.SetOptional (this.TransformExpression expr.Value) (Value (String fname)) (this.TransformExpression value)
            | M.IndexedField i ->
                ItemSet(this.TransformExpression expr.Value, Value (Int i), this.TransformExpression value) 
        | CustomTypeField ct ->
            match ct with
            | M.FSharpRecordInfo fields ->
                match fields |> List.tryPick (fun f -> if f.Name = field then Some (f.JSName, f.Optional) else None) with
                | Some (name, isOpt) ->
                    if isOpt then
                        JSRuntime.SetOptional (this.TransformExpression expr.Value) (Value (String name)) (this.TransformExpression value)
                    else
                        ItemSet(this.TransformExpression expr.Value, Value (String name), this.TransformExpression value)
                | _ -> this.Error(sprintf "Could not find field of F# record type: %s.%s" typ.Entity.Value.FullName field)
            | M.FSharpUnionCaseInfo _ -> failwith "Union case field should not be set" 
            | M.FSharpUnionInfo _ -> failwith "Union base type should not have fields"   
            | _ -> failwith "CustomTypeField error"          
        | PropertyField (_, setter) ->
            match setter with
            | Some m -> 
                this.TransformCall (expr, typ, NonGeneric m, [value])   
            | _ -> this.Error(sprintf "Could not setter of F# field: %s.%s" typ.Entity.Value.FullName field)
        | LookupFieldError err ->
            comp.AddError (this.CurrentSourcePos, err)
            ItemSet(errorPlaceholder, errorPlaceholder, this.TransformExpression value)

    override this.TransformTypeCheck(expr, typ) =
        match typ with
        | ConcreteType td ->
            if comp.HasGraph then
                this.AddTypeDependency td.Entity
        | _ -> ()
        let typeof x = 
            Binary (
                Unary(UnaryOperator.typeof, this.TransformExpression expr),
                BinaryOperator.``==``,
                Value (String x)
            )
        let instanceof x =
            Binary(this.TransformExpression expr, BinaryOperator.instanceof, Global [ x ])    
        match typ with
        | ConcreteType { Entity = t; Generics = gs } ->
            match t.Value.FullName with
            | "System.Void" ->                                                                
                typeof "undefined"
            | "Microsoft.FSharp.Core.Unit" ->
                Binary(this.TransformExpression expr, BinaryOperator.``===``, Value Null)    
            | "WebSharper.JavaScript.Object" ->
                typeof "object"
            | "WebSharper.JavaScript.Boolean"
            | "System.Boolean" ->
                typeof "boolean"
            | "WebSharper.JavaScript.Number"
            | "System.Byte"
            | "System.SByte"
            | "System.Char"
            | "System.Single"
            | "System.Double"
            | "System.Int16"
            | "System.Int32"
            | "System.Int64"
            | "System.UInt16"
            | "System.UInt32"
            | "System.UInt64" 
            | "System.Decimal" ->
                typeof "number"
            | "System.String" ->
                typeof "string"
            | "System.IDisposable" ->
                Binary(
                    Value (String "Dispose"),
                    BinaryOperator.``in``,
                    this.TransformExpression expr
                )
            | "WebSharper.JavaScript.Error"
            | "System.Exception" ->
                instanceof "Error"
            | "WebSharper.JavaScript.Array"
            | "System.Array" ->
                instanceof "Array"
            | "WebSharper.JavaScript.Function" ->
                typeof "function"
            | tname ->
                if not (List.isEmpty gs) then
                    this.Warning ("Generic type check is ignoring erased type parameter.")
                match comp.TryLookupClassInfo t with
                | Some c ->
                    match c.Address with
                    | Some a ->
                        Binary(this.TransformExpression expr, BinaryOperator.instanceof, GlobalAccess a)
                    | _ ->
                        this.Error("Type test cannot be translated because client-side class does not have a prototype: " + t.Value.FullName)
                | None -> 
                    match comp.GetCustomType t with
                    | M.FSharpUnionCaseInfo c ->
                        let tN = t.Value.FullName
                        let nestedIn = tN.[.. tN.LastIndexOf '+' - 1]
                        let uTyp = { Entity = TypeDefinition { t.Value with FullName = nestedIn } ; Generics = [] } 
                        let trE = this.TransformExpression expr
                        let i = Id.New (mut = false)
                        Let (i, trE, this.TransformTypeCheck(Var i, ConcreteType uTyp) ^&& this.TransformUnionCaseTest(Var i, uTyp, c.Name)) 
                    | M.DelegateInfo _ ->
                        this.Error("Type tests do not support delegate type, check against WebSharper.JavaScript.Function.")   
                    | _ -> 
                        this.Error(sprintf "Failed to compile a type check for type '%s'" tname)
        | TypeParameter _ | StaticTypeParameter _ -> 
            if currentIsInline then
                hasDelayedTransform <- true
                TypeCheck(this.TransformExpression expr, typ)
            else 
                this.Error("Using a type test on a type parameter requires the Inline attribute.")
        | ArrayType _ -> this.Error("Type tests do not support generic array type, check against System.Array.")
        | FSharpFuncType _ -> this.Error("Type tests do not support F# function type, check against WebSharper.JavaScript.Function.")   
        | _ ->  this.Error("Failed to compile a type check.")

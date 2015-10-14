module WebSharper.Compiler.ToJavaScript
 
open WebSharper.Core
open WebSharper.Core.AST

let fail _ = failwith "Transform error: .NET common to Core"

module M = WebSharper.Core.Metadata

#if DEBUG
type CheckNoInvalidJSForms(isInline) =
    inherit Transformer()

    let invalidForm() = failwith "invalid form"

    let allVars = System.Collections.Generic.HashSet()

    static let tr = CheckNoInvalidJSForms false 
    static let inl = CheckNoInvalidJSForms true 

    override this.TransformSelf ()                = invalidForm()
    override this.TransformHole i                = if not isInline then invalidForm() else base.TransformHole i
    override this.TransformFieldGet (_,_,_)            = invalidForm()
    override this.TransformFieldSet (_,_,_,_)            = invalidForm()
    override this.TransformLet (a,b,c)                 = if not isInline then invalidForm() else base.TransformLet (a,b,c)
    override this.TransformLetRec (_,_)             = invalidForm()
    override this.TransformStatementExpr _       = invalidForm()
    override this.TransformAwait _               = invalidForm()
    override this.TransformNamedParameter (_,_)      = invalidForm()
    override this.TransformRefOrOutParameter _   = invalidForm()
    override this.TransformCtor (_,_,_)                = invalidForm()
    override this.TransformCoalesce (_,_,_)           = invalidForm()
    override this.TransformTypeCheck (_,_)           = invalidForm()
    override this.TransformCall (_,_,_,_)                = invalidForm()

    override this.TransformWithVars (vars, expr) =
        if not isInline then invalidForm() else
        vars |> List.iter (allVars.Add >> ignore)
        base.TransformWithVars(vars, expr)

    override this.TransformNewVar (var, expr) =
        if not isInline then invalidForm() else
        allVars.Add var |> ignore    
        base.TransformNewVar(var, expr)

    override this.TransformVarDeclaration (var, value) =
        allVars.Add var |> ignore    
        base.TransformVarDeclaration (var, value)

    override this.TransformFunction (args, body) =
        args |> List.iter (allVars.Add >> ignore)
        base.TransformFunction (args, body)

    override this.TransformTryWith(body, var, catch) =
        var |> Option.iter (allVars.Add >> ignore)
        base.TransformTryWith(body, var, catch)

    override this.TransformId i =
        if not isInline then
            if allVars.Contains i then
                i
            else failwith "undefined variable found"
        else base.TransformId i

    static member Translated = tr
    static member Inline = inl 
#endif

type RemoveSourcePositions() =
    inherit Transformer()

    override this.TransformExprSourcePos(_, e) =
        this.TransformExpression e

    override this.TransformStatementSourcePos(_, s) =
        this.TransformStatement s

type Breaker() =
    inherit Transformer()
    
    override this.TransformStatement (a) =
        breakStatement (base.TransformStatement a)

let breaker = Breaker()
let breakExpr e = breaker.TransformExpression(e)

let defaultRemotingProvider = globalAccess ["WebSharper"; "Remoting"; "AjaxRemotingProvider"]

let removeSourcePos = RemoveSourcePositions()
let removeSourcePosFromInlines info expr =
    match info with
    | M.NotCompiled M.Inline 
    | M.NotGenerated (_, _, M.Inline) -> 
        removeSourcePos.TransformExpression expr
    | _ -> expr
    
let errorPlaceholder = Value (String "$$ERROR$$")

type ToJavaScript private (comp: M.Compilation, ?remotingProvider) =
    inherit Transformer()

    let remotingProvider = defaultArg remotingProvider defaultRemotingProvider

    let mutable selfAddress = None

//    let mutable innerVars = [] : list<Id>
    let mutable currentNode = M.AssemblyNode "" // placeholder
    let mutable currentSourcePos = None

    // TODO : cache instances

    let jsonLookup = M.Static (Address ["lookup"; "Json"; "WebSharper"])

    let isInline info =
        match info with
        | M.NotCompiled M.Inline 
        | M.NotGenerated (_, _, M.Inline) -> true
        | _ -> false

    member this.CheckResult (info, res) =
#if DEBUG
        if isInline info then
            CheckNoInvalidJSForms.Inline.TransformExpression res |> ignore
        else
            CheckNoInvalidJSForms.Translated.TransformExpression res |> ignore
#else
        ()
#endif
            
    member this.CompileMethod(info, expr, typ, meth) =
        currentNode <- M.MethodNode(typ, meth)
        let res = this.TransformExpression expr |> removeSourcePosFromInlines info |> breakExpr
        this.CheckResult(info, res)
        match info with
        | M.NotCompiled i -> 
            comp.AddCompiledMethod(typ, meth, i, res)
        | M.NotGenerated (g, p, i) ->
            failwith "TODO generated"

    member this.CompileImplementation(info, expr, typ, intf, meth) =
        currentNode <- M.ImplementationNode(typ, intf, meth)
        let res = this.TransformExpression expr |> breakExpr
        this.CheckResult(info, res)
        match info with
        | M.NotCompiled i -> 
            comp.AddCompiledImplementation(typ, intf, meth, i, res)
        | M.NotGenerated (g, p, i) ->
            failwith "TODO generated"

    member this.CompileConstructor(info, expr, typ, ctor) =
        currentNode <- M.ConstructorNode(typ, ctor)
        let res = this.TransformExpression expr |> removeSourcePosFromInlines info |> breakExpr
        this.CheckResult(info, res)
        match info with
        | M.NotCompiled i -> 
            comp.AddCompiledConstructor(typ, ctor, i, res)
        | M.NotGenerated (g, p, i) ->
            failwith "TODO generated"

    member this.CompileStaticConstructor(addr, expr, typ) =
        currentNode <- M.TypeNode typ
        selfAddress <- comp.TryLookupClassInfo(typ).Value.Address
        let res = this.TransformExpression expr |> breakExpr
        comp.AddCompiledStaticConstructor(typ, addr, res)

    static member CompileFull(comp: M.Compilation) =
        while comp.CompilingMethods.Count > 0 do
            let toJS = ToJavaScript(comp)
            let (KeyValue((t, m), (i, e))) =  Seq.head comp.CompilingMethods
            toJS.CompileMethod(i, e, t, m)
        
        while comp.CompilingConstructors.Count > 0 do
            let toJS = ToJavaScript(comp)
            let (KeyValue((t, m), (i, e))) =  Seq.head comp.CompilingConstructors
            toJS.CompileConstructor(i, e, t, m)

        for t, a, e in comp.GetCompilingStaticConstructors() do
            let toJS = ToJavaScript(comp)
            toJS.CompileStaticConstructor(a, e, t)

        for t, it, m, i, e in comp.GetCompilingImplementations() do
            let toJS = ToJavaScript(comp)
            toJS.CompileImplementation(i, e, t, it, m)

    member this.AnotherNode() = ToJavaScript(comp, remotingProvider)    

    member this.AddDependency(dep: M.Node) =
        let graph = comp.Graph
        graph.AddEdge(currentNode, dep)

    member this.Error(err) =
        comp.AddError(currentSourcePos, err)
        errorPlaceholder

    member this.CompileCall (info, expr, thisObj, typ, meth, args) =
        this.AddDependency(M.MethodNode (typ.Entity, meth.Entity))
        match info with
        | M.Instance name ->
//            M.addDependency node.Id (M.ClassDep typ.Entity) meta
            Application(ItemGet(this.TransformExpression (Option.get thisObj), Value (String name)), args |> List.map this.TransformExpression) 
        | M.Static address ->
//            M.addDependency node.Id (M.StaticMethodDep (typ.Entity, meth.Entity)) meta
            Application(GlobalAccess address, args |> List.map this.TransformExpression)
        | M.Inline ->
            Substitution(args |> List.map this.TransformExpression, ?thisObj = thisObj).TransformExpression(expr)
            |> this.TransformExpression
        // TODO : return dependencies/requires
        | M.Macro (macro, parameter, fallback) ->
            let macroResult = 
                match comp.GetOrInitMacro(macro) with
                | Some m ->
                    try m.TranslateCall(thisObj, typ, meth, args, parameter |> Option.map M.ParameterObject.ToObj) 
                    with e -> this.Error(M.SourceError (sprintf "Macro error in %s.TranslateCall error: %s" macro.Value.FullName e.Message))
                | _ -> errorPlaceholder
            match macroResult with
            | MacroFallback ->
                match fallback with
                | None -> this.Error(M.SourceError (sprintf "No macro fallback found for '%s'" macro.Value.FullName))
                | Some f -> this.CompileCall (f, expr, thisObj, typ, meth, args)      
            | res -> this.TransformExpression res
//        | M.NotCompiled info ->
//            this.CompileNode(node, info)
//            this.CompileCall(node, thisObj, typ, meth, args)
//        | M.NotGenerated _ ->
//            failwith "TODO: generators"
        | M.Remote (scope, kind, handle) ->
            let name =
                match kind with
                | M.RemoteAsync -> "Async"
                | M.RemoteSend -> "Send"
                | M.RemoteSync -> "Sync"
            //let str x = !~ (C.String x)                    
            let trArgs =
                match scope, args with
                | M.InstanceMember, _ :: args 
                | _, args -> NewArray ( args |> List.map this.TransformExpression )
            Application (ItemGet(remotingProvider, Value (String name)), [ Value (String (handle.Pack())); trArgs ])
    
    override this.TransformCall (thisObj, typ, meth, args) =
        match comp.LookupMethodInfo(typ.Entity, meth.Entity) with
        | M.Compiled (info, expr) ->
            this.CompileCall(info, expr, thisObj, typ, meth, args)
        | M.Compiling (info, expr) ->
            match info with
            | M.NotCompiled M.Inline | M.NotGenerated (_, _, M.Inline) ->
                this.AnotherNode().CompileMethod(info, expr, typ.Entity, meth.Entity)
                this.TransformCall (thisObj, typ, meth, args)
            | M.NotCompiled info | M.NotGenerated (_, _, info) ->
                this.CompileCall(info, expr, thisObj, typ, meth, args)
        | M.LookupMemberError err ->
            comp.AddError (currentSourcePos, err)
            match thisObj with 
            | Some thisObj ->
                Application(ItemGet(this.TransformExpression thisObj, errorPlaceholder), args |> List.map this.TransformExpression) 
            | _ ->
                Application(errorPlaceholder, args |> List.map this.TransformExpression)

    member this.CompileCtor(info, expr, typ, ctor, args) =
        this.AddDependency(M.ConstructorNode (typ.Entity, ctor))
        match info with
        | M.Constructor address ->
            New(GlobalAccess address, args |> List.map this.TransformExpression)
        | M.Static address ->
            Application(GlobalAccess address, args |> List.map this.TransformExpression)
        | M.Inline -> 
            let res = Substitution(args |> List.map this.TransformExpression).TransformExpression(expr)
            // TODO: no more recursive transform of inlines (do not use Let inside)
            match res with
            | WithVars(a, b) -> this.TransformWithVars(a, b)
            | _ -> this.TransformExpression(res)
        | M.Macro (macro, parameter, fallback) ->
//            fallback |> Option.iter (fun f -> this.Compile ({node with NodeInfo = f}, thisObj, args))
            let macroResult = 
                match comp.GetOrInitMacro(macro) with
                | Some m ->
                    try m.TranslateCtor(typ, ctor, args, parameter |> Option.map M.ParameterObject.ToObj)
                    with e -> 
                        this.Error(M.SourceError (sprintf "Macro error in %s.TranslateCtor error: %s" macro.Value.FullName e.Message))
                | _ -> errorPlaceholder
            match macroResult with
            | MacroFallback ->
                match fallback with
                | None -> this.Error(M.SourceError (sprintf "No macro fallback found for '%s'" macro.Value.FullName))
                | Some f -> this.CompileCtor (f, expr, typ, ctor, args)      
            | res -> this.TransformExpression res
//        | M.NotCompiled info ->
//            this.CompileNode(node, info)
//            this.CompileCtor(node, typ, ctor, args)
//        | M.NotGenerated _ ->
//            failwith "TODO: generators"
        | _ -> this.Error(M.SourceError "invalid metadata for constructor")

    override this.TransformNewObject(typ, objExpr) =
        match comp.TryLookupClassInfo typ.Entity |> Option.bind (fun c -> c.Address) with
        | Some a -> New (GlobalAccess a, [ this.TransformExpression objExpr ])
        | _ -> this.TransformExpression objExpr

    override this.TransformCtor(typ, ctor, args) =
        let node = comp.LookupConstructorInfo(typ.Entity, ctor)
        match node with
        | M.Compiled (info, expr) -> 
            this.CompileCtor(info, expr, typ, ctor, args)
        | M.Compiling (info, expr) ->
            match info with
            | M.NotCompiled M.Inline | M.NotGenerated (_, _, M.Inline) ->
                this.AnotherNode().CompileConstructor(info, expr, typ.Entity, ctor)
                this.TransformCtor(typ, ctor, args)
            | M.NotCompiled info | M.NotGenerated (_, _, info) ->
                this.CompileCtor(info, expr, typ, ctor, args)
        | M.LookupMemberError err ->
            comp.AddError (currentSourcePos, err)
            Application(errorPlaceholder, args |> List.map this.TransformExpression)
                  
    override this.TransformCctor(typ) =
        this.AddDependency(M.TypeNode typ)
        Application(GlobalAccess (comp.LookupStaticConstructorAddress typ), [])

//    member this.CompileCCtor (node: M.CompiledNode, typ) =
//        match node.Info with
//        | M.Static address ->
//            Application(GlobalAccess address, [])
//        | M.NotCompiled info ->
//            this.CompileNode(node, info)
//            this.CompileCCtor(node, typ)
//        | _ -> failwith "invalid metadata for static constructor"

//    override this.TransformCCtor typ =
//        let node = M.lookupStaticConstructor meta typ.Entity
//        this.CompileCCtor(node, typ)

    override this.TransformSelf () = GlobalAccess selfAddress.Value

    override this.TransformFieldGet (expr, typ, field) =
        this.AddDependency(M.TypeNode typ.Entity)
        match comp.LookupFieldInfo (typ.Entity, field) with
        | M.CompiledField f ->
            match f with
            | M.InstanceField fname ->
                ItemGet(this.TransformExpression expr.Value, Value (String fname)) 
            | M.StaticField faddr ->
                GlobalAccess faddr   
            | M.OptionalField fname -> 
                Application(runtimeGetOptional, [this.TransformExpression expr.Value; Value (String fname)])
        | M.LookupFieldError err ->
            comp.AddError (currentSourcePos, err)
            match expr with
            | Some expr ->
                ItemGet(this.TransformExpression expr, errorPlaceholder)
            | _ -> errorPlaceholder

    override this.TransformFieldSet (expr, typ, field, value) =
        this.AddDependency(M.TypeNode typ.Entity)
        match comp.LookupFieldInfo (typ.Entity, field) with
        | M.CompiledField f ->
            match f with
            | M.InstanceField fname ->
                ItemSet(this.TransformExpression expr.Value, Value (String fname), this.TransformExpression value) 
            | M.StaticField faddr ->
                let f :: a = faddr.Value
                ItemSet(GlobalAccess (Hashed a), Value (String f), this.TransformExpression value)
            | M.OptionalField fname -> 
                Application(runtimeSetOptional, [this.TransformExpression expr.Value; Value (String fname); this.TransformExpression value])
        | M.LookupFieldError err ->
            comp.AddError (currentSourcePos, err)
            match expr with
            | Some expr ->
                ItemSet(this.TransformExpression expr, errorPlaceholder, this.TransformExpression value)
            | _ ->
                ItemSet(errorPlaceholder, errorPlaceholder, this.TransformExpression value)

    override this.TransformTypeCheck(expr, typ) =
//        this.AddDependency(M.TypeNode typ) // TODO typecheck dependencies
        let typeof x = 
            Binary (
                Unary(UnaryOperator.typeof, this.TransformExpression expr),
                BinaryOperator.``==``,
                Value (String x)
            )
        match typ with
        | ConcreteType { Entity = t; Generics = [] } ->
            match t.Value.FullName with
            | "Microsoft.FSharp.Core.Unit"
            | "System.Void" ->
                typeof "undefined"
            | "System.Boolean" ->
                typeof "boolean"
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
            | "System.UInt64" ->
                typeof "number"
            | "System.String" ->
                typeof "string"
            | "System.IDisposable" ->
                Binary(
                    // TODO : rename
                    Value (String "System_IDisposable$Dispose"),
                    BinaryOperator.``in``,
                    this.TransformExpression expr
                )
            | tname ->
                match comp.TryLookupClassInfo t with
                | Some c ->
                    match c.Address with
                    | Some a ->
                        Binary(this.TransformExpression expr, BinaryOperator.instanceof, GlobalAccess a)
                    | _ ->
                        Value (String "TODO: type test for non custom class")

//                    match c.Address, c.HasPrototype with
//                    | Some a, true ->
//                        Binary(this.TransformExpression expr, BinaryOperator.instanceof, GlobalAccess a)
//                    | _ ->
//                        Value (String "TODO: type test")
                        //failwithf "Failed to compile a type test: " tname
                | None -> this.Error(M.SourceError (sprintf "Failed to compile a type check for type '%s'" tname))
        | _ -> this.Error(M.SourceError "Type tests do not support generic and array types.")

    override this.TransformExprSourcePos (pos, expr) =
        let p = currentSourcePos 
        currentSourcePos <- Some pos
        let res = this.TransformExpression expr
        currentSourcePos <- p
        res

    override this.TransformStatementSourcePos (pos, statement) =
        let p = currentSourcePos 
        currentSourcePos <- Some pos
        let res = this.TransformStatement statement
        currentSourcePos <- p
        res

module WebSharper.Compiler.ToJavaScript
 
open WebSharper.Core
open WebSharper.Core.AST

let fail _ = failwith "Transform error: .NET common to Core"

module M = WebSharper.Core.Metadata
module A = WebSharper.Core.Attributes

let errorPlaceholder = Value (String "$$ERROR$$")

type CheckNoInvalidJSForms(comp: M.Compilation) =
    inherit Transformer()

    let mutable currentSourcePos = None
    let invalidForm f = 
        comp.AddError(currentSourcePos, M.SourceError ("Invalid form after JS tranlation: " + f))
        errorPlaceholder

    override this.TransformSelf () = invalidForm "Self"
    override this.TransformHole _ = invalidForm "Hole"
    override this.TransformFieldGet (_,_,_) = invalidForm "FieldGet"
    override this.TransformFieldSet (_,_,_,_) = invalidForm "FieldSet"
    override this.TransformLet (_,_,_) = invalidForm "Let"
    override this.TransformLetRec (_,_) = invalidForm "LetRec"
    override this.TransformStatementExpr _ = invalidForm "StatementExpr"
    override this.TransformAwait _  = invalidForm "Await"
    override this.TransformNamedParameter (_,_) = invalidForm "NamedParameter"
    override this.TransformRefOrOutParameter _ = invalidForm "RefOrOutParamete"
    override this.TransformCtor (_,_,_) = invalidForm "Ctor"
    override this.TransformCoalesce (_,_,_) = invalidForm "Coalesce"
    override this.TransformTypeCheck (_,_) = invalidForm "TypeCheck"
    override this.TransformCall (_,_,_,_) = invalidForm "Call"
    override this.TransformWithVars (_, _) = invalidForm "WithVars"
    override this.TransformNewVar (_, _) = invalidForm "NewVar"

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
    
let emptyConstructor = Hashed { CtorParameters = [] }

let flags =
    System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

let inline private getItem n x = ItemGet(x, Value (String n))

type ToJavaScript private (comp: M.Compilation, ?remotingProvider) =
    inherit Transformer()

    let remotingProvider = defaultArg remotingProvider defaultRemotingProvider

    let mutable selfAddress = None

//    let mutable innerVars = [] : list<Id>
    let mutable currentNode = M.AssemblyNode ("", false) // placeholder
    let mutable currentSourcePos = None

    // TODO : cache instances

    let jsonLookup = M.Static (Address ["lookup"; "Json"; "WebSharper"])

    let isInline info =
        match info with
        | M.NotCompiled M.Inline 
        | M.NotGenerated (_, _, M.Inline) -> true
        | _ -> false

    member this.CheckResult (res, isInline) =
#if DEBUG
        if isInline then res else
            CheckNoInvalidJSForms(comp).TransformExpression res
#else
        res
#endif
     
    member this.Generate(g, p, m) =
        match comp.GetGeneratorInstance(g) with
        | Some gen ->
            let genResult = 
                try gen.Generate(m, p)
                with e -> GeneratorError e.Message
            let verifyFunction gres =
                match ignoreExprSourcePos gres with 
                | Function _
                | FuncWithThis _ -> gres
                | _ -> this.Error(M.SourceError (sprintf "Generator not returning a function: %s" g.Value.FullName))
            let rec getExpr gres = 
                match gres with
                | GeneratedQuotation q -> 
                    QuotationReader.transformExpression (QuotationReader.Environment.New(comp)) q
                    |> verifyFunction |> this.TransformExpression |> breakExpr
                | GeneratedAST resExpr -> resExpr |> verifyFunction |> this.TransformExpression |> breakExpr
//                | GeneratedString s -> Verbatim s
//                | GeneratedJavaScript js -> VerbatimJS js 
                | GeneratorError msg ->
                    this.Error(M.SourceError (sprintf "Generator error in %s: %s" g.Value.FullName msg))
                | GeneratorWarning (msg, gres) ->
                    this.Warning (M.SourceWarning (sprintf "Generator warning in %s: %s" g.Value.FullName msg))
                    getExpr gres
            getExpr genResult
        | None -> this.Error(M.SourceError ("Getting generator failed"))
    
    member this.GetCustomTypeMethodInline (i : M.CustomTypeInfo, meth: Concrete<Method>) =
        let me = meth.Entity.Value
        match i with
        | M.DelegateInfo di ->
            match me.MethodName with
            | "Invoke" ->
                // TODO: optional arguments
                let args = di.DelegateArgs |> List.mapi (fun i _ -> Hole (i + 1)) |> NewArray
                Application(runtimeInvokeDelegate, [Hole 0; args])
            | mn -> this.Error(M.SourceError ("Unrecognized delegate method: " + mn))
            
    member this.CompileMethod(info, expr, typ, meth) =
        currentNode <- M.MethodNode(typ, meth)
        match info with
        | M.NotCompiled i -> 
            let res = this.TransformExpression expr |> removeSourcePosFromInlines info |> breakExpr
            let res = this.CheckResult(res, (i = M.Inline))
            comp.AddCompiledMethod(typ, meth, i, res)
        | M.NotGenerated (g, p, i) ->
            let m = GeneratedMethod(typ, meth)
            let res = this.Generate (g, p, m)
            let res = this.CheckResult(res, false)
            comp.AddCompiledMethod(typ, meth, i, res)

    member this.CompileImplementation(info, expr, typ, intf, meth) =
        currentNode <- M.ImplementationNode(typ, intf, meth)
        match info with
        | M.NotCompiled i -> 
            let res = this.TransformExpression expr |> breakExpr
            let res = this.CheckResult(res, (i = M.Inline))
            comp.AddCompiledImplementation(typ, intf, meth, i, res)
        | M.NotGenerated (g, p, i) ->
            let m = GeneratedImplementation(typ, intf, meth)
            let res = this.Generate (g, p, m)
            let res = this.CheckResult(res, false)
            comp.AddCompiledImplementation(typ, intf, meth, i, res)

    member this.CompileConstructor(info, expr, typ, ctor) =
        currentNode <- M.ConstructorNode(typ, ctor)
        match info with
        | M.NotCompiled i -> 
            let res = this.TransformExpression expr |> removeSourcePosFromInlines info |> breakExpr
            let res = this.CheckResult(res, (i = M.Inline))
            comp.AddCompiledConstructor(typ, ctor, i, res)
        | M.NotGenerated (g, p, i) ->
            let m = GeneratedConstructor(typ, ctor)
            let res = this.Generate (g, p, m)
            let res = this.CheckResult(res, false)
            comp.AddCompiledConstructor(typ, ctor, i, res)

    member this.CompileStaticConstructor(addr, expr, typ) =
        currentNode <- M.TypeNode typ
        selfAddress <- comp.TryLookupClassInfo(typ).Value.Address
        let res = this.TransformExpression expr |> breakExpr
        let res = this.CheckResult(res, false)
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

        match comp.EntryPoint with
        | Some ep ->
            let toJS = ToJavaScript(comp)
            comp.EntryPoint <- Some (toJS.TransformStatement(ep))
        | _ -> ()

    member this.AnotherNode() = ToJavaScript(comp, remotingProvider)    

    member this.AddDependency(dep: M.Node) =
        comp.Graph.AddEdge(currentNode, dep)

    member this.Error(err) =
        comp.AddError(currentSourcePos, err)
        errorPlaceholder

    member this.Warning(wrn) =
        comp.AddWarning(currentSourcePos, wrn)

    member this.CompileCall (info, expr, thisObj, typ, meth, args, ?baseCall) =
        match thisObj with
        | Some ((IgnoreExprSourcePos Base) as tv) ->
            this.CompileCall (info, expr, Some (This |> withSourcePosOfExpr tv), typ, meth, args, true)
        | _ ->
        this.AddDependency(M.MethodNode (typ.Entity, meth.Entity))
        match info with
        | M.Instance name ->
            match baseCall with
            | Some true ->
                let ba = comp.TryLookupClassInfo(typ.Entity).Value.Address.Value
                Application(
                    GlobalAccess ba |> getItem "prototype" |> getItem name |> getItem "call",
                    This :: (args |> List.map this.TransformExpression))
            | _ ->
                Application(
                    this.TransformExpression thisObj.Value |> getItem name,
                    args |> List.map this.TransformExpression) 
        | M.Static address ->
            Application(GlobalAccess address, args |> List.map this.TransformExpression)
        | M.Inline ->
            Substitution(args |> List.map this.TransformExpression, ?thisObj = thisObj).TransformExpression(expr)
            |> this.TransformExpression
        // TODO : return dependencies/requires
        | M.Macro (macro, parameter, fallback) ->
            let macroResult = 
                match comp.GetMacroInstance(macro) with
                | Some m ->
                    try m.TranslateCall(thisObj, typ, meth, args, parameter |> Option.map M.ParameterObject.ToObj)
                    with e -> MacroError e.Message 
                | _ -> MacroError "Macro type failed to load"
            let rec getExpr mres =
                match mres with
                | MacroOk resExpr -> this.TransformExpression resExpr
                | MacroWarning (msg, mres) ->
                    this.Warning (M.SourceWarning (sprintf "Macro warning in %s.TranslateCall: %s" macro.Value.FullName msg))
                    getExpr mres
                | MacroError msg ->
                    this.Error(M.SourceError (sprintf "Macro error in %s.TranslateCall: %s" macro.Value.FullName msg))
                | MacroFallback ->
                    match fallback with
                    | None -> this.Error(M.SourceError (sprintf "No macro fallback found for '%s'" macro.Value.FullName))
                    | Some f -> this.CompileCall (f, expr, thisObj, typ, meth, args)      
                | MacroNeedsResolvedTypeArg -> failwith "TODO: MacroNeedsResolvedTypeArg"
            getExpr macroResult
        | M.Remote (scope, kind, handle) ->
            let name =
                match kind with
                | M.RemoteAsync -> "Async"
                | M.RemoteTask -> "Task"
                | M.RemoteSend -> "Send"
                | M.RemoteSync -> "Sync"
            //let str x = !~ (C.String x)                    
            let trArgs =
                match scope, args with
                | M.InstanceMember, _ :: args 
                | _, args -> NewArray ( args |> List.map this.TransformExpression )
            Application (remotingProvider |> getItem name, [ Value (String (handle.Pack())); trArgs ])
    
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
        | M.CustomTypeMember ct ->  
            let inl = this.GetCustomTypeMethodInline(ct, meth)
            Substitution(args |> List.map this.TransformExpression, ?thisObj = thisObj).TransformExpression(inl)
            |> this.TransformExpression
        | M.LookupMemberError err ->
            comp.AddError (currentSourcePos, err)
            match thisObj with 
            | Some thisObj ->
                Application(ItemGet(this.TransformExpression thisObj, errorPlaceholder), args |> List.map this.TransformExpression) 
            | _ ->
                Application(errorPlaceholder, args |> List.map this.TransformExpression)

    override this.TransformNewDelegate(thisObj, typ, meth) =
        // TODO: CustomTypeMember
        match comp.LookupMethodInfo(typ.Entity, meth.Entity) with
        | M.Compiled (info, _)
        | M.Compiling ((M.NotCompiled info | M.NotGenerated (_, _, info)), _) ->
            match info with 
            | M.Static address -> NewArray [ GlobalAccess address ]
            | M.Abstract name
            | M.Instance name -> 
                match comp.TryLookupClassInfo typ.Entity with
                | Some { Address = Some addr } ->
                    let funcBind = GlobalAccess addr |> getItem "prototype" |> getItem name |> getItem "bind"
                    NewArray [ Application (funcBind, [this.TransformExpression thisObj.Value]) ]
                | _ -> this.Error (M.SourceError "Cannot look up prototype for delegate creating")
            | M.Inline _ -> this.Error (M.SourceError "TODO : Cannot create delegate from inlined method")
            | M.Macro _ -> this.Error (M.SourceError "Cannot create delegate from macroed method")
            | M.Remote _ -> this.Error (M.SourceError "TODO : Cannot create delegate from remote method")
            | M.Constructor _ -> failwith "impossible"
        | M.CustomTypeMember _ -> this.Error (M.SourceError "TODO : Cannot create delegate from custom type members")
        | M.LookupMemberError err -> this.Error err

    member this.CompileCtor(info, expr, typ, ctor, args) =
        this.AddDependency(M.ConstructorNode (typ.Entity, ctor))
        match info with
        | M.Constructor address ->
            New(GlobalAccess address, args |> List.map this.TransformExpression)
        | M.Static address ->
            Application(GlobalAccess address, args |> List.map this.TransformExpression)
        | M.Inline -> 
            Substitution(args |> List.map this.TransformExpression).TransformExpression(expr)
            |> this.TransformExpression
        | M.Macro (macro, parameter, fallback) ->
//            fallback |> Option.iter (fun f -> this.Compile ({node with NodeInfo = f}, thisObj, args))
            let macroResult = 
                match comp.GetMacroInstance(macro) with
                | Some m ->
                    try m.TranslateCtor(typ, ctor, args, parameter |> Option.map M.ParameterObject.ToObj)
                    with e -> MacroError e.Message 
                | _ -> MacroError "Macro type failed to load"
            let rec getExpr mres =
                match mres with
                | MacroOk resExpr -> this.TransformExpression resExpr
                | MacroWarning (msg, mres) ->
                    this.Warning (M.SourceWarning (sprintf "Macro warning in %s.TranslateCall: %s" macro.Value.FullName msg))
                    getExpr mres
                | MacroError msg ->
                    this.Error(M.SourceError (sprintf "Macro error in %s.TranslateCall: %s" macro.Value.FullName msg))
                | MacroFallback ->
                    match fallback with
                    | None -> this.Error(M.SourceError (sprintf "No macro fallback found for '%s'" macro.Value.FullName))
                    | Some f -> this.CompileCtor (f, expr, typ, ctor, args)      
                | MacroNeedsResolvedTypeArg -> failwith "TODO: MacroNeedsResolvedTypeArg"
            getExpr macroResult
//        | M.NotCompiled info ->
//            this.CompileNode(node, info)
//            this.CompileCtor(node, typ, ctor, args)
//        | M.NotGenerated _ ->
//            failwith "TODO: generators"
        | _ -> this.Error(M.SourceError "Invalid metadata for constructor.")

    override this.TransformCopyCtor(typ, objExpr) =
        match comp.TryLookupClassInfo typ |> Option.bind (fun c -> c.Address) with
        | Some a -> New (GlobalAccess a, [ this.TransformExpression objExpr ])
        | _ -> this.TransformExpression objExpr

    override this.TransformNewRecord(typ, fields) =
        match comp.TryGetRecordConstructor typ.Entity with
        | Some rctor ->
            this.TransformCtor(typ, rctor, fields)
        | _ ->
            try
                let t = Reflection.loadTypeDefinition typ.Entity

                let fs =
                    FSharp.Reflection.FSharpType.GetRecordFields(t, flags) |> Seq.map (fun f -> 
                        let mutable name = f.Name
                        let mutable isOpt = false

                        for a in f.GetCustomAttributesData() do
                            if a.Constructor.DeclaringType = typeof<A.NameAttribute> then
                                name <- a.ConstructorArguments.[0].Value :?> string
                            elif a.Constructor.DeclaringType = typeof<A.OptionalFieldAttribute> then
                                isOpt <- 
                                    f.PropertyType.IsGenericType 
                                    && f.PropertyType.GetGenericTypeDefinition() = typedefof<option<_>>
                    
                        name, isOpt 
                    )
                    |> List.ofSeq
                let trFields = fields |> List.map this.TransformExpression
                let obj = 
                    Seq.zip fs trFields
                    |> Seq.map (fun ((name, opt), value) -> name, if opt then value |> getItem "$0" else value)
                    |> List.ofSeq |> Object
                let optFields = 
                    fs |> List.choose (fun (f, o) -> 
                        if o then Some (Value (String f)) else None)
            
                if List.isEmpty optFields then obj 
                else Application (runtimeDeleteEmptyFields, [obj; NewArray optFields])
            with _ -> this.Error(M.SourceError "Record creation by reflection failed.")
            
//        match comp.TryLookupClassInfo typ.Entity with
//        | Some cls ->
//            match cls.Constructors.Count with
//            | 1 -> this.TransformCtor(typ, Seq.head cls.Constructors.Keys, fields)
//            | 0 -> this.Error(M.SourceError "Record type constructor not found.")
//            | _ -> this.Error(M.SourceError "Record type must have exacly one constructor.")
//        | _ -> this.Error(M.SourceError "Record type not found in translation.")

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
                  
    override this.TransformBaseCtor(expr, typ, ctor, args) =
        let norm = this.TransformCtor(typ, ctor, args)
        match norm with
        | New (func, a) ->
            Application(func |> getItem "call", expr :: a)
        // TODO: not needing this workaround for inlines
        | Let (i1, a1, New(func, [Var v1])) when i1 = v1 ->
            Application(func |> getItem "call", expr :: [a1])
        | _ ->
            comp.AddError (currentSourcePos, M.SourceError "base class constructor is not regular")
            Application(errorPlaceholder, args |> List.map this.TransformExpression)

    override this.TransformCctor(typ) =
        this.AddDependency(M.TypeNode typ)
        Application(GlobalAccess (comp.LookupStaticConstructorAddress typ), [])

    override this.TransformOverrideName(typ, meth) =
        match comp.LookupMethodInfo(typ, meth) with
        | M.Compiled (M.Instance name, _) 
        | M.Compiling ((M.NotCompiled (M.Instance name) | M.NotGenerated (_,_,M.Instance name)), _) ->
            Value (String name)
        | M.LookupMemberError err ->
            this.Error err
        | _ -> 
            this.Error (M.SourceError "Could not get name of abstract method")

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
                this.TransformExpression expr.Value |> getItem fname
            | M.StaticField faddr ->
                GlobalAccess faddr   
            | M.OptionalField fname -> 
                Application(runtimeGetOptional, [this.TransformExpression expr.Value |> getItem fname])
        | M.LookupFieldError err ->
            match expr with
            | Some expr ->
                try
                    let t = Reflection.loadTypeDefinition typ.Entity
                    FSharp.Reflection.FSharpType.GetRecordFields(t, flags) |> Seq.pick (fun f ->
                        if f.Name = field then
                            let mutable name = field
                            let mutable isOpt = false
                            for a in f.GetCustomAttributesData() do
                                if a.Constructor.DeclaringType = typeof<A.NameAttribute> then
                                    name <- a.ConstructorArguments.[0].Value :?> string
                                elif a.Constructor.DeclaringType = typeof<A.OptionalFieldAttribute> then
                                    isOpt <- 
                                        f.PropertyType.IsGenericType 
                                        && f.PropertyType.GetGenericTypeDefinition() = typedefof<option<_>>
                            if isOpt then
                                Application(runtimeGetOptional, [this.TransformExpression expr |> getItem name])
                            else
                                this.TransformExpression expr |> getItem name 
                            |> Some
                        else None
                    )
                with _ ->
                    this.Warning(M.SourceWarning "Original field name used")
                    this.TransformExpression expr |> getItem field
            | _ -> 
                this.Error(err)

    override this.TransformFieldSet (expr, typ, field, value) =
        this.AddDependency(M.TypeNode typ.Entity)
        match comp.LookupFieldInfo (typ.Entity, field) with
        | M.CompiledField f ->
            match f with
            | M.InstanceField fname ->
                ItemSet(this.TransformExpression expr.Value, Value (String fname), this.TransformExpression value) 
            | M.StaticField faddr ->
                let f, a = List.head faddr.Value, List.tail faddr.Value
                ItemSet(GlobalAccess (Hashed a), Value (String f), this.TransformExpression value)
            | M.OptionalField fname -> 
                Application(runtimeSetOptional, [this.TransformExpression expr.Value; Value (String fname); this.TransformExpression value])
        | M.LookupFieldError err ->
            match expr with
            | Some expr ->
                try
                    let t = Reflection.loadTypeDefinition typ.Entity
                    FSharp.Reflection.FSharpType.GetRecordFields(t, flags) |> Seq.pick (fun f ->
                        if f.Name = field then
                            let mutable name = field
                            let mutable isOpt = false
                            for a in f.GetCustomAttributesData() do
                                if a.Constructor.DeclaringType = typeof<A.NameAttribute> then
                                    name <- a.ConstructorArguments.[0].Value :?> string
                                elif a.Constructor.DeclaringType = typeof<A.OptionalFieldAttribute> then
                                    isOpt <- 
                                        f.PropertyType.IsGenericType 
                                        && f.PropertyType.GetGenericTypeDefinition() = typedefof<option<_>>
                            if isOpt then
                                Application(runtimeSetOptional, [this.TransformExpression expr; Value (String name); this.TransformExpression value])
                            else
                                ItemSet(this.TransformExpression expr, Value (String name), this.TransformExpression value) 
                            |> Some
                        else None
                    )
                with _ ->
                    this.Warning(M.SourceWarning "Original field name used")
                    ItemSet(this.TransformExpression expr, Value (String field), this.TransformExpression value)
            | _ ->
                comp.AddError (currentSourcePos, err)
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

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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

namespace WebSharper.Compiler
  
open System.Collections.Generic
open WebSharper
open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata
open WebSharper.Core.DependencyGraph
open NotResolved

type ExtraBundleData =
    {
        EntryPoint: Statement
        Node: Node
        IncludeJsExports: bool
    }

//[<AutoOpen>]
//module private WSDefinitions =
//    let wsEnumeratorModule =
//        TypeDefinition {
//            Assembly = "WebSharper.Main"
//            FullName = "WebSharper.Enumerator"
//        } 

//    let seq0Ty =
//        TypeDefinition {
//            Assembly = "mscorlib"
//            FullName = "System.Collections.IEnumerable"
//        } 

//    let seqTy =
//        TypeDefinition {
//            Assembly = "mscorlib"
//            FullName = "System.Collections.Generic.IEnumerable`1"
//        } 

//    let enum0Ty =
//        TypeDefinition {
//            Assembly = "mscorlib"
//            FullName = "System.Collections.IEnumerator"
//        } 

//    let enumTy =
//        TypeDefinition {
//            Assembly = "mscorlib"
//            FullName = "System.Collections.Generic.IEnumerator`1"
//        }
    
type Compilation(meta: Info, ?hasGraph) =    
    let notResolvedInterfaces = Dictionary<TypeDefinition, NotResolvedInterface>()
    let notResolvedClasses = Dictionary<TypeDefinition, NotResolvedClass>()
    let notResolvedCustomTypes = Dictionary<TypeDefinition, CustomTypeInfo>()
    let proxies = Dictionary<TypeDefinition, TypeDefinition>()
    let internalProxies = Dictionary<TypeDefinition, TypeDefinition>()

    let classes = MergedDictionary meta.Classes
    let mergedProxies = HashSet<TypeDefinition>()
    let assumeClass typ =
        match classes.TryFind typ with
        | Some (_, _, Some cls) -> cls
        | Some _ -> failwithf "Found custom type but not class: %s" typ.Value.FullName
        | _ -> failwithf "Couldn't find class: %s" typ.Value.FullName
    let updateClass typ f =
        match classes.TryFind typ with
        | Some (addr, ct, Some cls) -> classes.[typ] <- (addr, ct, Some (f cls))
        | _ -> ()
    let cleanName (s: string) = s.Replace('.', '_').Replace('+', '_').Replace('`', '_')
    let interfaces = MergedDictionary meta.Interfaces
    let notAnnotatedCustomTypes = Dictionary()
    let macroEntries = MergedDictionary meta.MacroEntries
    let quotations = MergedDictionary meta.Quotations
    let quotedMethods = ResizeArray()

    let hasGraph = defaultArg hasGraph true
    let graph = if hasGraph then Graph.FromData(meta.Dependencies) else Unchecked.defaultof<_>

    let mutableExternals = Recognize.GetMutableExternals meta

    let compilingMethods = Dictionary<TypeDefinition * Method, CompilingMember * list<GenericParam> * Expression>()
    let compilingImplementations = Dictionary<TypeDefinition * TypeDefinition * Method, CompilingMember * Expression>()
    let compilingConstructors = Dictionary<TypeDefinition * Constructor, CompilingMember * Expression>()
    let compilingStaticConstructors = Dictionary<TypeDefinition, Statement>()
    let compilingQuotedArgMethods = Dictionary<TypeDefinition * Method, int[]>()
    let compilingExtraBundles = Dictionary<string, ExtraBundleData>()
    let compiledExtraBundles = Dictionary<string, ExtraBundleData>()
    let typesNeedingDeserialization = ResizeArray<Type * SourcePos>()

    let mutable generatedClass = None
    let resolver = getAllAddresses meta

    let typeErrors = HashSet()

    let errors = ResizeArray()
    let warnings = ResizeArray() 

    let mutable entryPoint = None : option<Statement>
    let jsExports = ResizeArray() 

    let macros = System.Collections.Generic.Dictionary<TypeDefinition, Macro option>()
    let generators = System.Collections.Generic.Dictionary<TypeDefinition, Generator option>()

    let typeTranslator =
        TypeTranslator.TypeTranslator(fun t ->
            let t =
                match proxies.TryFind t with
                | Some p -> p 
                | _ -> t
            match classes.TryFind t with
            | Some (a, ct, c) -> TypeTranslator.Class (a, ct, c)
            | _ ->
            match interfaces.TryFind t with
            | Some i -> TypeTranslator.Interface i
            | _ -> TypeTranslator.Unknown
        )

    let defaultAddressOf (typ: TypeDefinition) =
        let removeGen (n: string) =
            match n.LastIndexOf '`' with
            | -1 -> n
            | i -> n.[.. i - 1]
        typ.Value.FullName.Split('.', '+') |> List.ofArray |> List.map removeGen |> List.rev 

    member val UseLocalMacros = true with get, set
    member val SingleNoJSErrors = false with get, set
    member val SiteletDefinition: option<TypeDefinition> = None with get, set
    member val AssemblyName = "EntryPoint" with get, set
    member val ProxyTargetName: option<string> = None with get, set
    member val AssemblyRequires = [] : list<TypeDefinition * option<obj>> with get, set
    
    member val CustomTypesReflector = fun _ -> NotCustomType with get, set 
    member val LookupTypeAttributes = fun _ -> None with get, set
    member val LookupFieldAttributes = fun _ _ -> None with get, set
    member val LookupMethodAttributes = fun _ _ -> None with get, set
    member val LookupConstructorAttributes = fun _ _ -> None with get, set

    member this.CurrentExtraBundles =
        Set [|
            for KeyValue(k, _) in compiledExtraBundles do
                yield { AssemblyName = this.AssemblyName; BundleName = k }
        |]

    member this.AllExtraBundles =
        meta.ExtraBundles + this.CurrentExtraBundles

    member this.CompiledExtraBundles =
        compiledExtraBundles

    member this.MutableExternals = mutableExternals

    member this.TypeTranslator = typeTranslator

    member this.FindProxied typ =
        if proxies.Count = 0 then typ else
        match proxies.TryFind typ with
        | Some p -> p 
        | _ -> typ
    
    member this.FindProxiedAssembly(name: string) =
        if name = this.AssemblyName then
            this.ProxyTargetName |> Option.defaultValue name
        else
            name
    
    member this.GetRemoteHandle(path: string, args: Type list, ret: Type) =
        {
            Assembly = this.AssemblyName
            Path = path
            SignatureHash = hash (args, ret)
        }

    member this.AddError (pos : SourcePos option, error : CompilationError) =
        if this.SingleNoJSErrors then
            match error with
            | TypeNotFound _
            | MethodNotFound _
            | MethodNameNotFound _
            | ConstructorNotFound _
            | FieldNotFound _ ->
                if typeErrors.Add(error) then
                    errors.Add (pos, error)
            | _ ->
                errors.Add (pos, error)
        else
            errors.Add (pos, error)

    member this.Errors = List.ofSeq errors

    member this.SetErrors(e) =
        errors.Clear()
        errors.AddRange(e)

    member this.SetWarnings(w) =
        warnings.Clear()
        warnings.AddRange(w)

    member this.AddWarning (pos : SourcePos option, warning : CompilationWarning) =
        warnings.Add (pos, warning)

    member this.SetEntryPoint (st) =
        if Option.isSome entryPoint then
            errors.Add(None, SourceError "Multiple SPAEntryPoint attributes found.")
        else
            entryPoint <- Some st

    member this.EntryPoint
        with get () = entryPoint
        and set ep = entryPoint <- ep 

    member this.JavaScriptExports = List.ofSeq jsExports

    member this.AddJavaScriptExport (jsExport: JsExport) =
        jsExports.Add jsExport

    member this.Warnings = List.ofSeq warnings

    member this.GetGeneratedClass() =
        match generatedClass with
        | Some cls -> cls
        | _ ->
            let td = 
                TypeDefinition { 
                    FullName = "$Generated"
                    Assembly = this.AssemblyName 
                }
            let addr = { Module = DotNetType { Assembly = this.AssemblyName; Name = "$Generated" }; Address = [] } 
            classes.Add (td,
                (
                    addr,
                    CustomTypeInfo.NotCustomType,
                    Some {
                        BaseClass = None
                        Implements = []
                        Generics = []
                        Constructors = Dictionary() 
                        Fields = Dictionary() 
                        StaticConstructor = None 
                        Methods = Dictionary()
                        QuotedArgMethods = Dictionary()
                        Implementations = Dictionary()
                        HasWSPrototype = false
                        IsStub = false
                        Macros = []
                        Type = None
                    }
                )
            ) 
            resolver.AddClass(td, None)
            generatedClass <- Some (addr, td)
            addr, td
    
    member this.GetMacroEntries key =
        match macroEntries.TryFind key with
        | Some l -> l
        | _ -> []

    interface ICompilation with
        member this.GetCustomTypeInfo typ = 
            this.GetCustomType typ
        
        member this.GetInterfaceInfo typ =
            interfaces.TryFind (this.FindProxied typ)

        member this.GetClassInfo typ = 
            match classes.TryFind (this.FindProxied typ) with
            | Some (a, _, Some cls) ->
                Some { new IClassInfo with
                    member this.Address = a
                    member this.BaseClass = cls.BaseClass
                    member this.Implements = cls.Implements
                    member this.Constructors = cls.Constructors
                    member this.Fields = cls.Fields
                    member this.HasStaticConstructor = cls.StaticConstructor |> Option.isSome
                    member this.Methods = cls.Methods
                    member this.Implementations = cls.Implementations
                    member this.HasWSPrototype = cls.HasWSPrototype
                    member this.Macros = cls.Macros
                }
            | _ -> None

        member this.GetQuotation(pos) = quotations.TryFind pos

        member this.GetJavaScriptClasses() = classes.Keys |> List.ofSeq
        member this.GetTypeAttributes(typ) = this.LookupTypeAttributes typ
        member this.GetFieldAttributes(typ, field) = this.LookupFieldAttributes typ field
        member this.GetMethodAttributes(typ, meth) = this.LookupMethodAttributes typ meth
        member this.GetConstructorAttributes(typ, ctor) = this.LookupConstructorAttributes typ ctor
        member this.GetTSTypeOf(t, ?gs) = typeTranslator.TSTypeOf t

        member this.ParseJSInline(inl: string, args: Expression list, position: SourcePos, dollarVars: string[]): Expression = 
            let vars = args |> List.map (fun _ -> Id.New(mut = false))
            //let parsed = Recognize.createInline mutableExternals None vars false None inl
            //Substitution(args).TransformExpression(parsed)
            let dollarVars = if isNull dollarVars then [||] else dollarVars
            let position = if obj.ReferenceEquals(position, null) then None else Some position
            let parsed = Recognize.createInline mutableExternals None vars false None dollarVars this.AssemblyName inl
            parsed.Warnings |> List.iter (fun msg -> this.AddWarning(position, SourceWarning msg))
            Substitution(args).TransformExpression(parsed.Expr)
        
        member this.NewGenerated(name, ?generics, ?args, ?returns) =
            let caddr, td = this.GetGeneratedClass()
            let c = resolver.LookupClass(td)
            let rname = Resolve.getRenamedFunctionForClass name c
            let addr = caddr.Func(rname)
            let meth = 
                Method {
                    MethodName = rname
                    Parameters = defaultArg args []
                    ReturnType = defaultArg returns (TSType TSType.Any)
                    Generics = defaultArg generics 0
                }
            td, meth, addr

        member this.NewGeneratedVar(name, ?typ) =
            let _, td = this.GetGeneratedClass()
            let c = resolver.LookupClass(td)
            let rname = Resolve.getRenamedFunctionForClass name c
            let fvar = Id.New(rname, ?typ = typ)
            let _, _, cls = classes[td]
            cls.Value.Fields.Add(rname, { CompiledForm = VarField fvar; ReadOnly = false; Type = defaultArg typ (TSType TSType.Any); Order = 0 })
            fvar

        member this.AddGeneratedCode(meth: Method, body: Expression) =
            let _, td = this.GetGeneratedClass()
            let c =
                {
                    CompiledMember = Func (meth.Value.MethodName, false)
                    NotVirtual = true
                    Optimizations = Optimizations.None
                    JavaScriptOptions = JavaScriptOptions.None
                    Generator = None
                }
            compilingMethods.Add((td, meth), (c, [], body))

        member this.AddGeneratedInline(meth: Method, body: Expression) =
            let _, td = this.GetGeneratedClass()
            let c =
                {
                    CompiledMember = Inline (true, false)
                    NotVirtual = true
                    Optimizations = Optimizations.None
                    JavaScriptOptions = JavaScriptOptions.None
                    Generator = None
                }
            compilingMethods.Add((td, meth),(c, [], body))

        member this.AssemblyName = this.AssemblyName

        member this.GetMetadataEntries key =
            match macroEntries.TryFind key with
            | Some l -> l
            | _ -> []

        member this.AddMetadataEntry(key, value) =
            match macroEntries.TryFind key with
            | Some l ->
                macroEntries.[key] <- value :: l
            | _ -> 
                macroEntries.[key] <- [value]

        member this.GetJsonMetadataEntry (isEnc, typ) =
            let key = CompositeEntry [ StringEntry (if isEnc then "JsonEncoder" else "JsonDecoder"); TypeEntry typ ]
            match macroEntries.TryFind key with
            | Some (StringEntry "id" :: _) ->
                Some JsonId
            | Some (CompositeEntry [ TypeDefinitionEntry gtd; MethodEntry gm ] :: _) ->
                Some (JsonSerializer (gtd, gm))
            | me -> 
                None

        member this.AddJsonMetadataEntry (isEnc, typ, entry) =
            let key = CompositeEntry [ StringEntry (if isEnc then "JsonEncoder" else "JsonDecoder"); TypeEntry typ ]
            let e =
                match entry with
                | JsonId -> StringEntry "id"
                | JsonSerializer (gtd, gm) -> CompositeEntry [ TypeDefinitionEntry gtd; MethodEntry gm ]
            macroEntries.[key] <- [ e ]

    //abstract GetJsonMetadataEntry : Type -> option<JsonSerializerEntry>
    //abstract AddJsonMetadataEntry : Type * JsonSerializerEntry -> unit

        member this.AddError(pos, msg) =
            this.AddError(pos, SourceError msg)

        member this.AddWarning(pos, msg) =
            this.AddWarning(pos, SourceWarning msg)

        member this.AddBundle(name, entryPoint, includeJsExports) =
            this.AddBundle(name, entryPoint, includeJsExports)
              
        member this.AddJSImport(export, from) = 
            this.AddJSImport(export, from)

    member this.GetMacroInstance(macro) =
        match macros.TryFind macro with
        | Some res -> res
        | _ ->            
            let res =
                maybe {
                    let! mt =
                        try                                                             
                            match System.Type.GetType(macro.Value.AssemblyQualifiedName, true) with
                            | null ->
                                if this.UseLocalMacros then
                                    this.AddError(None, SourceError(sprintf "Failed to find macro type: '%s'" macro.Value.FullName))
                                None
                            | t -> Some t
                        with e -> 
                            if this.UseLocalMacros then
                                this.AddError(None, SourceError(sprintf "Failed to load macro type: '%s', Error: %s" macro.Value.FullName e.Message))
                            None
                    let! mctor, arg =
                        match mt.GetConstructor([||]) with
                        | null -> 
                            if this.UseLocalMacros then
                                this.AddError(None, SourceError(sprintf "Macro does not have supported constructor: '%s'" macro.Value.FullName))
                            None
                        | mctor -> Some (mctor, [||]) 
                    let! inv =
                        try mctor.Invoke(arg) |> Some
                        with e ->
                            if this.UseLocalMacros then
                                this.AddError(None, SourceError(sprintf "Creating macro instance failed: '%s', Error: %s" macro.Value.FullName e.Message))
                            None
                    match inv with 
                    | :? WebSharper.Core.Macro as m -> 
                        return m
                    | _ -> 
                        if this.UseLocalMacros then
                            this.AddError(None, SourceError(sprintf "Macro type does not inherit from WebSharper.Core.Macro: '%s'" macro.Value.FullName))
                } 
            macros.Add(macro, res)
            res

    member this.CloseMacros() =
        for m in macros.Values do
            m |> Option.iter (fun m -> m.Close this)      

    member this.GetGeneratorInstance(gen) =
        match generators.TryFind gen with
        | Some res -> res
        | _ ->            
            let res =
                maybe {
                    let! mt =
                        try                                                             
                            match System.Type.GetType(gen.Value.AssemblyQualifiedName, true) with
                            | null ->
                                if this.UseLocalMacros then
                                    this.AddError(None, SourceError(sprintf "Failed to find generator type: '%s'" gen.Value.FullName))
                                None
                            | t -> Some t
                        with e -> 
                            if this.UseLocalMacros then
                                this.AddError(None, SourceError(sprintf "Failed to load generator type: '%s', Error: %s" gen.Value.FullName e.Message))
                            None
                    let! mctor, arg =
                        match mt.GetConstructor([|typeof<Compilation>|]) with
                        | null ->
                            match mt.GetConstructor([||]) with
                            | null -> 
                                if this.UseLocalMacros then  
                                    this.AddError(None, SourceError(sprintf "Generator does not have supported constructor: '%s'" gen.Value.FullName))
                                None
                            | mctor -> Some (mctor, [||]) 
                        | mctor -> Some (mctor, [|box this|]) 
                    let! inv =
                        try mctor.Invoke(arg) |> Some
                        with e ->
                            if this.UseLocalMacros then
                                this.AddError(None, SourceError(sprintf "Creating generator instance failed: '%s', Error: %s" gen.Value.FullName e.Message))
                            None
                    match inv with 
                    | :? WebSharper.Core.Generator as g -> return g
                    | _ -> 
                        if this.UseLocalMacros then
                            this.AddError(None, SourceError(sprintf "Generator type does not inherit from WebSharper.Core.Generator: '%s'" gen.Value.FullName))
                } 
            generators.Add(gen, res)
            res

    member this.DependencyMetadata = meta

    member this.HasGraph = hasGraph
    member this.Graph = graph

    member this.ToCurrentMetadata(?ignoreErrors) =
        if errors.Count > 0 && not (ignoreErrors = Some true) then 
            failwith "This compilation has errors"
        {
            SiteletDefinition = this.SiteletDefinition 
            Dependencies = if hasGraph then graph.GetData() else GraphData.Empty
            Interfaces = interfaces.Current
            Classes = 
                classes.Current |> Dict.choose (function
                    | _, NotCustomType, None -> None
                    | a, ct, Some c as orig ->
                        match c.Methods with
                        | :? MergedDictionary<Method, CompiledMethodInfo> as m -> 
                            Some (a, ct, Some { c with Methods = m.Current })
                        | _ -> Some orig
                    | x -> Some x
                )
            MacroEntries = macroEntries.Current
            Quotations = quotations.Current
            ResourceHashes = Dictionary()
            ExtraBundles = this.CurrentExtraBundles
            PreBundle = Map.empty
            QuotedMethods = quotedMethods.ToArray()
        }    

    member this.HideInternalProxies(meta) =
        if internalProxies.Count > 0 then
            let updateType t =
                match internalProxies.TryGetValue(t) with
                | true, p -> p
                | _ -> t
            let updateNode n =
                match n with
                | MethodNode (td, m) ->
                    MethodNode(updateType td, m)
                | ConstructorNode (td, c) ->
                    ConstructorNode(updateType td, c)
                | ImplementationNode (td, i, m) ->
                    ImplementationNode (updateType td, updateType i, m)
                | AbstractMethodNode (td, m) ->
                    AbstractMethodNode (updateType td, m) 
                | TypeNode td ->
                    TypeNode (updateType td)
                | _ -> n
            { meta with
                Interfaces = meta.Interfaces |> Dict.mapKeys updateType
                Classes = meta.Classes |> Dict.mapKeys updateType
                Dependencies = 
                    { meta.Dependencies with
                        Nodes = meta.Dependencies.Nodes |> Array.map updateNode 
                    }
            }    
        else
            meta

    member this.ToRuntimeMetadata() =
        {
            SiteletDefinition = this.SiteletDefinition 
            Dependencies = if hasGraph then graph.GetData() else GraphData.Empty
            Interfaces = interfaces
            Classes = classes
            MacroEntries = 
                macroEntries |> Dict.filter (fun k _ ->
                    match k with
                    | CompositeEntry [ StringEntry "JsonDecoder"; TypeEntry _ ] -> true
                    | _ -> false
                )
            Quotations = quotations
            ResourceHashes = MergedDictionary meta.ResourceHashes
            ExtraBundles = this.AllExtraBundles
            PreBundle = Map.empty
            QuotedMethods = Seq.append meta.QuotedMethods quotedMethods |> Seq.toArray
        }    

    member this.AddProxy(tProxy, tTarget, isInternal) =
        // if the proxy is for internal use only, drop it with a warning if a proxy for target type already exists
        if isInternal && (classes.Original.ContainsKey tTarget || interfaces.Original.ContainsKey tTarget) then 
            this.AddWarning (None, SourceWarning (sprintf "Proxy for internal proxy target type '%s' already exists, ignoring the internal proxy." tTarget.Value.FullName))
        else
            proxies.Add(tProxy, tTarget)  
            if isInternal then
                internalProxies.Add(tTarget, tProxy) |> ignore

    member this.ResolveProxySignature (meth: Method) =        
        if proxies.Count = 0 then meth else
        let m = meth.Value
        AST.Method { 
            m with
                Parameters = m.Parameters |> List.map (fun t -> t.MapTypeDefinitions this.FindProxied)
                ReturnType = m.ReturnType.MapTypeDefinitions this.FindProxied
        }

    member this.ResolveProxySignature (ctor: Constructor) =        
        if proxies.Count = 0 then ctor else
        let c = ctor.Value
        AST.Constructor {
            CtorParameters = c.CtorParameters |> List.map (fun t -> t.MapTypeDefinitions this.FindProxied)
        }

    member this.ResolveProxySignature (mem: Member) =        
        if proxies.Count = 0 then mem else
        match mem with
        | Member.Method (i, m) ->
            Member.Method (i, this.ResolveProxySignature m)
        | Member.Implementation (i, m) ->
            Member.Implementation (this.FindProxied i, this.ResolveProxySignature m)
        | Member.Override (t, m) ->
            Member.Override (this.FindProxied t, this.ResolveProxySignature m)
        | Member.Constructor c ->
            Member.Constructor (this.ResolveProxySignature c)
        | Member.StaticConstructor ->
            Member.StaticConstructor

    member this.AddQuotedArgMethod(typ, m, a) =
        compilingQuotedArgMethods.Add((typ, m), a)

    member this.TryLookupQuotedArgMethod(typ, m) =
        match compilingQuotedArgMethods.TryFind(typ, m) with
        | Some x -> Some x
        | None ->
            match meta.Classes.TryFind(typ) with
            | Some (_, _, Some c) ->
                match c.QuotedArgMethods.TryFind(m) with
                | Some x -> Some x
                | None -> None
            | _ -> None

    member this.GetFakeMethodForCtor(c: Constructor) =
        Method {
            MethodName = ".ctor"
            Parameters = c.Value.CtorParameters
            ReturnType = Type.VoidType
            Generics = 0
        }
        
    member this.AddQuotedConstArgMethod(typ: TypeDefinition, c: Constructor, a) =
        compilingQuotedArgMethods.Add((typ, this.GetFakeMethodForCtor(c)), a)

    member this.TryLookupQuotedConstArgMethod(typ: TypeDefinition, c: Constructor) =
        this.TryLookupQuotedArgMethod(typ, this.GetFakeMethodForCtor(c))

    member this.AddQuotedMethod(typ, m) =        
        if quotedMethods |> Seq.contains (typ, m) |> not then
            quotedMethods.Add((typ, m))

    member this.AddClass(typ, cls) =
        try
            notResolvedClasses.Add(typ, cls)
        with _ ->
            if cls.IsProxy then
                if Option.isSome cls.StrongName then
                    this.AddError(None, SourceError ("Proxy extension can't be strongly named: " + typ.Value.FullName))
                elif Option.isSome cls.BaseClass && cls.BaseClass.Value.Entity <> Definitions.Object then
                    this.AddError(None, SourceError ("Proxy extension can't have a non-Object base class: " + typ.Value.FullName))
                else 
                    let orig = notResolvedClasses.[typ]                    
                    notResolvedClasses.[typ] <-
                        { orig with
                            Requires = cls.Requires @ orig.Requires
                            Members = cls.Members @ orig.Members
                        }
            else
                this.AddError(None, SourceError ("Multiple definitions found for type: " + typ.Value.FullName))

    member this.AddInterface(typ, intf) =
        try
            notResolvedInterfaces.Add(typ, intf)
        with _ ->
            this.AddError(None, SourceError ("Multiple definitions found for type: " + typ.Value.FullName))
    
    member this.TypeAddress(typ: TypeDefinition, hasWSPrototype) =
        let m = { Assembly = this.AssemblyName; Name = typ.Value.FullName.Replace('+', '.') }
        if hasWSPrototype then
            Address.TypeDefaultExport m
        else 
            Address.TypeModuleRoot m

    member this.ProcessCustomType(typ: TypeDefinition, ct) =
        let getAddr hasWSPrototype = 
            this.TypeAddress(typ, hasWSPrototype)
        let addr, cls = 
            match classes.TryFind typ with
            | Some ({ Address = []}, _, cls) -> getAddr (cls |> Option.exists (fun c -> c.HasWSPrototype)), cls
            | Some (a, _, cls) -> a, cls
            | _ -> getAddr false, None
        classes.Current.[typ] <- (addr, ct, cls)
        match ct with
        | FSharpUnionInfo u ->
            for c in u.Cases do
                match c.Kind with
                | ConstantFSharpUnionCase Null -> ()
                | _ ->
                let cAddr = addr.Sub c.Name 
                let cTyp =
                    TypeDefinition {
                        Assembly = typ.Value.Assembly
                        FullName = typ.Value.FullName + "+" + c.Name
                    } 
                classes.Add(cTyp, (cAddr, FSharpUnionCaseInfo c, None))
        | _ -> ()          
    
    member this.AddCustomType(typ: TypeDefinition, ct) =
        notResolvedCustomTypes.Add(typ, ct)

    member this.HasCustomTypeInfo(typ) =
        notResolvedCustomTypes.ContainsKey typ || classes.ContainsKey typ 

    member this.GetCustomType(typ) =
        if interfaces.ContainsKey typ then NotCustomType else
        let typ' = this.FindProxied typ
        match classes.TryFind typ' with
        | Some (addr, res, _) ->
            res
        | _ ->
            let res = this.CustomTypesReflector typ'
            this.ProcessCustomType(typ, res)
            res

    member private this.GetClassOrCustomType(typ) =
        match classes.TryFind typ with
        | Some (_, _, Some c) -> Choice1Of2 c
        | Some (_, c, None) -> Choice2Of2 c
        | None -> Choice2Of2 (this.GetCustomType typ)

    member this.TryLookupClassInfo (typ, ?staticAccess) =   
        let cl =
            if defaultArg staticAccess false && mergedProxies.Contains typ then
                classes.Original
            else
                classes
        match cl.TryFind(this.FindProxied typ) with
        | Some (a, _, Some cls) -> Some (a, cls)
        | _ -> None

    member this.TryLookupClassAddressOrCustomType typ =   
        match classes.TryFind(this.FindProxied typ), this.GetCustomType typ with
        | Some (addr, _, Some c), _ when c.HasWSPrototype -> 
            Choice1Of2 addr
        | Some (addr, _, _), NotCustomType -> 
            Choice1Of2 addr
        | _, ct -> Choice2Of2 ct
    
    member this.TryLookupInterfaceInfo typ =   
        interfaces.TryFind(this.FindProxied typ)
    
    member this.GetAbtractMethodGenerics typ meth =
        let typ = this.FindProxied typ
        let overrideOpt =
            match classes.TryFind typ with
            | Some (_, _, Some ci) ->
                match ci.Methods.TryFind meth with
                | Some m -> Some (Array.ofList (ci.Generics @ m.Generics))
                | _ ->
                    match compilingMethods.TryFind (typ, meth) with
                    | Some (_, mg, _) -> Some (Array.ofList (ci.Generics @ mg))
                    | _ -> None
            | _ -> None
        match overrideOpt with
        | Some res -> res
        | _ ->
            match interfaces.TryFind typ with
            | Some ii -> 
                let (_, _, mgen) = ii.Methods.[meth]
                Array.ofList (ii.Generics @ mgen)
            | _ ->
                failwithf "Error looking up abstract method generics %s.%s" typ.Value.FullName meth.Value.MethodName

    member this.GetMethods typ =
        compilingMethods |> Seq.choose (fun (KeyValue ((td, m), _)) ->
            if td = typ then Some m else None
        ) |> Seq.append (
            match this.TryLookupClassInfo typ with
            | Some (_, cls) when not (this.IsInterface(typ)) -> cls.Methods.Keys :> _ seq
            | _ ->
            match this.TryLookupInterfaceInfo typ with
            | Some intf -> intf.Methods.Keys :> _ seq
            | _ ->
                Seq.empty
        )

    member this.IsImplementing (typ, intf) : bool option =
        classes.TryFind(this.FindProxied typ)
        |> Option.bind (fun (_, _, cls) -> cls)
        |> Option.map (fun cls ->
            cls.Implements |> List.contains intf
            || cls.BaseClass |> Option.exists (fun b -> this.IsImplementing(b.Entity, intf) |> Option.exists id) 
        )

    member this.HasType(typ) =
        let typ = this.FindProxied typ
        classes.ContainsKey typ || interfaces.ContainsKey typ

    member this.IsInterface(typ) =
        let typ = this.FindProxied typ
        interfaces.ContainsKey typ

    member this.IsInterfaceMethodWithImpl(typ, meth) =
        let typ = this.FindProxied typ
        let compiled =
            // interfaces with default implementations are also stored in classes dictionary
            match classes.TryFind(typ) with
            | Some (_, _, Some cls) -> cls.Methods.ContainsKey meth
            | _ -> false
        compiled || compilingMethods.ContainsKey (typ, meth)

    member this.ConstructorExistsInMetadata (typ, ctor) =
        let typ = this.FindProxied typ
        match classes.TryFind typ with
        | Some (_, _, Some cls) ->
            cls.Constructors.ContainsKey ctor || compilingConstructors.ContainsKey (typ, ctor)
        | _ -> false

    member this.MethodExistsInMetadata (typ, meth) =
        let typ = this.FindProxied typ
        match interfaces.TryFind typ with
        | Some intf -> 
            intf.Methods.ContainsKey meth
        | _ -> false
        || 
        match classes.TryFind typ with
        | Some (_, _, Some cls) ->
            cls.Methods.ContainsKey meth || compilingMethods.ContainsKey (typ, meth)
        | _ -> false

    member private this.LookupMethodInfoInternal(typ, meth, noDefIntfImpl) = 
        let typ = this.FindProxied typ
                
        let tryFindClassMethod () =
            match classes.TryFind typ with
            | Some (_, _, Some cls) ->
                let applyClsMacros cm =
                    List.foldBack (fun (m, p) fb -> Some (Macro (m, p, fb))) cls.Macros cm |> Option.get    
                match cls.Methods.TryFind meth with
                | Some m -> 
                    if List.isEmpty cls.Macros then
                        Compiled (m.CompiledForm, m.Optimizations, m.Generics, m.Expression)
                    else
                        Compiled (applyClsMacros (Some m.CompiledForm), m.Optimizations, m.Generics, m.Expression)
                | _ -> 
                    match compilingMethods.TryFind (typ, meth) with
                    | Some ((cm, p, e) as m) -> 
                        if List.isEmpty cls.Macros then
                            Compiling m
                        else
                            Compiling ({ cm with CompiledMember = applyClsMacros (Some cm.CompiledMember) }, p, e)
                    | _ -> 
                        if not (List.isEmpty cls.Macros) then
                            let info = applyClsMacros None
                            Compiled (info, Optimizations.None, [], Undefined)
                        else
                            match this.GetCustomType typ with
                            | NotCustomType -> 
                                let mName = meth.Value.MethodName
                                let candidates = 
                                    [
                                        for m in cls.Methods.Keys do
                                            if m.Value.MethodName = mName then
                                                yield m
                                        for t, m in compilingMethods.Keys do
                                            if typ = t && m.Value.MethodName = mName then
                                                yield m
                                    ]
                                let bres =
                                    match cls.BaseClass with
                                    | Some bTyp -> 
                                        match this.LookupMethodInfoInternal(bTyp.Entity, meth, noDefIntfImpl) with
                                        | LookupMemberError _ -> None
                                        | res -> Some res
                                    | None -> None
                                match bres with
                                | Some m -> m
                                | None ->
                                    if List.isEmpty candidates then
                                        let names =
                                            seq {
                                                for m in cls.Methods.Keys do
                                                    yield m.Value.MethodName
                                                for t, m in compilingMethods.Keys do
                                                    if typ = t then
                                                        yield m.Value.MethodName
                                            }
                                            |> Seq.distinct |> List.ofSeq
                                        LookupMemberError (MethodNameNotFound (typ, meth, names))
                                    else
                                        LookupMemberError (MethodNotFound (typ, meth, candidates))
                            | i -> CustomTypeMember i
                |> Some
            | _ -> None

        let tryFindInterfaceMethod () = 
            match interfaces.TryFind typ with
            | Some intf -> 
                match intf.Methods.TryFind meth with
                | Some (m, mt, gpars) ->
                    Compiled (Instance (m, mt), Optimizations.None, gpars, Undefined)              
                | _ ->
                    let mName = meth.Value.MethodName
                    let candidates = 
                        [
                            for m in intf.Methods.Keys do
                                if m.Value.MethodName = mName then
                                    yield m
                        ]
                    if List.isEmpty candidates then
                        let names =
                            seq {
                                for m in intf.Methods.Keys do
                                    yield m.Value.MethodName
                            }
                            |> Seq.distinct |> List.ofSeq
                        LookupMemberError (MethodNameNotFound (typ, meth, names))
                    else
                        LookupMemberError (MethodNotFound (typ, meth, candidates))
                |> Some
            | _ -> None
            
        let fallback () =
            match this.GetCustomType typ with
            | NotCustomType -> LookupMemberError (TypeNotFound typ)
            | i -> CustomTypeMember i

        if noDefIntfImpl then
            tryFindInterfaceMethod ()
            |> Option.orElseWith tryFindClassMethod
            |> Option.defaultWith fallback
        else
            match tryFindClassMethod () with
            | Some (LookupMemberError _ as e) ->
                match tryFindInterfaceMethod () with
                | Some res -> res
                | _ -> e
            | Some res -> res
            | None ->
                tryFindInterfaceMethod ()
                |> Option.defaultWith fallback

    member this.LookupMethodInfo(typ, meth: Method, noDefIntfImpl) = 
        let m = meth.Value

        let otherType() = 
            match m.Parameters, m.ReturnType with
            | [ ConcreteType pt ], ConcreteType rt when pt.Generics = [] && rt.Generics = [] ->
                if pt.Entity = typ then
                    Some rt.Entity
                elif rt.Entity = typ then
                    Some pt.Entity
                else None
            | _ -> None

        let res = this.LookupMethodInfoInternal(typ, meth, noDefIntfImpl)
        if m.MethodName = "op_Explicit" then
            match res with
            | LookupMemberError _ ->
                match otherType() with
                | Some ot ->
                    match this.LookupMethodInfoInternal(ot, meth, noDefIntfImpl) with
                    | LookupMemberError _ -> 
                        let implicitMeth = Method { m with MethodName = "op_Implicit" }
                        match this.LookupMethodInfoInternal(typ, implicitMeth, noDefIntfImpl) with
                        | LookupMemberError _ -> 
                            match this.LookupMethodInfoInternal(ot, implicitMeth, noDefIntfImpl) with
                            | LookupMemberError _ -> res
                            | sres -> sres
                        | sres -> sres
                    | sres -> sres
                | _ -> res
            | res -> res
        elif m.MethodName = "op_Implicit" then
            match res with
            | LookupMemberError _ ->
                match otherType() with
                | Some ot ->
                    match this.LookupMethodInfoInternal(ot, meth, noDefIntfImpl) with
                    | LookupMemberError _ -> res
                    | sres -> sres
                | _ -> res
            | res -> res
        else
            res

    member this.LookupFieldInfo(typ, field) =
        let typ = this.FindProxied typ
        match this.GetClassOrCustomType typ with
        | Choice1Of2 cls ->
            match cls.Fields.TryFind field with
            | Some f -> CompiledField (f.CompiledForm, f.ReadOnly, f.Type)
            | _ -> 
                let gname = "get_" + field
                let getter =
                    cls.Methods |> Seq.tryPick (fun (KeyValue (m, i)) ->
                        if m.Value.MethodName = gname && List.isEmpty m.Value.Parameters then
                            Some m
                        else None
                    )
                let getter = 
                    match getter with
                    | Some _ -> getter
                    | _ ->
                        compilingMethods |> Seq.tryPick (fun (KeyValue ((td, m), i)) ->
                            if td = typ then
                                if m.Value.MethodName = gname && List.isEmpty m.Value.Parameters then
                                    Some m
                                else None
                            else None
                        )
                let sname = "set_" + field
                let setter =
                    cls.Methods |> Seq.tryPick (fun (KeyValue (m, i)) ->
                        if m.Value.MethodName = sname && List.length m.Value.Parameters = 1 then
                            Some m
                        else None
                    )
                let setter = 
                    match setter with
                    | Some _ -> setter
                    | _ ->
                        compilingMethods |> Seq.tryPick (fun (KeyValue ((td, m), i)) ->
                            if td = typ then
                                if m.Value.MethodName = sname && List.length m.Value.Parameters = 1 then
                                    Some m
                                else None
                            else None
                        )
                match getter, setter with
                | None, None ->
                    match this.GetCustomType typ with
                    | NotCustomType -> 
                        let bres =
                            match cls.BaseClass with
                            | Some bTyp ->
                                match this.LookupFieldInfo(bTyp.Entity, field) with
                                | LookupFieldError _ -> None
                                | f -> Some f
                            | _ ->
                                None
                        match bres with 
                        | Some f -> f
                        | None -> LookupFieldError (FieldNotFound (typ, field))
                    | i -> CustomTypeField i
                | _ -> 
                    PropertyField (getter, setter)
        | Choice2Of2 NotCustomType -> LookupFieldError (TypeNotFound typ)
        | Choice2Of2 i -> CustomTypeField i

    member this.LookupConstructorInfo(typ, ctor) =
        let typ = this.FindProxied typ
        match this.GetClassOrCustomType typ with
        | Choice1Of2 cls ->
            let applyClsMacros cm =
                List.foldBack (fun (m, p) fb -> Some (Macro (m, p, fb))) cls.Macros cm |> Option.get    
            match cls.Constructors.TryFind ctor with
            | Some c -> 
                if List.isEmpty cls.Macros then
                    Compiled (c.CompiledForm, c.Optimizations, cls.Generics, c.Expression)
                else
                    Compiled (applyClsMacros (Some c.CompiledForm), c.Optimizations, cls.Generics, c.Expression)
            | _ -> 
                match compilingConstructors.TryFind (typ, ctor) with
                | Some (m, e) -> 
                    if List.isEmpty cls.Macros then
                        Compiling (m, cls.Generics, e)
                    else
                        Compiling ({ m with CompiledMember = applyClsMacros (Some m.CompiledMember) }, cls.Generics, e)
                | _ -> 
                    if not (List.isEmpty cls.Macros) then
                        let info = applyClsMacros None
                        Compiled (info, Optimizations.None, [], Undefined)
                    else
                        match this.GetCustomType typ with
                        | NotCustomType -> 
                            let candidates = 
                                [
                                    yield! cls.Constructors.Keys
                                    for t, m in compilingConstructors.Keys do
                                        if typ = t then
                                            yield m
                                ]
                            LookupMemberError (ConstructorNotFound (typ, ctor, candidates))
                        | i -> CustomTypeMember i
        | Choice2Of2 NotCustomType -> LookupMemberError (TypeNotFound typ)
        | Choice2Of2 i -> CustomTypeMember i
        
    //member this.TryLookupStaticConstructorAddress(typ) =
    //    match classes.TryFind(this.FindProxied typ) with
    //    | Some (_, _, Some cls) ->
    //        match cls.StaticConstructor with
    //        | Some(_, GlobalAccess a) when a.Address.Value = [ "ignore" ] -> None
    //        | Some (cctor, _) -> Some cctor
    //        | None -> None
    //    | _ -> None

    member this.TryGetRecordConstructor(typ) =
        let typ = this.FindProxied typ
        match classes.TryFind typ with
        | Some (_, _, Some cls) ->
            match Seq.tryHead cls.Constructors.Keys with
            | Some _ as res -> res
            | _ ->
                compilingConstructors |> Seq.tryPick (fun (KeyValue ((td, c), _)) ->
                    if typ = td then Some c else None
                )
        | _ ->
            None

    member this.CompilingConstructors = compilingConstructors

    member this.CompilingMethods = compilingMethods  

    member this.AddCompiledMethod(typ, meth, info, opts, comp) =
        let typ = this.FindProxied typ 
        let _, gc, _ = compilingMethods.[typ, meth]
        compilingMethods.Remove(typ, meth) |> ignore
        match classes.TryFind typ with
        | Some (_, _, Some cls) ->
            match cls.Methods.TryFind meth with
            | Some { Expression = Undefined }
            | None ->    
                cls.Methods.[meth] <- { CompiledForm = info; Optimizations = opts; Generics = gc; Expression = comp }
            | _ ->
                failwithf "Method already added: %s %s" typ.Value.FullName (string meth.Value)
        | _ -> failwithf "Adding method to non-compiled method: %s %s" typ.Value.FullName (string meth.Value)

    member this.FailedCompiledMethod(typ, meth) =
        let typ = this.FindProxied typ 
        compilingMethods.Remove(typ, meth) |> ignore

    member this.AddCompiledConstructor(typ, ctor, info, opts, comp) = 
        let typ = this.FindProxied typ 
        compilingConstructors.Remove(typ, ctor) |> ignore
        let cls = assumeClass typ
        cls.Constructors.Add(ctor, { CompiledForm = info; Optimizations = opts; Expression = comp })

    member this.FailedCompiledConstructor(typ, ctor) =
        let typ = this.FindProxied typ 
        compilingConstructors.Remove(typ, ctor) |> ignore

    member this.CompilingStaticConstructors = compilingStaticConstructors

    member this.AddCompiledStaticConstructor(typ, cctor) =
        let typ = this.FindProxied typ 
        compilingStaticConstructors.Remove typ |> ignore
        match classes.TryFind typ with
        | Some (caddr, ct, Some cls) ->
            classes.[typ] <- (caddr, ct, Some { cls with StaticConstructor = Some cctor })
        | _ -> failwithf "Adding compiled static constructor to %s" typ.Value.FullName

    member this.CompilingImplementations = compilingImplementations

    member this.AddCompiledImplementation(typ, intf, meth, info, comp) =
        let typ = this.FindProxied typ 
        compilingImplementations.Remove(typ, intf, meth) |> ignore
        let cls = assumeClass typ
        cls.Implementations.Add((intf, meth), { CompiledForm = info; Expression = comp })

    member this.CompilingExtraBundles = compilingExtraBundles

    member this.AddCompiledExtraBundle(name, compiledEntryPoint) =
        let bundle = compilingExtraBundles.[name]
        compilingExtraBundles.Remove(name) |> ignore
        compiledExtraBundles.[name] <- { bundle with EntryPoint = compiledEntryPoint }

    member this.AddBundle(baseName, entryPoint, includeJsExports) =
        let shouldAdd name =
            not <| compilingExtraBundles.ContainsKey(name)
        let computedName =
            Seq.append
                (Seq.singleton baseName)
                (Seq.initInfinite <| fun i -> baseName + string i)
            |> Seq.find shouldAdd
        let node = ExtraBundleEntryPointNode(this.AssemblyName, computedName)
        compilingExtraBundles.Add(computedName, {
            EntryPoint = entryPoint
            Node = node
            IncludeJsExports = includeJsExports
        })
        { AssemblyName = this.AssemblyName; BundleName = computedName }

    member this.TypesNeedingDeserialization = typesNeedingDeserialization

    member this.JSImport(export: string option, from: string) =
        Address.Import this.AssemblyName (export, from)

    member this.AddJSImport(export: string option, from: string) =
        GlobalAccess (this.JSImport(export, from))

    member this.GetMethodNameAndKind (m: Method) =
        let mname = m.Value.MethodName 
        if mname.StartsWith("get_") && m.Value.Parameters.IsEmpty then
            mname[4..], MemberKind.Getter         
        elif mname.StartsWith("set_") && m.Value.Parameters.Length = 1 then
            mname[4..], MemberKind.Setter                       
        else
            mname, MemberKind.Simple

    member this.GetMemberNameAndKind (nr: NotResolvedMember) =
        match nr with
        | NotResolvedMember.Method (m, _) -> this.GetMethodNameAndKind m 
        | _ -> "", MemberKind.Simple

    member this.Resolve () =
        
        let printerrf x = Printf.kprintf (fun s -> this.AddError (None, SourceError s)) x

        let add k v (d: IDictionary<_,_>) =
            try 
                d.Add(k, v)
            with _ ->
                printerrf "Duplicate client-side representation found for member: %A" k

        let addType (k: TypeDefinition) v (d: IDictionary<_,_>) =
            try 
                d.Add(k, v)
            with _ ->
                printerrf "Duplicate client-side representation found for type: %s" k.Value.AssemblyQualifiedName

        let addMethod (t: TypeDefinition) (k: Method) v (d: IDictionary<_,_>) =
            try 
                d.Add(k, v)
            with _ ->
                printerrf "Duplicate client-side representation found for method: %s.%s" t.Value.FullName k.Value.MethodName

        let addCMethod (t: TypeDefinition, m: Method) v (d: IDictionary<_,_>) =
            try 
                d.Add((t, m), v)
            with _ ->
                printerrf "Duplicate client-side representation found for method: %s.%s" t.Value.FullName m.Value.MethodName

        let stronglyNamedTypes = ResizeArray()
        let remainingTypes = ResizeArray()
        let clStatics = Dictionary<TypeDefinition, Address>()

        let rec resolveInterface (typ: TypeDefinition) (nr: NotResolvedInterface) =
            notResolvedInterfaces.Remove typ |> ignore
            let clsOpt = notResolvedClasses.TryFind typ
            // If this is an interface used for remoting, do not process further
            match clsOpt with
            | Some cls when cls.Members |> List.exists (function NotResolvedMember.Method (_, { Kind = N.Remote _}) -> true | _ -> false) -> ()
            | _ ->
            
            let allMembers = HashSet()
            let allNames = HashSet()
            let extended = Dictionary() // has Some value if directly extended and JavaScript annotated

            let rec addInherited (i: TypeDefinition) (n: InterfaceInfo) =
                for i in n.Extends do
                    let i = i.Entity
                    let alreadyExtended = extended.ContainsKey i
                    extended.[i] <- None
                    if not alreadyExtended then 
                        interfaces.TryFind i |> Option.iter (addInherited i)
                for KeyValue(m, (n, k, _)) in n.Methods do
                    if not (allMembers.Add (i, m)) then
                        if not (allNames.Add (n, k)) then
                            printerrf "Interface method name collision: %s on %s" n typ.Value.FullName
            
            for ei in nr.Extends do
                let i = ei.Entity
                if not (extended.ContainsKey i) then
                    notResolvedInterfaces.TryFind i |> Option.iter (resolveInterface i)       
                    match interfaces.TryFind i with
                    | Some ii ->
                        extended.Add(i, Some ei) |> ignore
                        addInherited i ii
                    | _ ->
                        extended.Add(i, None) |> ignore
                        
            if allMembers.Count = 0 && List.isEmpty nr.NotResolvedMethods then () else
            
            let resMethods = Dictionary()
                            
            for m, n, c in nr.NotResolvedMethods do
                match n with
                | Some n -> 
                    let _, k = this.GetMethodNameAndKind(m)
                    if not (allNames.Add (n, k)) then
                        printerrf "Explicitly declared interface method name collision: %s on %s" n typ.Value.FullName
                    resMethods.Add(m, (n, k, c))
                | _ -> ()
            
            let intfName = 
                match nr.StrongName with
                | Some "" -> ""
                | Some sn -> sn + "$"
                | _ ->                     
                    (cleanName typ.Value.FullName) + "$"

            for m, n, c in nr.NotResolvedMethods do
                match n with
                | None ->
                    let mname, k = this.GetMethodNameAndKind(m)
                    let rename =
                        if nr.IsStub then
                            mname
                        else
                            intfName + mname
                    let n = Resolve.getRenamedWithKind (rename) k allNames
                    resMethods.Add(m, (n, k, c))
                | _ -> ()

            let resNode =
                {
                    Address = this.TypeAddress(typ, false)
                    Extends = extended.Values |> Seq.choose id |> List.ofSeq
                    Methods = resMethods
                    Generics = nr.Generics
                    Type = nr.Type
                }
            interfaces |> addType typ resNode
            match nr.StrongName with
            | Some sn ->
                stronglyNamedTypes.Add (typ, sn, false)
            | _ -> 
                remainingTypes.Add (typ, false)

            let cls =
                match clsOpt with
                | Some cls -> cls
                | _ ->
                    let cls =
                        {
                            StrongName = None
                            BaseClass = None
                            Implements = []
                            Generics = []
                            Requires = []
                            Members = []
                            Kind = NotResolvedClassKind.Class
                            IsProxy = false
                            Macros = []
                            ForceNoPrototype = false
                            ForceAddress = false
                            Type = None
                            SourcePos = 
                                {
                                    FileName = ""
                                    Start = 0, 0
                                    End = 0, 0
                                }
                        }
                    cls
            let isFunc = 
                let m = isFunctionMethodForInterface typ
                let x = Id.New("x", typ = TSType TSType.Any)
                let intfType =
                    let tt = TSType.Importing (this.TypeAddress(typ, true))
                    match m.Value.Generics with
                    | 0 -> tt
                    | g ->
                        TSType.Generic(tt, List.init g TSType.Param)
                let returnType =
                    Some (TSType (TSType.TypeGuard(x, intfType)))
                let body =
                    if Seq.isEmpty allNames then
                        Function([x], None, returnType, Return (Value (Bool true)))
                    else         
                        let check = 
                            allNames
                            |> Seq.map fst
                            |> Seq.distinct
                            |> Seq.map (fun name -> Binary(Value (String name), BinaryOperator.``in``, Var x))
                            |> Seq.reduce (^&&)                            
                        Function([x], None, returnType, Return check)
                let nrm =
                    {
                        Kind = NotResolvedMemberKind.Static
                        StrongName = None
                        Generics = []
                        Macros = []
                        Generator = None
                        Compiled = true
                        Pure = true
                        FuncArgs = None
                        Args = [ x ]
                        Body = body
                        Requires = []
                        Warn = None
                        JavaScriptOptions = JavaScriptOptions.None
                    }
                NotResolvedMember.Method(m, nrm)
            notResolvedClasses[typ] <- { cls  with Members = isFunc :: cls.Members }
        
        while notResolvedInterfaces.Count > 0 do
            let (KeyValue(typ, nr)) = Seq.head notResolvedInterfaces  
            resolveInterface typ nr

        let unresolvedCctor = Some Empty

        let resNode (t, p) =
            ResourceNode (t, p |> Option.map ParameterObject.OfObj)

        let asmNodeIndex = 
            if hasGraph then graph.AddOrLookupNode(AssemblyNode (this.AssemblyName, true, false)) else 0
        if hasGraph then
            for req in this.AssemblyRequires do
                graph.AddEdge(asmNodeIndex, resNode req)

        let objectMethods =
            HashSet [ "toString"; "Equals"; "GetHashCode" ]

        let typesWithSingleConstructor = HashSet()
        let classHasPrototype = Dictionary()
        classHasPrototype.Add(Definitions.Exception, true)
        let rec doesClassHavePrototype typ =
            match classHasPrototype.TryFind(typ) with
            | Some res -> res
            | None ->
                let res =
                    match classes.TryFind typ with
                    | Some (a, _, Some c) ->
                        match a.Module with
                        | DotNetType _ -> c.HasWSPrototype
                        | _ -> true
                    | _ ->
                        match notResolvedClasses.TryFind typ with
                        | Some cls ->
                            let baseCls = 
                                cls.BaseClass |> Option.bind (fun b ->
                                    let be = this.FindProxied b.Entity
                                    if classes.ContainsKey be || notResolvedClasses.ContainsKey be then Some { b with Entity = be } else None
                                )
                            baseCls |> Option.forall (fun bc -> doesClassHavePrototype bc.Entity) && 
                            hasWSPrototype cls.Kind baseCls cls.Members
                        | _ -> 
                            true
                classHasPrototype.Add(typ, res)
                res

        // initialize all class entries
        for KeyValue(typ, cls) in notResolvedClasses do
            if cls.ForceNoPrototype then
                for mem in cls.Members do
                    match mem with
                    | NotResolvedMember.Method (_, mi) when mi.Kind = NotResolvedMemberKind.Instance ->
                        mi.Kind <- NotResolvedMemberKind.AsStatic         
                        mi.Body <- 
                            match mi.Body with
                            | Function(args, thisVar, typ, b) ->
                                let thisVar = thisVar |> Option.defaultWith Id.NewThis
                                Function (thisVar :: args, None, typ, b)
                            | _ ->
                                failwith "Unexpected: instance member not a function"
                    | NotResolvedMember.Constructor (_, mi) when mi.Kind = NotResolvedMemberKind.Constructor ->
                        mi.Kind <- NotResolvedMemberKind.AsStatic  
                        mi.Body <- 
                            match mi.Body with
                            | Function(args, thisVar, typ, b) ->
                                match thisVar with
                                | Some t ->
                                    let body =
                                        Return(Let(t, Object [], Sequential [StatementExpr (b, None); Var t]))
                                    Function (args, None, typ, body)
                                | None -> 
                                    failwith "Unexpected: constructor not using 'this'"
                            | _ ->
                                failwith "Unexpected: instance member not a function"
                    | _ -> ()
            let isInterfaceProxy =
                cls.ForceNoPrototype && interfaces.ContainsKey typ
            
            let cctor = cls.Members |> Seq.tryPick (function M.StaticConstructor e -> Some e | _ -> None)
            let baseCls =
                cls.BaseClass |> Option.bind (fun b ->
                    let be = this.FindProxied b.Entity
                    if classes.ContainsKey be || notResolvedClasses.ContainsKey be then Some { b with Entity = be } else None
                )
            let implements =
                cls.Implements |> List.filter (fun i -> 
                    let ie = i.Entity
                    interfaces.ContainsKey ie && 
                    cls.Members |> List.exists (function
                        | NotResolvedMember.Method (_, mi) ->
                            match mi.Kind with
                            | NotResolvedMemberKind.Implementation ii
                            | NotResolvedMemberKind.InlineImplementation ii -> ii = ie
                            | _ -> false
                        | _ -> false
                    )
                )
            let hasWSPrototype = doesClassHavePrototype typ
            let isStub = cls.Kind = NotResolvedClassKind.Stub
            let methods =
                match classes.TryFind typ with
                | Some (_, _, Some c) -> 
                    mergedProxies.Add typ |> ignore
                    MergedDictionary c.Methods :> IDictionary<_,_>
                | _ -> Dictionary() :> _
            let resCls =
                {
                    BaseClass = if hasWSPrototype then baseCls else None
                    Implements = implements
                    Generics = cls.Generics
                    Constructors = Dictionary() 
                    Fields = Dictionary() 
                    StaticConstructor = if Option.isSome cctor then unresolvedCctor else None 
                    Methods = methods
                    Implementations = Dictionary()
                    QuotedArgMethods = Dictionary()
                    HasWSPrototype = hasWSPrototype
                    IsStub = isStub
                    Macros = cls.Macros |> List.map (fun (m, p) -> m, p |> Option.map ParameterObject.OfObj)
                    Type =
                        match cls.Type with
                        | Some _ as t -> t
                        | None -> if isStub then Some TSType.Any else None
                }
            
            let clAddress = 
                let defCtorInline =
                    cls.Members |> Seq.tryPick (fun m ->
                        match m with
                        | NotResolvedMember.Constructor(ctor, { Kind = N.Inline _; Body = expr }) when ctor = ConstructorInfo.Default() -> Some expr
                        | _ -> None
                    )    
                match defCtorInline with
                | Some (IgnoreSourcePos.New(IgnoreSourcePos.GlobalAccess ctorAddr, _, [])) ->
                    ctorAddr
                | _ -> 
                    this.TypeAddress(typ, hasWSPrototype)

            match notResolvedCustomTypes.TryFind typ with
            | Some ct ->
                classes |> addType typ (clAddress, ct, Some resCls)
                notResolvedCustomTypes.Remove typ |> ignore
            | _ ->
                classes |> addType typ (clAddress, NotCustomType, Some resCls)
            
            if cls.Members |> Seq.filter (function | NotResolvedMember.Constructor (_, { Kind = N.Constructor }) -> true | _ -> false) |> Seq.tryExactlyOne |> Option.isSome then             
                typesWithSingleConstructor.Add(typ) |> ignore

            // set up dependencies
            if hasGraph then
                let clsNodeIndex = graph.AddOrLookupNode(TypeNode typ)
                graph.AddEdge(clsNodeIndex, asmNodeIndex)
                for req in cls.Requires do
                    graph.AddEdge(clsNodeIndex, resNode req)
                cls.BaseClass |> Option.iter (fun b -> graph.AddEdge(clsNodeIndex, TypeNode (this.FindProxied b.Entity)))
                for m in cls.Members do
                    match m with
                    | M.Constructor (ctor, { Kind = k; Requires = reqs }) -> 
                        match k with
                        | N.NoFallback -> ()
                        | _ ->
                            let cNode = graph.AddOrLookupNode(ConstructorNode(typ, ctor))
                            graph.AddEdge(cNode, clsNodeIndex)
                            for req in reqs do
                                graph.AddEdge(cNode, resNode req)
                    | M.Method (meth, { Kind = k; Requires = reqs }) -> 
                        match k with
                        | N.Override btyp ->
                            let btyp = this.FindProxied btyp  
                            let mNode = graph.AddOrLookupNode(MethodNode(typ, meth))
                            graph.AddEdge(mNode, AbstractMethodNode(btyp, meth))
                            graph.AddOverride(typ, btyp, meth)
                            graph.AddEdge(mNode, clsNodeIndex)
                            for req in reqs do
                                graph.AddEdge(mNode, resNode req)
                        | N.Implementation intf 
                        | N.MissingImplementation intf ->
                            let intf = this.FindProxied intf 
                            let mNode = graph.AddOrLookupNode(ImplementationNode(typ, intf, meth))
                            graph.AddImplementation(typ, intf, meth)
                            graph.AddEdge(mNode, clsNodeIndex)
                            for req in reqs do
                                graph.AddEdge(mNode, resNode req)
                        | N.Abstract ->
                            let mNode = graph.AddOrLookupNode(MethodNode(typ, meth))
                            graph.AddEdge(mNode, AbstractMethodNode(typ, meth))
                            graph.AddEdge(mNode, clsNodeIndex)
                            for req in reqs do
                                graph.AddEdge(mNode, resNode req)
                        | N.NoFallback -> ()
                        | _ -> 
                            let mNode = graph.AddOrLookupNode(MethodNode(typ, meth))
                            graph.AddEdge(mNode, clsNodeIndex)
                            for req in reqs do
                                graph.AddEdge(mNode, resNode req)
                            if isInterfaceProxy then
                                graph.AddEdge(AbstractMethodNode(typ, meth), mNode) 
                                 
                    | M.Field (_, f) ->
                        let rec addTypeDeps (t: Type) =
                            match t with
                            | ConcreteType c ->
                                graph.AddEdge(clsNodeIndex, TypeNode(c.Entity))
                                c.Generics |> List.iter addTypeDeps
                            | ArrayType(t, _) -> addTypeDeps t
                            | TupleType (ts, _) -> ts |> List.iter addTypeDeps
                            | _ -> ()
                        addTypeDeps f.FieldType
                    | _ -> ()

        classes.Current
        |> Dict.choose (fun (_, _, cls) -> cls)
        |> Resolve.addInherits resolver

        if hasGraph then
            for KeyValue(ct, (_, cti, _)) in classes.Current do
                let clsNodeIndex = lazy graph.AddOrLookupNode(TypeNode ct)
                let rec addTypeDeps (t: Type) =
                    match t with
                    | ConcreteType c ->
                        graph.AddEdge(clsNodeIndex.Value, TypeNode(c.Entity))
                        c.Generics |> List.iter addTypeDeps
                    | ArrayType(t, _) -> addTypeDeps t
                    | TupleType (ts, _) -> ts |> List.iter addTypeDeps
                    | _ -> ()
                let unionCase (uci: FSharpUnionCaseInfo) =
                    match uci.Kind with
                    | NormalFSharpUnionCase fs ->
                        for f in fs do addTypeDeps f.UnionFieldType
                    | _ -> ()
                match cti with
                | FSharpRecordInfo fs ->
                    for f in fs do addTypeDeps f.RecordFieldType
                | FSharpUnionInfo ui ->
                    for uci in ui.Cases do unionCase uci
                | FSharpUnionCaseInfo uci -> unionCase uci
                | _ -> ()

            for KeyValue(ct, i) in interfaces.Current do
                let intfNodeIndex = lazy graph.AddOrLookupNode(TypeNode ct)
                for ii in i.Extends do
                    graph.AddEdge(intfNodeIndex.Value, TypeNode(ii.Entity))    

        let withMacros (nr : NotResolvedMethod) woMacros =
            if List.isEmpty nr.Macros then woMacros else
                if nr.Kind = N.NoFallback then None else Some woMacros
                |> List.foldBack (fun (p, o) fb -> Some (Macro(p, o |> Option.map ParameterObject.OfObj, fb))) nr.Macros
                |> Option.get

        let compiledStaticMember n k p typ (nr : NotResolvedMethod) =
            match clStatics.TryGetValue(typ) with
            | true, a ->
                GlobalFunc (a.Sub(n), false)
            | _ ->
                match nr.Kind with
                | N.Quotation _
                | N.Static -> 
                    if mergedProxies.Contains typ then
                        match this.TryLookupClassInfo(typ) with
                        | Some (a, _) -> GlobalFunc (a.Sub(n), false)
                        | _ -> Func (n, false)
                    elif p then 
                        Static (n, false, k) 
                    else 
                        Func (n, false)
                | N.Constructor -> 
                    if typesWithSingleConstructor.Contains typ then
                        New None
                    else
                        New (Some n)
                | N.AsStatic -> Func (n, true)
                | N.Remote (_, h, _, _, _, args) -> Remote(n, h, args.IsSome)
                | _ -> failwith "Invalid static member kind"
                |> withMacros nr        

        let compiledNoAddressMember (nr : NotResolvedMethod) =
            match nr.Kind with
            | N.Inline ta -> Inline (true, ta)
            | N.NoFallback -> Inline (true, false) // will be erased
            | _ -> failwith "Invalid not compiled member kind"
            |> withMacros nr

        let compiledInstanceMember (name: string) k (nr: NotResolvedMethod) =
            match nr.Kind with
            | N.Instance  
            | N.Abstract
            | N.Override _  
            | N.Implementation _ -> Instance (name, k)
            | _ -> failwith "Invalid instance member kind"
            |> withMacros nr

        let notVirtual k =
            match k with
            | N.Abstract 
            | N.Override _
            | N.Implementation _ -> false
            | _ -> true

        let opts isPure (nr: NotResolvedMethod) =
            {
                IsPure = isPure
                FuncArgs = nr.FuncArgs
                Warn = nr.Warn 
            }

        let toCompilingMember (nr : NotResolvedMethod) (comp: CompiledMember) =
            {
                CompiledMember = comp
                NotVirtual = notVirtual nr.Kind
                Optimizations = opts nr.Pure nr
                JavaScriptOptions = nr.JavaScriptOptions
                Generator = nr.Generator
            }

        //let setClassAddress typ (clAddr: PlainAddress) =
        //    match classes.Current.TryFind typ with
        //    | Some (addr, ct, cls) -> classes.Current.[typ] <- (this.LocalAddress clAddr, ct, cls)
        //    | None -> ()

        //let setInterfaceAddress typ clAddr =
        //    let res = interfaces.[typ]
        //    interfaces.[typ] <- { res with Address = this.LocalAddress clAddr }
            
        // split to resolve steps
        let fullyNamedStaticMembers = ResizeArray()
        let remainingNamedStaticMembers = Dictionary()
        let namedInstanceMembers = Dictionary() // includes implementations
        let remainingStaticMembers = Dictionary()
        let remainingInstanceMembers = Dictionary() // includes overrides
        let missingImplementations = ResizeArray()
        
        for KeyValue(typ, cls) in notResolvedClasses do
            let namedCls =
                match cls.StrongName with
                | Some sn ->
                    stronglyNamedTypes.Add (typ, sn, true)
                    true
                | _ -> 
                    remainingTypes.Add (typ, true)
                    false
            
            let cc = assumeClass typ
            // merging abstract and default entries for methods
            let members =
                let abstractAndOverrideMethods, otherMembers =
                    cls.Members
                    |> List.partition (function 
                        | M.Method (_, { Kind = N.Abstract | N.Override _ }) -> true 
                        | _ -> false
                    )
                
                let mergeVirtual mdef (abs: NotResolvedMethod) (ovr: NotResolvedMethod) =
                    let m =
                        { ovr with
                            Kind = N.Abstract
                            StrongName = abs.StrongName |> Option.orElse ovr.StrongName
                        }
                    M.Method (mdef, m)
                           
                let mergedMethods =
                    abstractAndOverrideMethods
                    |> List.groupBy (function
                        | M.Method (mdef, _) -> mdef
                        | _ -> failwith "impossible, must be a method"
                    )
                    |> Seq.map (fun (mdef, ms) ->
                        match ms with
                        | [ m ] -> m
                        | [ M.Method (_, ({ Kind = N.Abstract } as a)); 
                            M.Method (_, ({ Kind = N.Override _ } as b)) ]
                             -> mergeVirtual mdef a b
                        | [ M.Method (_, ({ Kind = N.Override _ } as a)); 
                            M.Method (_, ({ Kind = N.Abstract } as b)) ]
                                -> mergeVirtual mdef b a
                        | _ -> 
                            printerrf "Unexpected definitions found for a method in type %s: %A - %A" typ.Value.FullName mdef ms
                            ms.Head
                    )

                Seq.append mergedMethods otherMembers

            let mutable hasInlinedCtor = false

            for m in members do
                
                let strongName, isStatic, isError =  
                    match m with
                    | M.Constructor (_, { StrongName = sn; Kind = k }) 
                    | M.Method (_, { StrongName = sn; Kind = k }) -> 
                        match k with
                        | N.Override _
                        | N.Implementation _ ->
                            if Option.isSome sn then
                                match m with 
                                | M.Method (mdef, _) ->
                                    printerrf "Interface or implementation cannot be explicity named: %s.%s" typ.Value.FullName mdef.Value.MethodName 
                                | _ -> failwith "impossible: constructor cannot be override or implementation"
                                None, None, true
                            else 
                                None, Some false, false
                        | N.MissingImplementation _ -> None, Some false, false
                        | N.Abstract
                        | N.Instance -> sn, Some false, false
                        | N.InlineImplementation _ -> None, Some false, false
                        | N.Static
                        | N.AsStatic
                        | N.Remote _
                        | N.Constructor -> sn, Some true, false
                        | N.Quotation (pos, argNames) -> 
                            match m with 
                            | M.Method (mdef, _) ->                     
                                try 
                                    quotations.Add(pos, (typ, mdef, argNames))
                                with _ ->
                                    printerrf "Cannot have two instances of quoted JavaScript code at the same location of files with the same name: %s (%i, %i - %i, %i)"
                                        pos.FileName (fst pos.Start) (snd pos.Start) (fst pos.End) (snd pos.End)
                            | _ -> failwith "Quoted javascript code must be inside a method"
                            sn, Some true, false 
                        | N.Inline _
                        | N.NoFallback -> None, None, false
                    | M.Field (_, { StrongName = sn; IsStatic = s }) -> 
                        sn, Some s, false 
                    | M.StaticConstructor _ -> None, Some true, false
                
                if isError then () else
                
                let canHavePrototype = not cls.ForceNoPrototype
                match strongName, isStatic with
                | Some sn, Some true ->
                    let multiPart = sn.Contains "."
                    if namedCls || multiPart then
                        if multiPart && not (List.isEmpty cls.Generics) then
                            let m = 
                                match m with
                                | NotResolvedMember.Constructor (c, nr) -> 
                                    NotResolvedMember.Constructor (c, { nr with Generics = cls.Generics @ nr.Generics })
                                | NotResolvedMember.Method (m, nr) -> 
                                    NotResolvedMember.Method (m, { nr with Generics = cls.Generics @ nr.Generics })
                                | NotResolvedMember.Field _ -> 
                                    printerrf "Field of %s cannot have a fully qualified strong name %s" typ.Value.FullName sn
                                    m
                                | NotResolvedMember.StaticConstructor _ -> 
                                    printerrf "Static constructor of %s cannot have a fully qualified strong name %s" typ.Value.FullName sn
                                    m
                            fullyNamedStaticMembers.Add (typ, m, sn)
                        else
                            fullyNamedStaticMembers.Add (typ, m, sn) 
                    else 
                        Dict.addToMulti remainingNamedStaticMembers typ (m, sn)
                | Some sn, Some false ->
                    Dict.addToMulti namedInstanceMembers typ (m, sn)
                | _, None ->
                    match m with 
                    | M.Constructor (cDef, nr) ->
                        hasInlinedCtor <- true
                        let comp = compiledNoAddressMember nr
                        if nr.Compiled then
                            let isPure =
                                nr.Pure || (Option.isNone cc.StaticConstructor && isPureFunction nr.Body)
                            cc.Constructors |> add cDef { CompiledForm = comp; Optimizations = opts isPure nr; Expression = nr.Body }
                        else 
                            compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, nr.Body))      
                    | M.Method (mDef, nr) -> 
                        let comp = compiledNoAddressMember nr
                        if nr.Compiled then
                            let isPure =
                                nr.Pure || (notVirtual nr.Kind && Option.isNone cc.StaticConstructor && isPureFunction nr.Body)
                            cc.Methods |> addMethod typ mDef { CompiledForm = comp; Optimizations = opts isPure nr; Generics = nr.Generics; Expression = nr.Body }
                        else 
                            compilingMethods |> addCMethod (typ, mDef) (toCompilingMember nr comp, nr.Generics, nr.Body)
                    | _ -> failwith "Fields and static constructors are always named"     
                | None, Some true ->
                    Dict.addToMulti remainingStaticMembers typ m
                | None, Some false ->
                    match m with
                    | M.Method (mDef, ({ Kind = N.Override td } as nr)) ->
                        let td, m = 
                            match proxies.TryFind td with
                            | Some p ->
                                p, M.Method(mDef, { nr with Kind = N.Override p }) 
                            | _ -> td, m
                        if td = Definitions.Obj then
                            let n = 
                                match mDef.Value.MethodName with
                                | "ToString" -> "toString"
                                | n -> n
                            Dict.addToMulti namedInstanceMembers typ (m, n)
                        else
                            Dict.addToMulti remainingInstanceMembers typ m
                    | M.Method (mDef, ({ Kind = N.Implementation td } as nr)) ->
                        let td, m = 
                            match proxies.TryFind td with
                            | Some p ->
                                p, M.Method(mDef, { nr with Kind = N.Implementation p }) 
                            | _ -> td, m
                        //if td.Value.FullName = "System.Collections.Generic.IEnumerable`1" then
                        //    Dict.addToMulti namedInstanceMembers typ (m, "GetEnumerator")
                        //else 
                        match interfaces.TryFind td with
                        | Some i ->
                            match i.Methods.TryFind mDef with
                            | Some (n, k, c) ->    
                                Dict.addToMulti namedInstanceMembers typ (m, n)
                            | _ -> printerrf "Failed to look up name for implemented member: %s.%s in type %s" td.Value.FullName mDef.Value.MethodName typ.Value.FullName 
                        | _ ->
                            printerrf "Failed to look up interface for implementing: %s by type %s" td.Value.FullName typ.Value.FullName
                    | M.Method (mDef, ({ Kind = N.InlineImplementation td } as nr)) ->
                        () // TODO check redirection
                    | M.Method (mDef, ({ Kind = N.MissingImplementation td } as nr)) ->
                        missingImplementations.Add (typ, this.FindProxied td, mDef)
                    | _ -> 
                        Dict.addToMulti remainingInstanceMembers typ m    
            
            //let addConstructor (cDef, nr) comp =
            //    if nr.Compiled then
            //        let isPure =
            //            nr.Pure || isPureFunction nr.Body
            //        cc.Constructors.Add(cDef, (comp, opts isPure nr, nr.Body))
            //    else
            //        compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, nr.Body))
            
            //match constructors.Count with
            //| 0 ->
            //    if hasInlinedCtor && Option.isNone cls.Type && cls.Kind <> NotResolvedClassKind.Stub && not (TypeTranslator.CustomTranslations.ContainsKey typ) then
            //        this.AddWarning(Some cls.SourcePos, SourceWarning ("Class " + typ.Value.FullName + " only has inlined constructors, consider adding a Type attribute" ))
            ////| 1 -> addConstructor constructors.[0] New
            //| _ -> 
            //    for i, ctor in Seq.indexed constructors do
            //        addConstructor ctor (NewIndexed i)

        let processCustomType typ  =
            match classes.TryFind typ with
            | Some (_, ct, _) ->
                this.ProcessCustomType(typ, ct)
            | _ -> ()

        for typ, sn, isClass in stronglyNamedTypes do
            let addr = 
                match sn.Split('.') with
                | [||] -> 
                    printerrf "Invalid Name attribute argument on type '%s'" typ.Value.FullName
                    ["$$ERROR$$"]
                | a -> List.ofArray a
                |> List.rev
            if isClass then
                let cc = assumeClass typ
                //if not (resolver.ExactClassAddress(addr, cc.HasWSPrototype)) then
                //    this.AddError(None, NameConflict ("Class name conflict", sn))
                //setClassAddress typ (Hashed addr)
                processCustomType typ
            else
                //setInterfaceAddress typ (Hashed addr)
                ()

        let fixKindAndNameForProps typ name k m =
            if k = MemberKind.Simple then 
                name, k
            else
                let fix() =
                    match k with
                    | MemberKind.Getter -> "get_" + name, MemberKind.Simple
                    | MemberKind.Setter -> "set_" + name, MemberKind.Simple
                    | _ -> name, k
                match m with
                | M.Method (_, nr) ->
                    match clStatics.TryGetValue(typ) with
                    | true, _ ->
                        fix()
                    | _ ->
                        match nr.Kind with
                        | N.Quotation _
                        | N.Static -> 
                            if mergedProxies.Contains typ then
                                fix()
                            elif (assumeClass typ).HasWSPrototype then 
                                name, k
                            else 
                                fix()
                        | N.AsStatic -> fix()
                        | _ -> name, k
                | _ -> name, k

        let objTy = NonGenericType Definitions.Obj

        let rpcMethod name ret =
            Method {
                MethodName = name
                Parameters = [ NonGenericType Definitions.String; ArrayType (objTy, 1) ]
                ReturnType = ret
                Generics = 0       
            }

        let syncRpcMethod = rpcMethod "Sync" objTy
        let asyncRpcMethod = rpcMethod "Async" (GenericType Definitions.Async [objTy])
        let taskRpcMethod = rpcMethod "Task" (GenericType Definitions.Task1 [objTy])
        let sendRpcMethod = rpcMethod "Send" VoidType

        let defaultRemotingProvider =
            TypeDefinition {
                Assembly = "WebSharper.Main"
                FullName =  "WebSharper.Remoting+AjaxRemotingProvider"
            }, ConstructorInfo.Default(), []

        let webSharperJson =
            TypeDefinition {
                Assembly = "WebSharper.Core"
                FullName = "WebSharper.TypedJson"
            } 

        let encodeMethod = 
            Method {
                MethodName = "Encode"
                Parameters = [ TypeParameter 0 ]
                ReturnType = NonGenericType Definitions.Obj
                Generics = 1       
            }

        let decodeMethod = 
            Method {
                MethodName = "Decode"
                Parameters = [ NonGenericType Definitions.Obj ]
                ReturnType = TypeParameter 0
                Generics = 1       
            }

        let decodeAsyncMethod = 
            Method {
                MethodName = "DecodeAsync"
                Parameters = [ GenericType Definitions.FSharpAsync [ NonGenericType Definitions.Obj ] ]
                ReturnType = GenericType Definitions.FSharpAsync [ TypeParameter 0 ]
                Generics = 1       
            }

        let decodeTaskMethod = 
            Method {
                MethodName = "DecodeTask"
                Parameters = [ GenericType Definitions.Task1 [ NonGenericType Definitions.Obj ] ]
                ReturnType = GenericType Definitions.Task1 [ TypeParameter 0 ]
                Generics = 1       
            }

        let nameStaticMember typ name k m = 
            let res = assumeClass typ
            match m with
            | M.Constructor (cDef, nr) ->
                let comp = compiledStaticMember name k res.HasWSPrototype typ nr
                //let body =
                //    match nr.Body with
                //    | Function(cargs, _, typ, cbody) ->
                //        let o = Id.New "o"
                //        let b = 
                //            Let(o, Object[], Sequential [ StatementExpr (ReplaceThisWithVar(o).TransformStatement(cbody), None); Var o ])
                //        Function(cargs, false, typ, Return b)
                //    | _ -> 
                //        failwith "Expecting a function as compiled form of constructor"
                if nr.Compiled then
                    let isPure =
                        nr.Pure || (Option.isNone res.StaticConstructor && isPureFunction nr.Body)
                    res.Constructors |> add cDef { CompiledForm = comp; Optimizations = opts isPure nr; Expression = nr.Body }
                else
                    compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, nr.Body))
            | M.Field (fName, nr) ->
                res.Fields.Add(fName, { CompiledForm = StaticField name; ReadOnly = nr.IsReadonly; Type = nr.FieldType; Order = nr.Order })
            | M.Method (mDef, nr) ->
                let body = 
                    match nr.Kind with
                    | N.Remote (kind, handle, args, rh, rt, argTypes) ->

                        let name, m =
                            match kind with
                            | RemoteAsync -> "Async", asyncRpcMethod
                            | RemoteTask -> "Task", taskRpcMethod
                            | RemoteSend -> "Send", sendRpcMethod
                            | RemoteSync -> "Sync", syncRpcMethod
                        let remotingProvider =
                            let rpTyp, rpCtor, rpArgs =
                                match rh with
                                | Some (rp, p) -> 
                                    let p = p |> Option.map ParameterObject.OfObj
                                    let paramInfo =
                                        let getParamInfo o = 
                                            let v = o |> ParameterObject.ToObj 
                                            let argType = 
                                                if isNull v then 
                                                    NonGenericType Definitions.String 
                                                else 
                                                    Reflection.ReadType (v.GetType())
                                            argType, v |> ReadLiteral |> Value
                                        match p with
                                        | None -> []
                                        | Some (ParameterObject.Array ps) ->
                                            ps |> Seq.map getParamInfo |> List.ofSeq   
                                        | Some p ->
                                            [ getParamInfo p ]
                                    rp, Constructor { CtorParameters = paramInfo |> List.map fst }, paramInfo |> List.map snd 
                                | _ -> defaultRemotingProvider   
                            Ctor(NonGeneric rpTyp, rpCtor, rpArgs) 
                        
                        let mNode = MethodNode(typ, mDef)
                        if hasGraph then
                            let rec addTypeDeps (t: Type) =
                                match t with
                                | ConcreteType c ->
                                    graph.AddEdge(mNode, TypeNode c.Entity)
                                    if not (this.HasType c.Entity) && not (c.Entity.Value.FullName.StartsWith("<>f__AnonymousType")) then
                                        warnings.Add(None, CompilationWarning.SourceWarning ("Remote method is returning a type which is not fully supported on client side. Add a JavaScript attribute or proxy for " + c.Entity.Value.FullName))
                                    c.Generics |> List.iter addTypeDeps
                                | ArrayType(t, _) -> addTypeDeps t
                                | TupleType (ts, _) -> ts |> List.iter addTypeDeps
                                | _ -> ()
                            addTypeDeps mDef.Value.ReturnType
                        
                        let encodedArgs =
                            match argTypes with
                            | None ->
                                (args, mDef.Value.Parameters) ||> List.map2 (fun a p ->
                                    Call(None, NonGeneric webSharperJson, Generic encodeMethod [ p ], [ Var a ])
                                )
                            | Some argTypes ->
                                (args, argTypes) ||> List.map2 (fun a p ->
                                    Call(None, NonGeneric webSharperJson, Generic encodeMethod [ p ], [ Var a ])
                                )
                        let decode x =
                            let returnTypePlain() =
                                match rt with
                                | Some rt -> 
                                    match rt with
                                    | ConcreteType c ->
                                        match c.Generics with
                                        | [] -> None
                                        | t :: _ ->
                                            if t = Type.VoidType then None else Some t
                                    | _ -> failwith "Expecting Async or Task return type"
                                | _ -> 
                                    match mDef.Value.ReturnType with
                                    | ConcreteType c ->
                                        match c.Generics with
                                        | [] -> None
                                        | t :: _ ->
                                            if t = Type.VoidType then None else Some t
                                    | _ -> failwith "Expecting Async or Task return type"
                            let decoded dm arg =
                                Call(None, NonGeneric webSharperJson, Generic dm [ arg ], [ x ])
                            match kind with
                            | RemoteAsync -> 
                                match returnTypePlain() with
                                | Some t -> decoded decodeAsyncMethod t
                                | _ -> x
                            | RemoteTask -> 
                                match returnTypePlain() with
                                | Some t -> decoded decodeTaskMethod t
                                | _ -> x
                            | RemoteSend -> x
                            | RemoteSync -> 
                                decoded decodeMethod mDef.Value.ReturnType

                        let callRP = 
                            Call(Some remotingProvider, NonGeneric Definitions.IRemotingProvider, NonGeneric m, 
                            [ 
                                Value (String (handle.Pack()))
                                NewArray encodedArgs 
                            ]) 
                            |> decode
                        Function(args, None, None, Return callRP)
                    | _ ->
                        nr.Body
                let comp = compiledStaticMember name k res.HasWSPrototype typ nr
                if nr.Compiled then 
                    let isPure =
                        nr.Pure || (notVirtual nr.Kind && Option.isNone res.StaticConstructor && isPureFunction body)
                    res.Methods |> addMethod typ mDef { CompiledForm = comp; Optimizations = opts isPure nr; Generics = nr.Generics; Expression = body }
                else
                    compilingMethods |> addCMethod (typ, mDef) (toCompilingMember nr comp, nr.Generics, body)
            | M.StaticConstructor st ->                
                compilingStaticConstructors.Add(typ, st)
        
        let nameInstanceMember typ name k m =
            let res = assumeClass typ
            let addf k v (d: IDictionary<_,_>) =
                try d.Add(k, v)
                with _ -> failwithf "Instance field name already used: %s in type %s" name typ.Value.FullName
            let add k v g (d: IDictionary<_,_>) =
                match d.TryGetValue(k) with
                | true, e ->
                    if g e = Undefined then
                        d[k] <- v
                    else
                        failwithf "Instance method name already used: %s in type %s" name typ.Value.FullName
                | _ ->
                    d.Add(k, v)
            let trd (_, _, x) = x  
            match m with
            | M.Field (fName, f) ->
                let fi =
                    if f.IsOptional then 
                        OptionalField name
                    else
                        match System.Int32.TryParse name with
                        | true, i -> IndexedField i
                        | _ -> InstanceField name
                res.Fields |> addf fName {CompiledForm = fi; ReadOnly = f.IsReadonly; Type = f.FieldType; Order = f.Order }
            | M.Method (mDef, nr) ->
                let comp = compiledInstanceMember name k nr
                match nr.Kind with
                | N.Implementation dtyp ->
                    if nr.Compiled then 
                        res.Implementations |> add (dtyp, mDef) { CompiledForm = comp; Expression = nr.Body } (fun i -> i.Expression)
                    else
                        compilingImplementations |> add (typ, dtyp, mDef) (toCompilingMember nr comp, nr.Body) snd
                | _ ->
                    if nr.Compiled then 
                        let isPure = nr.Pure || (Option.isNone res.StaticConstructor && isPureFunction nr.Body)
                        res.Methods |> add mDef { CompiledForm = comp; Optimizations = opts isPure nr; Generics = nr.Generics; Expression = nr.Body} (fun m -> m.Expression)
                    else
                        compilingMethods |> add (typ, mDef) (toCompilingMember nr comp, nr.Generics, nr.Body) trd
            | _ -> failwith "Invalid instance member kind"   

        let getClassAddress typ =
            let (addr, _, _) = classes.[typ]
            addr
                                     
        for typ, m, sn in fullyNamedStaticMembers do
            let c = resolver.LookupClass typ
            let name =
                match sn.Split('.') with
                | [||] ->
                    printerrf "Invalid Name attribute argument on type '%s'" typ.Value.FullName
                    "$$ERROR$$"
                | [| n |] -> 
                    n
                | a -> 
                    this.AddWarning(None, SourceWarning (sprintf "Deprecated Name attribute argument on type '%s'. Full names are no longer used." typ.Value.FullName))
                    Array.head a
            let (_, k) = this.GetMemberNameAndKind(m)
            let name, k = fixKindAndNameForProps typ name k m
            if not (Resolve.addStaticMemberToClass c (name, k)) then
                this.AddError(None, NameConflict ("Static member name conflict", typ.Value.FullName, sn)) 
            nameStaticMember typ name k m
              
        for KeyValue((td, m), args) in compilingQuotedArgMethods do
            let cls =
                match classes.TryFind td with
                | Some (_, _, Some cls) -> cls
                | _ ->
                    let a = this.TypeAddress(td, false)
                    let cls =
                        {
                            //Address = Some (r.ClassAddress(defaultAddressOf td, false))
                            BaseClass = None
                            Constructors = Dictionary()
                            Generics = []
                            IsStub = false
                            Fields = Dictionary()
                            StaticConstructor = None
                            Methods = Dictionary()
                            QuotedArgMethods = Dictionary()
                            Implementations = Dictionary()
                            HasWSPrototype = false
                            Macros = []
                            Implements = []
                            Type = None
                        }
                    classes |> addType td (a, NotCustomType, Some cls)
                    cls
            cls.QuotedArgMethods |> add m args

        for typ, isClass in remainingTypes do
            let a = this.TypeAddress(typ, isClass && (assumeClass typ).HasWSPrototype)
            if isClass then
                //setClassAddress typ a
                processCustomType typ
            else 
                //setInterfaceAddress typ a
                ()
        
        // initialize remaining non-TS-class custom types
        for KeyValue(typ, ct) in notResolvedCustomTypes do
            let clAddr = this.TypeAddress(typ, false)
            classes.Add(typ, (clAddr, ct, None))
            this.ProcessCustomType(typ, ct)

        for KeyValue(typ, ms) in remainingNamedStaticMembers do
            let clAddr = getClassAddress typ

            let pr = resolver.LookupClass typ
            for m, n in ms do
                let addr = clAddr.Sub(n)
                let (_, k) = this.GetMemberNameAndKind(m)
                let n, k = fixKindAndNameForProps typ n k m
                if not (Resolve.addStaticMemberToClass pr (n, k)) then
                    this.AddError(None, NameConflict ("Static member name conflict", typ.Value.FullName, addr.Address |> String.concat "."))
                nameStaticMember typ n k m
        let isImplementation m =
            match m with
            | M.Method (_, { Kind = N.Implementation _ }) -> true
            | _ -> false
           
        // TODO: check nothing is hiding (exclude overrides and implementations)
        for KeyValue(typ, ms) in namedInstanceMembers do
            let pr = resolver.LookupClass typ
            for m, n in ms do
                //pr.Add n |> ignore
                let (_, k) = this.GetMemberNameAndKind(m)
                if not (Resolve.addInstanceMemberToClass pr (n, k) || objectMethods.Contains n || isImplementation m) then
                    printerrf "Instance member name conflict on type %s name %s" typ.Value.FullName n
                nameInstanceMember typ n k m      

        let simplifyFieldName (f: string) =
            f.Split('@').[0]

        for KeyValue(typ, ms) in remainingStaticMembers do
            let clAddr = getClassAddress typ
            let pr = resolver.LookupClass typ
            for m in ms do
                let uname, k = 
                    match m with
                    | M.Constructor _ -> "New", MemberKind.Simple
                    | M.Field (fName, _) -> simplifyFieldName fName, MemberKind.Simple
                    | M.Method (meth, _) ->
                        let n, k = this.GetMemberNameAndKind(m)
                        // Simplify names of active patterns
                        if n.StartsWith "|" then n.Split('|').[1], k 
                        // Simplify names of static F# extension members 
                        elif n.EndsWith ".Static" then
                            let s = n.Split('.')
                            s[.. s.Length - 2] |> String.concat("_"), k
                        else cleanName n, k 
                    | M.StaticConstructor _ -> "cctor", MemberKind.Simple 
                let uname, k = fixKindAndNameForProps typ uname k m
                let addr = Resolve.getRenamedStaticMemberForClass uname k pr
                nameStaticMember typ addr k m

        let resolved = HashSet()
        
        // TODO: add abstract/interface methods even if there are no implementations
        let rec resolveRemainingInstanceMembers typ (cls: ClassInfo) ms =
            if resolved.Add typ then
                let pr = resolver.LookupClass typ
                // inherit members
                match cls.BaseClass with
                | None -> ()
                | Some { Entity = bTyp } ->
                    match classes.Current.TryFind bTyp with
                    | Some (_, _, Some bCls) ->
                        let bMs =
                            match remainingInstanceMembers.TryFind bTyp with
                            | Some bMs -> bMs 
                            | _ -> []  
                        resolveRemainingInstanceMembers bTyp bCls bMs
                    | _ -> ()
                    //pr.UnionWith(resolver.LookupPrototype bTyp) 
                
                let ms =
                    let nonOverrides, overrides = 
                        ms |> List.partition (fun m ->
                            match m with
                            | M.Method (_, { Kind = N.Override _ }) -> false
                            | _ -> true 
                        )
                    Seq.append nonOverrides overrides

                for m in ms do
                    let name, k = 
                        match m with
                        | M.Field (fName, _) -> 
                            Resolve.getRenamedInstanceMemberForClass (simplifyFieldName fName) MemberKind.Simple pr |> Some,
                            MemberKind.Simple
                        | M.Method (mDef, { Kind = N.Instance | N.Abstract }) -> 
                            let n, k = this.GetMemberNameAndKind(m)
                            Resolve.getRenamedInstanceMemberForClass (cleanName n) k pr |> Some,
                            k
                        | M.Method (mDef, { Kind = N.Override td }) ->
                            match classes.TryFind td with
                            | Some (_, _, Some tCls) -> 
                                let smi = 
                                    match tCls.Methods.TryFind mDef with
                                    | Some smi -> Some smi.CompiledForm
                                    | _ ->
                                    match compilingMethods.TryFind (td, mDef) with
                                    | Some (cm,_,_) -> Some cm.CompiledMember
                                    | None ->
                                        printerrf "Abstract method not found in compilation: %s in %s" (string mDef.Value) td.Value.FullName
                                        None
                                match smi with
                                | Some (Instance (n, kind)) -> Some n, kind
                                | None -> None, MemberKind.Simple
                                | _ -> 
                                    printerrf "Abstract method must be compiled as instance member: %s in %s" (string mDef.Value) td.Value.FullName
                                    None, MemberKind.Simple
                            | _ ->
                                printerrf "Base type not found in compilation: %s" td.Value.FullName
                                None, MemberKind.Simple
                        | _ -> 
                            failwith "Invalid instance member kind"
                            None, MemberKind.Simple
                    name |> Option.iter (fun n -> nameInstanceMember typ n k m)

        for KeyValue(typ, ms) in remainingInstanceMembers do
            resolveRemainingInstanceMembers typ (assumeClass typ) ms

        for typ, intf, meth in missingImplementations do
            let mNameOpt =
                interfaces.TryFind intf
                |> Option.bind (fun i -> i.Methods.TryFind meth)
            match mNameOpt with
            | Some (mName, _,_) ->
                let _, _, cls = classes.[typ]
                let tryFindByName n =
                    cls.Value.Methods
                    |> Seq.tryPick (fun (KeyValue(m, cm)) ->
                        match cm.CompiledForm with 
                        | Instance (name, kind) when name = n -> Some (MethodNode(typ, m))
                        | _ -> None
                    )
                    |> Option.orElseWith (fun () ->
                        compilingMethods 
                        |> Seq.tryPick(fun (KeyValue((td, m), (cm, _, _))) ->
                            if td = typ then
                                match cm.CompiledMember with 
                                | Instance (name, _) when name = n -> Some (MethodNode(typ, m))
                                | _ -> None
                            else None
                        )
                    ) 
                    |> Option.orElseWith (fun () ->
                        compilingImplementations 
                        |> Seq.tryPick(fun (KeyValue((td, i, m), (cm, _))) ->
                            if td = typ then
                                match cm.CompiledMember with 
                                | Instance (name, _) when name = n -> Some (ImplementationNode(typ, i, m))
                                | _ -> None
                            else None
                        )
                    ) 
                let methFallbackOpt =
                    tryFindByName mName
                    |> Option.orElseWith (fun () ->
                        if mName.EndsWith("`0") then 
                            tryFindByName (mName[.. mName.Length - 2]) 
                        else None
                    )
                match methFallbackOpt with
                | Some methFallback ->
                    graph.AddEdge(ImplementationNode(typ, intf, meth), methFallback)
                | _ ->
                    let hasStaticImpl =
                        match classes.TryFind intf with
                        | Some (_, _, Some i) -> 
                            i.Methods.ContainsKey meth || compilingMethods.ContainsKey (intf, meth)
                        | _ -> false
                    if not hasStaticImpl then
                        let found = 
                            cls.Value.Methods.Values 
                            |> Seq.choose (fun cm -> 
                                match cm.CompiledForm with 
                                | Instance (name, kind) -> Some name
                                | _ -> None
                            )
                            |> Seq.append (
                                compilingMethods 
                                |> Seq.choose(fun (KeyValue((td, m), (cm, _, _))) ->
                                    if td = typ && m = meth then
                                        match cm.CompiledMember with 
                                        | Instance (name, _) -> Some name
                                        | _ -> None
                                    else None
                                )
                            )
                            |> Seq.append (
                                compilingImplementations 
                                |> Seq.choose(fun (KeyValue((td, _, m), (cm, _))) ->
                                    if td = typ && m = meth then
                                        match cm.CompiledMember with 
                                        | Instance (name, _) -> Some name
                                        | _ -> None
                                    else None
                                )
                            )
                            |> List.ofSeq
                        printerrf "Failed to look up fallback method for missing proxy implementation: %s on type %s for interface %s found: %A" mName typ.Value.FullName intf.Value.FullName found
            | _ ->
                ()
    
        // Add graph edges for Object methods redirections
        if hasGraph && this.AssemblyName = "WebSharper.Main" then
            
            let equals =
                Method {
                    MethodName = "Equals"
                    Parameters = [ ConcreteType (NonGeneric Definitions.Obj) ]
                    ReturnType = NonGenericType Definitions.Bool
                    Generics = 0
                }

            let equalsImpl =
                Method { equals.Value with MethodName = "EqualsImpl" }

            let getHashCode =
                Method {
                    MethodName = "GetHashCode"
                    Parameters = []
                    ReturnType = NonGenericType Definitions.Int
                    Generics = 0
                } 

            let getHashCodeImpl =
                Method { getHashCode.Value with MethodName = "GetHashCodeImpl" } 

            let toString =
                Method {
                    MethodName = "ToString"
                    Parameters = []
                    ReturnType = NonGenericType Definitions.String
                    Generics = 0
                } 

            let uncheckedMdl =
                TypeDefinition {
                    FullName = "Microsoft.FSharp.Core.Operators+Unchecked"
                    Assembly = "FSharp.Core"
                }

            let uncheckedEquals =
                Method {
                    MethodName = "Equals"
                    Parameters = [ TypeParameter 0; TypeParameter 0 ]
                    ReturnType = NonGenericType Definitions.Bool
                    Generics = 1
                }

            let uncheckedHash =
                Method { 
                    MethodName = "Hash"
                    Parameters = [ TypeParameter 0 ]
                    ReturnType = NonGenericType Definitions.Int
                    Generics = 1
                }

            let operatorsMdl =
                TypeDefinition {
                    FullName = "Microsoft.FSharp.Core.Operators"
                    Assembly = "FSharp.Core"
                }

            let operatorsToString = 
                Method {
                    MethodName = "ToString"
                    Parameters = [ TypeParameter 0 ]
                    ReturnType = NonGenericType Definitions.String
                    Generics = 1
                } 

            graph.AddOverride(Definitions.Obj, Definitions.Obj, equals)
            graph.AddOverride(Definitions.Obj, Definitions.Obj, getHashCode)
            graph.AddOverride(Definitions.Obj, Definitions.Obj, toString)

            let objEqIndex = graph.Lookup.[AbstractMethodNode(Definitions.Obj, equals)]         
            let unchmod =
                graph.Lookup.Keys |> Seq.choose (
                    function 
                    | MethodNode (t, m) when t = uncheckedMdl -> Some m
                    | _ -> None
                )
                |> Array.ofSeq
            unchmod |> Array.iter (printfn "%A")
            let uchEqIndex = graph.Lookup.[MethodNode (uncheckedMdl, uncheckedEquals)]
            let implEqIndex = graph.Lookup.[MethodNode(Definitions.Obj, equalsImpl)]

            graph.AddEdge(objEqIndex, uchEqIndex)
            graph.AddEdge(uchEqIndex, implEqIndex)
            graph.AddEdge(implEqIndex, objEqIndex)

            let objHashIndex = graph.Lookup.[AbstractMethodNode(Definitions.Obj, getHashCode)]
            let uchHashIndex = graph.Lookup.[MethodNode (uncheckedMdl, uncheckedHash)]
            let implHashIndex = graph.Lookup.[MethodNode (Definitions.Obj, getHashCodeImpl)]

            graph.AddEdge(objHashIndex, uchHashIndex)
            graph.AddEdge(uchHashIndex, implHashIndex)
            graph.AddEdge(implHashIndex, objHashIndex)

            let objToStringIndex = graph.Lookup.[AbstractMethodNode(Definitions.Obj, toString)]
            let oprToString = MethodNode (operatorsMdl, operatorsToString)
            graph.AddEdge(oprToString, objToStringIndex)

        // Add graph edge needed for decimal remoting
        if hasGraph && this.AssemblyName = "WebSharper.MathJS.Extensions" then
            let createDecimalBits =
                MethodNode(
                    TypeDefinition {
                        Assembly = "WebSharper.MathJS.Extensions"
                        FullName = "WebSharper.Decimal"
                    },
                    Method {
                        MethodName = "CreateDecimalBits"
                        Parameters = [ArrayType(NonGenericType Definitions.Int, 1)]
                        ReturnType = NonGenericType Definitions.Decimal
                        Generics = 0
                    } 
                )
            
            let createDecimalBitsIndex = graph.Lookup.[createDecimalBits]

            graph.AddEdge(TypeNode Definitions.Decimal, createDecimalBitsIndex)

    member this.VerifyRPCs () =
        let rec isWebControl (cls: ClassInfo) =
            match cls.BaseClass with
            | Some { Entity = bT } ->
                bT.Value.FullName = "WebSharper.Web.Control" || isWebControl (assumeClass bT)
            | _ -> false
        let iControlBody =
            TypeDefinition {
                Assembly = "WebSharper.Main"
                FullName = "WebSharper.IControlBody"
            }
        let getBody =
            Method {
                MethodName = "get_Body"
                Parameters = []
                ReturnType = ConcreteType (NonGeneric iControlBody)
                Generics = 0
            } 
        let jP = Json.ServerSideProvider
        let st = Verifier.State(jP)
        for KeyValue(t, cls) in classes.Current do
            match cls with
            | _, _, None -> ()
            | _, _, Some cls ->
            for KeyValue(m, mi) in cls.Methods do
                match mi.CompiledForm with
                | Remote _ ->
                    match st.VerifyRemoteMethod(t, m) with
                    | Verifier.Incorrect msg ->
                        this.AddWarning (None, SourceWarning (msg + " at " + t.Value.FullName + "." + m.Value.MethodName))
                    | Verifier.CriticallyIncorrect msg ->
                        this.AddError (None, SourceError (msg + " at " + t.Value.FullName + "." + m.Value.MethodName))
                    | _ -> ()
                | _ -> ()
            if isWebControl cls then
                match st.VerifyWebControl(t) with 
                | Verifier.CriticallyIncorrect msg ->
                    this.AddError (None, SourceError msg)
                | _ -> ()

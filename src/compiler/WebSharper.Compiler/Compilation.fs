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

namespace WebSharper.Compiler
  
open System.Collections.Generic
open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata
open WebSharper.Core.DependencyGraph
open NotResolved
    
type Compilation(meta: Info, ?hasGraph) =    
    let notResolvedInterfaces = Dictionary<TypeDefinition, NotResolvedInterface>()
    let notResolvedClasses = Dictionary<TypeDefinition, NotResolvedClass>()
    let proxies = Dictionary<TypeDefinition, TypeDefinition>()

    let classes = MergedDictionary meta.Classes
    let interfaces = MergedDictionary meta.Interfaces
    let notAnnotatedCustomTypes = Dictionary()
    let customTypes = MergedDictionary meta.CustomTypes
    let macroEntries = MergedDictionary meta.MacroEntries
    let quotations = MergedDictionary meta.Quotations

    let hasGraph = defaultArg hasGraph true
    let graph = if hasGraph then Graph.FromData(meta.Dependencies) else Unchecked.defaultof<_>

    let mutableExternals = Recognize.GetMutableExternals meta

    let compilingMethods = Dictionary<TypeDefinition * Method, CompilingMember * Expression>()
    let compilingImplementations = Dictionary<TypeDefinition * TypeDefinition * Method, CompilingMember * Expression>()
    let compilingConstructors = Dictionary<TypeDefinition * Constructor, CompilingMember * Expression>()
    let compilingStaticConstructors = Dictionary<TypeDefinition, Address * Expression>()
    let compilingQuotedArgMethods = Dictionary<TypeDefinition * Method, int[]>()

    let mutable generatedClass = None
    let mutable resolver = None : option<Resolve.Resolver>
    let generatedMethodAddresses = Dictionary()

    let errors = ResizeArray()
    let warnings = ResizeArray() 

    let mutable entryPoint = None
    let jsExports = ResizeArray() 

    let macros = System.Collections.Generic.Dictionary<TypeDefinition, Macro option>()
    let generators = System.Collections.Generic.Dictionary<TypeDefinition, Generator option>()

    let defaultAddressOf (typ: TypeDefinition) =
        let removeGen (n: string) =
            match n.LastIndexOf '`' with
            | -1 -> n
            | i -> n.[.. i - 1]
        typ.Value.FullName.Split('.', '+') |> List.ofArray |> List.map removeGen |> List.rev 

    member val UseLocalMacros = true with get, set
    member val SiteletDefinition: option<TypeDefinition> = None with get, set
    member val AssemblyName = "EntryPoint" with get, set
    member val AssemblyRequires = [] : list<TypeDefinition * option<obj>> with get, set
    
    member val CustomTypesReflector = fun _ -> NotCustomType with get, set 
    member val LookupTypeAttributes = fun _ -> None with get, set
    member val LookupFieldAttributes = fun _ _ -> None with get, set
    member val LookupMethodAttributes = fun _ _ -> None with get, set
    member val LookupConstructorAttributes = fun _ _ -> None with get, set

    member this.MutableExternals = mutableExternals

    member this.FindProxied typ =
        match proxies.TryFind typ with
        | Some p -> p 
        | _ -> typ
    
    member this.GetRemoteHandle(path: string, args: Type list, ret: Type) =
        {
            Assembly = this.AssemblyName
            Path = path
            SignatureHash = hash (args, ret)
        }

    member this.AddError (pos : SourcePos option, error : CompilationError) =
        errors.Add (pos, error)

    member this.Errors = List.ofSeq errors

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
            let name = "Generated$" + this.AssemblyName.Replace('.', '_')
            let td = 
                TypeDefinition { 
                    FullName = name
                    Assembly = this.AssemblyName 
                }
            classes.Add (td,
                {
                    Address = None
                    BaseClass = None
                    Constructors = Dictionary() 
                    Fields = Dictionary() 
                    StaticConstructor = None 
                    Methods = Dictionary()
                    QuotedArgMethods = Dictionary()
                    Implementations = Dictionary()
                    HasWSPrototype = false
                    Macros = []
                }
            ) 
            generatedClass <- Some td
            td

    interface ICompilation with
        member this.GetCustomTypeInfo typ = 
            this.GetCustomType typ
        
        member this.GetInterfaceInfo typ =
            interfaces.TryFind (this.FindProxied typ)

        member this.GetClassInfo typ = 
            let fstOf3 (x, _, _) = x
            match classes.TryFind (this.FindProxied typ) with
            | None -> None
            | Some cls ->
                Some { new IClassInfo with
                    member this.Address = cls.Address
                    member this.BaseClass = cls.BaseClass
                    member this.Constructors = MappedDictionary(cls.Constructors, fstOf3) :> _
                    member this.Fields = cls.Fields
                    member this.StaticConstructor = cls.StaticConstructor |> Option.map fst
                    member this.Methods = MappedDictionary(cls.Methods, fstOf3) :> _
                    member this.Implementations = MappedDictionary(cls.Implementations, fst) :> _
                    member this.HasWSPrototype = cls.HasWSPrototype
                    member this.Macros = cls.Macros
                }

        member this.GetQuotation(pos) = quotations.TryFind pos

        member this.GetJavaScriptClasses() = classes.Keys |> List.ofSeq
        member this.GetTypeAttributes(typ) = this.LookupTypeAttributes typ
        member this.GetFieldAttributes(typ, field) = this.LookupFieldAttributes typ field
        member this.GetMethodAttributes(typ, meth) = this.LookupMethodAttributes typ meth
        member this.GetConstructorAttributes(typ, ctor) = this.LookupConstructorAttributes typ ctor

        member this.ParseJSInline(inl: string, args: Expression list): Expression = 
            let vars = args |> List.map (fun _ -> Id.New(mut = false))
            let parsed = Recognize.createInline mutableExternals None vars false inl
            Substitution(args).TransformExpression(parsed)
                
        member this.NewGenerated addr =
            let resolved = resolver.Value.StaticAddress (List.rev addr)
            let td = this.GetGeneratedClass()
            let meth = 
                Method {
                    MethodName = resolved.Value |> List.rev |> String.concat "."
                    Parameters = []
                    ReturnType = VoidType
                    Generics = 0       
                }
            generatedMethodAddresses.Add(meth, resolved)
            td, meth, resolved

        member this.AddGeneratedCode(meth: Method, body: Expression) =
            let td = this.GetGeneratedClass()
            let addr = generatedMethodAddresses.[meth]
            compilingMethods.Add((td, meth),(NotCompiled (Static addr, true, Optimizations.None), body))

        member this.AddGeneratedInline(meth: Method, body: Expression) =
            let td = this.GetGeneratedClass()
            compilingMethods.Add((td, meth),(NotCompiled (Inline, true, Optimizations.None), body))

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

        member this.AddError(pos, msg) =
            this.AddError(pos, SourceError msg)

        member this.AddWarning(pos, msg) =
            this.AddWarning(pos, SourceWarning msg)

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
                classes.Current |> Dict.map (fun c ->
                    match c.Methods with
                    | :? MergedDictionary<Method, CompiledMember * Optimizations * Expression> as m -> 
                        { c with Methods = m.Current }
                    | _ -> c
                )
            CustomTypes = 
                customTypes.Current |> Dict.filter (fun _ v -> v <> NotCustomType)
            EntryPoint = entryPoint
            MacroEntries = macroEntries.Current
            Quotations = quotations.Current
            ResourceHashes = Dictionary()
        }    

    member this.AddProxy(tProxy, tTarget) =
        proxies.Add(tProxy, tTarget)  

    member this.AddQuotedArgMethod(typ, m, a) =
        compilingQuotedArgMethods.Add((typ, m), a)

    member this.TryLookupQuotedArgMethod(typ, m) =
        match compilingQuotedArgMethods.TryFind(typ, m) with
        | Some x -> Some x
        | None ->
            match meta.Classes.TryFind(typ) with
            | Some c ->
                match c.QuotedArgMethods.TryFind(m) with
                | Some x -> Some x
                | None -> None
            | None -> None

    member this.AddClass(typ, cls) =
        try
            notResolvedClasses.Add(typ, cls)
        with _ ->
            if cls.IsProxy then
                let orig = notResolvedClasses.[typ]                    
                if Option.isSome cls.StrongName then
                    this.AddError(None, SourceError ("Proxy extension can't be strongly named: " + typ.Value.FullName))
                elif Option.isSome cls.BaseClass && cls.BaseClass <> orig.BaseClass then
                    this.AddError(None, SourceError ("Proxy extension must have the same base class as the original: " + typ.Value.FullName))
                else 
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
    
    member this.AddCustomType(typ, ct, ?notAnnotated) =
        let toDict =
            if Option.isNone notAnnotated || not notAnnotated.Value then
                customTypes :> IDictionary<_,_>
            else notAnnotatedCustomTypes :> _   
        toDict.Add(typ, ct)
        match ct with
        | FSharpUnionInfo u ->
            for c in u.Cases do
                match c.Kind with
                | ConstantFSharpUnionCase Null -> ()
                | _ ->
                let cTyp =
                    TypeDefinition {
                        Assembly = typ.Value.Assembly
                        FullName = typ.Value.FullName + "+" + c.Name
                    } 
                toDict.Add(cTyp, FSharpUnionCaseInfo c)
        | _ -> ()

    member this.HasCustomTypeInfo(typ) =
        customTypes.ContainsKey typ || notAnnotatedCustomTypes.ContainsKey typ

    member this.GetCustomType(typ) = 
        let typ = this.FindProxied typ
        match customTypes.TryFind typ with
        | Some res -> res
        | _ ->
        let res =
            match notAnnotatedCustomTypes.TryFind typ with
            | Some res -> res // TODO unions with cases 
            | _ -> this.CustomTypesReflector typ
        customTypes.Add(typ, res)
        res

    member this.TryLookupClassInfo typ =   
        classes.TryFind(this.FindProxied typ)

    member this.TryLookupClassAddressOrCustomType typ =   
        // unions may have an address for singleton fields but no prototype, treat this case first
        match this.TryLookupClassInfo typ, this.GetCustomType typ with
        | Some c, ((FSharpUnionInfo _ | FSharpUnionCaseInfo _) as ct) when not c.HasWSPrototype -> Choice2Of2 ct
        | Some c, _ -> Choice1Of2 c.Address
        | _, ct -> Choice2Of2 ct
    
    member this.TryLookupInterfaceInfo typ =   
        interfaces.TryFind(this.FindProxied typ)
    
    member this.GetMethods typ =
        compilingMethods |> Seq.choose (fun (KeyValue ((td, m), _)) ->
            if td = typ then Some m else None
        ) |> Seq.append (
            match this.TryLookupClassInfo typ with
            | Some cls -> cls.Methods.Keys :> _ seq
            | _ ->
            match this.TryLookupInterfaceInfo typ with
            | Some intf -> intf.Methods.Keys :> _ seq
            | _ ->
                Seq.empty
        )

    member this.IsImplementing (typ, intf) : bool option =
        classes.TryFind(this.FindProxied typ)
        |> Option.map (fun cls ->
            cls.Implementations |> Seq.exists (fun (KeyValue ((i, _), _)) -> i = intf)
            || cls.BaseClass |> Option.exists (fun b -> this.IsImplementing(b, intf) |> Option.exists id) 
        )

    member this.HasType(typ) =
        let typ = this.FindProxied typ
        classes.ContainsKey typ || interfaces.ContainsKey typ

    member this.IsInterface(typ) =
        let typ = this.FindProxied typ
        interfaces.ContainsKey typ

    member this.ConstructorExistsInMetadata (typ, ctor) =
        let typ = this.FindProxied typ
        match classes.TryFind typ with
        | Some cls ->
            cls.Constructors.ContainsKey ctor || compilingConstructors.ContainsKey (typ, ctor)
        | _ -> false

    member this.MethodExistsInMetadata (typ, meth) =
        let typ = this.FindProxied typ
        match interfaces.TryFind typ with
        | Some intf -> 
            intf.Methods.ContainsKey meth
        | _ ->
        match classes.TryFind typ with
        | Some cls ->
            cls.Methods.ContainsKey meth || compilingMethods.ContainsKey (typ, meth)
        | _ -> false

    member this.LookupMethodInfo(typ, meth) = 
        let typ = this.FindProxied typ
        match interfaces.TryFind typ with
        | Some intf -> 
            match intf.Methods.TryFind meth with
            | Some m ->
                if typ.Value.Assembly = "netstandard" then
                    match typ.Value.FullName with
                    | "System.Collections.IEnumerable" ->
                        Compiled (Inline, Optimizations.None, Application(Global ["WebSharper"; "Enumerator"; "Get0"], [Hole 0], NonPure, Some 1))
                    | "System.Collections.Generic.IEnumerable`1" ->
                        Compiled (Inline, Optimizations.None, Application(Global ["WebSharper"; "Enumerator"; "Get"], [Hole 0], NonPure, Some 1))
                    | _ -> 
                        Compiled (Instance m, Optimizations.None, Undefined)
                else
                    Compiled (Instance m, Optimizations.None, Undefined)              
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
        | _ -> 
        match classes.TryFind typ with
        | Some cls ->
            match cls.Methods.TryFind meth with
            | Some m -> Compiled m
            | _ -> 
                match compilingMethods.TryFind (typ, meth) with
                | Some m -> Compiling m
                | _ -> 
                    if not (List.isEmpty cls.Macros) then
                        let info =
                            List.foldBack (fun (m, p) fb -> Some (Macro (m, p, fb))) cls.Macros None |> Option.get
                        Compiled (info, Optimizations.None, Undefined)
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
        | _ ->
            match this.GetCustomType typ with
            | NotCustomType -> LookupMemberError (TypeNotFound typ)
            | i -> CustomTypeMember i

    member this.LookupFieldInfo(typ, field) =
        let typ = this.FindProxied typ
        match classes.TryFind typ with
        | Some cls ->
            match cls.Fields.TryFind field with
            | Some f -> CompiledField f
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
                    | NotCustomType -> LookupFieldError (FieldNotFound (typ, field))
                    | i -> CustomTypeField i
                | _ -> 
                    PropertyField (getter, setter)
        | _ ->
            match this.GetCustomType typ with
            | NotCustomType -> LookupFieldError (TypeNotFound typ)
            | i -> CustomTypeField i

    member this.LookupConstructorInfo(typ, ctor) =
        let typ = this.FindProxied typ
        match classes.TryFind typ with
        | Some cls ->
            match cls.Constructors.TryFind ctor with
            | Some c -> Compiled c
            | _ -> 
                match compilingConstructors.TryFind (typ, ctor) with
                | Some c -> Compiling c
                | _ -> 
                    if not (List.isEmpty cls.Macros) then
                        let info =
                            List.foldBack (fun (m, p) fb -> Some (Macro (m, p, fb))) cls.Macros None |> Option.get
                        Compiled (info, Optimizations.None, Undefined)
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
        | _ ->
            match this.GetCustomType typ with
            | NotCustomType -> LookupMemberError (TypeNotFound typ)
            | i -> CustomTypeMember i
        
    member this.TryLookupStaticConstructorAddress(typ) =
        let typ = this.FindProxied typ
        let cls = classes.[typ]
        match cls.StaticConstructor with
        | Some(_, GlobalAccess a) when a.Value = [ "ignore" ] -> None
        | Some (cctor, _) -> Some cctor
        | None -> None

    member this.TryGetRecordConstructor(typ) =
        let typ = this.FindProxied typ
        match classes.TryFind typ with
        | Some cls ->
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
        compilingMethods.Remove(typ, meth) |> ignore
        let cls = classes.[typ]
        match cls.Methods.TryFind meth with
        | Some (_, _, Undefined)
        | None ->    
            cls.Methods.[meth] <- (info, opts, comp)
        | _ ->
            failwithf "Method already added: %s %s" typ.Value.FullName (string meth.Value)

    member this.FailedCompiledMethod(typ, meth) =
        let typ = this.FindProxied typ 
        compilingMethods.Remove(typ, meth) |> ignore

    member this.AddCompiledConstructor(typ, ctor, info, opts, comp) = 
        let typ = this.FindProxied typ 
        compilingConstructors.Remove(typ, ctor) |> ignore
        let cls = classes.[typ]
        cls.Constructors.Add(ctor, (info, opts, comp))

    member this.FailedCompiledConstructor(typ, ctor) =
        let typ = this.FindProxied typ 
        compilingConstructors.Remove(typ, ctor) |> ignore

    member this.CompilingStaticConstructors = compilingStaticConstructors

    member this.AddCompiledStaticConstructor(typ, addr, cctor) =
        let typ = this.FindProxied typ 
        compilingStaticConstructors.Remove typ |> ignore
        let cls = classes.[typ]
        classes.[typ] <- { cls with StaticConstructor = Some (addr, cctor) }

    member this.CompilingImplementations = compilingImplementations

    member this.AddCompiledImplementation(typ, intf, meth, info, comp) =
        let typ = this.FindProxied typ 
        compilingImplementations.Remove(typ, intf, meth) |> ignore
        let cls = classes.[typ]
        cls.Implementations.Add((intf, meth), (info, comp))

    member this.Resolve () =
        
        let printerrf x = Printf.kprintf (fun s -> this.AddError (None, SourceError s)) x

        let rec resolveInterface (typ: TypeDefinition) (nr: NotResolvedInterface) =
            let allMembers = HashSet()
            let allNames = HashSet()
            
            let getInterface i =
                match interfaces.TryFind i with
                | Some i -> Some i
                | _ ->
                    if i <> Definitions.Obj then
                        printerrf "Failed to look up interface '%s'" i.Value.FullName 
                    None

            let rec addInherited i (n: InterfaceInfo) =
                for i in n.Extends do
                    getInterface i |> Option.iter (addInherited i)
                for KeyValue(m, n) in n.Methods do
                    if not (allMembers.Add (i, m)) then
                        if not (allNames.Add n) then
                            printerrf "Interface method name collision: %s on %s" n typ.Value.FullName
            
            for i in nr.Extends do
                notResolvedInterfaces.TryFind i |> Option.iter (resolveInterface i)       
                getInterface i |> Option.iter (addInherited i)
            
            let resMethods = Dictionary()
                            
            for m, n in nr.NotResolvedMethods do
                match n with
                | Some n -> 
                    if not (allNames.Add n) then
                        printerrf "Explicitly declared interface method name collision: %s on %s" n typ.Value.FullName
                    resMethods.Add(m, n)
                | _ -> ()
            
            let intfName = 
                match nr.StrongName with
                | Some "" -> ""
                | Some sn -> sn + "$"
                | _ ->                     
                    typ.Value.FullName.Replace('.', '_').Replace('+', '_').Replace('`', '_') + "$"

            for m, n in nr.NotResolvedMethods do
                match n with
                | None ->
                    let n = Resolve.getRenamed (intfName + m.Value.MethodName) allNames
                    resMethods.Add(m, n)
                | _ -> ()

            let resNode =
                {
                    Extends = nr.Extends
                    Methods = resMethods
                }
            interfaces.Add(typ, resNode)
            notResolvedInterfaces.Remove typ |> ignore
        
        while notResolvedInterfaces.Count > 0 do
            let (KeyValue(typ, nr)) = Seq.head notResolvedInterfaces  
            resolveInterface typ nr

        let r = getAllAddresses meta
        resolver <- Some r

        let someEmptyAddress = Some (Address [])
        let unresolvedCctor = Some (Address [], Undefined)

        let resNode (t, p) =
            ResourceNode (t, p |> Option.map ParameterObject.OfObj)

        let asmNodeIndex = 
            if hasGraph then graph.AddOrLookupNode(AssemblyNode (this.AssemblyName, true)) else 0
        if hasGraph then
            for req in this.AssemblyRequires do
                graph.AddEdge(asmNodeIndex, resNode req)

        // initialize all class entries
        for KeyValue(typ, cls) in notResolvedClasses do
            if cls.ForceNoPrototype then
                for mem in cls.Members do
                    match mem with
                    | NotResolvedMember.Method (_, mi) when mi.Kind = NotResolvedMemberKind.Instance ->
                        mi.Kind <- NotResolvedMemberKind.Static         
                        mi.Body <- 
                            match mi.Body with
                            | Function(args, b) ->
                                let thisVar = Id.New("$this", mut = false)
                                Function (thisVar :: args,
                                    ReplaceThisWithVar(thisVar).TransformStatement(b) 
                                )
                            | _ ->
                                failwith "Unexpected: instance member not a function"
                    | _ -> ()
            
            let cctor = cls.Members |> Seq.tryPick (function M.StaticConstructor e -> Some e | _ -> None)
            let baseCls =
                cls.BaseClass |> Option.bind (fun b ->
                    let b = this.FindProxied b
                    if classes.ContainsKey b || notResolvedClasses.ContainsKey b then Some b else None
                )
            let hasWSPrototype = hasWSPrototype cls.Kind baseCls cls.Members                
            let methods =
                match classes.TryFind typ with
                | Some c -> MergedDictionary c.Methods :> IDictionary<_,_>
                | _ -> Dictionary() :> _
            classes.Add (typ,
                {
                    Address = if hasWSPrototype || cls.ForceAddress then someEmptyAddress else None
                    BaseClass = if hasWSPrototype then baseCls else None
                    Constructors = Dictionary() 
                    Fields = Dictionary() 
                    StaticConstructor = if Option.isSome cctor then unresolvedCctor else None 
                    Methods = methods
                    Implementations = Dictionary()
                    QuotedArgMethods = Dictionary()
                    HasWSPrototype = hasWSPrototype
                    Macros = cls.Macros |> List.map (fun (m, p) -> m, p |> Option.map ParameterObject.OfObj)
                }
            ) 
            // set up dependencies
            if hasGraph then
                let clsNodeIndex = graph.AddOrLookupNode(TypeNode typ)
                graph.AddEdge(clsNodeIndex, asmNodeIndex)
                for req in cls.Requires do
                    graph.AddEdge(clsNodeIndex, resNode req)
                cls.BaseClass |> Option.iter (fun b -> graph.AddEdge(clsNodeIndex, TypeNode (this.FindProxied b)))
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
                        | N.Implementation intf ->
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

        if hasGraph then
            for KeyValue(ct, cti) in Seq.append customTypes notAnnotatedCustomTypes do
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

        let withMacros (nr : NotResolvedMethod) woMacros =
            if List.isEmpty nr.Macros then woMacros else
                if nr.Kind = N.NoFallback then None else Some woMacros
                |> List.foldBack (fun (p, o) fb -> Some (Macro(p, o |> Option.map ParameterObject.OfObj, fb))) nr.Macros
                |> Option.get

        let compiledStaticMember (address: Address) (nr : NotResolvedMethod) =
            match nr.Kind with
            | N.Quotation _
            | N.Static -> Static address
            | N.Constructor -> Constructor address
            | _ -> failwith "Invalid static member kind"
            |> withMacros nr        

        let compiledNoAddressMember (nr : NotResolvedMethod) =
            match nr.Kind with
            | N.Inline -> Inline
            | N.Remote (k, h, r) -> Remote (k, h, r |> Option.map (fun (t, p) -> t, p |> Option.map ParameterObject.OfObj))
            | N.NoFallback -> Inline // will be erased
            | _ -> failwith "Invalid not compiled member kind"
            |> withMacros nr

        let compiledInstanceMember (name: string) (nr: NotResolvedMethod) =
            match nr.Kind with
            | N.Instance  
            | N.Abstract
            | N.Override _  
            | N.Implementation _ -> Instance name
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
            match nr.Generator with
            | Some (g, p) -> NotGenerated(g, p, comp, notVirtual nr.Kind, opts nr.Pure nr)
            | _ -> NotCompiled (comp, notVirtual nr.Kind, opts nr.Pure nr)
            
        let extraClassAddresses = Dictionary()

        let setClassAddress typ clAddr =
            let res = classes.[typ]
            if Option.isSome res.Address then
                classes.[typ] <- { res with Address = Some clAddr }
            else
                extraClassAddresses.[typ] <- clAddr.Value

        // split to resolve steps
        let stronglyNamedClasses = ResizeArray()
        let fullyNamedStaticMembers = ResizeArray()
        let remainingClasses = ResizeArray()
        let remainingNamedStaticMembers = Dictionary()
        let namedInstanceMembers = Dictionary() // includes implementations
        let remainingStaticMembers = Dictionary()
        let remainingInstanceMembers = Dictionary() // includes overrides

        let addCctorCall typ (ci: ClassInfo) expr =
            if Option.isSome ci.StaticConstructor then
                match expr with
                | Function (args, body) ->
                    Function(args, CombineStatements [ ExprStatement (Cctor typ); body ])
                // inlines
                | _ -> Sequential [ Cctor typ; expr ]
            else expr
        
        for KeyValue(typ, cls) in notResolvedClasses do
            let namedCls =
                match cls.StrongName with
                | Some sn ->
                    stronglyNamedClasses.Add (typ, sn)
                    true
                | _ -> 
                    remainingClasses.Add typ
                    false
            
            let cc = classes.[typ]

            for m in cls.Members do
                
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
                                | _ -> failwith "impossible: constructor cannot be overridden or implementation"
                                None, None, true
                            else 
                                None, Some false, false
                        | N.Abstract
                        | N.Instance -> sn, Some false, false
                        | N.Static
                        | N.Constructor -> sn, Some true, false
                        | N.Quotation (pos, argNames) -> 
                            match m with 
                            | M.Method (mdef, _) ->
                                try quotations.Add(pos, (typ, mdef, argNames))
                                with e ->
                                    printerrf "Cannot have two instances of quoted JavaScript code at the same location of files with the same name: %s (%i, %i - %i, %i)"
                                        pos.FileName (fst pos.Start) (snd pos.Start) (fst pos.End) (snd pos.End)
                            | _ -> failwith "Quoted javascript code must be inside a method"
                            sn, Some true, false 
                        | N.Remote _
                        | N.Inline
                        | N.NoFallback -> sn, None, false
                    | M.Field (_, { StrongName = sn; IsStatic = s }) -> 
                        sn, Some s, false 
                    | M.StaticConstructor _ -> None, Some true, false
                
                if isError then () else
                
                match strongName, isStatic with
                | Some sn, Some true ->
                    if namedCls || sn.Contains "." then
                        fullyNamedStaticMembers.Add (typ, m, sn) 
                    else 
                        Dict.addToMulti remainingNamedStaticMembers typ (m, sn)
                | Some sn, Some false ->
                    Dict.addToMulti namedInstanceMembers typ (m, sn)
                | _, None ->
                    match m with 
                    | M.Constructor (cDef, nr) ->
                        let comp = compiledNoAddressMember nr
                        if nr.Compiled && Option.isNone cc.StaticConstructor then
                            try
                                let isPure =
                                    nr.Pure || (Option.isNone cc.StaticConstructor && isPureFunction nr.Body)
                                cc.Constructors.Add (cDef, (comp, opts isPure nr, nr.Body))
                            with _ ->
                                printerrf "Duplicate definition for constructor of %s" typ.Value.FullName
                        else 
                            compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, addCctorCall typ cc nr.Body))      
                    | M.Method (mDef, nr) -> 
                        let comp = compiledNoAddressMember nr
                        if nr.Compiled && Option.isNone cc.StaticConstructor then
                            try
                                let isPure =
                                    nr.Pure || (notVirtual nr.Kind && Option.isNone cc.StaticConstructor && isPureFunction nr.Body)
                                cc.Methods.Add (mDef, (comp, opts isPure nr, nr.Body))
                            with _ ->
                                printerrf "Duplicate definition for method %s.%s" typ.Value.FullName mDef.Value.MethodName
                        else 
                            compilingMethods.Add((typ, mDef), (toCompilingMember nr comp, addCctorCall typ cc nr.Body)) 
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
                        if td.Value.FullName = "System.Collections.Generic.IEnumerable`1" then
                            Dict.addToMulti namedInstanceMembers typ (m, "GetEnumerator")
                        else 
                        match interfaces.TryFind td with
                        | Some i ->
                            match i.Methods.TryFind mDef with
                            | Some n ->    
                                Dict.addToMulti namedInstanceMembers typ (m, n)
                            | _ -> printerrf "Failed to look up name for implemented member: %s.%s in type %s" td.Value.FullName mDef.Value.MethodName typ.Value.FullName 
                        | _ ->
                            printerrf "Failed to look up interface for implementing: %s by type %s" td.Value.FullName typ.Value.FullName
                    | _ -> 
                        Dict.addToMulti remainingInstanceMembers typ m                   

        for typ, sn in stronglyNamedClasses do
            let addr = 
                match sn.Split('.') with
                | [||] -> 
                    printerrf "Invalid Name attribute argument on type '%s'" typ.Value.FullName
                    ["$$ERROR$$"]
                | a -> List.ofArray a
                |> List.rev
            if not (r.ExactClassAddress(addr, classes.[typ].HasWSPrototype)) then
                this.AddError(None, NameConflict ("Class name conflict", sn))
            setClassAddress typ (Address addr)

        let nameStaticMember typ addr m = 
            let res = classes.[typ]
            match m with
            | M.Constructor (cDef, nr) ->
                let comp = compiledStaticMember addr nr
                if nr.Compiled && Option.isNone res.StaticConstructor then
                    let isPure =
                        nr.Pure || (Option.isNone res.StaticConstructor && isPureFunction nr.Body)
                    res.Constructors.Add(cDef, (comp, opts isPure nr, nr.Body))
                else
                    compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
            | M.Field (fName, nr) ->
                res.Fields.Add(fName, (StaticField addr, nr.IsReadonly, nr.FieldType))
            | M.Method (mDef, nr) ->
                let comp = compiledStaticMember addr nr
                if nr.Compiled && Option.isNone res.StaticConstructor then 
                    let isPure =
                        nr.Pure || (notVirtual nr.Kind && Option.isNone res.StaticConstructor && isPureFunction nr.Body)
                    res.Methods.Add(mDef, (comp, opts isPure nr, nr.Body))
                else
                    compilingMethods.Add((typ, mDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
            | M.StaticConstructor expr ->                
                // TODO: do not rely on address on compiled state
                let cls = classes.[typ]
                classes.[typ] <- { cls with StaticConstructor = Some (addr, Undefined) }
                compilingStaticConstructors.Add(typ, (addr, expr))
        
        let nameInstanceMember typ name m =
            let res = classes.[typ]
            match m with
            | M.Field (fName, f) ->
                let fi =
                    if f.IsOptional then 
                        OptionalField name
                    else
                        match System.Int32.TryParse name with
                        | true, i -> IndexedField i
                        | _ -> InstanceField name
                res.Fields.Add(fName, (fi, f.IsReadonly, f.FieldType))
            | M.Method (mDef, nr) ->
                let comp = compiledInstanceMember name nr
                match nr.Kind with
                | N.Implementation intf ->
                    if nr.Compiled && Option.isNone res.StaticConstructor then 
                        res.Implementations.Add((intf, mDef), (comp, nr.Body))
                    else
                        compilingImplementations.Add((typ, intf, mDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
                | _ ->
                    if nr.Compiled && Option.isNone res.StaticConstructor then 
                        let isPure = nr.Pure || isPureFunction nr.Body
                        res.Methods.Add(mDef, (comp, opts isPure nr, nr.Body))
                    else
                        compilingMethods.Add((typ, mDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
            | _ -> failwith "Invalid instance member kind"   

        let getClassAddress typ =
            match classes.[typ].Address with
            | Some a -> a.Value
            | _ -> 
            match extraClassAddresses.TryFind typ with
            | Some a -> a
            | _ ->
                let a = r.ClassAddress(typ.Value.FullName.Split('.') |> List.ofArray |> List.rev, false).Value    
                extraClassAddresses.Add(typ, a)
                a
                                     
        for typ, m, sn in fullyNamedStaticMembers do
            let addr =
                match sn.Split('.') with
                | [||] ->
                    printerrf "Invalid Name attribute argument on type '%s'" typ.Value.FullName
                    ["$$ERROR$$"]
                | [| n |] -> 
                    n :: getClassAddress typ
                | a -> List.ofArray (Array.rev a)
            if not (r.ExactStaticAddress addr) then
                this.AddError(None, NameConflict ("Static member name conflict", sn)) 
            nameStaticMember typ (Address addr) m

        for KeyValue((td, m), args) in compilingQuotedArgMethods do
            let cls =
                match classes.TryFind td with
                | Some cls -> cls
                | None ->
                    let cls =
                        {
                            Address = Some (r.ClassAddress(defaultAddressOf td, false))
                            BaseClass = None
                            Constructors = Dictionary()
                            Fields = Dictionary()
                            StaticConstructor = None
                            Methods = Dictionary()
                            QuotedArgMethods = Dictionary()
                            Implementations = Dictionary()
                            HasWSPrototype = false
                            Macros = []
                        }
                    classes.Add(td, cls)
                    cls
            cls.QuotedArgMethods.Add(m, args)
              
        for typ in remainingClasses do
            let addr = defaultAddressOf typ
            r.ClassAddress(addr, classes.[typ].HasWSPrototype)
            |> setClassAddress typ
        
        for KeyValue(typ, ms) in remainingNamedStaticMembers do
            let clAddr = getClassAddress typ
            for m, n in ms do
                let addr = n :: clAddr
                if not (r.ExactStaticAddress addr) then
                    this.AddError(None, NameConflict ("Static member name conflict", addr |> String.concat "."))
                nameStaticMember typ (Address addr) m
           
        // TODO: check nothing is hiding (exclude overrides and implementations)
        for KeyValue(typ, ms) in namedInstanceMembers do
            let pr = r.LookupPrototype typ
            for m, n in ms do
                pr.Add n |> ignore
//                if not (pr.Add n) then
//                    failwith "instance member name collision"
                nameInstanceMember typ n m      

        let simplifyFieldName (f: string) =
            f.Split('@').[0]

        for KeyValue(typ, ms) in remainingStaticMembers do
            let clAddr = getClassAddress typ
            for m in ms do
                let uaddr = 
                    match m with
                    | M.Constructor _ -> "New" :: clAddr
                    | M.Field (fName, _) -> simplifyFieldName fName :: clAddr
                    | M.Method (meth, _) ->
                        let n = meth.Value.MethodName
                        // Simplify names of active patterns
                        if n.StartsWith "|" then n.Split('|').[1] :: clAddr
                        // Simplify names of static F# extension members 
                        elif n.EndsWith ".Static" then
                            (n.Split('.') |> List.ofArray |> List.rev |> List.tail) @ clAddr
                        else n :: clAddr
                    | M.StaticConstructor _ -> "$cctor" :: clAddr
                let addr = r.StaticAddress uaddr
                nameStaticMember typ addr m

        let resolved = HashSet()
        
        // TODO: add abstract/interface methods even if there are no implementations
        let rec resolveRemainingInstanceMembers typ (cls: ClassInfo) ms =
            if resolved.Add typ then
                let pr = r.LookupPrototype typ
                // inherit members
                match cls.BaseClass with
                | None -> ()
                | Some bTyp ->
                    match classes.Current.TryFind bTyp with
                    | Some bCls ->
                        let bMs =
                            match remainingInstanceMembers.TryFind bTyp with
                            | Some bMs -> bMs 
                            | _ -> []  
                        resolveRemainingInstanceMembers bTyp bCls bMs
                    | _ -> ()
                    pr.UnionWith(r.LookupPrototype bTyp) 
                
                let ms =
                    let nonOverrides, overrides = 
                        ms |> List.partition (fun m ->
                            match m with
                            | M.Method (_, { Kind = N.Override _ }) -> false
                            | _ -> true 
                        )
                    Seq.append nonOverrides overrides

                for m in ms do
                    let name = 
                        match m with
                        | M.Field (fName, _) -> Resolve.getRenamed (simplifyFieldName fName) pr |> Some
                        | M.Method (mDef, { Kind = N.Instance | N.Abstract }) -> 
                            Resolve.getRenamed mDef.Value.MethodName pr |> Some
                        | M.Method (mDef, { Kind = N.Override td }) ->
                            match classes.TryFind td with
                            | Some tCls -> 
                                let smi = 
                                    match tCls.Methods.TryFind mDef with
                                    | Some (smi, _, _) -> Some smi
                                    | _ ->
                                    match compilingMethods.TryFind (td, mDef) with
                                    | Some ((NotCompiled (smi, _, _) | NotGenerated (_, _, smi, _, _)), _) -> Some smi
                                    | None ->
                                        printerrf "Abstract method not found in compilation: %s in %s" (string mDef.Value) td.Value.FullName
                                        None
                                match smi with
                                | Some (Instance n) -> Some n
                                | None -> None
                                | _ -> 
                                    printerrf "Abstract method must be compiled as instance member: %s in %s" (string mDef.Value) td.Value.FullName
                                    None
                            | _ ->
                                printerrf "Base type not found in compilation: %s" td.Value.FullName
                                None
                        | _ -> 
                            failwith "Invalid instance member kind"
                            None
                    name |> Option.iter (fun n -> nameInstanceMember typ n m)

        for KeyValue(typ, ms) in remainingInstanceMembers do
            resolveRemainingInstanceMembers typ classes.[typ] ms    

        // Add graph edges for GetEnumerator and Object methods redirection
        if hasGraph && this.AssemblyName = "WebSharper.Main" then
            let wsEnumeratorModule =
                TypeDefinition {
                    Assembly = "WebSharper.Main"
                    FullName = "WebSharper.Enumerator"
                } 

            let seq0Ty =
                TypeDefinition {
                    Assembly = "netstandard"
                    FullName = "System.Collections.IEnumerable"
                } 

            let seqTy =
                TypeDefinition {
                    Assembly = "netstandard"
                    FullName = "System.Collections.Generic.IEnumerable`1"
                } 

            let enum0Ty =
                TypeDefinition {
                    Assembly = "netstandard"
                    FullName = "System.Collections.IEnumerator"
                } 

            let enumTy =
                TypeDefinition {
                    Assembly = "netstandard"
                    FullName = "System.Collections.Generic.IEnumerator`1"
                }

            let getEnumerator0 =
                Method {
                    MethodName = "GetEnumerator"
                    Parameters = []
                    ReturnType = ConcreteType (NonGeneric enum0Ty)
                    Generics = 0
                } 
            
            let wsGetEnumerator0 =
                Method {
                    MethodName = "Get0"
                    Parameters = [ ConcreteType (NonGeneric seq0Ty) ]
                    ReturnType = ConcreteType (NonGeneric enum0Ty)
                    Generics = 0
                } 
    
            let getEnumerator =
                Method {
                    MethodName = "GetEnumerator"
                    Parameters = []
                    ReturnType = ConcreteType (Generic enumTy [TypeParameter 0])
                    Generics = 0
                } 

            let wsGetEnumerator =
                Method {
                    MethodName = "Get"
                    Parameters = [ ConcreteType (Generic seqTy [TypeParameter 0]) ]
                    ReturnType = ConcreteType (Generic enumTy [TypeParameter 0])
                    Generics = 1
                } 
            
            graph.AddEdge(AbstractMethodNode(seq0Ty, getEnumerator0), MethodNode(wsEnumeratorModule, wsGetEnumerator0))
            graph.AddEdge(AbstractMethodNode(seqTy, getEnumerator), MethodNode(wsEnumeratorModule, wsGetEnumerator))

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
            let uchEqIndex =
                try graph.Lookup.[MethodNode (uncheckedMdl, uncheckedEquals)]
                with e -> failwithf "%A | %A" uncheckedMdl.Value uncheckedEquals.Value
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

        // Add graph edge needed for Sitelets: Web.Controls will be looked up
        // and initialized on client-side by Activator.Activate
        if hasGraph && this.AssemblyName = "WebSharper.Web" then
            let activate =
                MethodNode(
                    TypeDefinition {
                        Assembly = "WebSharper.Main"
                        FullName = "WebSharper.Activator"
                    },
                    Method {
                        MethodName = "Activate"
                        Parameters = []
                        ReturnType = AST.VoidType
                        Generics = 0
                    } 
                )
            let control = 
                TypeNode(
                    TypeDefinition {
                        Assembly = "WebSharper.Web"
                        FullName = "WebSharper.Web.Control"
                    }
                )   
            
            let controlIndex = graph.Lookup.[control] 

            graph.AddEdge(controlIndex, activate)

    member this.VerifyRPCs () =
        let rec isWebControl (cls: ClassInfo) =
            match cls.BaseClass with
            | Some bT ->
                bT.Value.FullName = "WebSharper.Web.Control" || isWebControl classes.[bT]
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
        let info =
            {
                SiteletDefinition = this.SiteletDefinition 
                Dependencies = GraphData.Empty
                Interfaces = interfaces
                Classes = classes        
                CustomTypes = 
                    customTypes |> Dict.filter (fun _ v -> v <> NotCustomType)
                EntryPoint = None
                MacroEntries = macroEntries
                Quotations = quotations
                ResourceHashes = Dictionary()
            }    
        let jP = Json.Provider.CreateTyped(info)
        let st = Verifier.State(jP)
        for KeyValue(t, cls) in classes.Current do
            for KeyValue(m, (mi, _, _)) in cls.Methods do
                match mi with
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

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

[<AutoOpen>]
module private WSDefinitions =
    let wsEnumeratorModule =
        TypeDefinition {
            Assembly = "WebSharper.Main"
            FullName = "WebSharper.Enumerator"
        } 

    let seq0Ty =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Collections.IEnumerable"
        } 

    let seqTy =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Collections.Generic.IEnumerable`1"
        } 

    let enum0Ty =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Collections.IEnumerator"
        } 

    let enumTy =
        TypeDefinition {
            Assembly = "mscorlib"
            FullName = "System.Collections.Generic.IEnumerator`1"
        }
    
type Compilation(meta: Info, ?hasGraph) =    
    let notResolvedInterfaces = Dictionary<TypeDefinition, NotResolvedInterface>()
    let notResolvedClasses = Dictionary<TypeDefinition, NotResolvedClass>()
    let notResolvedCustomTypes = Dictionary<TypeDefinition, CustomTypeInfo>()
    let proxies = Dictionary<TypeDefinition, TypeDefinition>()

    let classes = MergedDictionary meta.Classes
    let assumeClass typ =
        match classes.TryFind typ with
        | Some (_, _, Some cls) -> cls
        | Some _ -> failwithf "Found custom type but not class: %s" typ.Value.FullName
        | _ -> failwithf "Couldn't find class: %s" typ.Value.FullName
    let updateClass typ f =
        match classes.TryFind typ with
        | Some (addr, ct, Some cls) -> classes.[typ] <- (addr, ct, Some (f cls))
        | _ -> ()
    let interfaces = MergedDictionary meta.Interfaces
    let macroEntries = MergedDictionary meta.MacroEntries

    let hasGraph = defaultArg hasGraph true
    let graph = if hasGraph then Graph.FromData(meta.Dependencies) else Unchecked.defaultof<_>

    let mutableExternals = Recognize.GetMutableExternals meta

    let compilingMethods = Dictionary<TypeDefinition * Method, CompilingMember * list<GenericParam> * Expression>()
    let compilingImplementations = Dictionary<TypeDefinition * TypeDefinition * Method, CompilingMember * Expression>()
    let compilingConstructors = Dictionary<TypeDefinition * Constructor, CompilingMember * Expression>()
    let compilingStaticConstructors = Dictionary<TypeDefinition, Address * Expression>()

    let mutable generatedClass = None
    let resolver = Resolve.Resolver()
    let generatedMethodAddresses = Dictionary()

    let errors = ResizeArray()
    let warnings = ResizeArray() 

    let mutable entryPoint = None

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

    member this.TypeTranslator = typeTranslator

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
                (
                    { Module = CurrentModule; Address = Hashed [ name ] },
                    CustomTypeInfo.NotCustomType,
                    Some {
                        BaseClass = None
                        Implements = []
                        Generics = []
                        Constructors = Dictionary() 
                        Fields = Dictionary() 
                        StaticConstructor = None 
                        Methods = Dictionary()
                        Implementations = Dictionary()
                        HasWSPrototype = false
                        IsStub = false
                        Macros = []
                        Type = None
                    }
                )
            ) 
            generatedClass <- Some td
            td

    member this.LocalAddress addr =
        { Module = CurrentModule; Address = addr }
    
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
                    member this.Constructors = MappedDictionary(cls.Constructors, fun (x, _, _) -> x) :> _
                    member this.Fields = cls.Fields
                    member this.StaticConstructor = cls.StaticConstructor |> Option.map fst
                    member this.Methods = MappedDictionary(cls.Methods, fun (x, _, _, _) -> x) :> _
                    member this.Implementations = MappedDictionary(cls.Implementations, fst) :> _
                    member this.HasWSPrototype = cls.HasWSPrototype
                    member this.Macros = cls.Macros
                }
            | _ -> None

        member this.GetJavaScriptClasses() = classes.Keys |> List.ofSeq
        member this.GetTypeAttributes(typ) = this.LookupTypeAttributes typ
        member this.GetFieldAttributes(typ, field) = this.LookupFieldAttributes typ field
        member this.GetMethodAttributes(typ, meth) = this.LookupMethodAttributes typ meth
        member this.GetConstructorAttributes(typ, ctor) = this.LookupConstructorAttributes typ ctor
        member this.GetTSTypeOf(t, ?gs) = typeTranslator.TSTypeOf (Array.ofList (defaultArg gs [])) t

        member this.ParseJSInline(inl: string, args: Expression list): Expression = 
            let vars = args |> List.map (fun _ -> Id.New(mut = false))
            let parsed = Recognize.createInline mutableExternals None vars false (Some (JavaScriptFile "")) inl
            Substitution(args).TransformExpression(parsed)
        
        member this.NewGenerated(addr, ?generics, ?args, ?returns) =
            let resolved = resolver.StaticAddress (List.rev addr)
            let td = this.GetGeneratedClass()
            let meth = 
                Method {
                    MethodName = resolved.Value |> List.rev |> String.concat "."
                    Parameters = defaultArg args []
                    ReturnType = defaultArg returns (TSType TSType.Any)
                    Generics = defaultArg generics 0
                }
            generatedMethodAddresses.Add(meth, resolved)
            td, meth, this.LocalAddress resolved

        member this.AddGeneratedCode(meth: Method, body: Expression) =
            let addr = this.LocalAddress generatedMethodAddresses.[meth]
            let td = this.GetGeneratedClass()
            compilingMethods.Add((td, meth),(NotCompiled (Static addr, true, Optimizations.None), [], body))

        member this.AddGeneratedInline(meth: Method, body: Expression) =
            let td = this.GetGeneratedClass()
            compilingMethods.Add((td, meth),(NotCompiled (Inline (true, false), true, Optimizations.None), [], body))

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
                classes.Current |> Dict.choose (function
                    | _, NotCustomType, None -> None
                    | a, ct, Some c as orig ->
                        match c.Methods with
                        | :? MergedDictionary<Method, CompiledMember * Optimizations * list<GenericParam> * Expression> as m -> 
                            Some (a, ct, Some { c with Methods = m.Current })
                        | _ -> Some orig
                    | x -> Some x
                )
            EntryPoint = entryPoint
            MacroEntries = macroEntries.Current
            ResourceHashes = Dictionary()
        }    

    member this.AddProxy(tProxy, tTarget) =
        proxies.Add(tProxy, tTarget)  

    member this.AddClass(typ, cls) =
        try
            notResolvedClasses.Add(typ, cls)
        with _ ->
            if cls.IsProxy then
                if Option.isSome cls.StrongName then
                    this.AddError(None, SourceError ("Proxy extension can't be strongly named: " + typ.Value.FullName))
                elif Option.isSome cls.BaseClass then
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
    
    member this.ProcessCustomType(typ: TypeDefinition, ct) =
        let getAddr hasWSPrototype = 
            resolver.ClassAddress(typ.Value, hasWSPrototype)
            |> this.LocalAddress
        let addr, cls = 
            match classes.TryFind typ with
            | Some ({ Address = Hashed []}, _, cls) -> getAddr (cls |> Option.exists (fun c -> c.HasWSPrototype)), cls
            | Some (a, _, cls) -> a, cls
            | _ -> getAddr false, None
        classes.[typ] <- (addr, ct, cls)
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

    member this.TryLookupClassInfo typ =   
        match classes.TryFind(this.FindProxied typ) with
        | Some (a, _, Some cls) -> Some (a, cls)
        | _ -> None

    member this.TryLookupClassAddressOrCustomType typ =   
        match classes.TryFind(this.FindProxied typ), this.GetCustomType typ with
        | Some (addr, _, Some c), (FSharpUnionInfo _) when c.HasWSPrototype -> 
            Choice1Of2 (addr.Sub("$"))
        | Some (addr, _, Some c), _ when c.HasWSPrototype -> 
            Choice1Of2 addr
        | Some (addr, _, _), NotCustomType -> 
            Choice1Of2 addr
        | _, ct -> Choice2Of2 ct
    
    member this.TryLookupInterfaceInfo typ =   
        interfaces.TryFind(this.FindProxied typ)
    
    member this.GetAbtractMethodGenerics typ meth =
        let typ = this.FindProxied typ
        try
            match classes.TryFind typ with
            | Some (_, _, Some ci) ->
                let mg =
                    match ci.Methods.TryFind meth with
                    | Some (_, _, mg, _) -> mg
                    | _ ->
                        let (_, mg, _) =  compilingMethods.[typ, meth]
                        mg
                Array.ofList (ci.Generics @ mg)
            | _ ->
                let ii = interfaces.[typ]
                Array.ofList (ii.Generics @ snd ii.Methods.[meth])
        with _ ->
            failwithf "Error looking up abstract method generics %s.%s" typ.Value.FullName meth.Value.MethodName

    member this.GetMethods typ =
        compilingMethods |> Seq.choose (fun (KeyValue ((td, m), _)) ->
            if td = typ then Some m else None
        ) |> Seq.append (
            match this.TryLookupClassInfo typ with
            | Some (_, cls) -> cls.Methods.Keys :> _ seq
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
        | _ ->
        match classes.TryFind typ with
        | Some (_, _, Some cls) ->
            cls.Methods.ContainsKey meth || compilingMethods.ContainsKey (typ, meth)
        | _ -> false

    member this.LookupMethodInfo(typ, meth) = 
        let typ = this.FindProxied typ
        match interfaces.TryFind typ with
        | Some intf -> 
            match intf.Methods.TryFind meth with
            | Some (m, c) ->
                Compiled (Instance m, Optimizations.None, intf.Generics @ c, Undefined)              
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
        match this.GetClassOrCustomType typ with
        | Choice1Of2 cls ->
            match cls.Methods.TryFind meth with
            | Some (mem, opts, gc, e) -> Compiled (mem, opts, cls.Generics @ gc, e)
            | _ -> 
                match compilingMethods.TryFind (typ, meth) with
                | Some (mem, gc, e) -> Compiling (mem, cls.Generics @ gc, e)
                | _ -> 
                    if not (List.isEmpty cls.Macros) then
                        let info =
                            List.foldBack (fun (m, p) fb -> Some (Macro (m, p, fb))) cls.Macros None |> Option.get
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
        | Choice2Of2 NotCustomType -> LookupMemberError (TypeNotFound typ)
        | Choice2Of2 i -> CustomTypeMember i

    member this.LookupFieldInfo(typ, field) =
        let typ = this.FindProxied typ
        match this.GetClassOrCustomType typ with
        | Choice1Of2 cls ->
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
        | Choice2Of2 NotCustomType -> LookupFieldError (TypeNotFound typ)
        | Choice2Of2 i -> CustomTypeField i

    member this.LookupConstructorInfo(typ, ctor) =
        let typ = this.FindProxied typ
        match this.GetClassOrCustomType typ with
        | Choice1Of2 cls ->
            match cls.Constructors.TryFind ctor with
            | Some (m, o, e) -> Compiled (m, o, cls.Generics, e)
            | _ -> 
                match compilingConstructors.TryFind (typ, ctor) with
                | Some  (m, e) -> Compiling (m,  cls.Generics, e)
                | _ -> 
                    if not (List.isEmpty cls.Macros) then
                        let info =
                            List.foldBack (fun (m, p) fb -> Some (Macro (m, p, fb))) cls.Macros None |> Option.get
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
        
    member this.TryLookupStaticConstructorAddress(typ) =
        match classes.TryFind(this.FindProxied typ) with
        | Some (_, _, Some cls) ->
            match cls.StaticConstructor with
            | Some(_, GlobalAccess a) when a.Address.Value = [ "ignore" ] -> None
            | Some (cctor, _) -> Some cctor
            | None -> None
        | _ -> None

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
            | Some (_, _, _, Undefined)
            | None ->    
                cls.Methods.[meth] <- (info, opts, gc, comp)
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
        cls.Constructors.Add(ctor, (info, opts, comp))

    member this.FailedCompiledConstructor(typ, ctor) =
        let typ = this.FindProxied typ 
        compilingConstructors.Remove(typ, ctor) |> ignore

    member this.CompilingStaticConstructors = compilingStaticConstructors

    member this.AddCompiledStaticConstructor(typ, addr, cctor) =
        let typ = this.FindProxied typ 
        compilingStaticConstructors.Remove typ |> ignore
        match classes.TryFind typ with
        | Some (caddr, ct, Some cls) ->
            classes.[typ] <- (caddr, ct, Some { cls with StaticConstructor = Some (addr, cctor) })
        | _ -> failwithf "Adding compiled static constructor to %s" typ.Value.FullName

    member this.CompilingImplementations = compilingImplementations

    member this.AddCompiledImplementation(typ, intf, meth, info, comp) =
        let typ = this.FindProxied typ 
        compilingImplementations.Remove(typ, intf, meth) |> ignore
        let cls = assumeClass typ
        cls.Implementations.Add((intf, meth), (info, comp))

    member this.Resolve () =
        
        let printerrf x = Printf.kprintf (fun s -> this.AddError (None, SourceError s)) x

        let stronglyNamedTypes = ResizeArray()
        let remainingTypes = ResizeArray()

        let rec resolveInterface (typ: TypeDefinition) (nr: NotResolvedInterface) =
            let allMembers = HashSet()
            let allNames = HashSet()
            
            let rec addInherited (i: TypeDefinition) (n: InterfaceInfo) =
                for i in n.Extends do
                    let i = i.Entity
                    interfaces.TryFind i |> Option.iter (addInherited i)
                for KeyValue(m, (n, c)) in n.Methods do
                    if not (allMembers.Add (i, m)) then
                        if not (allNames.Add n) then
                            printerrf "Interface method name collision: %s on %s" n typ.Value.FullName
            
            for i in nr.Extends do
                let i = i.Entity
                notResolvedInterfaces.TryFind i |> Option.iter (resolveInterface i)       
                interfaces.TryFind i |> Option.iter (addInherited i)
            
            let resMethods = Dictionary()
                            
            for m, n, c in nr.NotResolvedMethods do
                match n with
                | Some n -> 
                    if not (allNames.Add n) then
                        printerrf "Explicitly declared interface method name collision: %s on %s" n typ.Value.FullName
                    resMethods.Add(m, (n, c))
                | _ -> ()
            
            let intfName = 
                match nr.StrongName with
                | Some "" -> ""
                | Some sn -> sn + "$"
                | _ ->                     
                    typ.Value.FullName.Replace('.', '_').Replace('+', '_').Replace('`', '_') + "$"

            for m, n, c in nr.NotResolvedMethods do
                match n with
                | None ->
                    let n = Resolve.getRenamed (intfName + m.Value.MethodName) allNames
                    resMethods.Add(m, (n, c))
                | _ -> ()

            let resNode =
                {
                    Address = Address.Empty()
                    Extends = nr.Extends |> List.filter (fun i -> interfaces.ContainsKey i.Entity)
                    Methods = resMethods
                    Generics = nr.Generics
                    Type = nr.Type
                }
            interfaces.Add(typ, resNode)
            notResolvedInterfaces.Remove typ |> ignore
            match nr.StrongName with
            | Some sn ->
                stronglyNamedTypes.Add (typ, sn, false)
            | _ -> 
                remainingTypes.Add (typ, false)
            
        
        while notResolvedInterfaces.Count > 0 do
            let (KeyValue(typ, nr)) = Seq.head notResolvedInterfaces  
            resolveInterface typ nr

        let unresolvedCctor = Some (Address.Empty(), Undefined)

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
                        mi.Kind <- NotResolvedMemberKind.AsStatic         
                        mi.Body <- 
                            match mi.Body with
                            | Function(args, typ, b) ->
                                let thisVar = Id.New("$this", mut = false)
                                Function (thisVar :: args, typ,
                                    ReplaceThisWithVar(thisVar).TransformStatement(b) 
                                )
                            | _ ->
                                failwith "Unexpected: instance member not a function"
                    | _ -> ()
            
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
            let hasWSPrototype = hasWSPrototype cls.Kind baseCls cls.Members                
            let isStub = cls.Kind = NotResolvedClassKind.Stub
            let methods =
                match classes.TryFind typ with
                | Some (_, _, Some c) -> MergedDictionary c.Methods :> IDictionary<_,_>
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
                    HasWSPrototype = hasWSPrototype
                    IsStub = isStub
                    Macros = cls.Macros |> List.map (fun (m, p) -> m, p |> Option.map ParameterObject.OfObj)
                    Type =
                        match cls.Type with
                        | Some _ as t -> t
                        | None -> if isStub then Some TSType.Any else None
                }
            
            match notResolvedCustomTypes.TryFind typ with
            | Some ct ->
                classes.Add(typ, (Address.Empty(), ct, Some resCls))
                notResolvedCustomTypes.Remove typ |> ignore
            | _ ->
                classes.Add(typ, (Address.Empty(), NotCustomType, Some resCls))
            
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
            for KeyValue(ct, (_, cti, _)) in classes do
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

        let compiledStaticMember a (nr : NotResolvedMethod) =
            match nr.Kind with
            | N.Constructor
            | N.Static -> Static a
            | N.AsStatic -> AsStatic a
            | _ -> failwith "Invalid static member kind"
            |> withMacros nr        

        let compiledNoAddressMember (nr : NotResolvedMethod) =
            match nr.Kind with
            | N.Inline ta -> Inline (true, ta)
            | N.Remote (k, h, r) -> Remote (k, h, r |> Option.map (fun (t, p) -> t, p |> Option.map ParameterObject.OfObj))
            | N.NoFallback -> Inline (true, false) // will be erased
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
            

        let setClassAddress typ (clAddr: PlainAddress) =
            match classes.TryFind typ with
            | Some (addr, ct, cls) -> classes.[typ] <- (this.LocalAddress clAddr, ct, cls)
            | None -> ()

        let setInterfaceAddress typ clAddr =
            let res = interfaces.[typ]
            interfaces.[typ] <- { res with Address = this.LocalAddress clAddr }
            
        // split to resolve steps
        let fullyNamedStaticMembers = ResizeArray()
        let remainingNamedStaticMembers = Dictionary()
        let namedInstanceMembers = Dictionary() // includes implementations
        let remainingStaticMembers = Dictionary()
        let remainingInstanceMembers = Dictionary() // includes overrides

        let addCctorCall typ (ci: ClassInfo) expr =
            if Option.isSome ci.StaticConstructor then
                match expr with
                | Function (args, ret, body) ->
                    Function(args, ret, CombineStatements [ ExprStatement (Cctor typ); body ])
                // inlines
                | _ -> Sequential [ Cctor typ; expr ]
            else expr
        
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

            let constructors = ResizeArray()
            let mutable hasInlinedCtor = false

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
                                | _ -> failwith "impossible: constructor cannot be override or implementation"
                                None, None, true
                            else 
                                None, Some false, false
                        | N.Abstract
                        | N.Instance -> sn, Some false, false
                        | N.InlineImplementation _ -> None, Some false, false
                        | N.Static
                        | N.AsStatic
                        | N.Constructor -> sn, Some true, false
                        | N.Remote _
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
                                cc.Methods.Add (mDef, (comp, opts isPure nr, nr.Generics, nr.Body))
                            with _ ->
                                printerrf "Duplicate definition for method %s.%s" typ.Value.FullName mDef.Value.MethodName
                        else 
                            compilingMethods.Add((typ, mDef), (toCompilingMember nr comp, nr.Generics, addCctorCall typ cc nr.Body)) 
                    | _ -> failwith "Fields and static constructors are always named"     
                | None, Some true ->
                    match m with 
                    | M.Constructor (cDef, nr) ->
                        if canHavePrototype && nr.Kind = N.Constructor then
                            constructors.Add (cDef, nr)
                        else
                            Dict.addToMulti remainingStaticMembers typ m
                    | _ ->
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
                            | Some (n, c) ->    
                                Dict.addToMulti namedInstanceMembers typ (m, n)
                            | _ -> printerrf "Failed to look up name for implemented member: %s.%s in type %s" td.Value.FullName mDef.Value.MethodName typ.Value.FullName 
                        | _ ->
                            printerrf "Failed to look up interface for implementing: %s by type %s" td.Value.FullName typ.Value.FullName
                    | M.Method (mDef, ({ Kind = N.InlineImplementation td } as nr)) ->
                        () // TODO check redirection
                    | _ -> 
                        Dict.addToMulti remainingInstanceMembers typ m    
            
            let addConstructor (cDef, nr) comp =
                if nr.Compiled && Option.isNone cc.StaticConstructor then
                    let isPure =
                        nr.Pure || (Option.isNone cc.StaticConstructor && isPureFunction nr.Body)
                    cc.Constructors.Add(cDef, (comp, opts isPure nr, nr.Body))
                else
                    compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, addCctorCall typ cc nr.Body))
            
            match constructors.Count with
            | 0 ->
                if hasInlinedCtor && Option.isNone cls.Type && cls.Kind <> NotResolvedClassKind.Stub && not (TypeTranslator.CustomTranslations.ContainsKey typ) then
                    this.AddWarning(Some cls.SourcePos, SourceWarning ("Class " + typ.Value.FullName + " only has inlined constructors, consider adding a Type attribute" ))
            | 1 -> addConstructor constructors.[0] New
            | _ -> 
                for i, ctor in Seq.indexed constructors do
                    addConstructor ctor (NewIndexed i)

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
                if not (resolver.ExactClassAddress(addr, cc.HasWSPrototype)) then
                    this.AddError(None, NameConflict ("Class name conflict", sn))
                setClassAddress typ (Hashed addr)
                processCustomType typ
            else
                setInterfaceAddress typ (Hashed addr)

        let nameStaticMember typ a m = 
            let la = this.LocalAddress a
            let res = assumeClass typ
            match m with
            | M.Constructor (cDef, nr) ->
                let comp = compiledStaticMember la nr
                let body =
                    match nr.Body with
                    | Function(cargs, typ, cbody) ->
                        let o = Id.New "o"
                        let b = 
                            Let(o, Object[], Sequential [ StatementExpr (ReplaceThisWithVar(o).TransformStatement(cbody), None); Var o ])
                        Function(cargs, typ, Return b)
                    | _ -> 
                        failwith "Expecting a function as compiled form of constructor"
                compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, addCctorCall typ res body))
            | M.Field (fName, nr) ->
                res.Fields.Add(fName, (StaticField la, nr.IsReadonly, nr.FieldType))
            | M.Method (mDef, nr) ->
                let comp = compiledStaticMember la nr
                if nr.Compiled && Option.isNone res.StaticConstructor then 
                    let isPure =
                        nr.Pure || (notVirtual nr.Kind && Option.isNone res.StaticConstructor && isPureFunction nr.Body)
                    res.Methods.Add(mDef, (comp, opts isPure nr, nr.Generics, nr.Body))
                else
                    compilingMethods.Add((typ, mDef), (toCompilingMember nr comp, nr.Generics, addCctorCall typ res nr.Body))
            | M.StaticConstructor expr ->                
                // TODO: do not rely on address on compiled state
                updateClass typ (fun cls -> { cls with StaticConstructor = Some (la, Undefined) })
                compilingStaticConstructors.Add(typ, (la, expr))
        
        let nameInstanceMember typ name m =
            let res = assumeClass typ
            let add k v (d: IDictionary<_,_>) =
                try d.Add(k, v)
                with _ -> failwithf "Instance method name already used: %s in type %s" name typ.Value.FullName
            match m with
            | M.Field (fName, f) ->
                let fi =
                    if f.IsOptional then 
                        OptionalField name
                    else
                        match System.Int32.TryParse name with
                        | true, i -> IndexedField i
                        | _ -> InstanceField name
                res.Fields |> add fName (fi, f.IsReadonly, f.FieldType)
            | M.Method (mDef, nr) ->
                let comp = compiledInstanceMember name nr
                match nr.Kind with
                | N.Implementation dtyp 
                | N.Override dtyp when dtyp <> typ ->
                    if nr.Compiled && Option.isNone res.StaticConstructor then 
                        res.Implementations |> add (dtyp, mDef) (comp, nr.Body)
                    else
                        compilingImplementations |> add (typ, dtyp, mDef) (toCompilingMember nr comp, addCctorCall typ res nr.Body)
                | _ ->
                    if nr.Compiled && Option.isNone res.StaticConstructor then 
                        let isPure = nr.Pure || isPureFunction nr.Body
                        res.Methods |> add mDef (comp, opts isPure nr, nr.Generics, nr.Body)
                    else
                        compilingMethods |> add (typ, mDef) (toCompilingMember nr comp, nr.Generics, addCctorCall typ res nr.Body)
            | _ -> failwith "Invalid instance member kind"   

        let getClassAddress typ =
            let (addr, _, _) = classes.[typ]
            addr.Address.Value
                                     
        for typ, m, sn in fullyNamedStaticMembers do
            let addr =
                match sn.Split('.') with
                | [||] ->
                    printerrf "Invalid Name attribute argument on type '%s'" typ.Value.FullName
                    ["$$ERROR$$"]
                | [| n |] -> 
                    n :: getClassAddress typ
                | a -> List.ofArray (Array.rev a)
            if not (resolver.ExactStaticAddress addr) then
                this.AddError(None, NameConflict ("Static member name conflict", sn)) 
            nameStaticMember typ (Hashed addr) m
              
        for typ, isClass in remainingTypes do
            let a = resolver.ClassAddress(typ.Value, isClass && (assumeClass typ).HasWSPrototype)
            if isClass then
                setClassAddress typ a
                processCustomType typ
            else 
                setInterfaceAddress typ a
        
        // initialize remaining non-TS-class custom types
        for KeyValue(typ, ct) in notResolvedCustomTypes do
            classes.Add(typ, (Address.Empty(), ct, None))
            this.ProcessCustomType(typ, ct)

        for KeyValue(typ, ms) in remainingNamedStaticMembers do
            let clAddr = getClassAddress typ
            for m, n in ms do
                let addr = n :: clAddr
                if not (resolver.ExactStaticAddress addr) then
                    this.AddError(None, NameConflict ("Static member name conflict", addr |> String.concat "."))
                nameStaticMember typ (Hashed addr) m
           
        // TODO: check nothing is hiding (exclude overrides and implementations)
        for KeyValue(typ, ms) in namedInstanceMembers do
            let pr = resolver.LookupPrototype typ
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
                let addr = resolver.StaticAddress uaddr
                nameStaticMember typ addr m

        let resolved = HashSet()
        
        // TODO: add abstract/interface methods even if there are no implementations
        let rec resolveRemainingInstanceMembers typ (cls: ClassInfo) ms =
            if resolved.Add typ then
                let pr = resolver.LookupPrototype typ
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
                    pr.UnionWith(resolver.LookupPrototype bTyp) 
                
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
                            | Some (_, _, Some tCls) -> 
                                let smi = 
                                    match tCls.Methods.TryFind mDef with
                                    | Some (smi,_,_,_) -> Some smi
                                    | _ ->
                                    match compilingMethods.TryFind (td, mDef) with
                                    | Some ((NotCompiled (smi,_,_) | NotGenerated (_,_,smi,_,_)),_,_) -> Some smi
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
            resolveRemainingInstanceMembers typ (assumeClass typ) ms

        // Add graph edges for Object methods redirection
        if hasGraph && this.AssemblyName = "WebSharper.Main" then
            
            let equals =
                Method {
                    MethodName = "Equals"
                    Parameters = [ ConcreteType (NonGeneric Definitions.Obj) ]
                    ReturnType = NonGenericType Definitions.Bool
                    Generics = 0
                }

            let getHashCode =
                Method {
                    MethodName = "GetHashCode"
                    Parameters = []
                    ReturnType = NonGenericType Definitions.Int
                    Generics = 0
                } 

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
            let uchEqIndex = graph.Lookup.[MethodNode (uncheckedMdl, uncheckedEquals)]

            graph.AddEdge(objEqIndex, uchEqIndex)
            graph.AddEdge(uchEqIndex, objEqIndex)

            let objHashIndex = graph.Lookup.[AbstractMethodNode(Definitions.Obj, getHashCode)]
            let uchHashIndex = graph.Lookup.[MethodNode (uncheckedMdl, uncheckedHash)]

            graph.AddEdge(objHashIndex, uchHashIndex)
            graph.AddEdge(uchHashIndex, objHashIndex)

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
        let info =
            {
                SiteletDefinition = this.SiteletDefinition 
                Dependencies = GraphData.Empty
                Interfaces = interfaces
                Classes =
                    classes |> Dict.filter (fun _ -> function
                        | (_, NotCustomType, None) -> false
                        | _ -> true)
                EntryPoint = None
                MacroEntries = macroEntries
                ResourceHashes = Dictionary()
            }    
        let jP = Json.Provider.CreateTyped(info)
        let st = Verifier.State(jP)
        for KeyValue(t, cls) in classes.Current do
            match cls with
            | _, _, None -> ()
            | _, _, Some cls ->
            for KeyValue(m, (mi,_,_,_)) in cls.Methods do
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

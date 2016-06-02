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
    
type Compilation(meta: Info) =    
    let notResolvedInterfaces = Dictionary<TypeDefinition, NotResolvedInterface>()
    let notResolvedClasses = Dictionary<TypeDefinition, NotResolvedClass>()
    let proxies = Dictionary<TypeDefinition, TypeDefinition>()

    let classes = MergedDictionary meta.Classes
    let interfaces = MergedDictionary meta.Interfaces
    let notAnnotatedCustomTypes = Dictionary()
    let customTypes = MergedDictionary meta.CustomTypes

    let graph = Graph.FromData(meta.Dependencies)

    let compilingMethods = Dictionary<TypeDefinition * Method, CompilingMember * Expression>()
    let compilingImplementations = Dictionary<TypeDefinition * TypeDefinition * Method, CompilingMember * Expression>()
    let compilingConstructors = Dictionary<TypeDefinition * Constructor, CompilingMember * Expression>()
    let compilingStaticConstructors = Dictionary<TypeDefinition, Address * Expression>()

    let findProxied typ = 
        match proxies.TryFind typ with
        | Some p -> p 
        | _ -> typ

    let errors = ResizeArray()
    let warnings = ResizeArray() 

    let mutable entryPoint = None
    let mutable remotingCode = -1

    let macros = System.Collections.Generic.Dictionary<TypeDefinition, Macro option>()
    let generators = System.Collections.Generic.Dictionary<TypeDefinition, Generator option>()

    member val UseMacros = true with get, set
    member val SiteletDefinition: option<TypeDefinition> = None with get, set
    member val AssemblyName = "EntryPoint" with get, set
    member val AssemblyRequires = [] : list<TypeDefinition> with get, set
    
    member val CustomTypesReflector = None : option<TypeDefinition -> CustomTypeInfo> with get, set 

    member this.FindProxied typ =
        match proxies.TryFind typ with
        | Some p -> p 
        | _ -> typ
    
    member this.GetRemoteHandle() =
        remotingCode <- remotingCode + 1
        {
            Assembly = this.AssemblyName
            Code = remotingCode
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

    interface ICompilation with
        member this.GetCustomTypeInfo typ = 
            this.GetCustomType typ
        
        member this.GetInterfaceInfo typ =
            interfaces.TryFind (findProxied typ)

        member this.GetClassInfo typ = 
            let fstOf3 (x, _, _) = x
            match classes.TryFind (findProxied typ) with
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
                    member this.IsModule = cls.IsStatic
                    member this.Macros = cls.Macros
                }

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
                                if this.UseMacros then
                                    this.AddError(None, SourceError(sprintf "Failed to find macro type: '%s'" macro.Value.FullName))
                                None
                            | t -> Some t
                        with e -> 
                            if this.UseMacros then
                                this.AddError(None, SourceError(sprintf "Failed to load macro type: '%s', Error: %s" macro.Value.FullName e.Message))
                            None
                    let! mctor, arg =
                        match mt.GetConstructor([||]) with
                        | null -> 
                            if this.UseMacros then
                                this.AddError(None, SourceError(sprintf "Macro does not have supported constructor: '%s'" macro.Value.FullName))
                            None
                        | mctor -> Some (mctor, [||]) 
                    let! inv =
                        try mctor.Invoke(arg) |> Some
                        with e ->
                            if this.UseMacros then
                                this.AddError(None, SourceError(sprintf "Creating macro instance failed: '%s', Error: %s" macro.Value.FullName e.Message))
                            None
                    match inv with 
                    | :? WebSharper.Core.Macro as m -> 
                        return m
                    | _ -> 
                        if this.UseMacros then
                            this.AddError(None, SourceError(sprintf "Macro type does not inherit from WebSharper.Core.Macro: '%s'" macro.Value.FullName))
                } 
            macros.Add(macro, res)
            res

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
                                if this.UseMacros then
                                    this.AddError(None, SourceError(sprintf "Failed to find generator type: '%s'" gen.Value.FullName))
                                None
                            | t -> Some t
                        with e -> 
                            if this.UseMacros then
                                this.AddError(None, SourceError(sprintf "Failed to load generator type: '%s', Error: %s" gen.Value.FullName e.Message))
                            None
                    let! mctor, arg =
                        match mt.GetConstructor([|typeof<Compilation>|]) with
                        | null ->
                            match mt.GetConstructor([||]) with
                            | null -> 
                                if this.UseMacros then  
                                    this.AddError(None, SourceError(sprintf "Generator does not have supported constructor: '%s'" gen.Value.FullName))
                                None
                            | mctor -> Some (mctor, [||]) 
                        | mctor -> Some (mctor, [|box this|]) 
                    let! inv =
                        try mctor.Invoke(arg) |> Some
                        with e ->
                            if this.UseMacros then
                                this.AddError(None, SourceError(sprintf "Creating generator instance failed: '%s', Error: %s" gen.Value.FullName e.Message))
                            None
                    match inv with 
                    | :? WebSharper.Core.Generator as g -> return g
                    | _ -> 
                        if this.UseMacros then
                            this.AddError(None, SourceError(sprintf "Generator type does not inherit from WebSharper.Core.Generator: '%s'" gen.Value.FullName))
                } 
            generators.Add(gen, res)
            res

    member this.DependencyMetadata = meta

    member this.Graph = graph

    member this.ToCurrentMetadata(?ignoreErrors) =
        if errors.Count > 0 && not (ignoreErrors = Some true) then 
            failwith "This compilation has errors"
        {
            SiteletDefinition = this.SiteletDefinition 
            Dependencies = graph.GetCurrentData()
            Interfaces = interfaces.Current
            Classes = classes.Current        
            CustomTypes = 
                customTypes.Current |> Dict.filter (fun _ v -> v <> NotCustomType)
            EntryPoint = entryPoint
        }    

    member this.AddProxy(tProxy, tTarget) =
        proxies.Add(tProxy, tTarget)  

    member this.AddClass(typ, cls) =
        try
            notResolvedClasses.Add(typ, cls)
        with _ ->
            if cls.IsProxy then
                if Option.isSome cls.StrongName then
                    this.AddError(None, SourceError ("Proxy extension can't be strongly named."))
                elif Option.isSome cls.BaseClass then
                    this.AddError(None, SourceError ("Proxy extension can't have a non-Object base class."))
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
        let typ = findProxied typ
        match customTypes.TryFind typ with
        | Some res -> res
        | _ ->
        match notAnnotatedCustomTypes.TryFind typ with
        | Some res ->
            // TODO unions with cases 
            customTypes.Add (typ, res)
            res
        | _ ->
            let res = this.CustomTypesReflector.Value typ
            customTypes.Add(typ, res)
            res

    member this.TryLookupClassInfo typ =   
        classes.TryFind(findProxied typ)
    
    member this.IsImplementing (typ, intf) : bool option =
        classes.TryFind(findProxied typ)
        |> Option.map (fun cls ->
            cls.Implementations |> Seq.exists (fun (KeyValue ((i, _), _)) -> i = intf)
            || cls.BaseClass |> Option.exists (fun b -> this.IsImplementing(b, intf) |> Option.exists id) 
        )

    member this.IsInterface(typ) =
        let typ = findProxied typ
        interfaces.ContainsKey typ

    member this.ConstructorExistsInMetadata (typ, ctor) =
        let typ = findProxied typ
        match classes.TryFind typ with
        | Some cls ->
            cls.Constructors.ContainsKey ctor || compilingConstructors.ContainsKey (typ, ctor)
        | _ -> false

    member this.MethodExistsInMetadata (typ, meth) =
        let typ = findProxied typ
        match interfaces.TryFind typ with
        | Some intf -> 
            intf.Methods.ContainsKey meth
        | _ ->
        match classes.TryFind typ with
        | Some cls ->
            cls.Methods.ContainsKey meth || compilingMethods.ContainsKey (typ, meth)
        | _ -> false

    member this.LookupMethodInfo(typ, meth) = 
        let typ = findProxied typ
        match interfaces.TryFind typ with
        | Some intf -> 
            match intf.Methods.TryFind meth with
            | Some m ->
                if typ.Value.Assembly = "mscorlib" then
                    match typ.Value.FullName with
                    | "System.Collections.IEnumerable" ->
                        Compiled (Inline, false, Application(Global ["WebSharper"; "Enumerator"; "Get0"], [Hole 0], false, Some 1))
                    | "System.Collections.Generic.IEnumerable`1" ->
                        Compiled (Inline, false, Application(Global ["WebSharper"; "Enumerator"; "Get"], [Hole 0], false, Some 1))
                    | _ -> 
                        Compiled (Instance m, false, Undefined)
                else
                    Compiled (Instance m, false, Undefined)              
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
                        Compiled (info, false, Undefined)
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
        let typ = findProxied typ
        match classes.TryFind typ with
        | Some cls ->
            match cls.Fields.TryFind field with
            | Some f -> CompiledField f
            | _ -> 
                match this.GetCustomType typ with
                | NotCustomType -> LookupFieldError (FieldNotFound (typ, field))
                | i -> CustomTypeField i
        | _ ->
            match this.GetCustomType typ with
            | NotCustomType -> LookupFieldError (TypeNotFound typ)
            | i -> CustomTypeField i

    member this.LookupConstructorInfo(typ, ctor) =
        let typ = findProxied typ
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
                        Compiled (info, false, Undefined)
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
        let typ = findProxied typ
        let cls = classes.[typ]
        cls.StaticConstructor |> Option.map fst

    member this.LookupStaticConstructorAddress(typ) =
        this.TryLookupStaticConstructorAddress(typ).Value

    member this.TryGetRecordConstructor(typ) =
        let typ = findProxied typ
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

    member this.CompilingMethods = compilingMethods  
    member this.CompilingConstructors = compilingConstructors

    member this.AddCompiledMethod(typ, meth, info, isPure, comp) =
        let typ = findProxied typ 
        compilingMethods.Remove(typ, meth) |> ignore
        let cls = classes.[typ]
        match cls.Methods.TryFind meth with
        | Some (_, _, Undefined)
        | None ->    
            cls.Methods.[meth] <- (info, isPure, comp)
        | _ ->
            failwithf "Method already added: %s %s" typ.Value.FullName (string meth.Value)

    member this.AddCompiledConstructor(typ, ctor, info, isPure, comp) = 
        let typ = findProxied typ 
        compilingConstructors.Remove(typ, ctor) |> ignore
        let cls = classes.[typ]
        cls.Constructors.Add(ctor, (info, isPure, comp))

    member this.GetCompilingStaticConstructors() =
        compilingStaticConstructors |> Seq.map (fun (KeyValue(t, (a, c))) -> t, a, c) |> Array.ofSeq

    member this.AddCompiledStaticConstructor(typ, addr, cctor) =
        let typ = findProxied typ 
        compilingStaticConstructors.Remove typ |> ignore
        let cls = classes.[typ]
        classes.[typ] <- { cls with StaticConstructor = Some (addr, cctor) }

    member this.GetCompilingImplementations() =
        compilingImplementations |> Seq.map (fun (KeyValue((t, i, m), (n, e))) -> t, i, m, n, e) |> Array.ofSeq

    member this.AddCompiledImplementation(typ, intf, meth, info, comp) =
        let typ = findProxied typ 
        compilingImplementations.Remove(typ, intf, meth) |> ignore
        let cls = classes.[typ]
        cls.Implementations.Add((intf, meth), (info, comp))

    member this.Resolve () =
        
        let printerrf x = Printf.kprintf (fun s -> this.AddError (None, SourceError s)) x

        let rec resolveInterface typ (nr: NotResolvedInterface) =
            let allMembers = HashSet()
            
            let getInterface i =
                match interfaces.TryFind i with
                | Some i -> Some i
                | _ ->
                    if i <> Definitions.Obj then
                        printerrf "Failed to look up interface '%s'" i.Value.FullName 
                    None

            let rec addInherited (n: InterfaceInfo) =
                for i in n.Extends do
                    getInterface i |> Option.iter addInherited
                for m in n.Methods.Values do
                    if not (allMembers.Add m) then
                        failwith "Interface method name collision."
            
            for i in nr.Extends do
                notResolvedInterfaces.TryFind i |> Option.iter (resolveInterface i)       
                getInterface i |> Option.iter addInherited
            
            let resMethods = Dictionary()
                            
            for m, n in nr.NotResolvedMethods do
                match n with
                | Some n -> 
                    if not (allMembers.Add n) then
                        failwith "Explicitly declared interface method name collision."
                    resMethods.Add(m, n)
                | _ -> ()

            let intfName = typ.Value.FullName.Replace('.', '_').Replace('+', '_').Replace('`', '_') + "$"

            for m, n in nr.NotResolvedMethods do
                match n with
                | None ->
                    let n = Resolve.getRenamed (intfName + m.Value.MethodName) allMembers
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

        let someEmptyAddress = Some (Address [])
        let unresolvedCctor = Some (Address [], Undefined)

        let isInstanceKind k = match k with N.Instance | N.Override _ | N.Implementation _ -> true | _ -> false

        let isInstanceMember m =
            match m with
            | M.Constructor (_, { Kind = k })
            | M.Method (_, { Kind = k }) -> isInstanceKind k 
            | _ -> false

        let asmNodeIndex = graph.AddOrLookupNode(AssemblyNode (this.AssemblyName, true))
        for req in this.AssemblyRequires do
            graph.AddEdge(asmNodeIndex, ResourceNode req)

        // initialize all class entries
        for KeyValue(typ, cls) in notResolvedClasses do
            let hasPrototype = cls.Members |> Seq.exists isInstanceMember
            let cctor = cls.Members |> Seq.tryPick (function M.StaticConstructor e -> Some e | _ -> None)
            let baseCls =
                cls.BaseClass |> Option.bind (fun b ->
                    let b = findProxied b
                    if classes.ContainsKey b || notResolvedClasses.ContainsKey b then Some b else None
                )
            let isStatic =                
                match cls.Kind with
                | NotResolvedClassKind.Static -> true
                | NotResolvedClassKind.FSharpType -> false
                | _ ->
                    cls.Members
                    |> Seq.forall (
                        function
                        | M.Constructor (_, nr)
                        | M.Method (_, nr) ->
                            match nr.Kind with
                            | N.Instance 
                            | N.Constructor 
                            | N.Override _
                            | N.Implementation _ -> false
                            | _ -> true
//                        | M.Field (_, f) -> f.IsStatic
                        | _ -> true
                    )
            classes.Add (typ,
                {
                    Address = if hasPrototype then someEmptyAddress else None
                    BaseClass = baseCls
                    Constructors = Dictionary() 
                    Fields = Dictionary() 
                    StaticConstructor = if Option.isSome cctor then unresolvedCctor else None 
                    Methods = Dictionary()
                    Implementations = Dictionary()
                    IsStatic = isStatic
                    Macros = cls.Macros |> List.map (fun (m, p) -> m, p |> Option.map ParameterObject.OfObj)
                }
            ) 
            // set up dependencies
            let clsNodeIndex = graph.AddOrLookupNode(TypeNode typ)
            graph.AddEdge(clsNodeIndex, asmNodeIndex)
            for req in cls.Requires do
                graph.AddEdge(clsNodeIndex, ResourceNode req)
            cls.BaseClass |> Option.iter (fun b -> graph.AddEdge(clsNodeIndex, TypeNode (findProxied b)))
            for m in cls.Members do
                match m with
                | M.Constructor (ctor, { Kind = k; Requires = reqs }) -> 
                    match k with
                    | N.NoFallback -> ()
                    | _ ->
                        let cNode = graph.AddOrLookupNode(ConstructorNode(typ, ctor))
                        graph.AddEdge(cNode, clsNodeIndex)
                        for req in reqs do
                            graph.AddEdge(cNode, ResourceNode req)
                | M.Method (meth, { Kind = k; Requires = reqs }) -> 
                    match k with
                    | N.Override btyp ->
                        let btyp = findProxied btyp  
                        let mNode = graph.AddOrLookupNode(MethodNode(typ, meth))
                        graph.AddEdge(mNode, AbstractMethodNode(btyp, meth))
                        graph.AddOverride(typ, btyp, meth)
                        graph.AddEdge(mNode, clsNodeIndex)
                        for req in reqs do
                            graph.AddEdge(mNode, ResourceNode req)
                    | N.Implementation intf ->
                        let intf = findProxied intf 
                        let mNode = graph.AddOrLookupNode(ImplementationNode(typ, intf, meth))
                        graph.AddImplementation(typ, intf, meth)
                        graph.AddEdge(mNode, clsNodeIndex)
                        for req in reqs do
                            graph.AddEdge(mNode, ResourceNode req)
                    | N.NoFallback -> ()
                    | _ -> 
                        let mNode = graph.AddOrLookupNode(MethodNode(typ, meth))
                        graph.AddEdge(mNode, clsNodeIndex)
                        for req in reqs do
                            graph.AddEdge(mNode, ResourceNode req)
                | _ -> ()

        let withMacros (nr : NotResolvedMethod) woMacros =
            if List.isEmpty nr.Macros then woMacros else
                if nr.Kind = N.NoFallback then None else Some woMacros
                |> List.foldBack (fun (p, o) fb -> Some (Macro(p, o |> Option.map ParameterObject.OfObj, fb))) nr.Macros
                |> Option.get

        let compiledStaticMember (address: Address) (nr : NotResolvedMethod) =
            match nr.Kind with
            | N.Static -> Static address
            | N.Constructor -> Constructor address
            | _ -> failwith "Invalid static member kind"
            |> withMacros nr        

        let compiledNoAddressMember (nr : NotResolvedMethod) =
            match nr.Kind with
            | N.Inline -> Inline
            | N.Remote (k, h, r) -> Remote (k, h, r)
            | N.NoFallback -> Inline // will be erased
            | _ -> failwith "Invalid not compiled member kind"
            |> withMacros nr

        let compiledInstanceMember (name: string) (nr: NotResolvedMethod) =
            match nr.Kind with
            | N.Instance  
            | N.Override _  
            | N.Implementation _ -> Instance name
            | _ -> failwith "Invalid instance member kind"
            |> withMacros nr

        let toCompilingMember (nr : NotResolvedMethod) (comp: CompiledMember) =
            match nr.Generator with
            | Some (g, p) -> NotGenerated(g, p, comp)
            | _ -> NotCompiled comp
            
        let setClassAddress typ clAddr =
            let res = classes.[typ]
            classes.[typ] <- { res with Address = Some clAddr }

        // split to resolve steps
        let stronglyNamedClasses = ResizeArray()
        let fullyNamedStaticMembers = ResizeArray()
        let remainingClasses = ResizeArray()
        let remainingNamedStaticMembers = Dictionary()
        let namedInstanceMembers = Dictionary() // includes implementations
//        let abstractInstanceMembers = Dictionary()
        let remainingStaticMembers = Dictionary()
        let remainingInstanceMembers = Dictionary() // includes overrides

        let addCctorCall typ (ci: ClassInfo) expr =
            if Option.isSome ci.StaticConstructor then
                match expr with
                | Function (args, body) ->
                    Function(args, CombineStatements [ ExprStatement (Cctor typ); body ])
                | _ -> failwithf "Member body must be a function: %+A" expr
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

//            let getStrongNameIsStatic m =
            
            let cc = classes.[typ]

            // TODO: fail on named overrides and implementations
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
                        | N.Instance -> sn, Some false, false
                        | N.Static
                        | N.Constructor -> sn, Some true, false
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
                        if nr.Compiled then
                            try
                                cc.Constructors.Add (cDef, (comp, nr.Pure, addCctorCall typ cc nr.Body))
                            with _ ->
                                printerrf "Duplicate definition for constructor of %s" typ.Value.FullName
                        else 
                            compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, addCctorCall typ cc nr.Body))      
                    | M.Method (mDef, nr) -> 
                        let comp = compiledNoAddressMember nr
                        if nr.Compiled then
                            try
                                cc.Methods.Add (mDef, (comp, nr.Pure, addCctorCall typ cc nr.Body))
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
            if not (r.ExactClassAddress(addr, not classes.[typ].IsStatic)) then
                this.AddError(None, NameConflict ("Class name conflict", sn))
            setClassAddress typ (Address addr)

        let nameStaticMember typ addr m = 
            let res = classes.[typ]
            match m with
            | M.Constructor (cDef, nr) ->
                let comp = compiledStaticMember addr nr
                if nr.Compiled then
                    res.Constructors.Add(cDef, (comp, nr.Pure, addCctorCall typ res nr.Body))
                else
                    compilingConstructors.Add((typ, cDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
            | M.Field (fName, _) ->
                res.Fields.Add(fName, StaticField addr)
            | M.Method (mDef, nr) ->
                let comp = compiledStaticMember addr nr
                if nr.Compiled then 
                    res.Methods.Add(mDef, (comp, nr.Pure, addCctorCall typ res nr.Body))
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
                res.Fields.Add(fName, if f.IsOptional then OptionalField name else InstanceField name)
            | M.Method (mDef, nr) ->
                let comp = compiledInstanceMember name nr
                match nr.Kind with
                | N.Implementation intf ->
                    if nr.Compiled then 
                        res.Implementations.Add((intf, mDef), (comp, addCctorCall typ res nr.Body))
                    else
                        compilingImplementations.Add((typ, intf, mDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
                | _ ->
                    if nr.Compiled then 
                        res.Methods.Add(mDef, (comp, nr.Pure, addCctorCall typ res nr.Body))
                    else
                        compilingMethods.Add((typ, mDef), (toCompilingMember nr comp, addCctorCall typ res nr.Body))
            | _ -> failwith "Invalid instance member kind"   

        for typ, m, sn in fullyNamedStaticMembers do
            let res = classes.[typ]
            let addr =
                match sn.Split('.') with
                | [||] ->
                    printerrf "Invalid Name attribute argument on type '%s'" typ.Value.FullName
                    ["$$ERROR$$"]
                | [| n |] -> 
                    n :: res.Address.Value.Value
                | a -> List.ofArray (Array.rev a)
            if not (r.ExactStaticAddress addr) then
                this.AddError(None, NameConflict ("Static member name conflict", sn)) 
            nameStaticMember typ (Address addr) m
              
        for typ in remainingClasses do
            let removeGen (n: string) =
                match n.LastIndexOf '`' with
                | -1 -> n
                | i -> n.[.. i - 1]
            let addr = typ.Value.FullName.Split('.', '+') |> List.ofArray |> List.map removeGen |> List.rev 
            r.ClassAddress(addr, not classes.[typ].IsStatic)
            |> setClassAddress typ
        
        let extraClassAddresses = Dictionary()

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
                // TODO: fail on clashes, not on Stubs?
                pr.Add n |> ignore
//                if not (pr.Add n) then
//                    failwith "instance member name collision"
                nameInstanceMember typ n m      

        for KeyValue(typ, ms) in remainingStaticMembers do
            let clAddr = getClassAddress typ
            for m in ms do
                let n = 
                    match m with
                    | M.Constructor _ -> "New"
                    | M.Field (fName, _) -> fName
                    | M.Method (meth, _) -> meth.Value.MethodName
                    | M.StaticConstructor _ -> "$cctor"
                let addr = r.StaticAddress (n :: clAddr)
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
                        | M.Field (fName, _) -> Resolve.getRenamed fName pr |> Some
                        | M.Method (mDef, { Kind = N.Instance }) -> 
                            Resolve.getRenamed mDef.Value.MethodName pr |> Some
                        | M.Method (mDef, { Kind = N.Override td }) ->
                            match classes.TryFind td with
                            | Some tCls -> 
                                let smi = 
                                    match tCls.Methods.TryFind mDef with
                                    | Some (smi, _, _) -> Some smi
                                    | _ ->
                                    match compilingMethods.TryFind (td, mDef) with
                                    | Some ((NotCompiled smi | NotGenerated (_, _, smi)), _) -> Some smi
                                    | None ->
                                        printerrf "Abstract method not found in compilation: %s in %s" (string mDef.Value) td.Value.FullName
                                        None
                                match smi with
                                | Some (Instance n) -> Some n
                                | None -> None
                                | _ -> 
                                    printerrf "Abstract methods must be Instance"
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
        if this.AssemblyName = "WebSharper.Main" then
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

            let boolTy =
                TypeDefinition {
                    FullName = "System.Boolean"
                    Assembly = "mscorlib"
                } |> NonGeneric |> ConcreteType

            let intTy =
                TypeDefinition {
                    FullName = "System.Int32"
                    Assembly = "mscorlib"
                } |> NonGeneric |> ConcreteType

            let equals =
                Method {
                    MethodName = "Equals"
                    Parameters = [ ConcreteType (NonGeneric Definitions.Obj) ]
                    ReturnType = boolTy
                    Generics = 0
                }

            let getHashCode =
                Method {
                    MethodName = "GetHashCode"
                    Parameters = []
                    ReturnType = intTy
                    Generics = 0
                } 

            let toString =
                Method {
                    MethodName = "ToString"
                    Parameters = []
                    ReturnType =
                        TypeDefinition {
                            FullName = "System.String"
                            Assembly = "mscorlib"
                        } |> NonGeneric |> ConcreteType
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
                    ReturnType = boolTy
                    Generics = 1
                }

            let uncheckedHash =
                Method { 
                    MethodName = "Hash"
                    Parameters = [ TypeParameter 0 ]
                    ReturnType = intTy
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

        // Add graph edge needed for Sitelets: Web.Controls will be looked up
        // and initialized on client-side by Activator.Activate
        if this.AssemblyName = "WebSharper.Web" then
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
            
            let activateIndex = graph.Lookup.[activate]
            let controlIndex = graph.Lookup.[control] 

            graph.AddEdge(controlIndex, activateIndex)

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
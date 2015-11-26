module WebSharper.Compiler.Reflector

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata

let getTypeDefinition (tR: Mono.Cecil.TypeReference) =
    let name = tR.FullName.Split('<').[0]
    Hashed {
        Assembly =
            match tR.Scope.MetadataScopeType with
            | Mono.Cecil.MetadataScopeType.AssemblyNameReference ->
                let anr = tR.Scope :?> Mono.Cecil.AssemblyNameReference
                anr.FullName.Split(',').[0]
            | _ -> tR.Module.Assembly.FullName.Split(',').[0]
        FullName = name
    }

let rec getType tgen (tR: Mono.Cecil.TypeReference) =
    if tR.IsArray then
        let tR = tR :?> Mono.Cecil.ArrayType
        ArrayType(getType tgen tR.ElementType, tR.Rank)
    elif tR.IsGenericParameter then
        let tR = tR :?> Mono.Cecil.GenericParameter
        if tR.Owner.GenericParameterType = Mono.Cecil.GenericParameterType.Method then
            GenericType (tgen + tR.Position)
        else GenericType tR.Position
//        tR.Owner.GenericParameterType = Mono.Cecil.GenericParameterType.M
//        match tR.Owner with
//        | :? Mono.Cecil.MethodReference as mR ->
//            let tg = 
//                let dt = mR.DeclaringType
//                if dt.HasGenericParameters then dt.GenericParameters.Count else 0
//            GenericType (tg + tR.Position)
//        | _ -> GenericType tR.Position

//    elif FST.IsTuple t then
//        TupleType (FST.GetTupleElements t |> Seq.map getType |> List.ofSeq)
//    elif FST.IsFunction t then
//        let a, r = FST.GetFunctionElements t
//        FSharpFuncType (getType a, getType r) 
    else
        let name = tR.FullName.Split('<').[0] 
        if name = "System.Void" || name = "Microsoft.FSharp.Core.Unit" then
            VoidType
        elif name = "Microsoft.FSharp.Core.FSharpFunc`2" then
            let tR = tR :?> Mono.Cecil.GenericInstanceType
            let [a; r] = tR.GenericArguments |> Seq.map (getType tgen) |> List.ofSeq
            FSharpFuncType(a, r)    
        elif name.StartsWith "System.Tuple" then
            let tR = tR :?> Mono.Cecil.GenericInstanceType
            tR.GenericArguments |> Seq.map (getType tgen) |> List.ofSeq |> TupleType
        else
            concreteType (
                Hashed {
                    Assembly =
                        match tR.Scope.MetadataScopeType with
                        | Mono.Cecil.MetadataScopeType.AssemblyNameReference ->
                            let anr = tR.Scope :?> Mono.Cecil.AssemblyNameReference
                            anr.FullName.Split(',').[0]
                        | _ -> tR.Module.Assembly.FullName.Split(',').[0]
                    FullName = name
                },
                if tR.IsGenericInstance then
                    let tR = tR :?> Mono.Cecil.GenericInstanceType
                    tR.GenericArguments |> Seq.map (getType tgen) |> List.ofSeq 
                else []
            )

let getRequires attrs =
    attrs
    |> Seq.filter (fun (a: Mono.Cecil.CustomAttribute) -> 
        let attrTy = a.AttributeType
//            attrTy.Module.Assembly.Name = "WebSharper.Core" &&
        attrTy.FullName = "WebSharper.Core.Attributes/RequireAttribute" 
    )
    |> Seq.map (fun a ->
        a.ConstructorArguments.[0].Value :?> Mono.Cecil.TypeReference |> getTypeDefinition
    )
    |> List.ofSeq

let transformAssembly (assembly : Mono.Cecil.AssemblyDefinition) =
    let allTypes = assembly.MainModule.Types
        
    let asmName = assembly.FullName.Split(',').[0]

//    let asmNode =
//        {
//            Id = getId()
//            AssemblyName = asmName
//        }

    let graph = Graph.Empty
    
    // TODO: add resources
//    let addResource (nodeId: int) resTy =
//        graph.AddEdge(nodeId, ResourceNode resTy)

    let asmNodeIndex = graph.AddOrLookupNode(AssemblyNode asmName)
    for req in getRequires assembly.CustomAttributes do
        graph.AddEdge(asmNodeIndex, ResourceNode req)

    let transformInterface (typ: Mono.Cecil.TypeDefinition) =
        if not typ.IsInterface then None else
        let def =
            getTypeDefinition typ 
//            TypeDefinition {
//                Assembly = asmName
//                FullName = typ.FullName
//            }

        let reqs = typ.CustomAttributes |> getRequires
        if not reqs.IsEmpty then
            let i = graph.AddOrLookupNode(TypeNode def)
            for r in reqs do
                graph.AddEdge(i, ResourceNode r) 

        Some (def,
            {
                Extends = typ.Interfaces |> Seq.map getTypeDefinition |> List.ofSeq
                Methods = 
                    dict [
                        for meth in typ.Methods do
                            let tgen = if typ.HasGenericParameters then typ.GenericParameters.Count else 0
                            let mdef =
                                Hashed {
                                    MethodName = meth.Name
                                    Parameters = meth.Parameters |> Seq.map (fun p -> getType tgen p.ParameterType) |> List.ofSeq
                                    ReturnType = getType tgen meth.ReturnType
                                    Generics   = meth.GenericParameters |> Seq.length
                                }
                            yield mdef, meth.Name // TODO: Name attribute
                        
                    ]
            }
        )    
    
    let transformClass asmName (typ: Mono.Cecil.TypeDefinition) =
        if not typ.IsClass then None else

        let def = getTypeDefinition typ
//            TypeDefinition {
//                Assembly = asmName
//                FullName = typ.FullName
//            }


        let clsNodeIndex = graph.AddOrLookupNode(TypeNode def)
        graph.AddEdge(clsNodeIndex, asmNodeIndex)
        for req in getRequires typ.CustomAttributes do
            graph.AddEdge(clsNodeIndex, TypeNode req)
//        for m in cls.Members do
//            match m with
//            | M.Constructor (ctor, _) -> graph.AddEdge(ConstructorNode(typ, ctor), clsNodeIndex)
//            | M.Method (meth, _) -> graph.AddEdge(MethodNode(typ, meth), clsNodeIndex)
//            | _ -> ()

        let reqs = typ.CustomAttributes |> getRequires
        if not reqs.IsEmpty then
            let i = graph.AddOrLookupNode(TypeNode def)
            for r in reqs do
                graph.AddEdge(i, ResourceNode r) 

        let baseDef =
            let b = typ.BaseType
            if b = null then None else
//                TypeDefinition {
//                    Assembly = b.Scope.Name
//                    FullName = b.FullName
//                } 
                getTypeDefinition b
                |> ignoreSystemObject

        match baseDef with 
        | Some b ->
            graph.AddEdge(graph.AddOrLookupNode (TypeNode def), TypeNode b)
        | _ -> ()

        let methods = Dictionary()
        let constructors = Dictionary()

        let mutable address = None

        for meth in typ.Methods do
            let inlAttr = 
                meth.CustomAttributes |> 
                Seq.tryFind (fun a -> 
                    let attrTy = a.AttributeType
    //                attrTy.Module.Assembly.Name = "WebSharper.Core" &&
                    attrTy.FullName = "WebSharper.Core.Attributes/InlineAttribute" 
                )
                |> Option.map (fun a ->
                    a.ConstructorArguments.[0].Value :?> string
                )    
                    //S .GetCustomAttributes(typeof<InlineAttribute>, false)
            match inlAttr with
            | Some inl ->
                let vars = meth.Parameters |> Seq.map (fun p -> Id.New p.Name) |> List.ofSeq
                let thisArg =
                    if not (meth.IsStatic || meth.IsConstructor) then // || meth.HasThis then
                        Some (Id.New "this")    
                    else 
                        None
                let parsed = WebSharper.Compiler.Recognize.createInline thisArg vars inl 
                let tgen = if typ.HasGenericParameters then typ.GenericParameters.Count else 0
                if meth.IsConstructor then 
                    let cdef =
                        Hashed {
                            CtorParameters = meth.Parameters |> Seq.map (fun p -> getType tgen p.ParameterType) |> List.ofSeq
                        }
                    let cNode = graph.AddOrLookupNode(ConstructorNode (def, cdef))
                    graph.AddEdge(cNode, clsNodeIndex)
                    for req in getRequires meth.CustomAttributes do
                        graph.AddEdge(cNode, ResourceNode req)
                    constructors.Add(cdef, (Inline, parsed))
                    
                    // TODO: better recognise inlines
                    if address = None then
                        match parsed with
                        | New (GlobalAccess a, []) -> address <- Some a
                        | Let (i1, _, New (GlobalAccess a, [Var v1])) when i1 = v1 -> address <- Some a
                        | _ -> ()
                else 
                    let mdef =
                        Hashed {
                            MethodName = meth.Name
                            Parameters = meth.Parameters |> Seq.map (fun p -> getType tgen p.ParameterType) |> List.ofSeq
                            ReturnType = getType tgen meth.ReturnType
                            Generics   = meth.GenericParameters |> Seq.length
                        }
                    let mNode = graph.AddOrLookupNode(MethodNode (def, mdef))
                    graph.AddEdge(mNode, clsNodeIndex)
                    for req in getRequires meth.CustomAttributes do
                        graph.AddEdge(mNode, ResourceNode req)
                    methods.Add(mdef, (Inline, parsed))
            | _ -> ()

        if methods.Count = 0 && constructors.Count = 0 then None else

        Some (def, 
            {
                Address = address
                BaseClass = baseDef
                Constructors = constructors
                Fields = Map.empty 
                StaticConstructor = None         
                Methods = methods 
                Implementations = Map.empty // TODO
                IsModule = true
                Macros = []
            }
        )

    {
        SiteletDefinition = None
        Dependencies = graph.GetData()
        Interfaces = allTypes |> Seq.choose (fun t -> transformInterface t) |> dict
        Classes = allTypes |> Seq.choose (fun t -> transformClass asmName t) |> dict
        EntryPoint = None
    }
//        Assemblies = dict [asmName, asmNode]
//        SiteletDefinition = None
//        Dependencies = dependencies
//        ExternalDependencies = dict []
//        Resources = resourceLookup.Values |> List.ofSeq
//        Interfaces = allTypes |> Seq.choose (fun t -> transformInterface t) |> dict
//        Classes = allTypes |> Seq.choose (fun t -> transformClass asmName t) |> dict
//        Proxies = dict []

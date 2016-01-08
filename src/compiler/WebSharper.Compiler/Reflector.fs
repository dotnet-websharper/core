module WebSharper.Compiler.Reflector

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata

let getTypeDefinition (tR: Mono.Cecil.TypeReference) =
    Hashed {
        Assembly =
            match tR.Scope.MetadataScopeType with
            | Mono.Cecil.MetadataScopeType.AssemblyNameReference ->
                let anr = tR.Scope :?> Mono.Cecil.AssemblyNameReference
                anr.FullName.Split(',').[0]
            | _ -> tR.Module.Assembly.FullName.Split(',').[0]
        FullName = tR.FullName.Split('<').[0].Replace('/', '+')
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
            let rec collect (ts: seq<Mono.Cecil.TypeReference>) =
                let ts = Array.ofSeq ts  
                if ts.Length = 8 then
                    let rest = ts.[7] :?> Mono.Cecil.GenericInstanceType
                    Array.append ts.[.. 6] (collect rest.GenericArguments) 
                else ts
            collect tR.GenericArguments |> Seq.map (getType tgen) |> List.ofSeq |> TupleType
        else
            concreteType (
                Hashed {
                    Assembly =
                        match tR.Scope.MetadataScopeType with
                        | Mono.Cecil.MetadataScopeType.AssemblyNameReference ->
                            let anr = tR.Scope :?> Mono.Cecil.AssemblyNameReference
                            anr.FullName.Split(',').[0]
                        | _ -> tR.Module.Assembly.FullName.Split(',').[0]
                    FullName = name.Replace('/', '+')
                },
                if tR.IsGenericInstance then
                    let tR = tR :?> Mono.Cecil.GenericInstanceType
                    tR.GenericArguments |> Seq.map (getType tgen) |> List.ofSeq 
                else []
            )

let getRequires attrs =
    attrs
    |> Seq.filter (fun (a: Mono.Cecil.CustomAttribute) -> 
        a.AttributeType.FullName = "WebSharper.Core.Attributes/RequireAttribute" 
    )
    |> Seq.map (fun a ->
        a.ConstructorArguments.[0].Value :?> Mono.Cecil.TypeReference |> getTypeDefinition
    )
    |> List.ofSeq

let isResourceType (e: Mono.Cecil.TypeDefinition) =
    e.Interfaces |> Seq.exists (fun i ->
        i.FullName = "WebSharper.Core.Resources+IResource"
    )

let transformAssembly (assembly : Mono.Cecil.AssemblyDefinition) =
    let rec withNested (tD: Mono.Cecil.TypeDefinition) =
        if tD.HasNestedTypes then
            Seq.append (Seq.singleton tD) (Seq.collect withNested tD.NestedTypes)
        else Seq.singleton tD

    let allTypes = assembly.MainModule.Types |> Seq.collect withNested |> Array.ofSeq
        
    let asmName = assembly.FullName.Split(',').[0]

    let graph = Graph.Empty
    
    let asmNodeIndex = graph.AddOrLookupNode(AssemblyNode (asmName, false))
    for req in getRequires assembly.CustomAttributes do
        graph.AddEdge(asmNodeIndex, ResourceNode req)

    let transformInterface (typ: Mono.Cecil.TypeDefinition) =
        if not typ.IsInterface then None else
        let def = getTypeDefinition typ 

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
    
    let transformClass (typ: Mono.Cecil.TypeDefinition) =
        if not typ.IsClass then None else

        let def = getTypeDefinition typ

        if isResourceType typ then
            let thisRes = graph.AddOrLookupNode(ResourceNode def)
            graph.AddEdge(thisRes, asmNodeIndex)
            for req in getRequires typ.CustomAttributes do
                graph.AddEdge(thisRes, ResourceNode req)
            None
        else

        let clsNodeIndex = graph.AddOrLookupNode(TypeNode def)
        graph.AddEdge(clsNodeIndex, asmNodeIndex)
        for req in getRequires typ.CustomAttributes do
            graph.AddEdge(clsNodeIndex, ResourceNode req)

        let baseDef =
            let b = typ.BaseType
            if b = null then None else
                getTypeDefinition b
                |> ignoreSystemObject

        match baseDef with 
        | Some b ->
            graph.AddEdge(TypeNode def, TypeNode b)
        | _ -> ()

        let methods = Dictionary()
        let constructors = Dictionary()

        let mutable address = None
         
        for meth in typ.Methods do
            let macros =
                meth.CustomAttributes |> 
                Seq.filter (fun a -> 
                    a.AttributeType.FullName = "WebSharper.Core.Attributes/MacroAttribute" 
                )
                |> Seq.map (fun a ->
                    let ar = a.ConstructorArguments
                    getTypeDefinition (ar.[0].Value :?> Mono.Cecil.TypeDefinition)
                    ,
                    if ar.Count > 1 then
                        // Todo : System.Type parameter objects
                        Some (ParameterObject.OfObj ar.[1].Value)
                    else None 
                ) |> List.ofSeq
            let inlAttr = 
                meth.CustomAttributes |> 
                Seq.tryFind (fun a -> 
                    a.AttributeType.FullName = "WebSharper.Core.Attributes/InlineAttribute" 
                )
                |> Option.map (fun a ->
                    a.ConstructorArguments.[0].Value :?> string
                )
                                    
            if Option.isSome inlAttr || not (List.isEmpty macros) then
                let vars = meth.Parameters |> Seq.map (fun p -> Id.New p.Name) |> List.ofSeq
                let thisArg =
                    if not (meth.IsStatic || meth.IsConstructor) then // || meth.HasThis then
                        Some (Id.New "this")    
                    else 
                        None
                let parsed = inlAttr |> Option.map (WebSharper.Compiler.Recognize.createInline thisArg vars) 

                let kind =
                    if List.isEmpty macros then Inline else
                        if Option.isSome inlAttr then Some Inline else None 
                        |> List.foldBack (fun (m, p) x -> Some (Macro(m, p, x))) macros 
                        |> Option.get

                let body = match parsed with | Some b -> b | _ -> Undefined

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
                    
                    try 
                        constructors.Add(cdef, (kind, body))
                    with _ ->
                        failwithf "Duplicate definition for constructor of %s, arguments: %s" def.Value.FullName (Reflection.printCtorParams cdef.Value)
                    
                    // TODO: better recognise inlines
                    if address = None then
                        match body with
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
                    
                    try methods.Add(mdef, (kind, body))
                    with _ ->
                        failwithf "Duplicate definition for method of %s: %s" def.Value.FullName (Reflection.printMethod mdef.Value)

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

    let interfaces = allTypes |> Seq.choose (fun t -> transformInterface t) |> dict 
    let classes = allTypes |> Seq.choose (fun t -> transformClass t) |> dict

    {
        SiteletDefinition = None
        Dependencies = graph.GetData()
        Interfaces = interfaces
        Classes = classes
        EntryPoint = None
    }

let transformWSAssembly (assembly : WebSharper.Compiler.Assembly) =
    transformAssembly assembly.Raw     
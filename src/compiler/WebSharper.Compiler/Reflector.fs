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

// Uses reflection to get inline and macro information.
// Used by WebSharper Interface Generator and WebSharper.TypeScript
module WebSharper.Compiler.Reflector

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata
open WebSharper.Core.DependencyGraph

let getTypeDefinition (tR: Mono.Cecil.TypeReference) =
    let fullName = tR.FullName.Split('<').[0].Replace('/', '+')
    Hashed {
        Assembly =
            if AssemblyConventions.IsNetStandardType fullName then
                "netstandard"
            else
                match tR.Scope.MetadataScopeType with
                | Mono.Cecil.MetadataScopeType.AssemblyNameReference ->
                    let anr = tR.Scope :?> Mono.Cecil.AssemblyNameReference
                    anr.FullName.Split(',').[0]
                | _ -> tR.Module.Assembly.FullName.Split(',').[0]
        FullName = fullName
    }

let rec getType tgen (tR: Mono.Cecil.TypeReference) =
    if tR.IsArray then
        let tR = tR :?> Mono.Cecil.ArrayType
        ArrayType(getType tgen tR.ElementType, tR.Rank)
    elif tR.IsGenericParameter then
        let tR = tR :?> Mono.Cecil.GenericParameter
        if tR.Owner.GenericParameterType = Mono.Cecil.GenericParameterType.Method then
            TypeParameter (tgen + tR.Position)
        else TypeParameter tR.Position
    else
        let name = tR.FullName.Split('<').[0] 
        let getTupleType isStruct =
            let tR = tR :?> Mono.Cecil.GenericInstanceType
            let rec collect (ts: seq<Mono.Cecil.TypeReference>) =
                let ts = Array.ofSeq ts  
                if ts.Length = 8 then
                    let rest = ts.[7] :?> Mono.Cecil.GenericInstanceType
                    Array.append ts.[.. 6] (collect rest.GenericArguments) 
                else ts
            let tts = collect tR.GenericArguments |> Seq.map (getType tgen) |> List.ofSeq
            TupleType (tts, isStruct)
        if name = "System.Void" || name = "Microsoft.FSharp.Core.Unit" then
            VoidType
        elif name = "Microsoft.FSharp.Core.FSharpFunc`2" then
            let tR = tR :?> Mono.Cecil.GenericInstanceType
            match tR.GenericArguments |> Seq.map (getType tgen) |> List.ofSeq with
            | [a; r] -> FSharpFuncType(a, r)    
            | _ -> failwith "FSharpFunc type must have two type arguments"
        elif name.StartsWith "System.Tuple" then
            getTupleType false
        elif name.StartsWith "System.ValueTuple" then
            getTupleType true
        else
            GenericType
                (getTypeDefinition tR) 
                (
                    if tR.IsGenericInstance then
                        let tR = tR :?> Mono.Cecil.GenericInstanceType
                        tR.GenericArguments |> Seq.map (getType tgen) |> List.ofSeq 
                    else []
                )

let getName attrs =
    attrs
    |> Seq.tryPick (fun (a: Mono.Cecil.CustomAttribute) -> 
        if a.AttributeType.FullName = "WebSharper.NameAttribute" then
            Some (a.ConstructorArguments.[0].Value :?> string)
        else None
    )

let getRequires attrs =
    attrs
    |> Seq.filter (fun (a: Mono.Cecil.CustomAttribute) -> 
        a.AttributeType.FullName = "WebSharper.RequireAttribute" 
    )
    |> Seq.map (fun a ->
        let cargs = a.ConstructorArguments 
        cargs.[0].Value :?> Mono.Cecil.TypeReference |> getTypeDefinition
        ,
        if cargs.Count = 1 then None else (cargs.[1].Value |> ParameterObject.OfObj |> Some)
    )
    |> List.ofSeq

let getMacros attrs =
    attrs
    |> Seq.filter (fun (a: Mono.Cecil.CustomAttribute) -> 
        a.AttributeType.FullName = "WebSharper.MacroAttribute" 
    )
    |> Seq.map (fun a ->
        let ar = a.ConstructorArguments
        getTypeDefinition (ar.[0].Value :?> _)
        ,
        if ar.Count > 1 then
            // Todo : System.Type parameter objects
            Some (ParameterObject.OfObj ar.[1].Value)
        else None 
    ) |> List.ofSeq

let getInline attrs =
    attrs
    |> Seq.tryFind (fun (a: Mono.Cecil.CustomAttribute) -> 
        a.AttributeType.FullName = "WebSharper.InlineAttribute" 
    )
    |> Option.map (fun a ->
        a.ConstructorArguments.[0].Value :?> string
    )

let isResourceType (e: Mono.Cecil.TypeDefinition) =
    let b = e.BaseType
    not (isNull b) && b.FullName = "WebSharper.Core.Resources/BaseResource"

let trAsm (prototypes: IDictionary<string, string>) (assembly : Mono.Cecil.AssemblyDefinition) isTSasm =
    let rec withNested (tD: Mono.Cecil.TypeDefinition) =
        if tD.HasNestedTypes then
            Seq.append (Seq.singleton tD) (Seq.collect withNested tD.NestedTypes)
        else Seq.singleton tD

    let emptyMutableExternals = HashSet()
    
    let allTypes = assembly.MainModule.Types |> Seq.collect withNested |> Array.ofSeq
        
    let asmName = assembly.FullName.Split(',').[0]

    let graph = Graph.Empty
    
    let asmNodeIndex = graph.AddOrLookupNode(AssemblyNode (asmName, false))
    for req in getRequires assembly.CustomAttributes do
        graph.AddEdge(asmNodeIndex, ResourceNode req)

    let interfaces = Dictionary()
    let classes = Dictionary()

    let transformClass intfAsClass (typ: Mono.Cecil.TypeDefinition) =
        if not (intfAsClass || typ.IsClass) then () else

        let def = getTypeDefinition typ

        if isResourceType typ then
            let thisRes = graph.AddOrLookupNode(ResourceNode (def, None))
            for req in getRequires typ.CustomAttributes do
                graph.AddEdge(thisRes, ResourceNode req)
            ()
        else

        let clsNodeIndex = graph.AddOrLookupNode(TypeNode def)
        graph.AddEdge(clsNodeIndex, asmNodeIndex)
        for req in getRequires typ.CustomAttributes do
            graph.AddEdge(clsNodeIndex, ResourceNode req)

        let baseDef =
            let b = typ.BaseType
            if isNull b then None else
                getTypeDefinition b
                |> ignoreSystemObject

        match baseDef with 
        | Some b ->
            graph.AddEdge(TypeNode def, TypeNode b)
        | _ -> ()

        let methods = Dictionary()
        let constructors = Dictionary()
        let abstractAndVirtualMethods = ResizeArray()
        let mutable hasInstanceMethod = false
         
        for meth in typ.Methods do
            let macros = getMacros meth.CustomAttributes
            let inlAttr = getInline meth.CustomAttributes 
            let name = getName meth.CustomAttributes 

            let opts =
                {
                    IsPure =
                        meth.CustomAttributes
                        |>  Seq.exists (fun a -> 
                            a.AttributeType.FullName = "WebSharper.PureAttribute" 
                        )
                    FuncArgs = None
                    Warn = 
                        meth.CustomAttributes
                        |> Seq.tryPick (fun a -> 
                            if a.AttributeType.FullName = "WebSharper.WarnAttribute" then
                                Some (unbox (a.ConstructorArguments.[0].Value))
                            else None
                        )
                }
                                    
            if inlAttr.IsSome || name.IsSome || not (List.isEmpty macros) then
                let vars = meth.Parameters |> Seq.map (fun p -> Id.New p.Name) |> List.ofSeq
                let thisArg =
                    if not (meth.IsStatic || meth.IsConstructor) then // || meth.HasThis then
                        Some (Id.New "this")    
                    else 
                        None
                let parsed = inlAttr |> Option.map (WebSharper.Compiler.Recognize.createInline emptyMutableExternals thisArg vars opts.IsPure) 

                let kindWithoutMacros =
                    if inlAttr.IsSome then Some Inline else 
                        match name with
                        | Some n -> Some (Instance n) // named instance members only for mixin interfaces
                        | _ -> None
                let kind =
                    if List.isEmpty macros then kindWithoutMacros else
                        kindWithoutMacros
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
                        constructors.Add(cdef, (kind, opts, body))
                    with _ ->
                        failwithf "Duplicate definition for constructor of %s, arguments: %s" def.Value.FullName (string cdef.Value)
                    
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
                    
                    if not meth.IsStatic then hasInstanceMethod <- true

                    if meth.IsAbstract then
                        // Abstract method
                        let aNode = graph.AddOrLookupNode(AbstractMethodNode(def, mdef))
                        graph.AddEdge(mNode, aNode)
                        abstractAndVirtualMethods.Add(aNode) |> ignore
                    elif meth.IsVirtual then
                        let bdef =
                            if meth.HasOverrides then
                                // Override
                                getTypeDefinition meth.Overrides.[0].DeclaringType // why are there several Overrides?
                            else
                                // Virtual method
                                def
                        let aNode = graph.AddOrLookupNode(AbstractMethodNode(def, mdef))
                        graph.AddEdge(mNode, aNode)
                        graph.AddOverride(def, bdef, mdef)
                        abstractAndVirtualMethods.Add(aNode) |> ignore

                    try methods.Add(mdef, (kind, opts, body))
                    with _ ->
                        failwithf "Duplicate definition for method of %s: %s" def.Value.FullName (string mdef.Value)

        // Reflector is used for WIG, where abstract/virtual methods are generally meant to be called externally,
        // so any override of them should never be DCE'd. This enforces it.
        for aNode in abstractAndVirtualMethods do
            graph.AddEdge(clsNodeIndex, aNode)

        classes.Add(def, 
            {
                Address = prototypes.TryFind(def.Value.FullName) |> Option.map (fun s -> s.Split('.') |> List.ofArray |> List.rev |> Address)
                BaseClass = baseDef
                Constructors = constructors
                Fields = Map.empty 
                StaticConstructor = None         
                Methods = methods 
                Implementations = Map.empty // TODO
                QuotedArgMethods = Map.empty // TODO
                HasWSPrototype = false // do not overwrite external prototype
                Macros = []
            }
        )

    let transformInterface (typ: Mono.Cecil.TypeDefinition) =
        if not typ.IsInterface then () else
        let intfAsClass = 
            isTSasm ||
            typ.Methods 
            |> Seq.exists (fun meth -> 
                Option.isSome (getInline meth.CustomAttributes)
                || not (List.isEmpty (getMacros meth.CustomAttributes))
            )
        if intfAsClass then transformClass true typ else   
        let def = getTypeDefinition typ 

        let reqs = typ.CustomAttributes |> getRequires
        if not reqs.IsEmpty then
            let i = graph.AddOrLookupNode(TypeNode def)
            for r in reqs do
                graph.AddEdge(i, ResourceNode r) 

        interfaces.Add(def,
            {
                Extends = typ.Interfaces |> Seq.map (fun ii -> getTypeDefinition ii.InterfaceType) |> List.ofSeq
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
                            let name =
                                match getName meth.CustomAttributes with
                                | Some n -> n
                                | _ -> meth.Name
                            yield mdef, name
                    ]
            }
        )    
    
    allTypes |> Seq.iter transformInterface
    allTypes |> Seq.iter (transformClass false)

    {
        SiteletDefinition = None
        Dependencies = graph.GetData()
        Interfaces = interfaces
        Classes = classes
        CustomTypes = Map.empty
        MacroEntries = Map.empty
        Quotations = Map.empty
        ResourceHashes = Dictionary()
        ExtraBundles = Set.empty
    }

let TransformAssembly prototypes assembly =
    trAsm prototypes assembly false    

let TransformWSAssembly prototypes (assembly : WebSharper.Compiler.Assembly) =
    trAsm prototypes assembly.Raw true    

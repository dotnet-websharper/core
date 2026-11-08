// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
// Used by WebSharper Interface Generator
module WebSharper.Compiler.Reflector

open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata
open WebSharper.Core.DependencyGraph

type private PE = WebSharper.Compiler.PEMetadata.PEAssembly
type private CustomAttr = WebSharper.Compiler.PEMetadata.CustomAttr
module PM = WebSharper.Compiler.PEMetadata

/// A type in a signature: assembly (simple name, the current one for definitions and
/// generic parameters) and full name with `+` for nested types.
type private Sig =
    | SNamed of asm: string * fullName: string * args: Sig list
    | SArray of Sig * rank: int * sz: bool
    | SGenParam of isMethod: bool * index: int * name: string
    | SByRef of Sig
    | SPointer of Sig

type private SigContext =
    {
        TypeParameters : string[]
        MethodParameters : string[]
    }

type private SigProvider(md: MetadataReader, asmName: string) =
    member this.Decode (ctx: SigContext) (h: EntityHandle) =
        let p = this :> ISignatureTypeProvider<Sig, SigContext>
        match h.Kind with
        | HandleKind.TypeDefinition -> p.GetTypeFromDefinition(md, TypeDefinitionHandle.op_Explicit h, 0uy)
        | HandleKind.TypeReference -> p.GetTypeFromReference(md, TypeReferenceHandle.op_Explicit h, 0uy)
        | HandleKind.TypeSpecification -> p.GetTypeFromSpecification(md, ctx, TypeSpecificationHandle.op_Explicit h, 0uy)
        | k -> failwithf "Unexpected type handle kind: %O" k

    interface ISignatureTypeProvider<Sig, SigContext> with
        member _.GetArrayType(e, shape) = SArray(e, shape.Rank, false)
        member _.GetSZArrayType(e) = SArray(e, 1, true)
        member _.GetByReferenceType(e) = SByRef e
        member _.GetPointerType(e) = SPointer e
        member _.GetPinnedType(e) = e
        member _.GetModifiedType(_, unmodified, _) = unmodified
        member _.GetFunctionPointerType(_) = SNamed("netstandard", "System.IntPtr", [])
        member _.GetGenericInstantiation(g, args) =
            match g with
            | SNamed (a, n, _) -> SNamed (a, n, List.ofSeq args)
            | _ -> g
        member _.GetGenericMethodParameter(ctx, i) =
            SGenParam(true, i, (if i < ctx.MethodParameters.Length then ctx.MethodParameters.[i] else ""))
        member _.GetGenericTypeParameter(ctx, i) =
            SGenParam(false, i, (if i < ctx.TypeParameters.Length then ctx.TypeParameters.[i] else ""))
        member _.GetPrimitiveType(code) = SNamed("netstandard", "System." + string code, [])
        member _.GetTypeFromDefinition(_, h, _) = SNamed(asmName, PM.TypeDefinitionName md h, [])
        member _.GetTypeFromReference(_, h, _) =
            let a, n = PM.TypeReferenceName md h
            SNamed(defaultArg a asmName, n, [])
        member this.GetTypeFromSpecification(_, ctx, h, _) =
            md.GetTypeSpecification(h).DecodeSignature(this, ctx)

/// The full name Mono.Cecil gives a signature type (with `+` for nested types).
let rec private cecilName s =
    match s with
    | SNamed (_, n, []) -> n
    | SNamed (_, n, args) -> n + "<" + (args |> Seq.map cecilName |> String.concat ",") + ">"
    | SArray (e, _, true) -> cecilName e + "[]"
    | SArray (e, r, false) -> cecilName e + "[" + System.String(',', r - 1) + "]"
    | SGenParam (_, _, n) -> n
    | SByRef e -> cecilName e + "&"
    | SPointer e -> cecilName e + "*"

let rec private scopeOf current s =
    match s with
    | SNamed (a, _, _) -> a
    | SArray (e, _, _) | SByRef e | SPointer e -> scopeOf current e
    | SGenParam _ -> current

let private typeDefinition (asm: string) (fullName: string) =
    if fullName = "" then
        failwithf "Unexpected: empty class name"
    Hashed {
        Assembly =
            if AssemblyConventions.IsNetStandardType fullName then
                "netstandard"
            else asm
        FullName = fullName
    }

let private getTypeDefinition current (s: Sig) =
    match s with
    | SNamed (a, n, _) -> typeDefinition a n
    | _ -> typeDefinition (scopeOf current s) ((cecilName s).Split('<').[0])

let rec private getType current tgen (s: Sig) =
    match s with
    | SArray (e, rank, _) -> ArrayType(getType current tgen e, rank)
    | SGenParam (true, i, _) -> TypeParameter (tgen + i)
    | SGenParam (false, i, _) -> TypeParameter i
    | SNamed (_, name, args) ->
        let getTupleType isStruct =
            let rec collect (ts: Sig list) =
                if ts.Length = 8 then
                    match ts.[7] with
                    | SNamed (_, _, rest) -> List.append ts.[.. 6] (collect rest)
                    | _ -> ts
                else ts
            TupleType (collect args |> List.map (getType current tgen), isStruct)
        if name = "System.Void" || name = "Microsoft.FSharp.Core.Unit" then
            VoidType
        elif name = "Microsoft.FSharp.Core.FSharpFunc`2" then
            match args |> List.map (getType current tgen) with
            | [a; r] -> FSharpFuncType(a, r)
            | _ -> failwith "FSharpFunc type must have two type arguments"
        elif name.StartsWith "System.Tuple" then
            getTupleType false
        elif name.StartsWith "System.ValueTuple" then
            getTupleType true
        else
            GenericType (getTypeDefinition current s) (args |> List.map (getType current tgen))
    | SByRef _ | SPointer _ ->
        GenericType (getTypeDefinition current s) []

/// The type definition named by a System.Type argument of a custom attribute.
let private attributeTypeDefinition (md: MetadataReader) current (t: PM.AttributeType) =
    match t with
    | PM.Named (asm, fullName) ->
        let asm =
            match asm with
            | Some a -> a
            | None ->
                // as Mono.Cecil: the current module if it defines the type, the core library otherwise
                let defined =
                    md.TypeDefinitions |> Seq.exists (fun h -> PM.TypeDefinitionName md h = fullName)
                if defined then current else
                md.AssemblyReferences
                |> Seq.map (fun h -> md.GetString (md.GetAssemblyReference h).Name)
                |> Seq.tryFind (fun n -> n = "netstandard" || n = "mscorlib" || n = "System.Runtime" || n = "System.Private.CoreLib")
                |> Option.defaultValue current
        typeDefinition asm fullName
    | _ -> failwithf "Expected a type argument in custom attribute, found %A" t

let rec private parameterObject md current (v: obj) =
    match v with
    | :? PM.AttributeType as t -> ParameterObject.Type (NonGenericType (attributeTypeDefinition md current t))
    | :? ImmutableArray<CustomAttributeTypedArgument<PM.AttributeType>> as a ->
        a |> Seq.map (fun x -> parameterObject md current x.Value) |> Array.ofSeq |> ParameterObject.Array
    | _ -> ParameterObject.OfObj v

let private getName (attrs: seq<CustomAttr>) =
    attrs
    |> Seq.tryPick (fun a ->
        if a.TypeName = "WebSharper.NameAttribute" then
            Some (a.Arguments.[0] :?> string)
        else None
    )

let private getTSType (attrs: seq<CustomAttr>) =
    attrs
    |> Seq.tryPick (fun a ->
        if a.TypeName = "WebSharper.TypeAttribute" then
            Some (TSType.Parse (a.Arguments.[0] :?> string))
        else None
    )

let private getRequires md current (attrs: seq<CustomAttr>) =
    attrs
    |> Seq.filter (fun a ->
        a.TypeName = "WebSharper.RequireAttribute"
    )
    |> Seq.map (fun a ->
        let cargs = a.Arguments
        cargs.[0] :?> PM.AttributeType |> attributeTypeDefinition md current
        ,
        if cargs.Length = 1 then None else (cargs.[1] |> parameterObject md current |> Some)
    )
    |> List.ofSeq

let private getMacros md current (attrs: seq<CustomAttr>) =
    attrs
    |> Seq.filter (fun a ->
        a.TypeName = "WebSharper.MacroAttribute"
    )
    |> Seq.map (fun a ->
        let ar = a.Arguments
        attributeTypeDefinition md current (ar.[0] :?> _)
        ,
        if ar.Length > 1 then
            Some (parameterObject md current ar.[1])
        else None
    ) |> List.ofSeq

let private getInline (attrs: seq<CustomAttr>) =
    attrs
    |> Seq.tryFind (fun a ->
        a.TypeName = "WebSharper.InlineAttribute"
    )
    |> Option.map (fun a ->
        a.Arguments.[0] :?> string
    )

let private getImport (attrs: seq<CustomAttr>) asmName =
    attrs
    |> Seq.tryFind (fun a ->
        a.TypeName = "WebSharper.ImportAttribute"
    )
    |> Option.map (fun a ->
        match a.Arguments with
        | [| e; f |] -> Address.Import asmName (Some (e :?> string), (f :?> string))
        | [| f |] -> Address.Import asmName (None, (f :?> string))
        | _ -> failwith "invalid constructor arguments for ImportAttribute"
    )

let private trAsm (prototypes: IDictionary<string, string>) (assembly : PE) =
    let md = assembly.Reader
    let asmName = assembly.Name
    let sigs = SigProvider(md, asmName)
    let attrsOf (h: CustomAttributeHandleCollection) = PM.Attributes md h
    let getRequires = getRequires md asmName
    let getMacros = getMacros md asmName
    let typeDef (h: TypeDefinitionHandle) =
        typeDefinition asmName (PM.TypeDefinitionName md h)

    let isResourceType (td: System.Reflection.Metadata.TypeDefinition) =
        let b = td.BaseType
        not b.IsNil && PM.TypeName md b = Some "WebSharper.Core.Resources+BaseResource"

    let genericNames (ps: GenericParameterHandleCollection) =
        ps |> Seq.map (fun p -> md.GetString (md.GetGenericParameter p).Name) |> Array.ofSeq

    let getConstraints (ctx: SigContext) (genParams: GenericParameterHandleCollection) tgen =
        genParams |> Seq.map (fun p ->
            {
                Type = None
                Constraints =
                    (md.GetGenericParameter p).GetConstraints() |> Seq.map (fun c ->
                        (md.GetGenericParameterConstraint c).Type |> sigs.Decode ctx |> getType asmName tgen
                    ) |> List.ofSeq
            }
        ) |> List.ofSeq

    let rec withNested (h: TypeDefinitionHandle) =
        let nested = (md.GetTypeDefinition h).GetNestedTypes()
        if nested.Length > 0 then
            Seq.append (Seq.singleton h) (Seq.collect withNested nested)
        else Seq.singleton h

    // Only those types should be discovered recursively, which can be found in the prototypes dictionary above
    // As those are the types that are generated from the WIG definitions
    // We don't want to process the types coming from the Generator/Definition part
    let withNestedInit (h: TypeDefinitionHandle) =
        let td = md.GetTypeDefinition h
        if prototypes.ContainsKey (PM.TypeDefinitionName md h) || isResourceType td || td.Attributes.HasFlag TypeAttributes.Interface then
            withNested h
        else
            Seq.empty

    let emptyMutableExternals = HashSet()

    let allTypes =
        md.TypeDefinitions
        |> Seq.filter (fun h -> (md.GetTypeDefinition h).GetDeclaringType().IsNil)
        |> Seq.collect withNestedInit
        |> Array.ofSeq

    let graph = Graph.Empty

    let asmNodeIndex = graph.AddOrLookupNode(AssemblyNode asmName)
    for req in getRequires assembly.Attributes do
        graph.AddEdge(asmNodeIndex, ResourceNode req)

    let interfaces = Dictionary()
    let classes = Dictionary()

    /// Methods of a type with their custom attributes (property attributes first for accessors),
    /// and if they are property getters or setters.
    let methodsOf (td: System.Reflection.Metadata.TypeDefinition) =
        let accessors = Dictionary<MethodDefinitionHandle, PropertyDefinition * bool>()
        for p in td.GetProperties() do
            let pd = md.GetPropertyDefinition p
            let a = pd.GetAccessors()
            if not a.Getter.IsNil && not (accessors.ContainsKey a.Getter) then accessors.Add(a.Getter, (pd, true))
            if not a.Setter.IsNil && not (accessors.ContainsKey a.Setter) then accessors.Add(a.Setter, (pd, false))
        td.GetMethods() |> Seq.map (fun h ->
            let m = md.GetMethodDefinition h
            match accessors.TryGetValue h with
            | true, (pd, isGetter) ->
                h, m, Array.append (attrsOf (pd.GetCustomAttributes())) (attrsOf (m.GetCustomAttributes())), isGetter, not isGetter
            | _ ->
                h, m, attrsOf (m.GetCustomAttributes()), false, false
        )
        |> Array.ofSeq

    let methodContext typeCtx (m: MethodDefinition) =
        { typeCtx with MethodParameters = genericNames (m.GetGenericParameters()) }

    let signatureOf typeCtx (m: MethodDefinition) =
        m.DecodeSignature(sigs, methodContext typeCtx m)

    let parameterNames (m: MethodDefinition) (count: int) =
        let names = Array.create count ""
        for p in m.GetParameters() do
            let pd = md.GetParameter p
            let i = pd.SequenceNumber - 1
            if i >= 0 && i < count then names.[i] <- md.GetString pd.Name
        names

    let transformClass intfAsClass (h: TypeDefinitionHandle) =
        let typ = md.GetTypeDefinition h
        let fullName = PM.TypeDefinitionName md h
        let isClass = not (typ.Attributes.HasFlag TypeAttributes.Interface)
        // As a safety net, we should also throw away anything coming from the original dll's StartUp logic
        if not (intfAsClass || isClass) || fullName = "<Module>" || fullName.StartsWith "<StartupCode" then () else

        let def = typeDef h
        let typeAttrs = attrsOf (typ.GetCustomAttributes())
        let typeParams = typ.GetGenericParameters()
        let typeCtx = { TypeParameters = genericNames typeParams; MethodParameters = [||] }

        if isResourceType typ then
            let thisRes = graph.AddOrLookupNode(ResourceNode (def, None))
            for req in getRequires typeAttrs do
                graph.AddEdge(thisRes, ResourceNode req)
            ()
        else

        let clsNodeIndex = graph.AddOrLookupNode(TypeNode def)
        graph.AddEdge(clsNodeIndex, asmNodeIndex)
        let reqs = getRequires typeAttrs
        for req in reqs do
            graph.AddEdge(clsNodeIndex, ResourceNode req)

        let baseDef =
            let b = typ.BaseType
            if b.IsNil then None else
                sigs.Decode typeCtx b
                |> getType asmName 0
                |> getConcreteType
                |> ignoreSystemObject

        let implements =
            typ.GetInterfaceImplementations()
            |> Seq.map (fun i -> (md.GetInterfaceImplementation i).Interface |> sigs.Decode typeCtx |> getType asmName 0 |> getConcreteType)
            |> List.ofSeq

        match baseDef with
        | Some b ->
            graph.AddEdge(TypeNode def, TypeNode b.Entity)
        | _ -> ()

        let overrides = Dictionary<MethodDefinitionHandle, EntityHandle>()
        for i in typ.GetMethodImplementations() do
            let mi = md.GetMethodImplementation i
            if mi.MethodBody.Kind = HandleKind.MethodDefinition then
                let body = MethodDefinitionHandle.op_Explicit mi.MethodBody
                if not (overrides.ContainsKey body) then
                    overrides.Add(body, mi.MethodDeclaration)

        let methods = Dictionary<Method, CompiledMethodInfo>()
        let constructors = Dictionary<Constructor, CompiledConstructorInfo>()
        let abstractAndVirtualMethods = ResizeArray()
        let mutable hasInstanceMethod = false

        let tgen = typeParams.Count

        for (mh, meth, customAttributes, isGetter, isSetter) in methodsOf typ do
            let methName = md.GetString meth.Name
            let isStatic = meth.Attributes.HasFlag MethodAttributes.Static
            let isConstructor =
                meth.Attributes.HasFlag MethodAttributes.RTSpecialName && meth.Attributes.HasFlag MethodAttributes.SpecialName
                && (methName = ".ctor" || methName = ".cctor")
            let macros = getMacros customAttributes
            let inlAttr = getInline customAttributes
            let import = getImport (Seq.append customAttributes typeAttrs) asmName
            let name = getName customAttributes

            let opts =
                {
                    IsPure =
                        customAttributes
                        |>  Seq.exists (fun a ->
                            a.TypeName = "WebSharper.PureAttribute"
                        )
                    FuncArgs = None
                    Warn =
                        customAttributes
                        |> Seq.tryPick (fun a ->
                            if a.TypeName = "WebSharper.WarnAttribute" then
                                Some (unbox (a.Arguments.[0]))
                            else None
                        )
                }

            if inlAttr.IsSome || name.IsSome || not (List.isEmpty macros) then
                let signature = signatureOf typeCtx meth
                let parameters = signature.ParameterTypes |> Seq.map (getType asmName tgen) |> List.ofSeq
                let vars = parameterNames meth signature.ParameterTypes.Length |> Seq.map (fun p -> Id.New p) |> List.ofSeq
                let thisArg =
                    if not (isStatic || isConstructor) then
                        Some (Id.New "this")
                    else
                        None
                let parsed = inlAttr |> Option.map (WebSharper.Compiler.Recognize.createInline emptyMutableExternals thisArg vars opts.IsPure import [||] asmName)

                let kindWithoutMacros =
                    if inlAttr.IsSome then Some (Inline (true, false)) else
                        match name with
                        | Some n ->
                            let memberKind =
                                if isGetter then
                                    MemberKind.Getter
                                elif isSetter then
                                    MemberKind.Setter
                                else
                                    MemberKind.Simple
                            Some (Instance (n, memberKind, Modifier.None)) // named instance members only for mixin interfaces
                        | _ -> None
                let kind =
                    if List.isEmpty macros then kindWithoutMacros else
                        kindWithoutMacros
                        |> List.foldBack (fun (m, p) x -> Some (Macro(m, p, x))) macros
                    |> Option.get

                let body = match parsed with | Some b -> b.Expr | _ -> Undefined

                if isConstructor then
                    let cdef =
                        Hashed {
                            CtorParameters = parameters
                        }
                    let cNode = graph.AddOrLookupNode(ConstructorNode (def, cdef))
                    graph.AddEdge(cNode, clsNodeIndex)
                    for req in getRequires customAttributes do
                        graph.AddEdge(cNode, ResourceNode req)

                    try
                        constructors.Add(cdef, { CompiledForm = kind; Optimizations = opts; Expression = body })
                    with _ ->
                        failwithf "Duplicate definition for constructor of %s, arguments: %s" def.Value.FullName (string cdef.Value)

                else
                    let mdef =
                        Hashed {
                            MethodName = methName
                            Parameters = parameters
                            ReturnType = getType asmName tgen signature.ReturnType
                            Generics   = signature.GenericParameterCount
                        }
                    let mNode = graph.AddOrLookupNode(MethodNode (def, mdef))
                    graph.AddEdge(mNode, clsNodeIndex)
                    for req in getRequires customAttributes do
                        graph.AddEdge(mNode, ResourceNode req)

                    if not isStatic then hasInstanceMethod <- true

                    if meth.Attributes.HasFlag MethodAttributes.Abstract then
                        // Abstract method
                        let aNode = graph.AddOrLookupNode(AbstractMethodNode(def, mdef))
                        graph.AddEdge(mNode, aNode)
                        abstractAndVirtualMethods.Add(aNode) |> ignore
                    elif meth.Attributes.HasFlag MethodAttributes.Virtual then
                        let bdef =
                            match overrides.TryGetValue mh with
                            | true, decl ->
                                // Override
                                match PM.DeclaringTypeOf md decl with
                                | Some t -> sigs.Decode typeCtx t |> getTypeDefinition asmName
                                | None -> def
                            | _ ->
                                // Virtual method
                                def
                        let aNode = graph.AddOrLookupNode(AbstractMethodNode(def, mdef))
                        graph.AddEdge(mNode, aNode)
                        graph.AddOverride(def, bdef, mdef)
                        abstractAndVirtualMethods.Add(aNode) |> ignore

                    let gc = getConstraints (methodContext typeCtx meth) (meth.GetGenericParameters()) tgen

                    try methods.Add(mdef, { CompiledForm = kind; Optimizations = opts; Generics = gc; Expression = body })
                    with _ ->
                        failwithf "Duplicate definition for method of %s: %s" def.Value.FullName (string mdef.Value)

        // Reflector is used for WIG, where abstract/virtual methods are generally meant to be called externally,
        // so any override of them should never be DCE'd. This enforces it.
        for aNode in abstractAndVirtualMethods do
            graph.AddEdge(clsNodeIndex, aNode)

        let address =
            match getImport typeAttrs asmName with
            | Some addr -> addr
            | None ->
                prototypes.TryFind(def.Value.FullName)
                |> Option.defaultValue def.Value.FullName
                |> fun s -> Address.LibAddr (s.Split('.') |> List.ofArray)

        classes.Add(def,
            (
                address,
                NotCustomType,
                Some {
                    BaseClass = baseDef
                    Implements = implements
                    Generics = getConstraints typeCtx typeParams 0
                    Constructors = constructors
                    Fields = Map.empty
                    StaticConstructor = None
                    Methods = methods
                    Implementations = Map.empty // TODO
                    QuotedArgMethods = Map.empty // TODO
                    HasWSPrototype = false // do not overwrite external prototype
                    IsStub = true
                    Macros = []
                    Type = getTSType typeAttrs
                }
            )
        )

    let transformInterface (h: TypeDefinitionHandle) =
        let typ = md.GetTypeDefinition h
        if not (typ.Attributes.HasFlag TypeAttributes.Interface) then () else
        let methodsWithAttrs = methodsOf typ
        let intfAsClass =
            methodsWithAttrs
            |> Seq.exists (fun (_, _, customAttributes, _, _) ->
                Option.isSome (getInline customAttributes)
                || not (List.isEmpty (getMacros customAttributes))
            )
        if intfAsClass then transformClass true h else
        let def = typeDef h
        let typeAttrs = attrsOf (typ.GetCustomAttributes())
        let typeParams = typ.GetGenericParameters()
        let typeCtx = { TypeParameters = genericNames typeParams; MethodParameters = [||] }

        let reqs = typeAttrs |> getRequires
        if not reqs.IsEmpty then
            let i = graph.AddOrLookupNode(TypeNode def)
            for r in reqs do
                graph.AddEdge(i, ResourceNode r)

        let address =
            match getImport typeAttrs asmName with
            | Some addr -> addr
            | None ->
                prototypes.TryFind(def.Value.FullName)
                |> Option.defaultValue def.Value.FullName
                |> fun s -> Address.LibAddr (s.Split('.') |> List.ofArray)

        let methods = Dictionary()
        let tgen = typeParams.Count
        for (_, meth, customAttributes, isGetter, isSetter) in methodsWithAttrs do
            let signature = signatureOf typeCtx meth
            let methName = md.GetString meth.Name
            let mdef =
                Hashed {
                    MethodName = methName
                    Parameters = signature.ParameterTypes |> Seq.map (getType asmName tgen) |> List.ofSeq
                    ReturnType = getType asmName tgen signature.ReturnType
                    Generics   = signature.GenericParameterCount
                }
            let name =
                match getName customAttributes with
                | Some n -> n
                | _ -> methName
            let gc = getConstraints (methodContext typeCtx meth) (meth.GetGenericParameters()) tgen
            let memberKind =
                if isGetter then
                    MemberKind.Getter
                elif isSetter then
                    MemberKind.Setter
                else
                    MemberKind.Simple
            methods.Add(mdef, (name, memberKind, gc))

        interfaces.Add(def,
            {
                Address = address
                Extends =
                    typ.GetInterfaceImplementations()
                    |> Seq.map (fun ii -> (md.GetInterfaceImplementation ii).Interface |> sigs.Decode typeCtx |> getType asmName 0 |> getConcreteType)
                    |> List.ofSeq
                Methods = methods
                Generics = getConstraints typeCtx typeParams 0
                Type = getTSType typeAttrs
            }
        )

    allTypes |> Seq.iter transformInterface
    allTypes |> Seq.iter (transformClass false)

    {
        SiteletDefinition = None
        Dependencies = graph.GetData()
        Interfaces = interfaces
        Classes = classes
        MacroEntries = Map.empty
        Quotations = Map.empty
        ResourceHashes = Dictionary()
        ExtraBundles = Set.empty
        PreBundle = Map.empty
        QuotedMethods = Map.empty
        WebControls = Map.empty
        RemoteMethods = Map.empty
    }

/// Metadata of a WIG-generated assembly image.
let TransformAssembly prototypes (assembly: WebSharper.Compiler.PEMetadata.PEAssembly) =
    trAsm prototypes assembly

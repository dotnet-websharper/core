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

/// The assemblies WIG generates: a small object model of types with members that have
/// attributes and stub bodies, resolution of referenced types from reference assemblies,
/// and writing with System.Reflection.Metadata.
namespace WebSharper.InterfaceGenerator

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable

type internal AssemblyRef =
    {
        Name : string
        Version : Version
        Culture : string
        PublicKeyToken : byte[]
        IsRetargetable : bool
    }

    member r.FullName =
        let token =
            if r.PublicKeyToken.Length = 0 then "null" else
            r.PublicKeyToken |> Array.map (fun b -> b.ToString("x2")) |> String.concat ""
        sprintf "%s, Version=%O, Culture=%s, PublicKeyToken=%s" r.Name r.Version (if r.Culture = "" then "neutral" else r.Culture) token

/// A type defined in a referenced assembly.
[<Sealed; AllowNullLiteral>]
type internal ExtType(scope: AssemblyRef, ns: string, name: string, declaring: ExtType, isValueType: bool, isInterface: bool, genericParameterCount: int) =
    member _.Scope = scope
    member _.Namespace = ns
    member _.Name = name
    member _.DeclaringType = declaring
    member _.IsValueType = isValueType
    member _.IsInterface = isInterface
    member _.GenericParameterCount = genericParameterCount
    member t.FullName : string =
        if isNull declaring then (if ns = "" then name else ns + "." + name)
        else declaring.FullName + "+" + name

type internal TypeRef =
    | Local of TypeDef
    | External of ExtType
    | Instance of TypeRef * TypeRef list
    | ArrayOf of TypeRef * rank: int
    | Param of GenericParam

    member t.FullName : string =
        match t with
        | Local d -> d.FullName
        | External (e: ExtType) -> e.FullName
        | Instance (d, args) -> d.FullName + "<" + (args |> List.map (fun a -> a.FullName) |> String.concat ",") + ">"
        | ArrayOf (e, rank) -> e.FullName + "[" + String(',', rank - 1) + "]"
        | Param g -> g.Name

    /// Number of generic parameters of a type definition, or arguments of an instance.
    member t.GenericParameterCount =
        match t with
        | Local d -> d.GenericParameters.Count
        | External (e: ExtType) -> e.GenericParameterCount
        | Instance (_, args) -> args.Length
        | _ -> 0

    /// A generic type definition, not instantiated.
    member t.HasGenericParameters =
        match t with
        | Local d -> d.GenericParameters.Count > 0
        | External (e: ExtType) -> e.GenericParameterCount > 0
        | _ -> false

    member t.IsInterface =
        match t with
        | Local d -> d.IsInterface
        | External (e: ExtType) -> e.IsInterface
        | Instance (d, _) -> d.IsInterface
        | _ -> false

and [<Sealed; AllowNullLiteral>] internal GenericParam(name: string, isMethodParameter: bool, position: int) =
    member _.Name = name
    member _.IsMethodParameter = isMethodParameter
    member _.Position = position
    member val Constraints : List<TypeRef> = List<TypeRef>()

and internal AttrArg =
    | AString of string
    | AType of TypeRef

and internal MethodRef =
    {
        DeclaringType : TypeRef
        Name : string
        HasThis : bool
        ReturnType : TypeRef
        Parameters : TypeRef list
    }

and internal CustomAttr =
    {
        Constructor : MethodRef
        Arguments : AttrArg list
    }

and internal MethodBody =
    /// `throw new T()`
    | ThrowNew of ctor: MethodRef
    /// Calls the base constructor with string arguments, the last ones in a `params` array
    /// of the given element type.
    | CallBaseConstructor of ctor: MethodRef * args: string list * paramArray: option<TypeRef * string list>

and [<Sealed; AllowNullLiteral>] internal TypeDef(ns: string, name: string, attributes: TypeAttributes) =
    member val Namespace = ns
    member val Name : string = name with get, set
    member val Attributes : TypeAttributes = attributes with get, set
    member val BaseType : option<TypeRef> = None with get, set
    member val DeclaringType : TypeDef = null with get, set
    member val Interfaces : List<TypeRef> = List<TypeRef>()
    member val GenericParameters : List<GenericParam> = List<GenericParam>()
    member val Methods : List<MethodDef> = List<MethodDef>()
    member val Properties : List<PropertyDef> = List<PropertyDef>()
    member val NestedTypes : List<TypeDef> = List<TypeDef>()
    member val CustomAttributes : List<CustomAttr> = List<CustomAttr>()
    member t.IsInterface = t.Attributes.HasFlag TypeAttributes.Interface
    member t.FullName : string =
        if isNull t.DeclaringType then (if t.Namespace = "" then t.Name else t.Namespace + "." + t.Name)
        else t.DeclaringType.FullName + "+" + t.Name
    member t.AddNestedType(n: TypeDef) =
        n.DeclaringType <- t
        t.NestedTypes.Add n
    member t.AddGenericParameter(n: string) =
        let p = GenericParam(n, false, t.GenericParameters.Count)
        t.GenericParameters.Add p
        p

and [<Sealed; AllowNullLiteral>] internal MethodDef(name: string, attributes: MethodAttributes, returnType: TypeRef) =
    member val Name = name
    member val Attributes = attributes
    member val ReturnType = returnType with get, set
    member val Parameters : List<ParamDef> = List<ParamDef>()
    member val GenericParameters : List<GenericParam> = List<GenericParam>()
    member val CustomAttributes : List<CustomAttr> = List<CustomAttr>()
    member val Body : option<MethodBody> = None with get, set
    member m.IsStatic = m.Attributes.HasFlag MethodAttributes.Static
    member m.AddGenericParameter(n: string) =
        let p = GenericParam(n, true, m.GenericParameters.Count)
        m.GenericParameters.Add p
        p

and [<Sealed; AllowNullLiteral>] internal ParamDef(name: string, parameterType: TypeRef) =
    member val Name = name
    member val ParameterType = parameterType
    member val CustomAttributes : List<CustomAttr> = List<CustomAttr>()

and [<Sealed; AllowNullLiteral>] internal PropertyDef(name: string, propertyType: TypeRef) =
    member val Name = name
    member val PropertyType = propertyType
    member val GetMethod : MethodDef = null with get, set
    member val SetMethod : MethodDef = null with get, set
    member val CustomAttributes : List<CustomAttr> = List<CustomAttr>()

/// The assembly being generated.
type internal AssemblyDef(name: string, version: Version) =
    member val Name = name
    member val Version = version
    member val Culture = "" with get, set
    member val Flags = AssemblyFlags() with get, set
    member val PublicKey : byte[] = [||] with get, set
    member val KeyPair : option<WebSharper.Compiler.PEWriter.KeyPair> = None with get, set
    member val CustomAttributes : List<CustomAttr> = List<CustomAttr>()
    member val Types : List<TypeDef> = List<TypeDef>()
    /// Embedded manifest resources: name and contents.
    member val Resources : List<string * byte[]> = List<string * byte[]>()
    member val Win32Resources : ResourceSectionBuilder = null with get, set
    member val Header = PEHeaderBuilder.CreateLibraryHeader() with get, set

    /// All type definitions, enclosing types before their nested types.
    member a.AllTypes =
        let res = List<TypeDef>()
        let rec add (t: TypeDef) =
            res.Add t
            for n in t.NestedTypes do add n
        for t in a.Types do add t
        res

/// An assembly WIG references, read with System.Reflection.Metadata.
[<Sealed>]
type internal RefAssembly(path: string, netstandard: option<AssemblyRef>) =
    let md =
        use s = File.OpenRead path
        let pe = new PEReader(s, PEStreamOptions.PrefetchMetadata)
        pe.GetMetadataReader()
    let identity =
        let a = md.GetAssemblyDefinition()
        {
            Name = md.GetString a.Name
            Version = a.Version
            Culture = md.GetString a.Culture
            PublicKeyToken = WebSharper.Compiler.PEWriter.PublicKeyToken (md.GetBlobBytes a.PublicKey)
            IsRetargetable = a.Flags.HasFlag AssemblyFlags.Retargetable
        }
    let handles =
        let d = Dictionary<string, TypeDefinitionHandle>()
        for h in md.TypeDefinitions do
            d.[WebSharper.Compiler.PEMetadata.TypeDefinitionName md h] <- h
        d
    let types = Dictionary<TypeDefinitionHandle, ExtType>()

    let rec extType (h: TypeDefinitionHandle) =
        match types.TryGetValue h with
        | true, t -> t
        | _ ->
            let td = md.GetTypeDefinition h
            let declaring = td.GetDeclaringType()
            let ns = md.GetString td.Namespace
            let name = md.GetString td.Name
            let fullName = WebSharper.Compiler.PEMetadata.TypeDefinitionName md h
            let baseName =
                if td.BaseType.IsNil then None else WebSharper.Compiler.PEMetadata.TypeName md td.BaseType
            let isValueType =
                baseName = Some "System.Enum" || (baseName = Some "System.ValueType" && fullName <> "System.Enum")
            let scope =
                match netstandard with
                | Some n when WebSharper.Core.AssemblyConventions.IsNetStandardType fullName -> n
                | _ -> identity
            let t =
                ExtType(scope, ns, name, (if declaring.IsNil then null else extType declaring), isValueType,
                    td.Attributes.HasFlag TypeAttributes.Interface, td.GetGenericParameters().Count)
            types.[h] <- t
            t

    member _.Identity = identity

    /// A type by its full name, with `+` separating nested types.
    member _.TryGetType(fullName: string) =
        match handles.TryGetValue fullName with
        | true, h -> Some (extType h)
        | _ -> None

/// Resolves the types WIG references from reference assemblies.
[<Sealed>]
type internal ReferenceResolver(aR: WebSharper.Compiler.AssemblyResolver, referencePaths: seq<string>, outputName: string) =
    let assemblies = Dictionary<string, RefAssembly>()
    let runtimeDir = Path.GetDirectoryName(typeof<obj>.Assembly.Location)

    let netstandard =
        let path =
            referencePaths |> Seq.tryFind (fun p -> Path.GetFileName(p).ToLower() = "netstandard.dll")
        match path with
        | Some p -> RefAssembly(p, None).Identity
        | None -> failwith "WIG project requires netstandard.dll reference"

    let resolveAsm (asmName: string) =
        let name = AssemblyName(asmName)
        if name.Name = outputName then
            failwithf "Assembly %s is the interface generator assembly, its types can't be used in the generated assembly" asmName
        match assemblies.TryGetValue name.Name with
        | true, a -> a
        | _ ->
            let path =
                if name.Name = "System.Private.CoreLib" then
                    Some typeof<obj>.Assembly.Location
                else
                    match aR.ResolvePath name with
                    | Some p when File.Exists p -> Some p
                    | _ ->
                        let p = Path.Combine(runtimeDir, name.Name + ".dll")
                        if File.Exists p then Some p else None
            match path with
            | Some p ->
                let a = RefAssembly(p, Some netstandard)
                assemblies.[name.Name] <- a
                a
            | None -> failwithf "Could not resolve assembly: %s" asmName

    /// A type by its assembly name and full name, with `+` separating nested types.
    member _.Type(asmName: string, fullName: string) =
        let asm = resolveAsm asmName
        match asm.TryGetType fullName with
        | Some t -> External t
        | None ->
            let loaded = asm.Identity.FullName
            failwithf "Could not resolve type %s in %s" fullName (if loaded = asmName then loaded else asmName + " loaded as " + loaded)

/// Writes an AssemblyDef with System.Reflection.Metadata.
module internal AssemblyWriter =

    let private corlibNames = HashSet [ "netstandard"; "mscorlib"; "System.Private.CoreLib"; "System.Runtime" ]

    let private primitives =
        dict [
            "Boolean", PrimitiveTypeCode.Boolean
            "Char", PrimitiveTypeCode.Char
            "SByte", PrimitiveTypeCode.SByte
            "Byte", PrimitiveTypeCode.Byte
            "Int16", PrimitiveTypeCode.Int16
            "UInt16", PrimitiveTypeCode.UInt16
            "Int32", PrimitiveTypeCode.Int32
            "UInt32", PrimitiveTypeCode.UInt32
            "Int64", PrimitiveTypeCode.Int64
            "UInt64", PrimitiveTypeCode.UInt64
            "Single", PrimitiveTypeCode.Single
            "Double", PrimitiveTypeCode.Double
            "String", PrimitiveTypeCode.String
            "Object", PrimitiveTypeCode.Object
            "IntPtr", PrimitiveTypeCode.IntPtr
            "UIntPtr", PrimitiveTypeCode.UIntPtr
        ]

    let private isCorlibSystemType (e: ExtType) =
        isNull e.DeclaringType && e.Namespace = "System" && corlibNames.Contains e.Scope.Name

    let private primitive (e: ExtType) =
        if isCorlibSystemType e then
            match primitives.TryGetValue e.Name with
            | true, c -> Some c
            | _ -> None
        else None

    let private isVoid (t: TypeRef) =
        match t with
        | External e -> isCorlibSystemType e && e.Name = "Void"
        | _ -> false

    /// The name of a type as in custom attribute System.Type arguments.
    let rec private serializedName (topLevel: bool) (t: TypeRef) =
        match t with
        | Local d -> d.FullName
        | External e -> if topLevel then e.FullName + ", " + e.Scope.FullName else e.FullName
        | Instance (d, args) ->
            let args = args |> List.map (fun a -> "[" + serializedName true a + "]") |> String.concat ","
            match d with
            | External e ->
                e.FullName + "[" + args + "]" + (if topLevel then ", " + e.Scope.FullName else "")
            | _ -> serializedName false d + "[" + args + "]"
        | ArrayOf (e, rank) -> serializedName topLevel e + "[" + String(',', rank - 1) + "]"
        | Param g -> g.Name

    let Write (a: AssemblyDef) =
        let md = MetadataBuilder()
        let il = BlobBuilder()
        let bodies = MethodBodyStreamEncoder(il)
        let str (s: string) = if String.IsNullOrEmpty s then StringHandle() else md.GetOrAddString s
        let blob (b: byte[]) = if b.Length = 0 then BlobHandle() else md.GetOrAddBlob b

        let flags = if a.PublicKey.Length > 0 then a.Flags ||| AssemblyFlags.PublicKey else a.Flags
        let asmHandle =
            md.AddAssembly(str a.Name, a.Version, str a.Culture, blob a.PublicKey, flags, AssemblyHashAlgorithm.Sha1)
        let mvid = md.ReserveGuid()
        md.AddModule(0, str (a.Name + ".dll"), mvid.Handle, GuidHandle(), GuidHandle()) |> ignore

        let types = a.AllTypes
        let typeHandles = Dictionary<TypeDef, TypeDefinitionHandle>(HashIdentity.Reference)
        types |> Seq.iteri (fun i t -> typeHandles.[t] <- MetadataTokens.TypeDefinitionHandle(i + 2))
        let methodHandles = Dictionary<MethodDef, MethodDefinitionHandle>(HashIdentity.Reference)
        for t in types do
            for m in t.Methods do
                methodHandles.[m] <- MetadataTokens.MethodDefinitionHandle(methodHandles.Count + 1)

        let asmRefs = Dictionary<string, AssemblyReferenceHandle>()
        let asmRef (r: AssemblyRef) =
            let key = r.FullName
            match asmRefs.TryGetValue key with
            | true, h -> h
            | _ ->
                let h =
                    md.AddAssemblyReference(str r.Name, r.Version, str r.Culture, blob r.PublicKeyToken,
                        (if r.IsRetargetable then AssemblyFlags.Retargetable else AssemblyFlags()), BlobHandle())
                asmRefs.[key] <- h
                h

        let typeRefs = Dictionary<ExtType, TypeReferenceHandle>(HashIdentity.Reference)
        let rec typeRef (e: ExtType) =
            match typeRefs.TryGetValue e with
            | true, h -> h
            | _ ->
                let scope : EntityHandle =
                    if isNull e.DeclaringType then AssemblyReferenceHandle.op_Implicit (asmRef e.Scope)
                    else TypeReferenceHandle.op_Implicit (typeRef e.DeclaringType)
                let h = md.AddTypeReference(scope, (if isNull e.DeclaringType then str e.Namespace else StringHandle()), str e.Name)
                typeRefs.[e] <- h
                h

        let typeSpecs = Dictionary<string, TypeSpecificationHandle>()

        let rec encodeType (enc: SignatureTypeEncoder) (t: TypeRef) =
            match t with
            | Local d -> enc.Type(TypeDefinitionHandle.op_Implicit typeHandles.[d], false)
            | External e ->
                match primitive e with
                | Some c -> enc.PrimitiveType c
                | None -> enc.Type(TypeReferenceHandle.op_Implicit (typeRef e), e.IsValueType)
            | Instance (d, args) ->
                let (h: EntityHandle), isValueType =
                    match d with
                    | Local d -> TypeDefinitionHandle.op_Implicit typeHandles.[d], false
                    | External e -> TypeReferenceHandle.op_Implicit (typeRef e), e.IsValueType
                    | _ -> failwithf "Invalid generic type definition: %s" d.FullName
                let argsEnc = enc.GenericInstantiation(h, args.Length, isValueType)
                for x in args do
                    encodeType (argsEnc.AddArgument()) x
            | ArrayOf (e, 1) ->
                encodeType (enc.SZArray()) e
            | ArrayOf (e, rank) ->
                enc.Array(
                    (fun el -> encodeType el e),
                    (fun shape -> shape.Shape(rank, ImmutableArray<int>.Empty, ImmutableArray<int>.Empty)))
            | Param g ->
                if g.IsMethodParameter then enc.GenericMethodTypeParameter g.Position
                else enc.GenericTypeParameter g.Position

        /// A TypeDef, TypeRef or TypeSpec handle.
        let typeHandle (t: TypeRef) : EntityHandle =
            match t with
            | Local d -> TypeDefinitionHandle.op_Implicit typeHandles.[d]
            | External e -> TypeReferenceHandle.op_Implicit (typeRef e)
            | _ ->
                let b = BlobBuilder()
                encodeType (BlobEncoder(b).TypeSpecificationSignature()) t
                let bytes = b.ToArray()
                let key = Convert.ToBase64String bytes
                match typeSpecs.TryGetValue key with
                | true, h -> TypeSpecificationHandle.op_Implicit h
                | _ ->
                    let h = md.AddTypeSpecification(md.GetOrAddBlob bytes)
                    typeSpecs.[key] <- h
                    TypeSpecificationHandle.op_Implicit h

        let encodeReturnType (r: ReturnTypeEncoder) (t: TypeRef) =
            if isVoid t then r.Void() else encodeType (r.Type()) t

        let methodSignature (isInstance: bool) (genericCount: int) (returnType: TypeRef) (pars: seq<TypeRef>) =
            let pars = Array.ofSeq pars
            let b = BlobBuilder()
            BlobEncoder(b)
                .MethodSignature(SignatureCallingConvention.Default, genericCount, isInstance)
                .Parameters(pars.Length,
                    (fun r -> encodeReturnType r returnType),
                    (fun ps -> for p in pars do encodeType (ps.AddParameter().Type()) p))
            md.GetOrAddBlob b

        let memberRefs = Dictionary<string, MemberReferenceHandle>()
        let memberRef (m: MethodRef) =
            let parent = typeHandle m.DeclaringType
            let signature = methodSignature m.HasThis 0 m.ReturnType m.Parameters
            let key = sprintf "%d:%s:%d" (MetadataTokens.GetToken parent) m.Name (MetadataTokens.GetHeapOffset signature)
            match memberRefs.TryGetValue key with
            | true, h -> h
            | _ ->
                let h = md.AddMemberReference(parent, str m.Name, signature)
                memberRefs.[key] <- h
                h

        let customAttributes = List<EntityHandle * CustomAttr>()
        let addAttributes (parent: EntityHandle) (attrs: seq<CustomAttr>) =
            for x in attrs do customAttributes.Add((parent, x))

        let encodeBody (b: MethodBody) =
            let code = BlobBuilder()
            let ie = InstructionEncoder(code)
            match b with
            | ThrowNew ctor ->
                ie.OpCode ILOpCode.Newobj
                ie.Token(MemberReferenceHandle.op_Implicit (memberRef ctor) : EntityHandle)
                ie.OpCode ILOpCode.Throw
            | CallBaseConstructor (ctor, args, paramArray) ->
                ie.LoadArgument 0
                for x in args do
                    ie.LoadString(md.GetOrAddUserString x)
                match paramArray with
                | Some (elemType, xs) ->
                    ie.LoadConstantI4 xs.Length
                    ie.OpCode ILOpCode.Newarr
                    ie.Token(typeHandle elemType)
                    xs |> List.iteri (fun i x ->
                        ie.OpCode ILOpCode.Dup
                        ie.LoadConstantI4 i
                        ie.LoadString(md.GetOrAddUserString x)
                        ie.OpCode ILOpCode.Stelem_ref)
                | None -> ()
                ie.Call(MemberReferenceHandle.op_Implicit (memberRef ctor) : EntityHandle)
                ie.OpCode ILOpCode.Ret
            bodies.AddMethodBody(ie, 8, StandaloneSignatureHandle(), MethodBodyAttributes.None)

        md.AddTypeDefinition(
            TypeAttributes(), StringHandle(), str "<Module>", EntityHandle(),
            MetadataTokens.FieldDefinitionHandle 1, MetadataTokens.MethodDefinitionHandle 1) |> ignore

        let mutable nextParameter = 1
        let mutable nextMethod = 1
        for t in types do
            let firstMethod = MetadataTokens.MethodDefinitionHandle nextMethod
            for m in t.Methods do
                let signature = methodSignature (not m.IsStatic) m.GenericParameters.Count m.ReturnType (m.Parameters |> Seq.map (fun p -> p.ParameterType))
                let bodyOffset =
                    match m.Body with
                    | Some b when not (m.Attributes.HasFlag MethodAttributes.Abstract) -> encodeBody b
                    | _ -> -1
                let firstParameter = MetadataTokens.ParameterHandle nextParameter
                m.Parameters |> Seq.iteri (fun i p ->
                    let h = md.AddParameter(ParameterAttributes.None, str p.Name, i + 1)
                    addAttributes (ParameterHandle.op_Implicit h) p.CustomAttributes
                    nextParameter <- nextParameter + 1)
                md.AddMethodDefinition(
                    m.Attributes, MethodImplAttributes.IL ||| MethodImplAttributes.Managed, str m.Name,
                    signature, bodyOffset, firstParameter) |> ignore
                addAttributes (MethodDefinitionHandle.op_Implicit methodHandles.[m]) m.CustomAttributes
                nextMethod <- nextMethod + 1
            let baseType =
                match t.BaseType with
                | Some b -> typeHandle b
                | None -> EntityHandle()
            md.AddTypeDefinition(
                t.Attributes, (if isNull t.DeclaringType then str t.Namespace else StringHandle()), str t.Name,
                baseType, MetadataTokens.FieldDefinitionHandle 1, firstMethod) |> ignore
            addAttributes (TypeDefinitionHandle.op_Implicit typeHandles.[t]) t.CustomAttributes

        for t in types do
            if not (isNull t.DeclaringType) then
                md.AddNestedType(typeHandles.[t], typeHandles.[t.DeclaringType])
            for i in t.Interfaces do
                md.AddInterfaceImplementation(typeHandles.[t], typeHandle i) |> ignore

        // generic parameters are sorted by owner (types and methods interleaved by row)
        let genericParams =
            [|
                for t in types do
                    for g in t.GenericParameters do
                        yield (TypeDefinitionHandle.op_Implicit typeHandles.[t] : EntityHandle), g
                    for m in t.Methods do
                        for g in m.GenericParameters do
                            yield (MethodDefinitionHandle.op_Implicit methodHandles.[m] : EntityHandle), g
            |]
            |> Array.sortBy (fun (o, g) -> CodedIndex.TypeOrMethodDef o, g.Position)
        let constraints =
            genericParams |> Array.map (fun (o, g) ->
                md.AddGenericParameter(o, GenericParameterAttributes.None, str g.Name, g.Position), g.Constraints)
        for h, cs in constraints do
            for c in cs do
                md.AddGenericParameterConstraint(h, typeHandle c) |> ignore

        let mutable nextProperty = 1
        for t in types do
            if t.Properties.Count > 0 then
                md.AddPropertyMap(typeHandles.[t], MetadataTokens.PropertyDefinitionHandle nextProperty)
                for p in t.Properties do
                    let indexParameters =
                        if not (isNull p.GetMethod) then
                            p.GetMethod.Parameters |> Seq.map (fun x -> x.ParameterType) |> Array.ofSeq
                        else
                            p.SetMethod.Parameters |> Seq.map (fun x -> x.ParameterType) |> Array.ofSeq
                            |> fun ps -> ps.[.. ps.Length - 2]
                    let isInstance =
                        let m = if isNull p.GetMethod then p.SetMethod else p.GetMethod
                        not (isNull m) && not m.IsStatic
                    let b = BlobBuilder()
                    BlobEncoder(b)
                        .PropertySignature(isInstance)
                        .Parameters(indexParameters.Length,
                            (fun r -> encodeReturnType r p.PropertyType),
                            (fun ps -> for x in indexParameters do encodeType (ps.AddParameter().Type()) x))
                    let h = md.AddProperty(PropertyAttributes.None, str p.Name, md.GetOrAddBlob b)
                    if not (isNull p.GetMethod) then
                        md.AddMethodSemantics(PropertyDefinitionHandle.op_Implicit h, MethodSemanticsAttributes.Getter, methodHandles.[p.GetMethod])
                    if not (isNull p.SetMethod) then
                        md.AddMethodSemantics(PropertyDefinitionHandle.op_Implicit h, MethodSemanticsAttributes.Setter, methodHandles.[p.SetMethod])
                    addAttributes (PropertyDefinitionHandle.op_Implicit h) p.CustomAttributes
                    nextProperty <- nextProperty + 1

        addAttributes (AssemblyDefinitionHandle.op_Implicit asmHandle) a.CustomAttributes
        for parent, x in customAttributes |> Seq.sortBy (fun (p, _) -> CodedIndex.HasCustomAttribute p) do
            let value = BlobBuilder()
            value.WriteUInt16 1us
            for arg in x.Arguments do
                match arg with
                | AString s -> value.WriteSerializedString s
                | AType t -> value.WriteSerializedString (serializedName true t)
            value.WriteUInt16 0us
            md.AddCustomAttribute(parent, MemberReferenceHandle.op_Implicit (memberRef x.Constructor), md.GetOrAddBlob value) |> ignore

        let resources = BlobBuilder()
        for name, contents in a.Resources do
            resources.Align 8
            let offset = resources.Count
            resources.WriteInt32 contents.Length
            resources.WriteBytes contents
            md.AddManifestResource(ManifestResourceAttributes.Public, str name, EntityHandle(), uint32 offset) |> ignore

        WebSharper.Compiler.PEWriter.Serialize a.Header md il resources a.Win32Resources a.KeyPair (Some mvid)

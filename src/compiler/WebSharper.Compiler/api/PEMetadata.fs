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

/// Reads assembly images with System.Reflection.Metadata: names, custom attributes
/// and manifest resources.
module WebSharper.Compiler.PEMetadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable

/// A type as it appears in a custom attribute blob.
type AttributeType =
    | Primitive of PrimitiveTypeCode
    | SystemType
    /// A named type: the simple name of its assembly (None: the current one, or not given
    /// in a serialized name) and its full name, with `+` separating nested types.
    | Named of assembly: string option * fullName: string
    | SZArray of AttributeType

/// The full name of a type definition, with `+` separating nested types.
let rec TypeDefinitionName (md: MetadataReader) (h: TypeDefinitionHandle) =
    let td = md.GetTypeDefinition h
    let name = md.GetString td.Name
    let declaring = td.GetDeclaringType()
    if not declaring.IsNil then
        TypeDefinitionName md declaring + "+" + name
    else
        let ns = md.GetString td.Namespace
        if ns = "" then name else ns + "." + name

/// The full name of a type reference, with `+` separating nested types, and the simple
/// name of the assembly it is in (None for the current assembly).
let rec TypeReferenceName (md: MetadataReader) (h: TypeReferenceHandle) : string option * string =
    let tr = md.GetTypeReference h
    let name = md.GetString tr.Name
    let scope = tr.ResolutionScope
    if scope.Kind = HandleKind.TypeReference then
        let asm, outer = TypeReferenceName md (TypeReferenceHandle.op_Explicit scope)
        asm, outer + "+" + name
    else
        let ns = md.GetString tr.Namespace
        let fullName = if ns = "" then name else ns + "." + name
        let asm =
            if scope.Kind = HandleKind.AssemblyReference then
                Some (md.GetString (md.GetAssemblyReference(AssemblyReferenceHandle.op_Explicit scope).Name))
            else None
        asm, fullName

/// The name of the type declaring a method definition or reference, if it is a definition
/// or a reference.
let DeclaringTypeOf (md: MetadataReader) (h: EntityHandle) =
    match h.Kind with
    | HandleKind.MethodDefinition ->
        Some (TypeDefinitionHandle.op_Implicit (md.GetMethodDefinition(MethodDefinitionHandle.op_Explicit h).GetDeclaringType()) : EntityHandle)
    | HandleKind.MemberReference ->
        Some (md.GetMemberReference(MemberReferenceHandle.op_Explicit h).Parent)
    | _ -> None

/// The full name of a type definition or reference (None for other handles).
let TypeName (md: MetadataReader) (h: EntityHandle) =
    match h.Kind with
    | HandleKind.TypeDefinition -> Some (TypeDefinitionName md (TypeDefinitionHandle.op_Explicit h))
    | HandleKind.TypeReference -> Some (snd (TypeReferenceName md (TypeReferenceHandle.op_Explicit h)))
    | _ -> None

/// The full name of the type of a custom attribute, with `+` separating nested types.
let AttributeTypeName (md: MetadataReader) (attr: CustomAttribute) =
    DeclaringTypeOf md attr.Constructor
    |> Option.bind (TypeName md)
    |> Option.defaultValue ""

/// Splits a serialized type name (as in custom attributes) into the full name of the type,
/// without generic arguments, and the simple name of its assembly, if given.
let ParseSerializedTypeName (s: string) =
    // generic arguments are in brackets, the assembly name follows the first comma outside them
    let mutable depth = 0
    let mutable comma = -1
    let mutable bracket = -1
    let mutable i = 0
    while comma < 0 && i < s.Length do
        match s.[i] with
        | '[' ->
            if bracket < 0 then bracket <- i
            depth <- depth + 1
        | ']' -> depth <- depth - 1
        | ',' when depth = 0 -> comma <- i
        | _ -> ()
        i <- i + 1
    let typeEnd =
        if bracket >= 0 then bracket elif comma >= 0 then comma else s.Length
    let fullName = s.Substring(0, typeEnd).Trim()
    let asm =
        if comma < 0 then None else
        let a = s.Substring(comma + 1)
        let c = a.IndexOf ','
        Some ((if c < 0 then a else a.Substring(0, c)).Trim())
    fullName, asm

let private isEnum (md: MetadataReader) (td: TypeDefinition) =
    let b = td.BaseType
    not b.IsNil && TypeName md b = Some "System.Enum"

/// The underlying type of an enum defined in this module.
let private underlyingEnumType (md: MetadataReader) (fullName: string) =
    md.TypeDefinitions |> Seq.tryPick (fun h ->
        let td = md.GetTypeDefinition h
        if isEnum md td && TypeDefinitionName md h = fullName then
            td.GetFields() |> Seq.tryPick (fun f ->
                let fd = md.GetFieldDefinition f
                if fd.Attributes.HasFlag FieldAttributes.Static then None else
                let blob = md.GetBlobReader fd.Signature
                blob.ReadSignatureHeader() |> ignore
                match blob.ReadSignatureTypeCode() with
                | SignatureTypeCode.Boolean -> Some PrimitiveTypeCode.Boolean
                | SignatureTypeCode.Char -> Some PrimitiveTypeCode.Char
                | SignatureTypeCode.SByte -> Some PrimitiveTypeCode.SByte
                | SignatureTypeCode.Byte -> Some PrimitiveTypeCode.Byte
                | SignatureTypeCode.Int16 -> Some PrimitiveTypeCode.Int16
                | SignatureTypeCode.UInt16 -> Some PrimitiveTypeCode.UInt16
                | SignatureTypeCode.Int32 -> Some PrimitiveTypeCode.Int32
                | SignatureTypeCode.UInt32 -> Some PrimitiveTypeCode.UInt32
                | SignatureTypeCode.Int64 -> Some PrimitiveTypeCode.Int64
                | SignatureTypeCode.UInt64 -> Some PrimitiveTypeCode.UInt64
                | _ -> None)
        else None)

/// Decodes custom attribute blobs into AttributeType values. Enums of other assemblies
/// in attribute arguments are read as Int32 (their underlying type is not known without
/// resolving them).
type AttributeTypeProvider(md: MetadataReader) =
    interface ICustomAttributeTypeProvider<AttributeType> with
        member _.GetPrimitiveType(code) = Primitive code
        member _.GetSystemType() = SystemType
        member _.GetSZArrayType(e) = SZArray e
        member _.GetTypeFromDefinition(_, h, _) = Named (None, TypeDefinitionName md h)
        member _.GetTypeFromReference(_, h, _) = Named (TypeReferenceName md h)
        member _.GetTypeFromSerializedName(name) =
            if isNull name then Named (None, "") else
            let fullName, asm = ParseSerializedTypeName name
            Named (asm, fullName)
        member _.GetUnderlyingEnumType(t) =
            match t with
            | Named (None, n) -> underlyingEnumType md n |> Option.defaultValue PrimitiveTypeCode.Int32
            | _ -> PrimitiveTypeCode.Int32
        member _.IsSystemType(t) =
            match t with
            | SystemType -> true
            | Named (_, "System.Type") -> true
            | _ -> false

/// A custom attribute with its type name, decoded on first use.
type CustomAttr(md: MetadataReader, attr: CustomAttribute) =
    let typeName = lazy (AttributeTypeName md attr)
    let value = lazy attr.DecodeValue(AttributeTypeProvider(md))
    member _.TypeName = typeName.Value
    member _.Value = value.Value
    /// Fixed (constructor) argument values. Strings are strings, System.Type arguments
    /// are AttributeType values, arrays are ImmutableArray of typed arguments.
    member a.Arguments = a.Value.FixedArguments |> Seq.map (fun x -> x.Value) |> Array.ofSeq
    member a.ArgumentCount = a.Value.FixedArguments.Length

let Attributes (md: MetadataReader) (handles: CustomAttributeHandleCollection) =
    handles |> Seq.map (fun h -> CustomAttr(md, md.GetCustomAttribute h)) |> Array.ofSeq

/// An assembly image read with System.Reflection.Metadata.
[<Sealed; AllowNullLiteral>]
type PEAssembly private (pe: PEReader) =
    let md = pe.GetMetadataReader()
    let def = md.GetAssemblyDefinition()
    let name = md.GetString def.Name
    let fullName =
        let v = def.Version
        let culture = md.GetString def.Culture
        let token =
            let key = md.GetBlobBytes def.PublicKey
            if key.Length = 0 then "null" else
            use sha1 = System.Security.Cryptography.SHA1.Create()
            let hash = sha1.ComputeHash key
            Array.init 8 (fun i -> hash.[hash.Length - 1 - i].ToString("x2")) |> String.concat ""
        sprintf "%s, Version=%O, Culture=%s, PublicKeyToken=%s" name v (if culture = "" then "neutral" else culture) token
    let mvid = md.GetGuid(md.GetModuleDefinition().Mvid)
    let attributes = lazy (Attributes md (def.GetCustomAttributes()))
    let resources =
        lazy (
            md.ManifestResources |> Seq.map (fun h ->
                let r = md.GetManifestResource h
                md.GetString r.Name, r
            ) |> Array.ofSeq
        )

    /// Reads an image, from a stream or a byte array, into memory.
    static member Read(stream: Stream) =
        PEAssembly(new PEReader(stream, PEStreamOptions.PrefetchEntireImage))

    static member Read(bytes: byte[]) =
        PEAssembly.Read(new MemoryStream(bytes, false) :> Stream)

    static member ReadFile(path: string) =
        use s = File.OpenRead path
        PEAssembly.Read(s :> Stream)

    /// Reads only the metadata of an assembly file: embedded resources are not available.
    static member ReadFileMetadata(path: string) =
        use s = File.OpenRead path
        PEAssembly(new PEReader(s, PEStreamOptions.PrefetchMetadata))

    member _.PEReader = pe
    member _.Reader = md
    member _.Name = name
    member _.FullName = fullName
    member _.ModuleId = mvid
    member _.Attributes : CustomAttr[] = attributes.Value

    /// Names of the manifest resources embedded in the assembly.
    member _.ResourceNames =
        resources.Value |> Array.choose (fun (n, r) -> if r.Implementation.IsNil then Some n else None)

    member _.IsPublicResource(n: string) =
        resources.Value |> Array.exists (fun (rn, r) ->
            rn = n && r.Implementation.IsNil && (r.Attributes &&& ManifestResourceAttributes.VisibilityMask) = ManifestResourceAttributes.Public)

    /// The contents of an embedded resource.
    member _.TryGetResource(n: string) =
        resources.Value |> Array.tryPick (fun (rn, r) ->
            if rn = n && r.Implementation.IsNil then
                let dir = pe.PEHeaders.CorHeader.ResourcesDirectory
                let block = pe.GetSectionData(dir.RelativeVirtualAddress)
                let mutable reader = block.GetReader(int r.Offset, block.Length - int r.Offset)
                let length = reader.ReadInt32()
                Some (reader.ReadBytes length)
            else None)

    member a.HasResource(n: string) =
        resources.Value |> Array.exists (fun (rn, r) -> rn = n && r.Implementation.IsNil)

/// The module version id (MVID) of an assembly file.
let ReadModuleId (path: string) =
    use s = File.OpenRead path
    use pe = new PEReader(s, PEStreamOptions.LeaveOpen)
    let md = pe.GetMetadataReader()
    md.GetGuid(md.GetModuleDefinition().Mvid)

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

/// Writes assemblies without code: the attributes-only assembly of Proxy projects, and
/// the placeholder assembly of bundle-only projects. Uses System.Reflection.Metadata,
/// strong name signing with a .snk key pair like the compilers do.
module WebSharper.Compiler.StubAssembly

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Security.Cryptography

/// Copies a method signature blob, mapping type references with `mapType`. Only the
/// types that can occur in custom attribute constructors are supported.
let private copySignature (reader: BlobReader) (mapType: EntityHandle -> EntityHandle option) =
    let mutable reader = reader
    let b = BlobBuilder()
    let mutable ok = true
    let rec copyType () =
        let et = reader.ReadByte()
        b.WriteByte et
        match et with
        | 0x11uy | 0x12uy -> // VALUETYPE, CLASS
            match mapType (reader.ReadTypeHandle()) with
            | Some h -> b.WriteCompressedInteger(CodedIndex.TypeDefOrRefOrSpec h)
            | None -> ok <- false
        | 0x1duy -> copyType () // SZARRAY
        | et when (et >= 0x01uy && et <= 0x0euy) || et = 0x18uy || et = 0x19uy || et = 0x1cuy -> ()
        | _ -> ok <- false
    let header = reader.ReadSignatureHeader()
    b.WriteByte header.RawValue
    let count = reader.ReadCompressedInteger()
    b.WriteCompressedInteger count
    copyType () // return type
    for _ in 1 .. count do
        if ok then copyType ()
    if ok then Some b else None

let private write (path: string) (build: MetadataBuilder -> BlobBuilder -> unit) (keyPair: option<PEWriter.KeyPair>) =
    let metadata = MetadataBuilder()
    let resources = BlobBuilder()
    build metadata resources
    let pe = PEWriter.Serialize (PEHeaderBuilder.CreateLibraryHeader()) metadata (BlobBuilder()) resources null keyPair None
    File.WriteAllBytes(path, pe)

let private addModule (metadata: MetadataBuilder) (moduleName: string) =
    let mvid = metadata.ReserveGuid()
    metadata.AddModule(0, metadata.GetOrAddString moduleName, mvid.Handle, GuidHandle(), GuidHandle()) |> ignore
    metadata.AddTypeDefinition(
        TypeAttributes(), StringHandle(), metadata.GetOrAddString "<Module>",
        EntityHandle(), MetadataTokens.FieldDefinitionHandle 1, MetadataTokens.MethodDefinitionHandle 1) |> ignore
    mvid

/// Writes an assembly with no code and no references, for projects that only produce JavaScript.
let WriteEmpty (path: string) (assemblyName: string) =
    let mutable mvid = Unchecked.defaultof<ReservedBlob<GuidHandle>>
    let mutable contentGuid = Guid.Empty
    write path (fun metadata _ ->
        metadata.AddAssembly(
            metadata.GetOrAddString assemblyName, Version(0, 0, 0, 0), StringHandle(), BlobHandle(),
            AssemblyFlags(), AssemblyHashAlgorithm.Sha1) |> ignore
        mvid <- addModule metadata (assemblyName + ".dll")
        // a stable module id from the name
        use sha = SHA256.Create()
        contentGuid <- Guid(Array.sub (sha.ComputeHash(Text.Encoding.UTF8.GetBytes assemblyName)) 0 16)
        BlobWriter(mvid.Content).WriteGuid(contentGuid)
    ) None

/// Writes an assembly with only the assembly attributes of an existing one that satisfy
/// `keepAttribute` (getting the attribute type namespace), over that assembly.
/// Signs it with the key pair of the .snk file, if given one with a private key.
let WriteAttributesOnly (path: string) (keepAttribute: string -> bool) (keyFile: option<string>) =
    let orig = File.ReadAllBytes path
    use pe = new PEReader(ImmutableArray.Create<byte>(orig))
    let md = pe.GetMetadataReader()
    let keyPair = keyFile |> Option.bind (fun k -> PEWriter.ReadKeyPair (File.ReadAllBytes k))
    let mutable mvid = Unchecked.defaultof<ReservedBlob<GuidHandle>>
    write path (fun metadata resources ->
        let str (h: StringHandle) = if h.IsNil then StringHandle() else metadata.GetOrAddString(md.GetString h)
        let blob (h: BlobHandle) = if h.IsNil then BlobHandle() else metadata.GetOrAddBlob(md.GetBlobBytes h)
        let asm = md.GetAssemblyDefinition()
        let asmHandle =
            metadata.AddAssembly(
                str asm.Name, asm.Version, str asm.Culture, blob asm.PublicKey, asm.Flags, asm.HashAlgorithm)
        let modDef = md.GetModuleDefinition()
        mvid <- addModule metadata (md.GetString modDef.Name)
        let asmRefs = Dictionary<AssemblyReferenceHandle, AssemblyReferenceHandle>()
        let importAsmRef (h: AssemblyReferenceHandle) =
            match asmRefs.TryGetValue h with
            | true, n -> n
            | _ ->
                let r = md.GetAssemblyReference h
                let n = metadata.AddAssemblyReference(str r.Name, r.Version, str r.Culture, blob r.PublicKeyOrToken, r.Flags, blob r.HashValue)
                asmRefs.[h] <- n
                n
        let typeRefs = Dictionary<TypeReferenceHandle, TypeReferenceHandle option>()
        let rec importTypeRef (h: TypeReferenceHandle) =
            match typeRefs.TryGetValue h with
            | true, n -> n
            | _ ->
                let r = md.GetTypeReference h
                let scope =
                    match r.ResolutionScope.Kind with
                    | HandleKind.AssemblyReference ->
                        let s : EntityHandle = importAsmRef (AssemblyReferenceHandle.op_Explicit r.ResolutionScope)
                        Some s
                    | HandleKind.TypeReference ->
                        importTypeRef (TypeReferenceHandle.op_Explicit r.ResolutionScope)
                        |> Option.map (fun t -> let s : EntityHandle = t in s)
                    | _ -> None
                let n =
                    scope |> Option.map (fun s -> metadata.AddTypeReference(s, str r.Namespace, str r.Name))
                typeRefs.[h] <- n
                n
        let mapType (h: EntityHandle) =
            match h.Kind with
            | HandleKind.TypeReference ->
                importTypeRef (TypeReferenceHandle.op_Explicit h) |> Option.map (fun t -> let s : EntityHandle = t in s)
            | _ -> None
        for ah in md.GetAssemblyDefinition().GetCustomAttributes() do
            let a = md.GetCustomAttribute ah
            if a.Constructor.Kind = HandleKind.MemberReference then
                let ctor = md.GetMemberReference(MemberReferenceHandle.op_Explicit a.Constructor)
                if ctor.Parent.Kind = HandleKind.TypeReference then
                    let parent = md.GetTypeReference(TypeReferenceHandle.op_Explicit ctor.Parent)
                    if keepAttribute (md.GetString parent.Namespace) then
                        match importTypeRef (TypeReferenceHandle.op_Explicit ctor.Parent), copySignature (md.GetBlobReader ctor.Signature) mapType with
                        | Some p, Some signature ->
                            let newCtor = metadata.AddMemberReference(p, str ctor.Name, metadata.GetOrAddBlob signature)
                            metadata.AddCustomAttribute(asmHandle, newCtor, blob a.Value) |> ignore
                        | _ -> ()
        // embedded resources are kept
        let resourceData =
            lazy pe.GetSectionData(pe.PEHeaders.CorHeader.ResourcesDirectory.RelativeVirtualAddress)
        for h in md.ManifestResources do
            let r = md.GetManifestResource h
            if r.Implementation.IsNil then
                let data = resourceData.Value
                let mutable reader = data.GetReader(int r.Offset, data.Length - int r.Offset)
                let bytes = reader.ReadBytes(reader.ReadInt32())
                resources.Align 8
                let offset = resources.Count
                resources.WriteInt32 bytes.Length
                resources.WriteBytes bytes
                metadata.AddManifestResource(r.Attributes, str r.Name, EntityHandle(), uint32 offset) |> ignore
        // module id from the original one, so it changes with the original
        let guidBytes = md.GetGuid(modDef.Mvid).ToByteArray()
        guidBytes.[0] <- guidBytes.[0] ^^^ 0x5Auy
        BlobWriter(mvid.Content).WriteGuid(Guid guidBytes)
    ) keyPair

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

/// An RSA key pair from a .snk file (a CAPI PRIVATEKEYBLOB), None for a public key only file.
let private readKeyPair (snk: byte[]) =
    // BLOBHEADER: type 0x07 is PRIVATEKEYBLOB, 0x06 PUBLICKEYBLOB
    if snk.Length < 20 || snk.[0] <> 0x07uy then None else
    let bitLen = BitConverter.ToInt32(snk, 12)
    let pubExp = BitConverter.ToUInt32(snk, 16)
    let mutable p = 20
    let take n =
        let a = Array.sub snk p n
        Array.Reverse a
        p <- p + n
        a
    let mod8, mod16 = bitLen / 8, bitLen / 16
    let modulus = take mod8
    let prime1 = take mod16
    let prime2 = take mod16
    let exponent1 = take mod16
    let exponent2 = take mod16
    let coefficient = take mod16
    let privateExponent = take mod8
    let exponent =
        BitConverter.GetBytes pubExp |> Array.rev |> Array.skipWhile (fun b -> b = 0uy)
    Some (
        RSAParameters(
            Modulus = modulus, Exponent = exponent, P = prime1, Q = prime2,
            DP = exponent1, DQ = exponent2, InverseQ = coefficient, D = privateExponent
        ),
        mod8
    )

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

let private hashBlobs (h: HashAlgorithm) (blobs: seq<Blob>) =
    for b in blobs do
        let s = b.GetBytes()
        h.TransformBlock(s.Array, s.Offset, s.Count, null, 0) |> ignore
    h.TransformFinalBlock([||], 0, 0) |> ignore
    h.Hash

let private write (path: string) (build: MetadataBuilder -> BlobBuilder -> unit) (keyPair: option<RSAParameters * int>) =
    let metadata = MetadataBuilder()
    let resources = BlobBuilder()
    build metadata resources
    let peBuilder =
        ManagedPEBuilder(
            PEHeaderBuilder.CreateLibraryHeader(),
            MetadataRootBuilder(metadata),
            BlobBuilder(),
            managedResources = (if resources.Count > 0 then resources else null),
            flags = (if keyPair.IsSome then CorFlags.ILOnly ||| CorFlags.StrongNameSigned else CorFlags.ILOnly),
            strongNameSignatureSize = (match keyPair with Some (_, size) -> size | None -> 0),
            deterministicIdProvider = (fun content ->
                use sha = SHA256.Create()
                BlobContentId.FromHash(ImmutableArray.Create<byte>(hashBlobs sha content, 0, 20)))
        )
    let pe = BlobBuilder()
    peBuilder.Serialize(pe) |> ignore
    match keyPair with
    | Some (key, _) ->
        peBuilder.Sign(pe, fun blobs ->
            use sha1 = SHA1.Create()
            let hash = hashBlobs sha1 blobs
            use rsa = RSA.Create()
            rsa.ImportParameters key
            let signature = rsa.SignHash(hash, HashAlgorithmName.SHA1, RSASignaturePadding.Pkcs1)
            Array.Reverse signature
            signature)
    | None -> ()
    use f = File.Create path
    pe.WriteContentTo f

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
    let keyPair = keyFile |> Option.bind (fun k -> readKeyPair (File.ReadAllBytes k))
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

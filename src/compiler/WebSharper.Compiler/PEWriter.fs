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

/// Writes PE images built with System.Reflection.Metadata: strong name signing with
/// a .snk key pair like the compilers do, deterministic module ids, Win32 resources.
module WebSharper.Compiler.PEWriter

open System
open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Security.Cryptography

/// An RSA key pair and its signature size.
type KeyPair =
    {
        Parameters : RSAParameters
        SignatureSize : int
        /// The public key blob, as in the Assembly table.
        PublicKey : byte[]
    }

/// The RSA key pair of a .snk file (a CAPI PRIVATEKEYBLOB), None for a public key only file.
let ReadKeyPair (snk: byte[]) =
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
    // PublicKeyBlob: signature and hash algorithm ids, size, then a PUBLICKEYBLOB
    // (BLOBHEADER, RSAPUBKEY with magic RSA1, little endian modulus)
    let publicKey =
        Array.concat [
            BitConverter.GetBytes 0x2400u // CALG_RSA_SIGN
            BitConverter.GetBytes 0x8004u // CALG_SHA1
            BitConverter.GetBytes (uint32 (20 + mod8))
            [| 0x06uy; 0x02uy; 0uy; 0uy |]
            BitConverter.GetBytes 0x2400u
            "RSA1"B
            Array.sub snk 12 8 // bit length, public exponent
            Array.sub snk 20 mod8
        ]
    Some {
        Parameters =
            RSAParameters(
                Modulus = modulus, Exponent = exponent, P = prime1, Q = prime2,
                DP = exponent1, DQ = exponent2, InverseQ = coefficient, D = privateExponent
            )
        SignatureSize = mod8
        PublicKey = publicKey
    }

/// The public key token of a public key blob.
let PublicKeyToken (publicKey: byte[]) =
    if publicKey.Length = 0 then [||] else
    use sha1 = SHA1.Create()
    let hash = sha1.ComputeHash publicKey
    Array.init 8 (fun i -> hash.[hash.Length - 1 - i])

let private hashBlobs (h: HashAlgorithm) (blobs: seq<Blob>) =
    for b in blobs do
        let s = b.GetBytes()
        h.TransformBlock(s.Array, s.Offset, s.Count, null, 0) |> ignore
    h.TransformFinalBlock([||], 0, 0) |> ignore
    h.Hash

/// The Win32 resources (.rsrc section) of an existing image, relocated to where they are
/// written: the data entries of the resource directory tree hold RVAs.
type CopiedWin32Resources(data: byte[], originalRva: int) =
    inherit ResourceSectionBuilder()

    override this.Serialize(builder: BlobBuilder, location: SectionLocation) =
        let data = Array.copy data
        let delta = location.RelativeVirtualAddress - originalRva
        let rec directory (offset: int) =
            let named = int (BitConverter.ToUInt16(data, offset + 12))
            let ids = int (BitConverter.ToUInt16(data, offset + 14))
            for i in 0 .. named + ids - 1 do
                let entry = offset + 16 + i * 8
                let target = BitConverter.ToUInt32(data, entry + 4)
                if target &&& 0x80000000u <> 0u then
                    directory (int (target &&& 0x7FFFFFFFu))
                else
                    let dataEntry = int target
                    let rva = BitConverter.ToInt32(data, dataEntry) + delta
                    Buffer.BlockCopy(BitConverter.GetBytes rva, 0, data, dataEntry, 4)
        if data.Length > 0 then directory 0
        builder.WriteBytes data

    /// The Win32 resources of an image, if it has any.
    static member TryRead(pe: PEReader) =
        let dir = pe.PEHeaders.PEHeader.ResourceTableDirectory
        if dir.Size = 0 then None else
        let content = pe.GetSectionData(dir.RelativeVirtualAddress).GetContent(0, dir.Size)
        Some (CopiedWin32Resources(Seq.toArray content, dir.RelativeVirtualAddress))

/// Serializes and signs a PE image. When `mvid` is given, the module id is written there
/// from the hash of the contents, so that the same contents give the same image.
let Serialize (header: PEHeaderBuilder) (metadata: MetadataBuilder) (il: BlobBuilder)
        (resources: BlobBuilder) (win32Resources: ResourceSectionBuilder)
        (keyPair: option<KeyPair>) (mvid: option<ReservedBlob<GuidHandle>>) =
    let peBuilder =
        ManagedPEBuilder(
            header,
            MetadataRootBuilder(metadata),
            il,
            managedResources = (if resources.Count > 0 then resources else null),
            nativeResources = win32Resources,
            flags = (if keyPair.IsSome then CorFlags.ILOnly ||| CorFlags.StrongNameSigned else CorFlags.ILOnly),
            strongNameSignatureSize = (match keyPair with Some k -> k.SignatureSize | None -> 0),
            deterministicIdProvider = (fun content ->
                use sha = SHA256.Create()
                BlobContentId.FromHash(ImmutableArray.Create<byte>(hashBlobs sha content, 0, 20)))
        )
    let pe = BlobBuilder()
    let contentId = peBuilder.Serialize(pe)
    match mvid with
    | Some mvid -> BlobWriter(mvid.Content).WriteGuid(contentId.Guid)
    | None -> ()
    match keyPair with
    | Some key ->
        peBuilder.Sign(pe, fun blobs ->
            use sha1 = SHA1.Create()
            let hash = hashBlobs sha1 blobs
            use rsa = RSA.Create()
            rsa.ImportParameters key.Parameters
            let signature = rsa.SignHash(hash, HashAlgorithmName.SHA1, RSASignaturePadding.Pkcs1)
            Array.Reverse signature
            signature)
    | None -> ()
    pe.ToArray()

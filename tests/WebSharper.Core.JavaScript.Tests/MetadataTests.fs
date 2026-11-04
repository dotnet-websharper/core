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
module WebSharper.Core.JavaScript.Test.MetadataTests

open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open NUnit.Framework
open WebSharper.Core
open WebSharper.Core.AST
module M = WebSharper.Core.Metadata

let private readResource (path: string) (name: string) =
    use pe = new PEReader(File.OpenRead path)
    let md = pe.GetMetadataReader()
    md.ManifestResources
    |> Seq.tryPick (fun h ->
        let r = md.GetManifestResource h
        if md.GetString r.Name = name && r.Implementation.IsNil then
            let rva = pe.PEHeaders.CorHeader.ResourcesDirectory.RelativeVirtualAddress
            let block = pe.GetSectionData(rva)
            let mutable reader = block.GetReader(int r.Offset, block.Length - int r.Offset)
            let len = reader.ReadInt32()
            Some (reader.ReadBytes len)
        else None)

/// The WebSharper assemblies of this build, found from the test output folder.
let private builtAssemblies () =
    let rec findRoot (d: DirectoryInfo) =
        if isNull d then None
        elif File.Exists(Path.Combine(d.FullName, "WebSharper.sln")) then Some d.FullName
        else findRoot d.Parent
    match findRoot (DirectoryInfo(TestContext.CurrentContext.TestDirectory)) with
    | None -> [||]
    | Some root ->
        let release = Path.Combine(root, "build", "Release")
        if Directory.Exists release then
            Directory.GetFiles(release, "WebSharper*.dll", SearchOption.AllDirectories)
            |> Array.filter (fun p -> not (p.Contains "deploy") && not (p.Contains (string Path.DirectorySeparatorChar + "publish")))
            |> Array.distinctBy Path.GetFileName
        else [||]

let private roundTrip (meta: M.Info) =
    let e1 = MetadataIO.EncodeToBytes meta
    let m2 = MetadataIO.DecodeBytes e1
    let e2 = MetadataIO.EncodeToBytes m2
    e1, m2, e2

let private withCompression c f =
    let prev = MetadataIO.DefaultCompression
    MetadataIO.DefaultCompression <- c
    try f () finally MetadataIO.DefaultCompression <- prev

let private td n = TypeDefinition { Assembly = "Test"; FullName = n }

[<Test>]
let ``Metadata round trip of built WebSharper assemblies`` () =
    let mutable checkedCount = 0
    for path in builtAssemblies () do
        for res in [ WebSharper.Constants.EmbeddedResources.EMBEDDED_METADATA; WebSharper.Constants.EmbeddedResources.EMBEDDED_RUNTIME_METADATA ] do
            match readResource path res with
            | Some bytes ->
                let meta = MetadataIO.DecodeBytes bytes
                for c in [ MetadataIO.Compression.NoCompression; MetadataIO.Compression.Deflate ] do
                    withCompression c <| fun () ->
                        let e1, m2, e2 = roundTrip meta
                        Assert.AreEqual(e1, e2, sprintf "%s %s re-encodes differently (%A)" path res c)
                        Assert.AreEqual(meta.Classes.Count, m2.Classes.Count)
                        Assert.AreEqual(meta.Dependencies.Nodes, m2.Dependencies.Nodes)
                checkedCount <- checkedCount + 1
            | None -> ()
    if checkedCount = 0 then
        Assert.Ignore "No built WebSharper assemblies with metadata found"

[<Test>]
let ``Metadata string table holds more than 65536 strings`` () =
    let n = 70000
    let meta =
        { M.Info.Empty with
            ResourceHashes = dict [ for i in 0 .. n - 1 -> sprintf "res%d" i, i ]
        }
    let _, m2, _ = roundTrip meta
    Assert.AreEqual(n, m2.ResourceHashes.Count)
    for i in [ 0; 1; 65535; 65536; 65537; n - 1 ] do
        Assert.AreEqual(i, m2.ResourceHashes.[sprintf "res%d" i])

[<Test>]
let ``Metadata keeps Ids, Hashed values and literals`` () =
    let x = Id.New("x", mut = false, typ = ConcreteType (NonGeneric (td "T1")))
    let y = Id.New("y")
    let m = Method { MethodName = "M"; Parameters = [ ConcreteType (NonGeneric (td "T1")); VoidType ]; ReturnType = TypeParameter 0; Generics = 1 }
    let body =
        Function([ x; y ], None, None,
            Block [
                VarDeclaration(y, Value (String "é\u0000\U0001F600"))
                Return (Sequential [ Var x; Var y; Value (Double -0.5); Value (Decimal 1.25M); Value (Int64 System.Int64.MinValue); Value (Char 'c'); Value (ByteArray [| 0uy; 255uy |]) ])
            ])
    let cls =
        { M.ClassInfo.None with
            Methods = dict [ m, { CompiledForm = M.Inline (true, false); Optimizations = M.Optimizations.None; Generics = []; Expression = body } ]
            IsStub = false
        }
    let meta =
        { M.Info.Empty with
            Classes = dict [ td "C", (Address.Global(), M.NotCustomType, Some cls) ]
            SiteletDefinition = Some (td "C")
        }
    let _, m2, _ = roundTrip meta
    let _, _, c2 = m2.Classes.[td "C"]
    let e2 = c2.Value.Methods.[m].Expression
    Assert.AreEqual(string body, string e2)
    match e2 with
    | Function ([ x2; y2 ], _, _, Block [ VarDeclaration (y3, _); Return (Sequential (Var x3 :: Var y4 :: _)) ]) ->
        Assert.AreEqual(x, x2)
        Assert.AreEqual(x.VarType, x2.VarType)
        Assert.AreEqual(Some "x", x2.Name)
        Assert.IsFalse(x2.IsMutable)
        // Ids written once per block are shared after reading
        Assert.IsTrue(obj.ReferenceEquals(x2, x3))
        Assert.IsTrue(obj.ReferenceEquals(y2, y3) && obj.ReferenceEquals(y2, y4))
    | e -> Assert.Fail(sprintf "Unexpected shape: %O" e)
    // Hashed values are shared after reading
    let td2 = m2.SiteletDefinition.Value
    let k2 = m2.Classes.Keys |> Seq.head
    Assert.IsTrue(obj.ReferenceEquals(td2.Value, k2.Value))

[<Test>]
let ``Metadata rejects other formats`` () =
    Assert.Throws<MetadataIO.MetadataFormatException>(fun () ->
        MetadataIO.DecodeBytes [| 0x1fuy; 0x8buy; 8uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |] |> ignore)
    |> ignore

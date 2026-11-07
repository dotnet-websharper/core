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
            let stored =
                if res = WebSharper.Constants.EmbeddedResources.EMBEDDED_METADATA then
                    WebSharper.Compiler.Sidecar.TryReadFor path |> Option.bind (fun sc -> sc.TryGet res)
                else readResource path res
            match stored with
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

let private sectionsTestMeta () =
    let tId = Id.New("t")
    // a type with an Id inside: the Hashed method entry must not depend on the Id scope of the block it is first seen in
    let idType = TSType (TSType.Typeof tId)
    let mInline = Method { MethodName = "I"; Parameters = [ idType ]; ReturnType = VoidType; Generics = 0 }
    let mOther = Method { MethodName = "O"; Parameters = []; ReturnType = idType; Generics = 0 }
    let inlineBody = Sequential [ Var tId; Value (Int 1) ]
    // N is first seen inside the block, after t is already written there
    let mNew = Method { MethodName = "N"; Parameters = [ idType ]; ReturnType = VoidType; Generics = 0 }
    let otherBody = Function([ tId ], None, None, Return (Call(None, NonGeneric (td "C"), NonGeneric mNew, [ Var tId ])))
    let cls =
        { M.ClassInfo.None with
            Methods =
                dict [
                    mInline, { CompiledForm = M.Inline (true, false); Optimizations = M.Optimizations.None; Generics = []; Expression = inlineBody }
                    mOther, { CompiledForm = M.Static ("O", false, MemberKind.Simple); Optimizations = M.Optimizations.None; Generics = []; Expression = otherBody }
                ]
            StaticConstructor = Some (ExprStatement (Value (Int 2)))
            IsStub = false
        }
    let meta =
        { M.Info.Empty with
            Classes = dict [ td "C", (Address.Global(), M.NotCustomType, Some cls) ]
            Dependencies = { M.GraphData.Empty with Nodes = [| M.TypeNode (td "C") |]; Edges = [| [||] |] }
        }
    meta, mInline, mOther, inlineBody, otherBody

[<Test>]
let ``Metadata sections can be skipped`` () =
    let meta, mInline, mOther, inlineBody, otherBody = sectionsTestMeta ()
    let bytes = MetadataIO.EncodeToBytes meta
    let classOf (m: M.Info) = let _, _, c = m.Classes.[td "C"] in c.Value
    // full read
    let full = MetadataIO.DecodeBytesWith MetadataIO.ReadOptions.Full bytes 0 bytes.Length
    Assert.AreEqual(string inlineBody, string (classOf full).Methods.[mInline].Expression)
    Assert.AreEqual(string otherBody, string (classOf full).Methods.[mOther].Expression)
    Assert.AreEqual(Some (ExprStatement (Value (Int 2))), (classOf full).StaticConstructor)
    Assert.AreEqual(1, full.Dependencies.Nodes.Length)
    // library read: no non-inline code, no graph, static constructor kept as Empty
    let lib = MetadataIO.DecodeBytesWith MetadataIO.ReadOptions.Library bytes 0 bytes.Length
    Assert.AreEqual(string inlineBody, string (classOf lib).Methods.[mInline].Expression)
    Assert.AreEqual(Undefined, (classOf lib).Methods.[mOther].Expression)
    Assert.AreEqual(Some Empty, (classOf lib).StaticConstructor)
    Assert.AreEqual(0, lib.Dependencies.Nodes.Length)
    Assert.AreEqual(2, (classOf lib).Methods.Count)
    // methods with Ids in their types are read the same way in both
    Assert.AreEqual(string mOther, string ((classOf lib).Methods.Keys |> Seq.find (fun m -> m.Value.MethodName = "O")))
    let sizes = MetadataIO.SectionSizes bytes |> Array.map (fun (n, _, _) -> n)
    Assert.Contains("inline expressions", sizes)

[<Test>]
let ``Sidecar files round trip and are not rewritten when unchanged`` () =
    let dir = Path.Combine(Path.GetTempPath(), "ws-sidecar-test-" + string (System.Guid.NewGuid()))
    Directory.CreateDirectory dir |> ignore
    try
        let dll = Path.Combine(dir, "Lib.dll")
        let path = WebSharper.Compiler.Sidecar.PathFor dll
        Assert.AreEqual(Path.Combine(dir, "Lib.websharper"), path)
        let mvid = System.Guid.NewGuid()
        let js = System.Text.Encoding.UTF8.GetBytes(String.replicate 1000 "export function f() {}\n")
        let meta = MetadataIO.EncodeToBytes M.Info.Empty
        let entries = [ WebSharper.Constants.EmbeddedResources.EMBEDDED_METADATA, meta; "Lib.js", js; "empty.txt", [||] ]
        WebSharper.Compiler.Sidecar.Write(path, mvid, entries)
        let sc = WebSharper.Compiler.Sidecar.Read path
        Assert.AreEqual(mvid, sc.ModuleId)
        Assert.AreEqual([| WebSharper.Constants.EmbeddedResources.EMBEDDED_METADATA; "Lib.js"; "empty.txt" |], sc.Names)
        Assert.AreEqual(Some js, sc.TryGet "Lib.js")
        Assert.AreEqual(Some meta, sc.TryGet WebSharper.Constants.EmbeddedResources.EMBEDDED_METADATA)
        Assert.AreEqual(Some ([||] : byte[]), sc.TryGet "empty.txt")
        Assert.AreEqual(None, sc.TryGet "missing.js")
        Assert.Less(FileInfo(path).Length, int64 js.Length)
        let old = System.DateTime(2020, 1, 1)
        File.SetLastWriteTimeUtc(path, old)
        WebSharper.Compiler.Sidecar.Write(path, mvid, entries)
        Assert.AreEqual(old, File.GetLastWriteTimeUtc path)
        WebSharper.Compiler.Sidecar.Write(path, System.Guid.NewGuid(), entries)
        Assert.AreNotEqual(old, File.GetLastWriteTimeUtc path)
    finally
        Directory.Delete(dir, true)

let private meth n = Method { MethodName = n; Parameters = []; ReturnType = VoidType; Generics = 0 }

[<Test>]
let ``Runtime graph keeps only the nodes leading to resources`` () =
    let g = DependencyGraph.Graph.New()
    let f = M.MethodNode (td "A", meth "f")
    let f2 = M.MethodNode (td "A", meth "g")
    let h = M.MethodNode (td "A", meth "h")
    let res = M.ResourceNode (td "R", None)
    // f uses type T and h (no resources), g calls the abstract B.m, T overrides B.m with code needing R
    g.AddEdge(f, M.TypeNode (td "T"))
    g.AddEdge(f, h)
    g.AddEdge(f2, M.AbstractMethodNode (td "B", meth "m"))
    g.AddOverride(td "T", td "B", meth "m")
    g.AddEdge(M.MethodNode (td "T", meth "m"), res)
    let resources (gr: DependencyGraph.Graph) nodes =
        gr.GetDependencies nodes |> List.choose (function M.ResourceNode _ as r -> Some (string r) | _ -> None) |> List.sort
    let trimmed = DependencyGraph.Graph.FromData(g.GetResourceTrimmedData [ f; f2 ])
    Assert.IsFalse(trimmed.Lookup.ContainsKey h)
    for roots in [ [ f ]; [ f2 ]; [ f; f2 ] ] do
        Assert.AreEqual(resources g roots, resources trimmed roots, sprintf "%A" roots)
    Assert.AreEqual([ string res ], resources trimmed [ f; f2 ])

[<Test>]
let ``Runtime class info keeps addresses, static methods and anonymous record shapes`` () =
    let info form : M.CompiledMethodInfo = { CompiledForm = form; Optimizations = M.Optimizations.None; Generics = []; Expression = Value (Int 1) }
    let cls =
        { M.ClassInfo.None with
            Methods =
                dict [
                    meth "s", info (M.Static ("s", false, MemberKind.Simple))
                    meth "f", info (M.Func ("f", true))
                    meth "i", info (M.Instance ("i", MemberKind.Simple, M.Modifier.None))
                    meth "n", info (M.Inline (true, false))
                ]
            Fields = dict [ "x", ({ CompiledForm = M.InstanceField "x"; ReadOnly = false; Type = VoidType; Order = 0 } : M.CompiledFieldInfo) ]
            IsStub = false
        }
    let addr = Address.Global()
    let meta =
        { M.Info.Empty with
            Classes =
                dict [
                    td "C", (addr, M.FSharpRecordInfo [], Some cls)
                    td "I", (addr, M.NotCustomType, Some { M.ClassInfo.None with Methods = dict [ meth "i", info (M.Instance ("i", MemberKind.Simple, M.Modifier.None)) ] })
                    td "Anon", (addr, M.FSharpAnonRecordInfo [ "a" ], None)
                ]
        }
    let r = meta.DiscardNotRuntimeClassInfo()
    Assert.AreEqual(2, r.Classes.Count)
    let a, ct, c = r.Classes.[td "C"]
    Assert.AreEqual(addr, a)
    Assert.AreEqual(M.NotCustomType, ct)
    Assert.AreEqual([ "f"; "s" ], c.Value.Methods.Keys |> Seq.map (fun m -> m.Value.MethodName) |> Seq.sort |> List.ofSeq)
    Assert.AreEqual(Undefined, c.Value.Methods.[meth "s"].Expression)
    Assert.AreEqual(0, c.Value.Fields.Count)
    match r.Classes.[td "Anon"] with
    | _, M.FSharpAnonRecordInfo [ "a" ], None -> ()
    | x -> Assert.Fail(sprintf "Unexpected: %A" x)

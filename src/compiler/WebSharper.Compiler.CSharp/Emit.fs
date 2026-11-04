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

/// Emits a C# assembly with extra manifest resources, the way csc emits it from the same
/// command line: used to embed runtime metadata into website, bundle and offline projects
/// without rewriting the assembly csc wrote.
module WebSharper.Compiler.CSharp.Emit

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Diagnostics
open Microsoft.CodeAnalysis.Text

/// Parses a source file like csc: keeps the encoding and the checksum for the PDB,
/// and allows embedding it.
let ParseSourceFile (args: CSharpCommandLineArguments) (path: string) =
    use s = File.OpenRead path
    let text = SourceText.From(s, args.Encoding, args.ChecksumAlgorithm, throwIfBinaryDetected = false, canBeEmbedded = true)
    CSharpSyntaxTree.ParseText(text, args.ParseOptions, path)

type private AnalyzerLoader() =
    interface IAnalyzerAssemblyLoader with
        member _.AddDependencyLocation(_) = ()
        member _.LoadFromPath(path) = Assembly.LoadFrom path

type private DictOptions(values: ImmutableDictionary<string, string>) =
    inherit AnalyzerConfigOptions()
    override _.TryGetValue(key, value) = values.TryGetValue(key, &value)

type private OptionsProvider(set: AnalyzerConfigSet option) =
    inherit AnalyzerConfigOptionsProvider()
    let empty = DictOptions(ImmutableDictionary.Create(AnalyzerConfigOptions.KeyComparer))
    let forPath (path: string) =
        match set with
        | Some s when not (String.IsNullOrEmpty path) -> DictOptions(s.GetOptionsForSourcePath(path).AnalyzerOptions) :> AnalyzerConfigOptions
        | _ -> empty :> AnalyzerConfigOptions
    let globalOptions =
        match set with
        | Some s -> DictOptions(s.GlobalConfigOptions.AnalyzerOptions) :> AnalyzerConfigOptions
        | None -> empty :> AnalyzerConfigOptions
    override _.GlobalOptions = globalOptions
    override _.GetOptions(tree: SyntaxTree) = forPath tree.FilePath
    override _.GetOptions(file: AdditionalText) = forPath file.Path

type private FileText(path: string) =
    inherit AdditionalText()
    let text = lazy (use s = File.OpenRead path in SourceText.From(s))
    override _.Path = path
    override _.GetText(_) = text.Value

let private errorsOf (diags: seq<Diagnostic>) =
    diags
    |> Seq.filter (fun d -> d.Severity = DiagnosticSeverity.Error)
    |> Seq.map (fun d -> d.ToString())
    |> List.ofSeq

/// Runs the source generators of the command line on a compilation.
let private runGenerators (args: CSharpCommandLineArguments) (compilation: CSharpCompilation) =
    if args.AnalyzerReferences.IsEmpty then compilation else
    let loadErrors = ResizeArray()
    let refs = args.ResolveAnalyzerReferences(AnalyzerLoader())
    for r in refs do
        match r with
        | :? AnalyzerFileReference as f ->
            f.AnalyzerLoadFailed.Add(fun e ->
                if e.ErrorCode <> AnalyzerLoadFailureEventArgs.FailureErrorCode.NoAnalyzers then
                    loadErrors.Add(sprintf "%s: %s" f.FullPath e.Message))
        | _ -> ()
    let generators =
        refs |> Seq.collect (fun r -> r.GetGenerators(LanguageNames.CSharp)) |> ImmutableArray.CreateRange
    if loadErrors.Count > 0 then
        failwithf "Failed to load source generators to emit the assembly: %s" (String.concat "; " loadErrors)
    if generators.IsEmpty then compilation else
    let configSet =
        if args.AnalyzerConfigPaths.IsEmpty then None else
        let configs =
            args.AnalyzerConfigPaths |> Seq.map (fun p -> AnalyzerConfig.Parse(File.ReadAllText p, p)) |> ImmutableArray.CreateRange
        let mutable diags = ImmutableArray<Diagnostic>.Empty
        Some (AnalyzerConfigSet.Create(configs, &diags))
    let additionalTexts =
        args.AdditionalFiles |> Seq.map (fun f -> FileText(f.Path) :> AdditionalText) |> ImmutableArray.CreateRange
    let driver =
        CSharpGeneratorDriver.Create(generators, additionalTexts, args.ParseOptions, OptionsProvider(configSet))
    let mutable updated = Unchecked.defaultof<Compilation>
    let mutable diags = ImmutableArray<Diagnostic>.Empty
    driver.RunGeneratorsAndUpdateCompilation(compilation, &updated, &diags) |> ignore
    match errorsOf diags with
    | [] -> updated :?> CSharpCompilation
    | errors -> failwithf "Source generators failed: %s" (String.concat "; " errors)

/// Compilation options for a command line as csc sets them up for emitting: signing (with the
/// key file of the command line, or `keyFile`, the key file MSBuild passes to WebSharper),
/// path mapping and XML references. Used for the compilation WebSharper translates, so it can
/// emit it without binding it again.
let CompilationOptions (args: CSharpCommandLineArguments) (keyFile: string option) =
    let options = args.CompilationOptions
    let options =
        match keyFile with
        | Some k when String.IsNullOrEmpty options.CryptoKeyFile && String.IsNullOrEmpty options.CryptoKeyContainer ->
            options.WithCryptoKeyFile(Path.GetFullPath(Path.Combine(args.BaseDirectory, k)))
        | _ -> options
    options
        .WithStrongNameProvider(DesktopStrongNameProvider(args.KeyFileSearchPaths, null))
        .WithSourceReferenceResolver(SourceFileResolver(ImmutableArray<string>.Empty, args.BaseDirectory, args.PathMap))
        .WithXmlReferenceResolver(XmlFileResolver(args.BaseDirectory))
        .WithAssemblyIdentityComparer(DesktopAssemblyIdentityComparer.Default)

/// Type, method and field definition counts of an assembly image.
let private definitionCounts (pe: byte[]) =
    use r = new System.Reflection.PortableExecutable.PEReader(ImmutableArray.Create<byte>(pe))
    let md = System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader(r)
    md.TypeDefinitions.Count, md.MethodDefinitions.Count, md.FieldDefinitions.Count

/// Emits the assembly (and its PDB) of a compilation (created with CompilationOptions) to the
/// paths of the command line, with extra manifest resources. The XML documentation and the
/// reference assembly csc wrote are kept: they do not change.
/// Resource contents are only asked for when writing the assembly, after compiling the code,
/// so emitting can start before they are ready.
/// Source generators run only if the assembly emitted without them has other definition counts
/// than the one csc wrote. Returns if they ran.
let EmitWithResources (args: CSharpCommandLineArguments) (compilation: CSharpCompilation) (assemblyPath: string) (resources: seq<string * (unit -> byte[])>) =
    let cscCounts = definitionCounts (File.ReadAllBytes assemblyPath)
    let manifestResources =
        [
            yield! args.ManifestResources
            for name, getBytes in resources ->
                ResourceDescription(name, (fun () -> new MemoryStream(getBytes ()) :> Stream), true)
        ]
    let emitPdbFile = args.EmitPdbFile
    let pdbPath =
        if not (String.IsNullOrEmpty args.PdbPath) then args.PdbPath
        elif not (String.IsNullOrEmpty args.OutputDirectory || String.IsNullOrEmpty args.OutputFileName) then
            // as csc: next to the output, with the full path in the assembly
            Path.Combine(args.OutputDirectory, Path.ChangeExtension(args.OutputFileName, ".pdb"))
        else Path.GetFullPath(Path.ChangeExtension(assemblyPath, ".pdb"))
    let emitOptions =
        if emitPdbFile then args.EmitOptions.WithPdbFilePath(pdbPath) else args.EmitOptions
    let embeddedTexts =
        if args.EmbeddedFiles.IsEmpty then null else
        let trees = compilation.SyntaxTrees |> Seq.map (fun t -> t.FilePath, t) |> dict
        args.EmbeddedFiles |> Seq.map (fun f ->
            match trees.TryGetValue f.Path with
            | true, t -> EmbeddedText.FromSource(f.Path, t.GetText())
            | _ ->
                use s = File.OpenRead f.Path
                EmbeddedText.FromStream(f.Path, s, args.ChecksumAlgorithm)
        )
        |> Array.ofSeq :> seq<_>
    let openOrNull (path: string) =
        if String.IsNullOrEmpty path then null else File.OpenRead path :> Stream
    use win32Resources =
        if not (String.IsNullOrEmpty args.Win32ResourceFile) then
            File.OpenRead args.Win32ResourceFile :> Stream
        else
            use manifest = openOrNull args.Win32Manifest
            use icon = openOrNull args.Win32Icon
            compilation.CreateDefaultWin32Resources(true, args.NoWin32Manifest, manifest, icon)
    let win32Bytes =
        use m = new MemoryStream()
        win32Resources.CopyTo m
        m.ToArray()
    let sourceLinkBytes = if String.IsNullOrEmpty args.SourceLink then None else Some (File.ReadAllBytes args.SourceLink)
    let emit (compilation: CSharpCompilation) =
        use peStream = new MemoryStream()
        use pdbStream = if emitPdbFile then new MemoryStream() else null
        use win32 = new MemoryStream(win32Bytes)
        use sourceLink = match sourceLinkBytes with Some b -> new MemoryStream(b) | None -> null
        let result =
            compilation.Emit(
                peStream,
                pdbStream = pdbStream,
                win32Resources = win32,
                manifestResources = manifestResources,
                options = emitOptions,
                sourceLinkStream = sourceLink,
                embeddedTexts = embeddedTexts
            )
        if not result.Success then
            failwithf "Emitting the assembly failed: %s" (String.concat "; " (errorsOf result.Diagnostics))
        peStream.ToArray(), (if emitPdbFile then pdbStream.ToArray() else null)
    let pe, pdb, ranGenerators =
        let pe, pdb = emit compilation
        if definitionCounts pe = cscCounts || args.AnalyzerReferences.IsEmpty then pe, pdb, false else
        let pe, pdb = emit (runGenerators args compilation)
        pe, pdb, true
    File.WriteAllBytes(assemblyPath, pe)
    if emitPdbFile then
        File.WriteAllBytes(pdbPath, pdb)
    ranGenerators

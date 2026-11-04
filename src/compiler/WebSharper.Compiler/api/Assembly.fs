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

namespace WebSharper.Compiler

open WebSharper.Constants

module CT = WebSharper.Core.ContentTypes
type O = WebSharper.Core.JavaScript.Output

[<AutoOpen>]
module AssemblyUtility =

    let ReadStream (s: Stream) =
        use m = new MemoryStream()
        s.CopyTo(m)
        m.ToArray()

    let ReadResource name (def: Mono.Cecil.AssemblyDefinition) =
        def.MainModule.Resources
        |> Seq.tryPick (function
            | :? Mono.Cecil.EmbeddedResource as r when r.Name = name ->
                use reader = new StreamReader(r.GetResourceStream())
                reader.ReadToEnd() |> Some
            | _ -> None)

    let ReadResourceBytes name (def: Mono.Cecil.AssemblyDefinition) =
        let longName = def.Name.Name + "." + name
        def.MainModule.Resources
        |> Seq.tryPick (function
            | :? Mono.Cecil.EmbeddedResource as r when r.Name = name || r.Name = longName ->
                use rs = r.GetResourceStream()
                Some (ReadStream rs, r.Name)
            | _ -> None)

    let (|StringArg|_|) (attr: Mono.Cecil.CustomAttributeArgument) =
        if attr.Type.FullName = "System.String" then
            Some (attr.Value :?> string)
        else
            None

    let HasWebSharperMetadata (a: Mono.Cecil.AssemblyDefinition) =
        let key = EMBEDDED_METADATA
        a.MainModule.Resources
        |> Seq.exists (function
            | :? Mono.Cecil.EmbeddedResource as r when r.Name = key -> true
            | _ -> false)

    let IsWebSharperAssembly (a: Mono.Cecil.AssemblyDefinition) =
        match a.Name.Name with
        | "WebSharper.Core.JavaScript" -> true
        | n when n.Contains("WebSharper") -> true
        | _ -> HasWebSharperMetadata a

    let ParseWebResourcesUnchecked (def: Mono.Cecil.AssemblyDefinition) =
        def.CustomAttributes
        |> Seq.choose (fun attr ->
            if Utility.IsWebResourceAttribute attr.AttributeType.FullName then
                match Seq.toList attr.ConstructorArguments with
                | [StringArg resourceName; StringArg contentType] ->
                    ReadResourceBytes resourceName def
                    |> Option.map (fun (c, r) ->
                        EmbeddedFile.Create(string def.FullName, resourceName, c, CT.Parse contentType, r))
                | _ -> None
            else None)
    
    let ParseWebResources (def: Mono.Cecil.AssemblyDefinition) =
        if IsWebSharperAssembly def then
            ParseWebResourcesUnchecked def
        else Seq.empty

    let ReadAllScriptResources (def: Mono.Cecil.AssemblyDefinition) filter =
        let byAttr = ParseWebResourcesUnchecked def |> Array.ofSeq
        let byAttrNames = byAttr |> Seq.map (fun r -> r.EmbeddedFileName) |> System.Collections.Generic.HashSet
        def.MainModule.Resources
        |> Seq.choose (function
            | :? Mono.Cecil.EmbeddedResource as r when filter r.Name && not (byAttrNames.Contains r.Name) ->
                use s = r.GetResourceStream()
                let c = ReadStream s
                Some (EmbeddedFile.Create(string def.FullName, r.Name, c, CT.Text.JavaScript))
            | _ -> None)
        |> Seq.append byAttr
        |> Array.ofSeq

    let ParseAllScriptResources (def: Mono.Cecil.AssemblyDefinition) (t: O) =
        if IsWebSharperAssembly def then
            match t with 
            | O.JavaScript -> ReadAllScriptResources def (fun n -> n.EndsWith(".js") || n.EndsWith(".mjs") || n.EndsWith(".cjs") || n.EndsWith(".jsx"))
            | O.TypeScript -> ReadAllScriptResources def (fun n -> (n.EndsWith(".ts") && not (n.EndsWith(".d.ts")) || n.EndsWith(".tsx")))
            | O.TypeScriptDeclaration -> ReadAllScriptResources def (fun n -> n.EndsWith(".d.ts"))
        else Array.empty

    let ParseAllMapResources (def: Mono.Cecil.AssemblyDefinition) (t: O) =
        if IsWebSharperAssembly def then
            match t with 
            | O.JavaScript -> ReadAllScriptResources def (fun n -> n.EndsWith(".js.map") || n.EndsWith(".mjs.map") || n.EndsWith(".cjs.map") || n.EndsWith(".jsx.map"))
            | O.TypeScript -> ReadAllScriptResources def (fun n -> (n.EndsWith(".ts.map") && not (n.EndsWith(".d.ts.map")) || n.EndsWith(".tsx.map")))
            | O.TypeScriptDeclaration -> [||]
        else Array.empty

    let ParseAllSources (def: Mono.Cecil.AssemblyDefinition) =
        if IsWebSharperAssembly def then
            ReadAllScriptResources def (fun n -> n.StartsWith(EMBEDDED_SOURCES))
        else Array.empty

    let IsScriptFile (t: O) (n: string) =
        match t with
        | O.JavaScript -> n.EndsWith(".js") || n.EndsWith(".mjs") || n.EndsWith(".cjs") || n.EndsWith(".jsx")
        | O.TypeScript -> (n.EndsWith(".ts") && not (n.EndsWith(".d.ts"))) || n.EndsWith(".tsx")
        | O.TypeScriptDeclaration -> n.EndsWith(".d.ts")

    let IsMapFile (t: O) (n: string) =
        match t with
        | O.JavaScript -> n.EndsWith(".js.map") || n.EndsWith(".mjs.map") || n.EndsWith(".cjs.map") || n.EndsWith(".jsx.map")
        | O.TypeScript -> (n.EndsWith(".ts.map") && not (n.EndsWith(".d.ts.map"))) || n.EndsWith(".tsx.map")
        | O.TypeScriptDeclaration -> false

    /// Sidecar entries holding generated text files with a name filter.
    let SidecarFiles (def: Mono.Cecil.AssemblyDefinition) (sc: option<Sidecar>) (filter: string -> bool) contentType =
        match sc with
        | None -> [||]
        | Some sc ->
            sc.Names |> Array.choose (fun n ->
                if n <> Sidecar.MetadataEntry && filter n then
                    Some (EmbeddedFile.Create(string def.FullName, n, (sc.TryGet n).Value, contentType))
                else None)

    /// The marker attribute WebSharper adds to assemblies that have a sidecar file.
    let SidecarMarker = "WebSharper.Sidecar"

    let HasSidecarMarker (def: Mono.Cecil.AssemblyDefinition) =
        def.CustomAttributes |> Seq.exists (fun a ->
            a.AttributeType.FullName = "System.Reflection.AssemblyMetadataAttribute"
            && a.ConstructorArguments.Count = 2
            && (match a.ConstructorArguments.[0] with StringArg k -> k = SidecarMarker | _ -> false))

type Assembly =
    {
        Debug : option<Symbols>
        Definition : Mono.Cecil.AssemblyDefinition
        FullLoadPath : option<string>
        SidecarFile : Lazy<option<Sidecar>>
    }

    member this.LoadPath =
        this.FullLoadPath

    member this.Raw =
        this.Definition

    member this.FullName =
        this.Definition.FullName

    member this.Name =
        this.Definition.Name.Name

    member this.Sidecar = this.SidecarFile.Value

    member this.HasSidecarMarker = HasSidecarMarker this.Definition

    member this.ModuleId = this.Definition.MainModule.Mvid

    member this.GetScripts (t: O) =
        Seq.append
            (ParseAllScriptResources this.Definition t)
            (SidecarFiles this.Definition this.Sidecar (IsScriptFile t) CT.Text.JavaScript)

    member this.GetResScripts() =
        ParseWebResources this.Definition
        |> Seq.filter (fun r -> r.IsScript)

    member this.GetMapFiles (t: O) =
        Seq.append
            (ParseAllMapResources this.Definition t)
            (SidecarFiles this.Definition this.Sidecar (IsMapFile t) CT.Text.JavaScript)

    member this.GetSources() =
        Seq.append
            (ParseAllSources this.Definition)
            (SidecarFiles this.Definition this.Sidecar (fun n -> n.StartsWith EMBEDDED_SOURCES) CT.Text.JavaScript)

    member this.GetContents() =
        ParseWebResources this.Definition
        |> Seq.filter (fun r -> not r.IsScript)

    member this.OutputParameters(keyPair) =
        let par = Mono.Cecil.WriterParameters()
        match keyPair with
        | Some kp -> par.StrongNameKeyBlob <- kp
        | None -> ()
        par

    member this.RawBytes(kP: option<byte[]>) =
        use s = new System.IO.MemoryStream(16 * 1024)
        this.Definition.Write(s, this.OutputParameters kP)
        s.ToArray()

    member this.Symbols = this.Debug

    member this.Write(kP: option<byte[]>)(path: string) =
        let par = this.OutputParameters kP
        match this.Debug with
        | Some (Mdb _) ->
            par.WriteSymbols <- true
            par.SymbolWriterProvider <- Mono.Cecil.Mdb.MdbWriterProvider()
        | Some (Pdb _) ->
            par.WriteSymbols <- true
            par.SymbolWriterProvider <- Mono.Cecil.Pdb.PdbWriterProvider()
        | None -> ()
        this.Definition.Write(path, par)

    member this.HasWebSharperMetadata =
        HasWebSharperMetadata this.Definition
        || (this.Sidecar |> Option.exists (fun sc -> (sc.TryGet Sidecar.MetadataEntry).IsSome))

    member this.EmbeddedSourceFiles =
        this.GetSources()
        |> Seq.map (fun s -> s.EmbeddedFileName, s.Content)
        |> Seq.toArray

    static member Create(def, ?loadPath, ?symbols) =
        {
            Debug = symbols
            Definition = def
            FullLoadPath = loadPath
            SidecarFile = lazy (loadPath |> Option.bind Sidecar.TryReadFor)
        }

    static member GetAllResources(def) =
        ParseWebResourcesUnchecked def

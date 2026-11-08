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

namespace WebSharper.Compiler

open WebSharper.Constants

module CT = WebSharper.Core.ContentTypes
type O = WebSharper.Core.JavaScript.Output

[<AutoOpen>]
module AssemblyUtility =

    type PE = PEMetadata.PEAssembly

    let ReadStream (s: Stream) =
        use m = new MemoryStream()
        s.CopyTo(m)
        m.ToArray()

    let ReadResourceBytes name (def: PE) =
        let longName = def.Name + "." + name
        match def.TryGetResource name with
        | Some c -> Some (c, name)
        | None -> def.TryGetResource longName |> Option.map (fun c -> c, longName)

    let HasWebSharperMetadata (a: PE) =
        a.HasResource EMBEDDED_METADATA

    let IsWebSharperAssembly (a: PE) =
        match a.Name with
        | "WebSharper.Core.JavaScript" -> true
        | n when n.Contains("WebSharper") -> true
        | _ -> HasWebSharperMetadata a

    let ParseWebResourcesUnchecked (def: PE) =
        def.Attributes
        |> Seq.choose (fun attr ->
            if Utility.IsWebResourceAttribute attr.TypeName then
                match attr.Arguments with
                | [| :? string as resourceName; :? string as contentType |] ->
                    ReadResourceBytes resourceName def
                    |> Option.map (fun (c, r) ->
                        EmbeddedFile.Create(def.FullName, resourceName, c, CT.Parse contentType, r))
                | _ -> None
            else None)
    
    let ParseWebResources (def: PE) =
        if IsWebSharperAssembly def then
            ParseWebResourcesUnchecked def
        else Seq.empty

    let ReadAllScriptResources (def: PE) filter =
        let byAttr = ParseWebResourcesUnchecked def |> Array.ofSeq
        let byAttrNames = byAttr |> Seq.map (fun r -> r.EmbeddedFileName) |> System.Collections.Generic.HashSet
        def.ResourceNames
        |> Seq.choose (fun n ->
            if filter n && not (byAttrNames.Contains n) then
                def.TryGetResource n
                |> Option.map (fun c -> EmbeddedFile.Create(def.FullName, n, c, CT.Text.JavaScript))
            else None)
        |> Seq.append byAttr
        |> Array.ofSeq

    let ParseAllScriptResources (def: PE) (t: O) =
        if IsWebSharperAssembly def then
            match t with 
            | O.JavaScript -> ReadAllScriptResources def (fun n -> n.EndsWith(".js") || n.EndsWith(".mjs") || n.EndsWith(".cjs") || n.EndsWith(".jsx"))
            | O.TypeScript -> ReadAllScriptResources def (fun n -> (n.EndsWith(".ts") && not (n.EndsWith(".d.ts")) || n.EndsWith(".tsx")))
            | O.TypeScriptDeclaration -> ReadAllScriptResources def (fun n -> n.EndsWith(".d.ts"))
        else Array.empty

    let ParseAllMapResources (def: PE) (t: O) =
        if IsWebSharperAssembly def then
            match t with 
            | O.JavaScript -> ReadAllScriptResources def (fun n -> n.EndsWith(".js.map") || n.EndsWith(".mjs.map") || n.EndsWith(".cjs.map") || n.EndsWith(".jsx.map"))
            | O.TypeScript -> ReadAllScriptResources def (fun n -> (n.EndsWith(".ts.map") && not (n.EndsWith(".d.ts.map")) || n.EndsWith(".tsx.map")))
            | O.TypeScriptDeclaration -> [||]
        else Array.empty

    let ParseAllSources (def: PE) =
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
    let SidecarFiles (def: PE) (sc: option<Sidecar>) (filter: string -> bool) contentType =
        match sc with
        | None -> [||]
        | Some sc ->
            let webResources = sc.WebResources |> Seq.map fst |> System.Collections.Generic.HashSet
            sc.Names |> Array.choose (fun n ->
                if n <> Sidecar.MetadataEntry && n <> Sidecar.WebResourcesEntry && not (webResources.Contains n) && filter n then
                    Some (EmbeddedFile.Create(def.FullName, n, (sc.TryGet n).Value, contentType))
                else None)

    /// Web resources (extra.files and web worker bundles) in a sidecar, like WebResource
    /// attributes with embedded resources in an assembly.
    let SidecarWebResourceFiles (def: PE) (sc: option<Sidecar>) =
        match sc with
        | None -> [||]
        | Some sc ->
            sc.WebResources |> Array.choose (fun (n, contentType) ->
                sc.TryGet n |> Option.map (fun c ->
                    EmbeddedFile.Create(def.FullName, n, c, CT.Parse contentType)))

    /// The marker attribute WebSharper adds to assemblies that have a sidecar file.
    let SidecarMarker = "WebSharper.Sidecar"

    let HasSidecarMarker (def: PE) =
        def.Attributes |> Seq.exists (fun a ->
            a.TypeName = "System.Reflection.AssemblyMetadataAttribute"
            && a.ArgumentCount = 2
            && (match a.Arguments.[0] with :? string as k -> k = SidecarMarker | _ -> false))

type Assembly =
    {
        Debug : option<Symbols>
        Definition : PE
        FullLoadPath : option<string>
        SidecarFile : Lazy<option<Sidecar>>
    }

    member this.LoadPath =
        this.FullLoadPath

    member this.Image =
        this.Definition

    member this.FullName =
        this.Definition.FullName

    member this.Name =
        this.Definition.Name

    member this.Sidecar = this.SidecarFile.Value

    member this.HasSidecarMarker = HasSidecarMarker this.Definition

    member this.ModuleId = this.Definition.ModuleId

    member this.HasRuntimeMetadata =
        this.Definition.HasResource EMBEDDED_RUNTIME_METADATA

    member this.TryGetResource(name) =
        this.Definition.TryGetResource name

    member this.GetScripts (t: O) =
        Seq.concat [
            ParseAllScriptResources this.Definition t
            SidecarFiles this.Definition this.Sidecar (IsScriptFile t) CT.Text.JavaScript
            SidecarWebResourceFiles this.Definition this.Sidecar
        ]

    /// Web resources: WebResource attributes and embedded resources, or sidecar entries.
    member this.GetWebResources() =
        Seq.append (ParseWebResources this.Definition) (SidecarWebResourceFiles this.Definition this.Sidecar)

    member this.GetResScripts() =
        this.GetWebResources()
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
        this.GetWebResources()
        |> Seq.filter (fun r -> not r.IsScript)

    member this.Symbols = this.Debug

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

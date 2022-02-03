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
                use r = r.GetResourceStream()
                Some (ReadStream r)
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
                    |> Option.map (fun c ->
                        EmbeddedFile.Create(string def.FullName, resourceName, c, CT.Parse contentType))
                | _ -> None
            else None)
    
    let ParseWebResources (def: Mono.Cecil.AssemblyDefinition) =
        if IsWebSharperAssembly def then
            ParseWebResourcesUnchecked def
        else Seq.empty

type Assembly =
    {
        Debug : option<Symbols>
        Definition : Mono.Cecil.AssemblyDefinition
        FullLoadPath : option<string>
    }

    member this.LoadPath =
        this.FullLoadPath

    member this.Raw =
        this.Definition

    member this.FullName =
        this.Definition.FullName

    member this.Name =
        this.Definition.Name.Name

    member this.GetScripts() =
        ParseWebResources this.Definition
        |> Seq.filter (fun r -> r.IsScript)

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

    member this.ReadableJavaScript =
        ReadResource EMBEDDED_JS this.Definition

    member this.MapFileForReadable =
        ReadResource EMBEDDED_MAP this.Definition

    member this.CompressedJavaScript =
        ReadResource EMBEDDED_MINJS this.Definition

    member this.MapFileForCompressed =
        ReadResource EMBEDDED_MINMAP this.Definition

    member this.TypeScriptDeclarations =
        ReadResource EMBEDDED_DTS this.Definition

    member this.HasWebSharperMetadata =
        HasWebSharperMetadata this.Definition

    static member Create(def, ?loadPath, ?symbols) =
        {
            Debug = symbols
            Definition = def
            FullLoadPath = loadPath
        }

    static member GetAllResources(def) =
        ParseWebResourcesUnchecked def

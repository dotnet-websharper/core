// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

module internal WebSharper.Sitelets.Offline.Output

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open System.Web
open Mono.Cecil
open IntelliFactory.Core
open WebSharper.Sitelets
open WebSharper.Web

module C = WebSharper.Sitelets.Content
module H = WebSharper.Compiler.HtmlCommand
module Http = WebSharper.Sitelets.Http
module J = WebSharper.Core.Json
module M = WebSharper.Core.Metadata
module P = PathUtility
module Re = WebSharper.Core.Reflection
module R = WebSharper.Core.Resources

[<Literal>]
let EMBEDDED_JS = "WebSharper.js"

[<Literal>]
let EMBEDDED_MINJS = "WebSharper.min.js"

[<Literal>]
let EMBEDDED_MAP = "WebSharper.map"

[<Literal>]
let EMBEDDED_MINMAP = "WebSharper.min.map"

[<Literal>]
let EMBEDDED_DTS = "WebSharper.d.ts"

type Mode = H.Mode

type Config =
    {
        Actions : list<obj>
        Options : H.Config
        Sitelet : Sitelet<obj>
        UnpackSourceMap : bool
        UnpackTypeScript : bool
    }

    member this.OutputDirectory =
        this.Options.OutputDirectory

/// Collects metadata from all assemblies in referenced folders.
let getMetadata conf =
    Seq.append [conf.Options.MainAssemblyPath] conf.Options.ReferenceAssemblyPaths
    |> Seq.distinctBy (fun assemblyFile ->
        AssemblyName.GetAssemblyName(assemblyFile).Name)
    |> Seq.choose M.AssemblyInfo.Load
    |> M.Info.Create

/// Generates unique file names.
[<Sealed>]
type UniqueFileNameGenerator() =
    let table = Dictionary<string,unit>()

    /// Generates a new filename with a given basis.
    member this.Generate(name: string) =
        let rec addName n =
            let newName = name + string n
            if table.ContainsKey(newName) then
                addName (n + 1)
            else
                table.[newName] <- ()
                newName
        if table.ContainsKey(name) then
            addName 1
        else
            table.[name] <- ()
            name

/// Copies the contents of source stream to the target stream.
let streamCopy (source: Stream) (target: Stream) =
    source.CopyTo(target, 4096)

/// Represents embedded resources. Uses F# structural equality.
type EmbeddedResource =
    {
        Name : string
        Type : Type
    }

    static member Create(name, ty) =
        { Name = name; Type = ty }

/// The mutable state of the processing.
[<Sealed>]
type State(conf: Config) =
    let metadata = getMetadata conf
    let json = J.Provider.CreateTyped(metadata)
    let unique = UniqueFileNameGenerator()
    let usedAssemblies = HashSet<Re.AssemblyName>()
    let usedResources = HashSet()
    member this.Config = conf
    member this.Json = json
    member this.Metadata = metadata
    member this.Unique = unique
    member this.Assemblies = usedAssemblies :> seq<_>
    member this.Resources = usedResources :> seq<_>
    member this.UseAssembly(name: Re.AssemblyName) = usedAssemblies.Add(name) |> ignore
    member this.UseResource(res: EmbeddedResource) = usedResources.Add(res) |> ignore

/// Gets the JavaScript filename of an assembly, for example `WebSharper.js`.
let getAssemblyFileName (mode: Mode) (aN: Re.AssemblyName) =
    match mode with
    | H.Debug -> String.Format("{0}.js", aN.Name)
    | H.Release -> String.Format("{0}.min.js", aN.Name)

/// Gets the physical path to the assembly JavaScript.
let getAssemblyJavaScriptPath (conf: Config) (aN: Re.AssemblyName) =
    P.CreatePath ["Scripts"; aN.Name; getAssemblyFileName conf.Options.Mode aN]

/// Gets the physical path to the TypeScript declaration.
let getAssemblyTypeScriptPath (conf: Config) (aN: Re.AssemblyName) =
    P.CreatePath ["Scripts"; aN.Name; aN.Name + ".d.ts"]

/// Gets the source map filename of an assembly, for example `WebSharper.map`.
let getAssemblyMapFileName (mode: Mode) (aN: Re.AssemblyName) =
    match mode with
    | H.Debug -> String.Format("{0}.map", aN.Name)
    | H.Release -> String.Format("{0}.min.map", aN.Name)

/// Gets the physical path to the assembly source map.
let getAssemblyMapPath (conf: Config) (aN: Re.AssemblyName) =
    P.CreatePath ["Scripts"; aN.Name; getAssemblyMapFileName conf.Options.Mode aN]

/// Gets the physical path to the assembly source map.
let getSourceFilePath (conf: Config) (aN: Re.AssemblyName) (n: string) =
    P.CreatePath ["Scripts"; aN.Name; n]

/// Gets the physical path to the embedded resoure file.
let getEmbeddedResourcePath (conf: Config) (res: EmbeddedResource) =
    let x = res.Type.Assembly.GetName()
    P.CreatePath ["Scripts"; x.Name; res.Name]

/// Opens a file for writing, taking care to create folders.
let createFile (cfg: Config) (targetPath: P.Path) =
    let targetPath = P.ToAbsolute cfg.OutputDirectory targetPath
    let d = Path.GetDirectoryName(targetPath)
    if not (Directory.Exists(d)) then
        Directory.CreateDirectory(d)
        |> ignore
    File.Open(targetPath, FileMode.Create) :> Stream

/// Writes an embedded resource to the target path.
let writeEmbeddedResource (cfg: Config) (assemblyPath: string) (n: string) (targetPath: P.Path) =
    let aD = AssemblyDefinition.ReadAssembly(assemblyPath)
    let stream =
        aD.MainModule.Resources
        |> Seq.tryPick (fun r ->
            match r with
            | :? Mono.Cecil.EmbeddedResource as r ->
                if r.Name = n then Some (r.GetResourceStream()) else None
            | _ -> None)
    match stream with
    | None -> failwithf "No resource %s in %s at %s" n aD.FullName assemblyPath
    | Some s ->
        use s = s
        match Path.GetExtension(n) with
        | ".css" ->
            use r = new StreamReader(s)
            let text = r.ReadToEnd()
            use w = new StreamWriter(createFile cfg targetPath, Encoding.UTF8)
            w.Write(text)
        | _ ->
            use s2 = createFile cfg targetPath
            streamCopy s s2

/// Outputs all required resources. This is the last step of processing.
let writeResources (aR: AssemblyResolver) (st: State) (sourceMap: bool) (typeScript: bool) =
    for aN in st.Assemblies do
        let assemblyPath = aR.ResolvePath(AssemblyName aN.FullName)
        match assemblyPath with
        | Some aP ->
            let embeddedResourceName =
                match st.Config.Options.Mode with
                | H.Debug -> EMBEDDED_JS
                | H.Release -> EMBEDDED_MINJS
            let p = getAssemblyJavaScriptPath st.Config aN
            writeEmbeddedResource st.Config aP embeddedResourceName p
            if sourceMap then
                let sourceMapResourceName =
                    match st.Config.Options.Mode with
                    | H.Debug -> EMBEDDED_MAP
                    | H.Release -> EMBEDDED_MINMAP
                let sp = getAssemblyMapPath st.Config aN
                try writeEmbeddedResource st.Config aP sourceMapResourceName sp
                    let mapFileName = getAssemblyMapFileName st.Config.Options.Mode aN
                    File.AppendAllText(P.ToAbsolute st.Config.OutputDirectory sp,
                        "\n//# sourceMappingURL=" + mapFileName)       
                with _ -> ()
            if typeScript then
                let tp = getAssemblyTypeScriptPath st.Config aN
                writeEmbeddedResource st.Config aP EMBEDDED_DTS tp
        | None ->
            stderr.WriteLine("Could not resolve: {0}", aN)
    for res in st.Resources do
        let erp = getEmbeddedResourcePath st.Config res
        match aR.ResolvePath(AssemblyName res.Type.Assembly.FullName) with
        | Some aP -> writeEmbeddedResource st.Config aP res.Name erp
        | None -> stderr.WriteLine("Could not resolve {0}", res.Type.Assembly.FullName)

/// Generates a relative path prefix, such as "../../../" for level 3.
let relPath level =
    String.replicate level "../"

/// Creates a context for resource HTML printing.
let resourceContext (st: State) (level: int) : R.Context =
    let relPath = relPath level
    let scriptsFile folder file =
        let url = String.Format("{0}Scripts/{1}/{2}", relPath, folder, file)
        R.RenderLink url
    {
        DebuggingEnabled =
            match st.Config.Options.Mode with
            | H.Debug -> true
            | H.Release -> false

        DefaultToHttp = true

        GetSetting = fun _ -> None

        GetAssemblyRendering = fun aN ->
            st.UseAssembly(aN)
            scriptsFile aN.Name (getAssemblyFileName st.Config.Options.Mode aN)

        GetWebResourceRendering = fun ty name ->
            st.UseResource(EmbeddedResource.Create(name, ty))
            scriptsFile (ty.Assembly.GetName().Name) name
    }

/// Creates a dummy request.
let emptyRequest (uri: string) : Http.Request =
    {
        Method = Http.Method.Get
        Uri = Uri(uri, UriKind.Relative)
        Headers = Seq.empty
        Post = Http.ParameterCollection(Seq.empty)
        Get = Http.ParameterCollection(Seq.empty)
        Cookies = HttpCookieCollection()
        ServerVariables = Http.ParameterCollection(Seq.empty)
        Body = Stream.Null
        Files = Seq.empty
    }

/// Represents an action that was partially resolved to some content.
type ResolvedContent =
    {
        Path : P.Path
        RelativePath : string
        ResourceContext : R.Context
        Respond : Context<obj> -> Async<Http.Response>
    }

/// Partially resolves the content.
let resolveContent (projectFolder: string) (rootFolder: string) (st: State) (loc: Location) (content: Content<obj>) =
    let locationString =
        let locStr = loc.ToString()
        if locStr.EndsWith("/") then
            locStr + "index"
        else
            locStr
        |> st.Unique.Generate
    // Compute the level of nesting
    let level =
        let parts =
            locationString.Split '/'
            |> Array.filter (fun s -> s.Length > 0)
        parts.Length - 1
    let resContext = resourceContext st level
    let genResp = C.ToResponseAsync content
    async {
        let! response =
            genResp {
                Json = st.Json
                Link = fun _ -> ""
                ApplicationPath = "."
                ResolveUrl = fun x -> x
                Metadata = st.Metadata
                ResourceContext = resContext
                Request = emptyRequest locationString
                RootFolder = projectFolder
                UserSession = IUserSession.NotAvailable
                Environment = Map []
            }
        let path =
            let ext =
                response.Headers
                |> Seq.tryPick (fun header ->
                    if header.Name.ToLower() = "content-type" then
                        Some header.Value
                    else
                        None)
                |> Option.map (fun ct ->
                    if ct.StartsWith "application/json" then
                        ".json"
                    elif ct.StartsWith "text/html" then
                        ".html"
                    elif ct.StartsWith "text/css" then
                        ".css"
                    elif ct.StartsWith "text/plain" then
                        ".txt"
                    elif ct.StartsWith "image/gif" then
                        ".gif"
                    elif ct.StartsWith "image/jpeg" then
                        ".jpeg"
                    elif ct.StartsWith "image/png" then
                        ".png"
                    elif ct.StartsWith "image/svg+xml" then
                        ".xml"
                    elif ct.StartsWith "image/tiff" then
                        ".tiff"
                    else
                        ".html")
            match ext with
            | Some ext -> locationString + ext
            | None -> locationString
            |> P.ParsePath
        return {
            Path = path
            RelativePath = relPath level
            ResourceContext = resContext
            Respond = genResp
        }
    }

/// Trims the starting slashes from a path.
let trimPath (path: string) =
    path.TrimStart('/')

let WriteSite (aR: AssemblyResolver) (conf: Config) =
    let st = State(conf)
    let table = Dictionary()
    let projectFolder = conf.Options.ProjectDirectory
    let rootFolder = conf.Options.OutputDirectory
    let contents () =
        async {
            let res = ResizeArray()
            for action in conf.Actions do
                match conf.Sitelet.Router.Link(action) with
                | Some location ->
                    let content = conf.Sitelet.Controller.Handle(action)
                    let! rC = resolveContent projectFolder rootFolder st location content
                    do table.[action] <- rC.Path
                    do res.Add(rC)
                | None -> ()
            return res.ToArray()
        }
    // Write contents
    async {
        let! results = contents ()
        for rC in results do
            // Define context
            let context : Context<obj> =
                {
                    ApplicationPath = "."
                    ResolveUrl = fun x -> x
                    Json = st.Json
                    Link = fun action ->
                        // First try to find from url table.
                        if table.ContainsKey(action) then
                            rC.RelativePath + P.ShowPath table.[action]
                        else
                            // Otherwise, link to the action using the router
                            match conf.Sitelet.Router.Link action with
                            | Some loc ->
                                rC.RelativePath + trimPath (string loc)
                            | None ->
                                let msg = "Failed to link to action from " + P.ShowPath rC.Path
                                stdout.WriteLine("Warning: " + msg)
                                "#"
                    Metadata = st.Metadata
                    ResourceContext = rC.ResourceContext
                    Request = emptyRequest (P.ShowPath rC.Path)
                    RootFolder = projectFolder
                    UserSession = IUserSession.NotAvailable
                    Environment = Map []
                }
            let! response = rC.Respond context
            use stream = createFile conf rC.Path
            return response.WriteBody(stream)
        // Write resources determined to be necessary.
        return writeResources aR st conf.UnpackSourceMap conf.UnpackTypeScript
    }

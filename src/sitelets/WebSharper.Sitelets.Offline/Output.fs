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

module internal WebSharper.Sitelets.Offline.Output

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text
open System.Threading.Tasks
open System.Text.RegularExpressions
open System.Web
open Mono.Cecil
open WebSharper.Compiler
open WebSharper.Sitelets
open WebSharper.Web
open WebSharper.Constants

module C = WebSharper.Sitelets.Content
module H = WebSharper.Compiler.HtmlCommand
module Http = WebSharper.Sitelets.Http
module J = WebSharper.Core.Json
module M = WebSharper.Core.Metadata
module P = PathUtility
//module Re = WebSharper.Core.Reflection
module R = WebSharper.Core.Resources

type Mode = H.Mode

type Config =
    {
        Actions : list<obj>
        Options : H.Config
        Sitelet : Sitelet<obj>
        UnpackSourceMap : bool
        UnpackTypeScript : bool
        UnpackTypeScriptDeclaration : bool
        Metadata: M.Info
        Logger: LoggerBase
    }

    member this.OutputDirectory =
        this.Options.OutputDirectory

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
        AssemblyName : AssemblyName
    }

    static member Create(name, asmName) =
        { Name = name; AssemblyName = asmName }

/// The mutable state of the processing.
[<Sealed>]
type State(conf: Config) =
    let metadata = conf.Metadata
    let dependencies = Graph.FromData metadata.Dependencies
    let json = J.Provider.CreateTyped(metadata)
    let unique = UniqueFileNameGenerator()
    let usedAssemblies = HashSet<string>()
    let usedResources = HashSet()
    member this.Config = conf
    member this.Json = json
    member this.Metadata = metadata
    member this.Dependencies = dependencies
    member this.Unique = unique
    member this.Assemblies = usedAssemblies :> seq<_>
    member this.Resources = usedResources :> seq<_>
    member this.UseAssembly(name: string) = usedAssemblies.Add(name) |> ignore
    member this.UseResource(res: EmbeddedResource) = usedResources.Add(res) |> ignore

/// Gets the JavaScript filename of an assembly, for example `WebSharper.js`.
let getAssemblyFileName (mode: Mode) (aN: string) =
    match mode with
    | H.Debug -> String.Format("{0}.js", aN)
    | H.Release -> String.Format("{0}.min.js", aN)

/// Gets the physical path to the assembly JavaScript.
let getAssemblyJavaScriptPath (conf: Config) (aN: string) =
    P.CreatePath ["Scripts"; aN; getAssemblyFileName conf.Options.Mode aN]

/// Gets the physical path to the TypeScript declaration.
let getAssemblyTypeScriptPath (conf: Config) (aN: string) =
    P.CreatePath ["Scripts"; aN; aN + ".d.ts"]

/// Gets the source map filename of an assembly, for example `WebSharper.map`.
let getAssemblyMapFileName (mode: Mode) (aN: string) =
    match mode with
    | H.Debug -> String.Format("{0}.map", aN)
    | H.Release -> String.Format("{0}.min.map", aN)

/// Gets the physical path to the assembly source map.
let getAssemblyMapPath (conf: Config) (aN: string) =
    P.CreatePath ["Scripts"; aN; getAssemblyMapFileName conf.Options.Mode aN]

/// Gets the physical path to the assembly source map.
let getSourceFilePath (conf: Config) (aN: string) (n: string) =
    P.CreatePath ["Scripts"; aN; n]

/// Gets the physical path to the embedded resoure file.
let getEmbeddedResourcePath (conf: Config) (res: EmbeddedResource) =
    P.CreatePath ["Scripts"; res.AssemblyName.Name; res.Name]

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
    let fn = aD.Name.Name + "." + n
    let stream =
        aD.MainModule.Resources
        |> Seq.tryPick (fun r ->
            match r with
            | :? Mono.Cecil.EmbeddedResource as r ->
                if r.Name = n || r.Name = fn then Some (r.GetResourceStream()) else None
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
        let assemblyPath = aR.ResolvePath(AssemblyName aN)
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
                    File.AppendAllText(P.ToAbsolute st.Config.OutputDirectory p,
                        "\n//# sourceMappingURL=" + mapFileName)       
                with _ -> ()
            if typeScript then
                let tp = getAssemblyTypeScriptPath st.Config aN
                writeEmbeddedResource st.Config aP EMBEDDED_DTS tp
            st.Metadata.ExtraBundles
            |> Set.filter (fun b -> b.AssemblyName = aN)
            |> Set.iter (fun b ->
                // TODO: this will have to use b.MinifiedFileName
                // when we get to use .min.js for extra bundles.
                let filename = b.FileName
                let res = EmbeddedResource.Create(filename, AssemblyName(b.AssemblyName))
                let path = getEmbeddedResourcePath st.Config res
                writeEmbeddedResource st.Config aP filename path
            )
        | None ->
            stderr.WriteLine("Could not resolve: {0}", aN)
    for res in st.Resources do
        let erp = getEmbeddedResourcePath st.Config res
        match aR.ResolvePath(AssemblyName res.AssemblyName.FullName) with
        | Some aP -> writeEmbeddedResource st.Config aP res.Name erp
        | None -> stderr.WriteLine("Could not resolve {0}", res.AssemblyName.FullName)

/// Generates a relative path prefix, such as "../../../" for level 3.
let relPath level =
    String.replicate level "../"

/// Creates a context for resource HTML printing.
let resourceContext (st: State) (level: int) : R.Context =
    let relPath = relPath level
    let scriptsFolder = relPath + "Scripts/"
    let scriptsFile folder file =
        let url = String.Format("{0}{1}/{2}", scriptsFolder, folder, file)
        R.RenderLink url
    {
        DebuggingEnabled =
            match st.Config.Options.Mode with
            | H.Debug -> true
            | H.Release -> false

        DefaultToHttp = true

        ScriptBaseUrl = Some scriptsFolder

        GetSetting = 
            if st.Config.Options.DownloadResources then
                function
                | "UseDownloadedResources" -> Some "true"
                | _ -> None
            else  
                fun _ -> None

        GetAssemblyRendering = fun aN ->
            st.UseAssembly(aN)
            scriptsFile aN (getAssemblyFileName st.Config.Options.Mode aN)

        GetWebResourceRendering = fun ty name ->
            st.UseResource(EmbeddedResource.Create(name, ty.Assembly.GetName()))
            scriptsFile (ty.Assembly.GetName().Name) name

        WebRoot = relPath

        RenderingCache = System.Collections.Concurrent.ConcurrentDictionary()
        ResourceDependencyCache = System.Collections.Concurrent.ConcurrentDictionary()
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
let resolveContent (projectFolder: string) (rootFolder: string) (st: State) (loc: System.Uri) (link: obj -> string) (content: Content<obj>) =
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
    let genResp = C.ToResponse content
    let context =
        new Context<_>(
            Json = st.Json,
            Link = link,
            ApplicationPath = ".",
            Metadata = st.Metadata,
            Dependencies = st.Dependencies,
            ResourceContext = resContext,
            Request = Http.Request.Empty locationString,
            RootFolder = projectFolder,
            WebRootFolder = projectFolder,
            UserSession = IUserSession.NotAvailable,
            Environment = Map []
        )
    async {
        let! response = genResp context
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

let rec mvcContentHelper (stream: Stream) (context: Context<obj>) (content: obj) =
    task {
        match content with
        | :? string as stringContent ->
            let w = new System.IO.StreamWriter(stream, System.Text.Encoding.UTF8, 1024, leaveOpen = true)
            do! w.WriteAsync(stringContent)
        | _ ->
            let contentType = content.GetType()
            if contentType.IsGenericType && contentType.GetGenericTypeDefinition() = typedefof<Task<_>> then
                let contentTask = content :?> Task
                do! contentTask
                let contentResult =
                    let resultGetter = contentType.GetProperty("Result")
                    resultGetter.GetMethod.Invoke(contentTask, [||])
                return! mvcContentHelper stream context contentResult
            else
                let w = new System.IO.StreamWriter(stream, System.Text.Encoding.UTF8, 1024, leaveOpen = true)
                let json = WebSharper.Json.Serialize content
                do! w.WriteAsync(json)
    }

and contentHelper (stream: Stream) (context: Context<obj>) (rsp: Http.Response) =
    task {
        match rsp.WriteBody with
        | Http.EmptyBody ->
            ()
        | Http.WriteBody write ->
            write stream
        | Http.WriteBodyAsync write ->
            do! write stream
        | Http.MvcBody mvcObj ->
            do! mvcContentHelper stream context mvcObj
    }

let getError (outerException: Exception) action =
    let errors = ResizeArray<string>()
    let rec processExceptions (ex: Exception) =
        if ex.InnerException <> null then
            sprintf "[%A] - %s\n%A" action ex.Message ex.StackTrace |> errors.Add
            processExceptions (ex.InnerException)
        else
            sprintf "[%A] - %s\n%A" action ex.Message ex.StackTrace |> errors.Add
    processExceptions outerException
    errors

let WriteSite (aR: AssemblyResolver) (config: Config) =
    let st = State(config)
    let projectFolder = config.Options.ProjectDirectory
    let rootFolder = config.Options.OutputDirectory
    let errors = ResizeArray<string>()
    // Write contents
    async {
        config.Logger.TimedStage "Starting file generation"
        for action in config.Actions do
            match config.Sitelet.Router.Link(action) with
            | Some location ->
                try
                    let content = config.Sitelet.Controller.Handle(action)
                    let link action = config.Sitelet.Router.Link(action).Value.ToString()
                    let! rC = resolveContent projectFolder rootFolder st location link content
                    // Define context
                    let context =
                        new Context<_>(
                            ApplicationPath = ".",
                            Json = st.Json,
                            Link = (fun action ->
                                // Otherwise, link to the action using the router
                                match config.Sitelet.Router.Link action with
                                | Some loc ->
                                    rC.RelativePath + trimPath (string loc)
                                | None ->
                                    let msg = "Failed to link to action from " + P.ShowPath rC.Path
                                    config.Logger.Out("Warning: " + msg)
                                    "#"),
                            Metadata = st.Metadata,
                            Dependencies = st.Dependencies,
                            ResourceContext = rC.ResourceContext,
                            Request = Http.Request.Empty (P.ShowPath rC.Path),
                            RootFolder = projectFolder,
                            WebRootFolder = projectFolder,
                            UserSession = IUserSession.NotAvailable,
                            Environment = Map []
                        )
                    let! response = rC.Respond context
                    use stream = createFile config rC.Path
                    do! contentHelper stream context response |> Async.AwaitTask
                    config.Logger.TimedStage <| sprintf "Generating %s" (P.ShowPath rC.Path)
                with ex ->
                    let msgs = getError ex action
                    errors.AddRange(msgs)
            | None ->
                let msg = sprintf "Could not link to action: %A" action
                config.Logger.Out("Warning: " + msg)
        // Write resources determined to be necessary.
        writeResources aR st config.UnpackSourceMap config.UnpackTypeScript
        return errors :> seq<string>
    }

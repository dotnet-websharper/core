// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module internal IntelliFactory.WebSharper.Sitelets.Offline.Output

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open System.Web
open IntelliFactory.Core
open IntelliFactory.WebSharper.Sitelets

module C = IntelliFactory.WebSharper.Sitelets.Content
module Http = IntelliFactory.WebSharper.Sitelets.Http
module J = IntelliFactory.WebSharper.Core.Json
module M = IntelliFactory.WebSharper.Core.Metadata
module Re = IntelliFactory.WebSharper.Core.Reflection
module R = IntelliFactory.WebSharper.Core.Resources

[<Literal>]
let EMBEDDED_JS = "WebSharper.js"

[<Literal>]
let EMBEDDED_MINJS = "WebSharper.min.js"

type Mode =
    | Debug
    | Release

type Config =
    {
        Actions : list<obj>
        Mode : Mode
        Sitelet : Sitelet<obj>
        SourceDirs : list<DirectoryInfo>
        TargetDir : DirectoryInfo
    }

/// Collects metadata from all assemblies in referenced folders.
let getMetadata (conf: Config) =
    conf.SourceDirs
    |> Seq.collect (fun x ->
        let dlls = Directory.GetFiles(x.FullName, "*.dll")
        let exes = Directory.GetFiles(x.FullName, "*.exe")
        Seq.append dlls exes)
    |> Seq.distinctBy (fun assemblyFile ->
        AssemblyName.GetAssemblyName(assemblyFile).Name)
    |> Seq.choose (fun x ->
        match M.AssemblyInfo.Load(x) with
        | None -> None
        | Some r -> Some r)
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
    let buf = Array.create 4096 0uy
    let rec loop () =
        match source.Read(buf, 0, buf.Length) with
        | 0 -> ()
        | k ->
            target.Write(buf, 0, k)
            loop ()
    loop ()

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
    member this.UseAssembly(name) = usedAssemblies.Add(name) |> ignore
    member this.UseResource(res) = usedResources.Add(res) |> ignore

/// Utility: combines two paths with a slash or backslash.
let ( ++ ) a b = Path.Combine(a, b)

/// Gets the JavaScript filename of an assembly, for example `IntelliFactory.WebSharper.js`.
let getAssemblyFileName (mode: Mode) (aN: Re.AssemblyName) =
    match mode with
    | Debug -> String.Format("{0}.js", aN.Name)
    | Release -> String.Format("{0}.min.js", aN.Name)

/// Gets the physical path to the assembly JavaScript.
let getAssemblyJavaScriptPath (conf: Config) (aN: Re.AssemblyName) =
    conf.TargetDir.FullName ++ "Scripts" ++ aN.Name ++ getAssemblyFileName conf.Mode aN

/// Gets the physical path to the embedded resoure file.
let getEmbeddedResourcePath (conf: Config) (res: EmbeddedResource) =
    let x = res.Type.Assembly.GetName()
    conf.TargetDir.FullName ++ "Scripts" ++ x.Name ++ res.Name

// Remove WebResource annotating from CSS files.
// E.g. <%= WebResource("ResourceName.png") %> => ResourceName.png
let replaceWebResourceTags : string -> string =
    let rx = Regex(@"<%=\s*WebResource\s*\(\s*\""|\""\s*\)\s*%>")
    fun s -> rx.Replace(s, "")

/// Opens a file for writing, taking care to create folders.
let createFile (targetPath: string) =
    let d = Path.GetDirectoryName(targetPath)
    if not (Directory.Exists(d)) then
        Directory.CreateDirectory(d)
        |> ignore
    File.Open(targetPath, FileMode.Create) :> Stream

/// Writes an embedded resource to the target path.
let writeEmbeddedResource (a: Assembly) (n: string) (targetPath: string) =
    match Path.GetExtension(n) with
    | ".css" ->
        use r = new StreamReader(a.GetManifestResourceStream(n))
        let text =
            r.ReadToEnd()
            |> replaceWebResourceTags
        use w = new StreamWriter(createFile targetPath, Encoding.UTF8)
        w.Write(text)
    | _ ->
        use s1 = a.GetManifestResourceStream(n)
        use s2 = createFile targetPath
        streamCopy s1 s2

/// Outputs all required resources. This is the last step of processing.
let writeResources (aR: AssemblyResolver) (st: State) =
    for aN in st.Assemblies do
        let assembly = aR.Resolve(AssemblyName aN.FullName)
        match assembly with
        | Some assembly ->
            let embeddedResourceName =
                match st.Config.Mode with
                | Debug -> EMBEDDED_JS
                | Release -> EMBEDDED_MINJS
            let p = getAssemblyJavaScriptPath st.Config aN
            writeEmbeddedResource assembly embeddedResourceName p
        | None ->
            stderr.WriteLine("Could not resolve: {0}", aN)
    for res in st.Resources do
        getEmbeddedResourcePath st.Config res
        |> writeEmbeddedResource res.Type.Assembly res.Name

/// Generates a relative path prefix, such as "../../../" for level 3.
let relPath level =
    String.replicate level "../"

/// Creates a context for resource HTML printing.
let resourceContext (st: State) (level: int) : R.Context =
    let relPath = relPath level
    let scriptsFile folder file =
        String.Format("{0}Scripts/{1}/{2}", relPath, folder, file)
    {
        DebuggingEnabled =
            match st.Config.Mode with
            | Debug -> true
            | _ -> false

        GetSetting = fun _ -> None

        GetAssemblyUrl = fun aN ->
            st.UseAssembly(aN)
            scriptsFile aN.Name (getAssemblyFileName st.Config.Mode aN)

        GetWebResourceUrl = fun ty name ->
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
        Path : string
        RelativePath : string
        ResourceContext : R.Context
        Respond : Context<obj> -> Http.Response
    }

/// Partially resolves the content.
let resolveContent (rootFolder: string) (st: State) (loc: Location) (content: Content<obj>) =
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
    let response =
        genResp {
            Json = st.Json
            Link = fun _ -> ""
            ApplicationPath =  ""
            ResolveUrl = fun x -> x
            Metadata = st.Metadata
            ResourceContext = resContext
            Request = emptyRequest locationString
            RootFolder = rootFolder
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
    {
        Path = path
        RelativePath = relPath level
        ResourceContext = resContext
        Respond = genResp
    }

/// Trims the starting slashes from a path.
let trimPath (path: string) =
    path.TrimStart('/')

let WriteSite (aR: AssemblyResolver) (conf: Config) =
    let st = State(conf)
    let table = Dictionary()
    let rootFolder = conf.TargetDir.FullName
    let contents =
        conf.Actions
        |> List.ofSeq
        |> List.choose (fun action ->
            match conf.Sitelet.Router.Link(action) with
            | Some location ->
                let content = conf.Sitelet.Controller.Handle(action)
                let rC = resolveContent rootFolder st location content
                table.[action] <- rC.Path
                Some rC
            | None -> None)
    // Write contents
    for rC in contents do
        // Define context
        let context : Context<obj> =
            {
                ApplicationPath = ""
                ResolveUrl = fun x -> x
                Json = st.Json
                Link = fun action ->
                    // First try to find from url table.
                    if table.ContainsKey(action) then
                        rC.RelativePath + trimPath table.[action]
                    else
                        // Otherwise, link to the action using the router
                        match conf.Sitelet.Router.Link action with
                        | Some loc ->
                            rC.RelativePath + trimPath (string loc)
                        | None ->
                            let msg = "Failed to link to action from " + rC.Path
                            stdout.WriteLine("Warning: " + msg)
                            "#"
                Metadata = st.Metadata
                ResourceContext = rC.ResourceContext
                Request = emptyRequest rC.Path
                RootFolder = rootFolder
            }
        let fullPath = conf.TargetDir.FullName + rC.Path
        let response = rC.Respond context
        use stream = createFile fullPath
        response.WriteBody(stream)
    stdout.WriteLine("Used assemblies: ")
    for a in st.Assemblies do
        stdout.WriteLine("    + {0}", a)
    // Write resources determined to be necessary.
    writeResources aR st

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

/// Provides page to url resolution
module internal IntelliFactory.WebSharper.Sitelets.Offline.Output

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open System.Web

module C = IntelliFactory.WebSharper.Sitelets.Content
module Http = IntelliFactory.WebSharper.Sitelets.Http
module M = IntelliFactory.WebSharper.Core.Metadata

type Mode =
    | Debug
    | Release

let private MinJsExtension = "min.js"

let private AssemblyName (ass: Assembly) =
    ass.GetName().Name

// Remove WebResource annotating from CSS files.
// E.g. <%= WebResource("ResourceName.png") %> => ResourceName
let private ReplaceWebResources input  =
    let r1 = new Regex("<%=\s*WebResource\s*\(\s*\"")
    let s2 = r1.Replace(input, "")
    let r2 = new Regex("\"\s*\)\s*%>")
    r2.Replace(s2,"")

let private NonMinifiedFileName (minFileName: string) =
    minFileName.Substring(0, minFileName.Length - 7) + ".js"

let private MinifiedFileName (name: string) =
    name.Substring(0, name.Length - 2) + MinJsExtension

let private AssemblyNameKey ass name =
    AssemblyName ass + name

// Type for keeping track of referenced resources.
type ResourceDictionary() =
    let dict = new Dictionary<string, unit>()
    let keyName ass file = ass + "/" + file
    member this.Add(ass: string, file: string) =
        let key = keyName ass file
        if not <| dict.ContainsKey(key) then
            dict.[key] <- ()
    member this.Exists(ass, file) =
        dict.ContainsKey(keyName ass file)

/// Keep track of the referenced resources
let ReferencedJSResources =
    new ResourceDictionary()

// Represents a resource.
type AssemblyResources =
    {
        Location : string
        Files : list<string>
    }

/// Copies over the JS files.
let ComputeResources (mode: Mode) (srcDirs: list<DirectoryInfo>) =
    // Assembly name and list of file pairs.
    let assFiles = new Dictionary<string, AssemblyResources>()
    for srcDir in srcDirs do
        let files = srcDir.GetFiles("*.dll")
        for file in files do
            let ass = Assembly.LoadFrom(file.FullName)
            let assName = AssemblyName ass
            // <%= WebResource("ActionCheck.png") %>
            let names = ass.GetManifestResourceNames()
            for name in names do
                if assFiles.ContainsKey(assName) then
                    assFiles.[assName] <-
                        {
                            assFiles.[assName] with
                                Files = name :: assFiles.[assName].Files
                        }
                else
                    assFiles.[assName] <-
                        {
                            Location = file.FullName
                            Files = [name]
                        }
    assFiles.Keys
    |> Seq.fold (fun map key ->
        Map.add key assFiles.[key] map)
        Map.empty

let OutputResources (assFiles: Dictionary<string, AssemblyResources>) (targetDir: DirectoryInfo) =
    for assName in assFiles.Keys do
        let assRs = assFiles.[assName]
        let ass = Assembly.LoadFrom(assRs.Location)
        for name in assRs.Files do
            let endsWith = List.exists (fun s -> name.EndsWith s)
            use stream = ass.GetManifestResourceStream(name)

            // Create assembly folder if not already exists
            let namePath name =
                let dirPath =
                    Path.Combine(targetDir.FullName,
                        AssemblyName ass)
                if Directory.Exists(dirPath) |> not then
                    dirPath
                    |> Directory.CreateDirectory
                    |> ignore
                Path.Combine(dirPath , name)

            // Decide whether to include the file or not.
            let includeFile =
                if endsWith [".js"] then
                    ReferencedJSResources.Exists(assName, name)
                else
                    endsWith [".css"; ".jpg"; ".gif"; ".jpeg"; ".png"]

            if includeFile then
                if endsWith [".js"; ".jpg"; ".gif"; ".jpeg"; ".png"] then
                    use output = new FileStream(namePath name, FileMode.Create, FileAccess.Write)
                    let buffer : byte [] = Array.zeroCreate (32 * 1024)
                    let read = ref <| stream.Read(buffer, 0, buffer.Length)
                    while read.Value > 0 do
                        output.Write(buffer, 0, read.Value)
                        read := stream.Read(buffer, 0, buffer.Length)
                elif endsWith [".css"] then
                    // Get CSS content.
                    use sr = new StreamReader(stream)
                    let content = sr.ReadToEnd()

                    // Create the output file
                    use output = new FileStream(namePath name, FileMode.Create, FileAccess.Write)
                    let encoding = Encoding.UTF8

                    // Get bytes from content with resolved WebResources
                    let bytes = encoding.GetBytes( ReplaceWebResources content)
                    output.Write(bytes, 0, Array.length bytes)

let RelPath level =
    List.init level (fun _ -> "../")
    |> List.fold (+) ""

let ResourceContext (assFiles: Dictionary<string, AssemblyResources>)
    (mode: Mode) (targetDir: DirectoryInfo) (level: int)
    : IntelliFactory.WebSharper.Core.Resources.Context =
    let relPath = RelPath level
    {
        DebuggingEnabled =
            match mode with
            | Debug -> true
            | _ -> false

        GetSetting = fun _ -> None

        GetAssemblyUrl = fun assName ->
            let wsFile =
                match mode with
                | Mode.Debug -> "WebSharper.js"
                | Mode.Release -> "WebSharper." + MinJsExtension
            // Mark this js resource as referenced.
            ReferencedJSResources.Add(assName.Name, wsFile)
            // Check if assembly/file exists
//            let isExisting =
//                if assFiles.ContainsKey assName.Name then
//                    assFiles.[assName.Name].Files
//                    |> List.exists (fun e -> e = wsFile)
//                else false
            let file = String.Format("Scripts/{0}/{1}", assName.Name, wsFile)
            String.Format("{0}{1}", relPath, file)

        GetWebResourceUrl = fun ty name ->
            ReferencedJSResources.Add(AssemblyName ty.Assembly, name)
            String.Format("{0}Scripts/{1}/{2}", relPath, AssemblyName ty.Assembly, name)
    }

// Get unique file names
let UniqueFileName =
    // Keeps track of used file names
    let FileNameTable = Dictionary<string,unit>()
    fun (name:string) ->
        let rec addName n =
            let newName = name + string n
            if FileNameTable.ContainsKey(newName) then
                addName (n+1)
            else
                FileNameTable.[newName] <- ()
                newName
        if FileNameTable.ContainsKey(name) then
            addName 1
        else
            FileNameTable.[name] <- ()
            name

let private EmptyRequest (uri: string) :
    IntelliFactory.WebSharper.Sitelets.Http.Request =
    {
        Method = IntelliFactory.WebSharper.Sitelets.Http.Method.Get
        Uri = Uri(uri, UriKind.Relative)
        Headers = Seq.empty
        Post = Http.ParameterCollection(Seq.empty)
        Get = Http.ParameterCollection(Seq.empty)
        Cookies = new HttpCookieCollection()
        ServerVariables = Http.ParameterCollection(Seq.empty)
        Body = Stream.Null
        Files = Seq.empty
    }

// Write a content at the given url
let ResolveContent assFiles (mode: Mode)
    (targetDir: DirectoryInfo) metaData
    (lc: Uri * IntelliFactory.WebSharper.Sitelets.Content<'Action>) =

    let json =
        IntelliFactory.WebSharper.Core.Json.Provider.CreateTyped metaData

    let (loc, content) = lc
    let locationString =
        let locStr = loc.ToString()
        if locStr.EndsWith("/") then
            locStr + "index"
        else
            locStr
        |> UniqueFileName

    // Compute the level of nesting
    let level =
        let ( ++ ) a b = Path.Combine(a, b)
        let parts =
            locationString.Split '/'
            |> Array.filter (fun s -> s.Length > 0)
        parts.Length - 1

    let resContext = ResourceContext assFiles mode targetDir level
    let genResp = C.ToResponse content
    let response =
        genResp {
            Json = json
            Link = fun _ -> ""
            ApplicationPath =  ""
            ResolveUrl = fun x -> x
            Metadata = metaData
            ResourceContext = resContext
            Request = EmptyRequest locationString
        }

    let path =
        let ext =
            response.Headers
            |> Seq.tryPick (fun header ->
                if header.Name.ToLower() = "content-type" then
                    Some header.Value
                else
                    None
            )
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
    (genResp, resContext, path, RelPath level)

type WriteSiteConfiguration<'Action when 'Action : equality> =
    {
        AssemblyFiles : Dictionary<string, AssemblyResources>
        Sitelet : IntelliFactory.WebSharper.Sitelets.Sitelet<'Action>
        Mode : Mode
        SrcDir : list<DirectoryInfo>
        TargetDir : DirectoryInfo
        Actions : list<'Action>
    }

// let WriteSite (assFiles : IDictionary<string, AssemblyResources>)
//   (mode: Mode) site (srcDirs: list<DirectoryInfo>)
//   (targetDir: DirectoryInfo) (actions: seq<'Action>) =
let WriteSite (conf : WriteSiteConfiguration<'Action>) =
    let metaData =
        let a =
            conf.SrcDir
            |> Seq.collect (fun x ->
                let dlls = Directory.GetFiles(x.FullName, "*.dll")
                let exes = Directory.GetFiles(x.FullName, "*.exe")
                Seq.append dlls exes
                |> Seq.choose M.AssemblyInfo.Load)
        M.Info.Create a

    let table = Dictionary()

    let contents =
        conf.Actions
        |> List.ofSeq
        |> List.choose (fun action ->
            // Try to link to action
            match conf.Sitelet.Router.Link action with
            | Some location  ->
                let content = conf.Sitelet.Controller.Handle action
                let (genResp, resContext, path, relPath) =
                    ResolveContent conf.AssemblyFiles conf.Mode
                        conf.TargetDir metaData (location, content)
                table.Add(action, path)
                (genResp, resContext, path, relPath)
                |> Some
            | None -> None)

    let trimPath (path: string) =
        if path.StartsWith "/" then path.Substring 1 else path

    // Write contents
    for (genResp, resContext, path, relPath) in contents do
        let json =
            IntelliFactory.WebSharper.Core.Json.Provider.CreateTyped metaData
        // Define context
        let context : IntelliFactory.WebSharper.Sitelets.Context<_> =
            {
                ApplicationPath = ""
                ResolveUrl = fun x -> x
                Json = json
                Link = fun action ->
                    // First try to find from url table.
                    if table.ContainsKey(action) then
                        relPath + (trimPath table.[action])
                    else
                        // Otherwise, link to the action using the router
                        match conf.Sitelet.Router.Link action with
                        | Some loc ->
                            relPath + (trimPath <| loc.ToString())
                        | None ->
                            let msg = "Failed to link to action from " + path
                            stdout.WriteLine("Warning: " + msg)
                            "#"
                Metadata = metaData
                ResourceContext = resContext
                Request = EmptyRequest path
            }

        let fullPath = conf.TargetDir.FullName + path
        let fileInfo = FileInfo(fullPath)
        if not fileInfo.Directory.Exists then
            fileInfo.Directory.Create()
        let response = genResp context
        use stream = File.Create(fullPath) :> Stream
        let sw = new StringWriter()
        response.WriteBody stream

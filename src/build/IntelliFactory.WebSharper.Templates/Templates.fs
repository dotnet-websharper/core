// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Templates

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq

type LocalWebSharperSource =
    {
        TargetsFile : string
        TemplatesDirectory : string
    }

    static member Create(root) =
        {
            TargetsFile = Path.Combine(root, "build", "WebSharper.targets")
            TemplatesDirectory = Path.Combine(root, "templates")
        }

type NuGetOptions =
    {
        PackagesDirectory : string
    }

    static member Create() =
        {
            PackagesDirectory = "packages"
        }

type WebSharperSource =
    | LocalWS of source: LocalWebSharperSource
    | NuGetWS of version: option<string> * opts: NuGetOptions

    static member FromNuGet(?version, ?opts) =
        let opts =
            match opts with
            | Some opts -> opts
            | None -> NuGetOptions.Create()
        NuGetWS (version, opts)

    static member Local(source) =
        LocalWS source

type InitOptions =
    {
        Directory : string
        ProjectName : string
        WebSharperSource : WebSharperSource
    }

    static member Create() =
        let source = WebSharperSource.FromNuGet()
        {
            Directory = "."
            ProjectName = "MyProject"
            WebSharperSource = source
        }

[<AutoOpen>]
module TemplateUtility =

    let isFile path =
        FileInfo(path).Exists

    let isDir path =
        DirectoryInfo(path).Exists

    let notDir path =
        not (isDir path)

    let notFile path =
        not (isFile path)

    let checkNuGetOpts opts =
        if isFile opts.PackagesDirectory then
            [sprintf "Invalid PackagesDirectory: %s" opts.PackagesDirectory]
        else []

    let checkSource src =
        match src with
        | LocalWS loc ->
            [
                if notFile loc.TargetsFile then
                    yield sprintf "WebSharper.targets not found at %s" loc.TargetsFile
                if notDir loc.TemplatesDirectory then
                    yield sprintf "TemplatesDirectory is not found: %s" loc.TemplatesDirectory
            ]
        | _ -> []

    let complain fmt =
        Printf.ksprintf (fun str -> "TemplateOptions: " + str) fmt

    let check opts =
        [
            yield! checkSource opts.WebSharperSource
            if isFile opts.Directory then
                yield sprintf "Specified InitOptions.Directory is a file: %s" opts.Directory
        ]

    let unsafeChars =
        Regex("[^_0-9a-zA-Z]")

    let clean str =
        match str with
        | null | "" -> "MyProject"
        | str ->
            let str = unsafeChars.Replace(str, "_")
            if Char.IsDigit(str.[0]) then
                "_" + str
            else
                str

    let ensureDir path =
        if notDir path then
            Directory.CreateDirectory(path) |> ignore

    let prepare opts =
        match check opts with
        | [] ->
            ensureDir opts.Directory
            {
                opts with
                    ProjectName = clean opts.ProjectName
            }
        | errors ->
            failwith (String.concat Environment.NewLine errors)

    let isTextFile path =
        match Path.GetExtension(path) with
        | ".asax" | ".config" | ".cs" | ".csproj"
        | ".files" | ".fs" | ".fsproj" | ".fsx" -> true
        | _ -> false

    let neutralEncoding =
        let bom = false
        UTF8Encoding(bom, throwOnInvalidBytes = true) :> Encoding

    let copyTextFile source target =
        let lines = File.ReadAllLines(source)
        File.WriteAllLines(target, lines, neutralEncoding)

    let copyFile src tgt =
        if isTextFile src then
            copyTextFile src tgt
        else
            File.Copy(src, tgt)

    let rec copyDir src tgt =
        ensureDir tgt
        for d in Directory.EnumerateDirectories(src) do
            copyDir d (Path.Combine(tgt, Path.GetFileName(d)))
        for f in Directory.EnumerateFiles(src) do
            copyFile f (Path.Combine(tgt, Path.GetFileName(f)))

    let replace (a: string) (b: string) (main: string) =
        main.Replace(a, b)

    let isProjectFile path =
        match Path.GetExtension(path) with
        | ".csproj" | ".fsproj" -> true
        | _ -> false

    let moveProjectFile opts path =
        if isProjectFile path then
            let ext = Path.GetExtension(path)
            let tgt = Path.Combine(Path.GetDirectoryName(path), opts.ProjectName + ext)
            File.Move(path, tgt)

    let all opts =
        Directory.EnumerateFiles(opts.Directory, "*.*", SearchOption.AllDirectories)

    let moveProjectFiles opts =
        let projFiles = all opts |> Seq.filter isProjectFile |> Seq.toArray
        for p in projFiles do
            moveProjectFile opts p

    let freshGuid () =
        Guid.NewGuid().ToString()

    let expandVariables opts path =
        if isTextFile path then
            let t =
                File.ReadAllText(path)
                |> replace "$guid1$" (freshGuid ())
                |> replace "$guid2$" (freshGuid ())
                |> replace "$safeprojectname$" opts.ProjectName
            File.WriteAllText(path, t, neutralEncoding)

    let expandAllVariables opts =
        for p in all opts do
            expandVariables opts p

    let relPath baseDir path =
        let baseDir = Path.GetFullPath(baseDir)
        let path = Path.GetFullPath(path)
        let chars = [| Path.AltDirectorySeparatorChar; Path.DirectorySeparatorChar |]
        let split (path: string) =
            path.Split(chars, StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
        let rec loop baseDir path =
            match baseDir, path with
            | x :: xs, y :: ys when x = y ->
                loop xs ys
            | _ ->
                [
                    for i in 1 .. baseDir.Length do
                        yield ".."
                    yield! path
                ]
                |> String.concat "/"
        loop (split baseDir) (split path)

    let installTargetsTo targets p =
        let relPath = relPath (Path.GetDirectoryName(p)) targets
        let doc = XDocument.Parse(File.ReadAllText(p))
        let ns = doc.Root.Name.Namespace
        let imp = ns.GetName("Import")
        let proj = XName.Get("Project")
        let ok (el: XElement) =
            match el.Attribute(proj) with
            | null -> false
            | p when p.Value = relPath -> true
            | _ -> false
        if doc.Elements(imp) |> Seq.forall (ok >> not) then
            let el = XElement(imp)
            el.SetAttributeValue(proj, relPath)
            doc.Root.Add(el)
        let str = doc.ToString()
        File.WriteAllText(p, doc.ToString(), neutralEncoding)
        File.WriteAllLines(p, File.ReadAllLines(p), neutralEncoding)

    let initSource (src: WebSharperSource) =
        match src with
        | LocalWS local -> local
        | NuGetWS (ver, opts) ->
            let ws = FsNuGet.Package.GetLatest("WebSharper")
            let path = Path.Combine(opts.PackagesDirectory, ws.Text)
            ws.Install(path)
            LocalWebSharperSource.Create(path)

    let installTargets opts local =
        for p in all opts do
            if isProjectFile p then
                installTargetsTo local.TargetsFile p

    let init id opts =
        let opts = prepare opts
        let local = initSource opts.WebSharperSource
        let sourceDir = Path.Combine(local.TemplatesDirectory, id)
        let targetDir = opts.Directory
        copyDir sourceDir targetDir
        moveProjectFiles opts
        expandAllVariables opts
        installTargets opts local

[<Sealed>]
type Template(id: string) =

    member this.Init(opts) =
        init id opts

    static member All =
        [
            Template.BundleWebsite
            Template.Extension
            Template.Library
            Template.SiteletsHost
            Template.SiteletsHtml
            Template.SiteletsWebsite
        ]

    static member BundleWebsite = Template("bundle-website")
    static member Extension = Template("extension")
    static member Library = Template("library")
    static member SiteletsHost = Template("sitelets-host")
    static member SiteletsHtml = Template("sitelets-html")
    static member SiteletsWebsite = Template("sitelets-website")


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

type LocalSource =
    {
        FileSet : FileSet
        TargetsFile : string
    }

type NuGetPackage =
    | PkgBytes of byte[]
    | PkgLatestPublic

type NuGetSource =
    {
        NuGetPackage : NuGetPackage
        PackagesDirectory : string
    }

type Source =
    | SLocal of LocalSource
    | SNuGet of NuGetSource

type InitOptions =
    {
        Directory : string
        ProjectName : string
        Source : Source
    }

type Template =
    | T of string

[<AutoOpen>]
module Implementation =

    let CheckNuGetOpts opts =
        if IsFile opts.PackagesDirectory then
            [sprintf "Invalid PackagesDirectory: %s" opts.PackagesDirectory]
        else []

    let CheckSource src =
        match src with
        | SLocal loc ->
            [
                if NotFile loc.TargetsFile then
                    yield sprintf "WebSharper.targets not found at %s" loc.TargetsFile
            ]
        | _ -> []

    let Complain fmt =
        Printf.ksprintf (fun str -> "TemplateOptions: " + str) fmt

    let Check opts =
        [
            yield! CheckSource opts.Source
            if IsFile opts.Directory then
                yield sprintf "Specified InitOptions.Directory is a file: %s" opts.Directory
        ]

    let UnsafeChars =
        Regex("[^_0-9a-zA-Z]")

    let Clean str =
        match str with
        | null | "" -> "MyProject"
        | str ->
            let str = UnsafeChars.Replace(str, "_")
            if Char.IsDigit(str.[0]) then
                "_" + str
            else
                str

    let Prepare opts =
        match Check opts with
        | [] ->
            EnsureDir opts.Directory
            { opts with ProjectName = Clean opts.ProjectName }
        | errors ->
            failwith (String.concat Environment.NewLine errors)

    let IsTextFile path =
        match Path.GetExtension(path) with
        | ".asax" | ".config" | ".cs" | ".csproj"
        | ".files" | ".fs" | ".fsproj" | ".fsx" -> true
        | _ -> false

    let NeutralEncoding =
        let bom = false
        UTF8Encoding(bom, throwOnInvalidBytes = true) :> Encoding

    let CopyTextFile source target =
        let lines = File.ReadAllLines(source)
        File.WriteAllLines(target, lines, NeutralEncoding)

    let CopyFile src tgt =
        if IsTextFile src then
            CopyTextFile src tgt
        else
            File.Copy(src, tgt)

    let rec CopyDir src tgt =
        EnsureDir tgt
        for d in Directory.EnumerateDirectories(src) do
            CopyDir d (Path.Combine(tgt, Path.GetFileName(d)))
        for f in Directory.EnumerateFiles(src) do
            CopyFile f (Path.Combine(tgt, Path.GetFileName(f)))

    let Replace (a: string) (b: string) (main: string) =
        main.Replace(a, b)

    let IsProjectFile path =
        match Path.GetExtension(path) with
        | ".csproj" | ".fsproj" -> true
        | _ -> false

    let MoveProjectFile opts path =
        if IsProjectFile path then
            let ext = Path.GetExtension(path)
            let tgt = Path.Combine(Path.GetDirectoryName(path), opts.ProjectName + ext)
            File.Move(path, tgt)

    let All opts =
        Directory.EnumerateFiles(opts.Directory, "*.*", SearchOption.AllDirectories)

    let MoveProjectFiles opts =
        let projFiles = All opts |> Seq.filter IsProjectFile |> Seq.toArray
        for p in projFiles do
            MoveProjectFile opts p

    let FreshGuid () =
        Guid.NewGuid().ToString()

    let ExpandVariables opts path =
        if IsTextFile path then
            let t =
                File.ReadAllText(path)
                |> Replace "$guid1$" (FreshGuid ())
                |> Replace "$guid2$" (FreshGuid ())
                |> Replace "$safeprojectname$" opts.ProjectName
            File.WriteAllText(path, t, NeutralEncoding)

    let ExpandAllVariables opts =
        for p in All opts do
            ExpandVariables opts p

    let RelPath baseDir path =
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

    let InstallTargetsTo targets p =
        let relPath = RelPath (Path.GetDirectoryName(p)) targets
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
        File.WriteAllText(p, doc.ToString(), NeutralEncoding)
        File.WriteAllLines(p, File.ReadAllLines(p), NeutralEncoding)

    let InstallNuGet (nuget: NuGetSource) =
        let ws =
            match nuget.NuGetPackage with
            | PkgLatestPublic ->
                FsNuGet.Package.GetLatest("WebSharper")
            | PkgBytes bytes ->
                FsNuGet.Package.FromBytes(bytes)
        let path = Path.Combine(nuget.PackagesDirectory, ws.Text)
        ws.Install(path)
        path

    let CreateLocalSource root =
        {
            FileSet = FileSet.FromZipFile(Path.Combine(root, "templates/templates.zip"))
            TargetsFile = Path.Combine(root, "build", "WebSharper.targets")
        }

    let InitSource src =
        match src with
        | SLocal local -> local
        | SNuGet nuget ->
            InstallNuGet nuget
            |> CreateLocalSource

    let InstallTargets opts local =
        for p in All opts do
            if IsProjectFile p then
                InstallTargetsTo local.TargetsFile p

    let Init id opts =
        let opts = Prepare opts
        let local = InitSource opts.Source
        local.FileSet.[id].Populate(opts.Directory)
        MoveProjectFiles opts
        ExpandAllVariables opts
        InstallTargets opts local

type NuGetPackage with

    static member FromBytes(bytes) =
        Array.copy bytes |> PkgBytes

    static member FromFile(path) =
        File.ReadAllBytes(path) |> PkgBytes

    static member FromStream(s: Stream) =
        ReadStream s |> PkgBytes

    static member LatestPublic() =
        PkgLatestPublic

type Source with
    static member Local(s) = SLocal s
    static member NuGet(s) = SNuGet s

type LocalSource with
    static member Create(root) = CreateLocalSource root

type NuGetSource with

    static member Create() =
        {
            NuGetPackage = NuGetPackage.LatestPublic()
            PackagesDirectory = "packages"
        }

type InitOptions with

    static member Create() =
        let source = Source.NuGet(NuGetSource.Create())
        {
            Directory = "."
            ProjectName = "MyProject"
            Source = source
        }

type Template with

    member t.Init(opts) =
        match t with
        | T t -> Init t opts

    static member All =
        [
            Template.BundleWebsite
            Template.Extension
            Template.Library
            Template.SiteletsHost
            Template.SiteletsHtml
            Template.SiteletsWebsite
        ]

    static member BundleWebsite = T("bundle-website")
    static member Extension = T("extension")
    static member Library = T("library")
    static member SiteletsHost = T("sitelets-host")
    static member SiteletsHtml = T("sitelets-html")
    static member SiteletsWebsite = T("sitelets-website")


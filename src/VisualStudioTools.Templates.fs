// Copyright 2013 IntelliFactory
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

namespace IntelliFactory.VisualStudioTools

module Templates =
    open System
    open System.IO
    open System.Text
    open Microsoft.Win32
    module X = IntelliFactory.Core.XmlTools
    module NG = IntelliFactory.VisualStudioTools.NuGet
    type Content = IntelliFactory.VisualStudioTools.Utils.Content

    let xmlNamespace = "http://schemas.microsoft.com/developer/vstemplate/2005"
    let xmlElement name = X.XmlElement.Create(name, xmlNamespace)

    type ElementBuilder =
        | E

        static member ( ? ) (self: ElementBuilder, name: string) =
            xmlElement name

    type AttributeBuilder =
        | A

        static member ( ? ) (self: AttributeBuilder, name: string) =
            fun (value: string) -> (name, value)

    let ( ==> ) (x: option<'T>) (render: 'T -> 'R) : list<'R> =
        match x with
        | None -> []
        | Some x -> [render x]

    let boolToString (b: bool) : string =
        if b then "true" else "false"

    let intToString (x: int) : string =
        string x

    let readToEnd (s: Stream) =
        use m = new MemoryStream()
        s.CopyTo(m)
        m.ToArray()

    type ProjectItem =
        {
            PIContent : Content
            PIFileName : string
            PIReplaceParameters : bool
            PITargetFileName : option<string>
        }

        member this.ReplaceParameters(?doReplace: bool) =
            { this with PIReplaceParameters = defaultArg doReplace true }

        member this.ToXml() =
            let attrs =
                [
                    yield A?ReplaceParameters (boolToString this.PIReplaceParameters)
                    yield! this.PITargetFileName ==> A?TargetFileName
                ]
            E?ProjectItem + attrs -- this.PIFileName

        static member Create(fileName, c) =
            let fileName = Path.GetFileName(fileName)
            {
                PIContent = c
                PIFileName = fileName
                PIReplaceParameters = false
                PITargetFileName = None
            }

        static member FromBinaryFile(fullPath) =
            let fileName = Path.GetFileName(fullPath)
            ProjectItem.Create(fileName, Content.ReadBinaryFile(fullPath))

        static member FromTextFile(fullPath) =
            let fileName = Path.GetFileName(fullPath)
            ProjectItem.Create(fileName, Content.ReadTextFile(fullPath))

    type ItemSubType =
        | Form
        | Component
        | CustomControl
        | UserControl

        member this.GetLiteral() =
            match this with
            | Form -> "Form"
            | Component -> "Component"
            | CustomControl -> "CustomControl"
            | UserControl -> "UserControl"

        override this.ToString() =
            this.GetLiteral()

    type Item =
        {
            ItemContent : Content
            ItemCustomTool : option<string>
            ItemFileName: string
            ItemType : option<string>
            ItemReplaceParameters : bool
            ItemSubType : option<ItemSubType>
            ItemTargetFileName : option<string>
        }

        member this.ToXml() =
            let i = this
            let attrs =
                [
                    yield A?ReplaceParameters (boolToString i.ItemReplaceParameters)
                    yield! i.ItemTargetFileName ==> A?TargetFileName
                    yield! i.ItemCustomTool ==> A?CustomTool
                    yield! i.ItemType ==> A?ItemType
                    yield! i.ItemSubType ==> fun x -> A?SubType (x.GetLiteral())
                ]
            E?ProjectItem + attrs -- this.ItemFileName

        static member Create(fileName, content) =
            let fileName = Path.GetFileName(fileName)
            {
                ItemContent = content
                ItemCustomTool = None
                ItemFileName = fileName
                ItemType = None
                ItemReplaceParameters = false
                ItemSubType = None
                ItemTargetFileName = None
            }

        static member FromBinaryFile(path) =
            Item.Create(path, Content.ReadBinaryFile(path))

        static member FromTextFile(path) =
            Item.Create(path, Content.ReadTextFile(path))

    type Folder =
        {
            FolderElements : list<FolderElement>
            FolderName : string
            FolderTargetName : option<string>
        }

        member this.ToXml() =
            let attrs =
                [
                    yield A?Name this.FolderName
                    yield! this.FolderTargetName ==> A?TargetFolderName
                ]
            E?Folder + attrs -< Seq.cache (this.FolderElements |> Seq.map (fun x -> x.ToXml()))

        static member Create(name, elements) =
            let name = Path.GetFileName(name)
            {
                FolderElements = List.ofSeq elements
                FolderName = name
                FolderTargetName = None
            }

    and FolderElement =
        | FEFolder of Folder
        | FEProjectItem of ProjectItem

        member this.ToXml() =
            match this with
            | FEFolder f -> f.ToXml()
            | FEProjectItem i -> i.ToXml()

        static member Nested(item) =
            FEProjectItem item

        static member Folder(folder) =
            FEFolder folder

    type ProjectConfig =
        {
            PCIgnoreProjectParameter : option<string>
            PCReplaceParameters : bool
            PCTargetFileName : option<string>
        }

        static member Default =
            {
                PCIgnoreProjectParameter = None
                PCReplaceParameters = false
                PCTargetFileName = None
            }

    type Project =
        {
            ProjectContent : Content
            ProjectElements : list<FolderElement>
            ProjectFileName : string
            ProjectIgnoreProjectParameter : option<string>
            ProjectReplaceParameters : bool
            ProjectTargetFileName : option<string>
        }

        member this.ReplaceParameters(?doReplace: bool) =
            { this with ProjectReplaceParameters = defaultArg doReplace true }

        member this.ToXml() =
            let p = this
            let attrs =
                [
                    yield A?File this.ProjectFileName
                    yield A?ReplaceParameters (boolToString p.ProjectReplaceParameters)
                    yield! p.ProjectTargetFileName ==> A?TargetFileName
                    yield! p.ProjectIgnoreProjectParameter ==> A?IgnoreProjectParameter
                ]
            E?Project + attrs -< Seq.cache (this.ProjectElements |> Seq.map (fun x -> x.ToXml()))

        static member Create(fileName: string, content: string, elements: seq<FolderElement>) =
            let fileName = Path.GetFileName(fileName)
            {
                ProjectContent = Content.FromText(content)
                ProjectElements = Seq.toList elements
                ProjectFileName = fileName
                ProjectIgnoreProjectParameter = None
                ProjectReplaceParameters = false
                ProjectTargetFileName = None
            }

        static member FromFile(fullPath: string, elements: seq<FolderElement>) =
            let fileName = Path.GetFileName(fullPath)
            Project.Create(fileName, File.ReadAllText(fullPath), elements)

    type Icon =
        {
            IconContent : Content
            IconFileName : string
        }

        static member Create(fileName, c) =
            let fileName = Path.GetFileName(fileName)
            {
                IconContent = c
                IconFileName = fileName
            }

        static member FromFile(fullPath) =
            let fileName = Path.GetFileName(fullPath)
            Icon.Create(fileName, Content.ReadBinaryFile(fullPath))

    type ProjectType =
        | CSharp
        | FSharp
        | VisualBasic
        | Web

        member this.GetLiteral() =
            match this with
            | CSharp -> "CSharp"
            | FSharp -> "FSharp"
            | VisualBasic -> "VisualBasic"
            | Web -> "Web"

        override this.ToString() =
            this.GetLiteral()

    type TemplateData =
        {
            TDDefaultName : option<string>
            TDDescription : string
            TDIcon : Icon
            TDName : string
            TDProjectSubType : option<string>
            TDProjectType : ProjectType
            TDPromptForSaveOnCreation : bool
            TDSortOrder : option<int>
        }

        member this.WithDefaultProjectName(?name) =
            { this with TDDefaultName = name }

        static member Create(ty, name, description, icon) =
            {
                TDDefaultName = None
                TDDescription = description
                TDIcon = icon
                TDName = name
                TDPromptForSaveOnCreation = true
                TDProjectSubType = None
                TDProjectType = ty
                TDSortOrder = None
            }

        member this.ToXml() =
            let d = this
            let content =
                [
                    yield! d.TDDefaultName ==> fun x -> E?DefaultName -- x
                    yield! d.TDSortOrder ==> fun x -> E?SortOrder -- intToString x
                    match d.TDProjectSubType with
                    | None ->
                        match d.TDProjectType with
                        | Web -> yield E?ProjectSubType -- CSharp.GetLiteral()
                        | _ -> ()
                    | Some s -> yield E?ProjectSubType -- s
                ]
                |> List.append
                    [
                        E?Name -- d.TDName
                        E?Description -- d.TDDescription
                        E?Icon -- d.TDIcon.IconFileName
                        E?ProjectType -- d.TDProjectType.GetLiteral()
                        E?PromptForSaveOnCreation -- boolToString this.TDPromptForSaveOnCreation
                    ]
            E?TemplateData -< content

    type ExtensionIdentity =
        {
            EIGuid : Guid
            EIName : string
        }

        member this.FullName =
            String.Format("{0}.{1}", this.EIName, this.EIGuid)

        member this.Guid = this.EIGuid
        member this.Name = this.EIName

        static member Create(n, g) =
            {
                EIGuid = g
                EIName = n
            }

    type NuGetPackages =
        {
            NGIdentity : ExtensionIdentity
            NGPackages : list<NG.Package>
        }

        member this.ExtensionIdentity =
            this.NGIdentity

        member this.Packages =
            this.NGPackages

        static member Create(id, ps) =
            {
                NGIdentity = id
                NGPackages = Seq.toList ps
            }

    type ProjectTemplate =
        {
            PTNuGetPackages : option<NuGetPackages>
            PTProject : Project
            PTTemplateData : TemplateData
            PTVersion : string
        }

        member this.NuGetPackages =
            this.PTNuGetPackages

        static member Create(d, p) =
            {
                PTNuGetPackages = None
                PTProject = p
                PTTemplateData = d
                PTVersion = "3.0.0"
            }

        member this.ToXml() =
            E?VSTemplate + [A?Version this.PTVersion; A?Type "Project"] -< [
                yield this.PTTemplateData.ToXml()
                yield E?TemplateContent - [this.PTProject.ToXml()]
                match this.PTNuGetPackages with
                | None -> ()
                | Some pkgs ->
                    yield E?WizardExtension - [
                        E?Assembly -- "NuGet.VisualStudio.Interop, Version=1.0.0.0, \
                            Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
                        E?FullClassName -- "NuGet.VisualStudio.TemplateWizard"
                    ]
                    yield E?WizardData - [
                        E?packages + [A?repository "extension"; A?repositoryId pkgs.NGIdentity.FullName] -<
                        [
                            for p in pkgs.NGPackages ->
                                E?package + [A?id p.Id; A?version p.Version]
                        ]
                    ]
            ]

        member this.WithNuGetPackages(?pkgs) =
            { this with PTNuGetPackages = pkgs }

    let rec computeEntries (path: string) (e: FolderElement) : seq<string * Content> =
        seq {
            match e with
            | FEFolder f ->
                let subPath = Path.Combine(path, f.FolderName)
                for e in f.FolderElements do
                    yield! computeEntries subPath e
            | FEProjectItem i  ->
                let p = Path.Combine(path, i.PIFileName)
                yield (p, i.PIContent)
        }

    type VisualStudioVersion =
        | VisualStudio2008
        | VisualStudio2010
        | VisualStudio2012

        static member VS2008 = VisualStudio2008
        static member VS2010 = VisualStudio2010
        static member VS2012 = VisualStudio2012

        member this.Code =
            match this with
            | VisualStudio2008 -> "9.0"
            | VisualStudio2010 -> "10.0"
            | VisualStudio2012 -> "11.0"

    type TemplateKind =
        | ItemTemplateKind
        | ProjectTemplateKind

    let templateLocation (version: VisualStudioVersion) (kind: TemplateKind) =
        let ( ? ) (x: RegistryKey) y = x.OpenSubKey(y)
        let ms = Registry.CurrentUser?Software?Microsoft
        match ms?VisualStudio with
        | null -> None
        | vs ->
            match (?) vs version.Code with
            | null -> None
            | hive ->
                let s =
                    let obj =
                        match kind with
                        | ItemTemplateKind -> "UserItemTemplatesLocation"
                        | ProjectTemplateKind -> "UserProjectTemplatesLocation"
                        |> hive.GetValue
                    obj.ToString()
                if String.IsNullOrEmpty(s) then None else Some s

    type InstallConfig =
        {
            ICCategory : list<string>
            ICVisualStudio : VisualStudioVersion
        }

        member this.Locate(templateKind) =
            let baseDir = templateLocation this.ICVisualStudio templateKind
            match baseDir with
            | None -> None
            | Some bD ->
                this.ICCategory
                |> Seq.fold (fun d x -> Path.Combine(d, x)) bD
                |> Some

        static member Create(c, v) =
            {
                ICCategory = List.ofSeq c
                ICVisualStudio = v
            }

    type Archive =
        {
            ArchKind : TemplateKind
            ArchZip : Content
            ArchZipFileName : string
        }

        member this.Install(config: InstallConfig) =
            match config.Locate(this.ArchKind) with
            | None -> false
            | Some dir -> this.WriteToDirectory(dir); true

        member this.WriteToDirectory(fullPath: string) =
            Path.Combine(fullPath, this.ArchZipFileName)
            |> this.ArchZip.WriteFile

        member this.Uninstall(config: InstallConfig) =
            match config.Locate(this.ArchKind) with
            | None -> false
            | Some dir ->
                let path = Path.Combine(dir, this.ArchZipFileName)
                let f = FileInfo(path)
                if f.Exists
                    then f.Delete(); true
                    else false

        member this.ZipContent =
            this.ArchZip

        member this.ZipFileName =
            this.ArchZipFileName

        static member FromFile(kind, fullPath: string) =
            {
                ArchKind = kind
                ArchZipFileName = Path.GetFileName(fullPath)
                ArchZip = Content.ReadBinaryFile(fullPath)
            }

        static member Project(p: ProjectTemplate)  =
            let zipName = Path.ChangeExtension(p.PTProject.ProjectFileName, ".zip")
            let xml = p.ToXml().Write()
            let icon = p.PTTemplateData.TDIcon
            let zip =
                seq {
                    yield (p.PTProject.ProjectFileName, p.PTProject.ProjectContent)
                    yield (Path.ChangeExtension(p.PTProject.ProjectFileName, ".vstemplate"), Content.FromText(xml))
                    yield (icon.IconFileName, icon.IconContent)
                    for e in p.PTProject.ProjectElements do
                        yield! computeEntries "." e
                }
                |> Content.Zip
            {
                ArchKind = ProjectTemplateKind
                ArchZip = zip
                ArchZipFileName = zipName
            }

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

/// Attempt to target the 2010 format:
/// http://msdn.microsoft.com/en-us/library/vstudio/dd393754(v=vs.100).aspx
module Extensions =
    open System
    open System.Globalization
    open System.IO
    open System.Text
    module X = IntelliFactory.Core.XmlTools
    module NG = IntelliFactory.VisualStudioTools.NuGet
    type Content = IntelliFactory.VisualStudioTools.Utils.Content

    let ( +/ ) a b =
        Path.Combine(a, b)

    let xmlNamespace =
        "http://schemas.microsoft.com/developer/vsx-schema/2010"

    let xmlElement name =
        X.XmlElement.Create(name, xmlNamespace)

    type ElementBuilder =
        | E

        static member ( ? ) (self: ElementBuilder, name) =
            xmlElement name

    type AttributeBuilder =
        | A

        static member ( ? ) (self: AttributeBuilder, name: string) =
            fun (value: string) -> (name, value)

    /// See <http://msdn.microsoft.com/en-us/library/dd997170.aspx>
    let inferContentTypeByExtension (ext: string) =
        match ext.ToLower().TrimStart('.') with
        | "txt" -> "text/plain"
        | "pkgdef" -> "text/plain"
        | "xml" -> "text/xml"
        | "vsixmanifest" -> "text/xml"
        | "htm" | "html" -> "text/html"
        | "rtf" -> "application/rtf"
        | "pdf" -> "application/pdf"
        | "gif" -> "image/gif"
        | "jpg" | "jpeg" -> "image/jpg"
        | "tiff" -> "image/tiff"
        | "vsix" | "zip" -> "application/zip"
        | _ -> "application/octet-stream"

    let generateContentTypesXml (paths: seq<string>) =
        let extensions =
            paths
            |> Seq.map Path.GetExtension
            |> Seq.distinct
            |> Seq.append [".xml"; ".vsixmanifest"]
        let e name = X.XmlElement.Create(name, "http://schemas.openxmlformats.org/package/2006/content-types")
        e "Types" -< [
            for ex in extensions do
                let ext = ex.TrimStart('.')
                match ext.Trim() with
                | "" -> ()
                | ext ->
                    yield e "Default" + [
                        "Extension", ext
                        "ContentType", inferContentTypeByExtension ext
                    ]
        ]
        |> fun e -> e.Write()

    type FrameworkVersion =
        | Net20
        | Net35
        | Net40
        | Net45

        member this.GetLiteral() =
            match this with
            | Net20 -> "2.0"
            | Net35 -> "3.5"
            | Net40 -> "4.0"
            | Net45 -> "4.5"

        override this.ToString() =
            this.GetLiteral()

    type SupportedFrameworks =
        {
            Max : FrameworkVersion
            Min : FrameworkVersion
        }

        static member Create(a, b) =
            {
                Min = a
                Max = b
            }

    type VSEdition =
        | VS_IntegratedShell
        | VS_ExpressAll
        | VS_Premium
        | VS_Pro
        | VS_Ultimate
        | VS_VBExpress
        | VS_VCExpress
        | VS_VCSExpress
        | VS_VWDExpress

        static member IntegratedShell = VS_IntegratedShell
        static member ExpressAll = VS_ExpressAll
        static member Premium = VS_Premium
        static member Pro = VS_Pro
        static member Ultimate = VS_Ultimate
        static member VBExpress = VS_VBExpress
        static member VCExpress = VS_VCExpress
        static member VCSExpress = VS_VCSExpress
        static member VWDExpress = VS_VWDExpress

        member this.GetLiteral() =
            match this with
            | VS_IntegratedShell -> "IntegratedShell"
            | VS_ExpressAll -> "Express_All"
            | VS_Premium -> "Premium"
            | VS_Pro -> "Pro"
            | VS_Ultimate -> "Ultimate"
            | VS_VBExpress -> "VBExpress"
            | VS_VCExpress -> "VCExpress"
            | VS_VCSExpress -> "VCSExpress"
            | VS_VWDExpress -> "VWDExpress"

        override this.ToString() =
            this.GetLiteral()

    type VSProduct =
        {
            VSP_Editions : list<VSEdition>
            VSP_Version : string
        }

        static member Create(v, e) =
            {
                VSP_Version = v
                VSP_Editions = List.ofSeq e
            }

        member this.ToXml() =
            E?VisualStudio + [A?Version this.VSP_Version] -< [
                for e in this.VSP_Editions ->
                    E?Edition -- e.GetLiteral()
            ]

    type IsolatedShellProduct =
        {
            ISP_Name : string
            ISP_Version : option<string>
        }

        static member Create(n, ?v) =
            { ISP_Version = v; ISP_Name = n }

        member this.ToXml() =
            let attrs =
                match this.ISP_Version with
                | None -> []
                | Some v -> [A?Version v]
            E?IsolatedShell + attrs -- this.ISP_Name

    type SupportedProduct =
        | SP_ISP of IsolatedShellProduct
        | SP_VS of VSProduct

        member this.ToXml() =
            match this with
            | SP_ISP x -> x.ToXml()
            | SP_VS x -> x.ToXml()

        static member IsolatedShell(p) =
            SP_ISP p

        static member VSProduct(p) =
            SP_VS p

    type IsolatedShellProduct with
        member this.AsSupportedProduct() =
            SupportedProduct.IsolatedShell(this)

    type VSProduct with
        member this.AsSupportedProduct() =
            SupportedProduct.VSProduct(this)

    type Identifier =
        {
            ID_Author : string
            ID_Culture : CultureInfo
            ID_Description : string
            ID_Id : Templates.ExtensionIdentity
            ID_Name : string
            ID_Products : list<SupportedProduct>
            ID_SupportedFrameworks : SupportedFrameworks
            ID_Version : Version
        }

        member this.WithProducts(ps) =
            { this with ID_Products = Seq.toList ps }

        member this.WithVersion(v) =
            { this with ID_Version = v }

        member this.ToXml() =
            E?Identifier + [A?Id this.ID_Id.FullName] - [
                E?Author -- this.ID_Author
                E?Description -- this.ID_Description
                E?Name -- this.ID_Name
                E?Locale -- string this.ID_Culture.LCID
                E?Version -- string this.ID_Version
                E?SupportedFrameworkRuntimeEdition + [
                    A?MinVersion (this.ID_SupportedFrameworks.Min.GetLiteral())
                    A?MaxVersion (this.ID_SupportedFrameworks.Max.GetLiteral())
                ]
                E?SupportedProducts -< [
                    for p in this.ID_Products ->
                        p.ToXml()
                ]
            ]

        static member Create(author, id, name, desc) =
            {
                ID_Author = author
                ID_Culture = CultureInfo.InvariantCulture
                ID_Description = desc
                ID_Id = id
                ID_Name = name
                ID_Products = []
                ID_SupportedFrameworks = { Min = FrameworkVersion.Net40; Max = FrameworkVersion.Net45 }
                ID_Version = Version "1.0.0.0"
            }

    type Assembly =
        {
            mutable A_Content : Content
            mutable A_Path : string
        }

        static member Create(p, c)  =
            {
                A_Content = c
                A_Path = p
            }

        member this.ToXml() =
            let name =
                let bytes = this.A_Content.GetBytes()
                let assem = System.Reflection.Assembly.ReflectionOnlyLoad(bytes)
                assem.GetName()
                |> string
            E?Assembly + [A?AssemblyName name] -- this.A_Path

    type CustomExtension =
        {
            CE_Content : Content
            CE_Path : string
            CE_Type : string
        }

        static member Create(p, c) =
            {
                CE_Content = c
                CE_Path = p
                CE_Type = Path.GetFileName(p)
            }

        static member NuGet(pkg: NG.Package) =
            let name = String.Format("{0}.{1}.nupkg", pkg.Id, pkg.Version)
            {
                CE_Content = pkg.Content
                CE_Path = Path.Combine("packages", name)
                CE_Type = name
            }

        member this.ToXml() =
            E?CustomExtension + [A?Type this.CE_Type] -- this.CE_Path

    type ProjectTemplate =
        {
            PT_Archive : Templates.Archive
            PT_Category : list<string>
            PT_Definition : Templates.ProjectTemplate
        }

        member this.ToXml() =
            E?ProjectTemplate -- "ProjectTemplates"

        member this.Content =
            this.PT_Archive.ZipContent

        member this.Path =
            Array.reduce ( +/ ) [|
                yield "ProjectTemplates"
                yield! this.PT_Category
                yield this.PT_Archive.ZipFileName
            |]

        static member Create(cat, d) =
            {
                PT_Archive = Templates.Archive.Project d
                PT_Category = Seq.toList cat
                PT_Definition = d
            }

    type MEFComponent =
        {
            MEF_Content : Content
            MEF_Path : string
        }

        member this.ToXml() =
            E?MEFComponent -- this.MEF_Path

        static member Create(p, c) =
            {
                MEF_Content = c
                MEF_Path = p
            }

    type ToolboxControl =
        {
            TC_Content : Content
            TC_Path : string
        }

        member this.ToXml() =
            E?ToolboxControl -- this.TC_Path

        static member Create(p, c) =
            {
                TC_Content = c
                TC_Path = p
            }

    type VSPackage =
        {
            VSP_Content : Content
            VSP_Path : string
        }

        member this.ToXml() =
            E?VSPackage -- this.VSP_Path

        static member Create(p, c) =
            {
                VSP_Content = c
                VSP_Path = p
            }

    type VsixContent =
        | AssemblyContent of Assembly
        | CustomExtensionContent of CustomExtension
        | MEFComponentContent of MEFComponent
        | ProjectTemplateContent of ProjectTemplate
        | ToolboxControlContent of ToolboxControl
        | VSPackageContent of VSPackage

        member this.Content =
            match this with
            | AssemblyContent x -> x.A_Content
            | CustomExtensionContent x -> x.CE_Content
            | MEFComponentContent x -> x.MEF_Content
            | ProjectTemplateContent x -> x.Content
            | ToolboxControlContent x -> x.TC_Content
            | VSPackageContent x -> x.VSP_Content

        member this.Path =
            match this with
            | AssemblyContent x -> x.A_Path
            | CustomExtensionContent x -> x.CE_Path
            | MEFComponentContent x -> x.MEF_Path
            | ProjectTemplateContent x -> x.Path
            | ToolboxControlContent x -> x.TC_Path
            | VSPackageContent x -> x.VSP_Path

        member this.ToXml() =
            match this with
            | AssemblyContent x -> x.ToXml()
            | CustomExtensionContent x -> x.ToXml()
            | MEFComponentContent x -> x.ToXml()
            | ProjectTemplateContent x -> x.ToXml()
            | ToolboxControlContent x -> x.ToXml()
            | VSPackageContent x -> x.ToXml()

        static member ProjectTemplate(cat, t) =
            ProjectTemplateContent (ProjectTemplate.Create(cat, t))

    /// The `<Vsix>` element.
    type Vsix =
        {
            Vsix_Identifier : Identifier
            Vsix_Contents : list<VsixContent>
        }

        static member Create(id, c) =
            {
                Vsix_Identifier = id
                Vsix_Contents = List.ofSeq c
            }

        member this.GetPackages() =
            let identity = this.Vsix_Identifier.ID_Id
            [
                for c in this.Vsix_Contents do
                    match c with
                    | VsixContent.ProjectTemplateContent t ->
                        match t.PT_Definition.NuGetPackages with
                        | None -> ()
                        | Some packages ->
                            if identity <> packages.ExtensionIdentity then
                                failwith "Invalid NuGet package container identity"
                            yield! packages.Packages
                    | _ -> ()
            ]
            |> Seq.distinctBy (fun pkg -> (pkg.Id, pkg.Version))

        member this.GetContentsAndPackages() =
            this.Vsix_Contents @ [
                for p in this.GetPackages() ->
                    CustomExtension.NuGet p
                    |> CustomExtensionContent
            ]

        member this.GetEntries() =
            [for c in this.GetContentsAndPackages() -> (c.Path, c.Content)]

        member this.GetPaths() =
            [for c in this.GetContentsAndPackages() -> c.Path]

        member this.ToXml() =
            E?Vsix + [A?Version "1.0.0"] -< [
                yield this.Vsix_Identifier.ToXml()
                yield
//                    if Seq.isEmpty (this.GetPackages()) then
                        E?References
//                    else
//                        E?References - [
//                            E?Reference
//                                + [
//                                    A?Id "NuPackToolsVsix.Microsoft.67e54e40-0ae3-42c5-a949-fddf5739e7a5"
//                                    A?MinVersion "1.7.30402.9028"
//                                ]
//                                - [
//                                    E?Name -- "NuGet Package Manager"
//                                    E?MoreInfoUrl -- "http://docs.nuget.org/"
//                                ]
//                        ]
                yield E?Content -< [for x in this.GetContentsAndPackages() -> x.ToXml()]
            ]

    /// Represents an in-memory VSIX package.
    type VsixFile =
        {
            VFFileName : string
            VFZip : Templates.Content
        }

        member this.FileName =
            this.VFFileName

        member this.WriteToDirectory(fullPath) =
            this.VFZip.WriteFile(Path.Combine(fullPath, this.VFFileName))

        static member Create(fileName: string, vsix: Vsix) =
            let zip =
                seq {
                    yield ("extension.vsixmanifest",
                        vsix.ToXml().Write()
                        |> Content.FromText)
                    yield ("[Content_Types].xml",
                        vsix.GetPaths()
                        |> generateContentTypesXml
                        |> Content.FromText)
                    yield! vsix.GetEntries()
                }
                |> Content.Zip
            {
                VFFileName = Path.GetFileName(fileName)
                VFZip = zip
            }

        static member FromFile(fullPath) =
            {
                VFFileName = Path.GetFileName(fullPath)
                VFZip = Content.ReadBinaryFile(fullPath)
            }


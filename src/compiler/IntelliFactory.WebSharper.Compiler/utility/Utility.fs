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

namespace IntelliFactory.WebSharper.Compiler

module M = IntelliFactory.WebSharper.Core.Metadata

module internal Utility =

    /// Reads a manifest resource from an assembly that defines the given type.
    let ReadResourceFromAssembly (ty: Type) (name: string) =
        ty.Assembly.GetManifestResourceNames()
        |> Seq.tryFind (fun x -> x.Contains(name))
        |> Option.bind (fun name ->
            use s = ty.Assembly.GetManifestResourceStream(name)
            use r = new StreamReader(s)
            Some (r.ReadToEnd()))

    /// Like `ReadResourceFromAssembly`, but checks if it is marked as WebResource.
    /// Returns a tuple of resource contents and its content-type.
    let ReadWebResource (ty: Type) (name: string) =
        try
            let content = defaultArg (ReadResourceFromAssembly ty name) ""
            let contentType =
                let cT =
                    CustomAttributeData.GetCustomAttributes(ty.Assembly)
                    |> Seq.tryPick (fun attr ->
                        if attr.Constructor.DeclaringType = typeof<System.Web.UI.WebResourceAttribute> then
                            match [for a in attr.ConstructorArguments -> a.Value] with
                            | [(:? string as n); (:? string as contentType)] ->
                                if n.Contains(name)
                                    then Some contentType
                                    else None
                            | _ -> None
                        else None)
                defaultArg cT "text/plain"
            (content, contentType)
        with e ->
            ("", "text/plain")

    /// Writes WebSharper startup code to a text writer.
    let WriteStartCode (withScript: bool) (writer: TextWriter) =
        writer.WriteLine()
        if withScript then
            writer.WriteLine("<script type='text/javascript'>")
        writer.WriteLine @"if (typeof IntelliFactory !=='undefined')"
        writer.WriteLine @"  IntelliFactory.Runtime.Start();"
        if withScript then
            writer.WriteLine("</script>")

    /// Reads Core.Metadata.AssemblyInfo from a Mono.Cecil assembly.
    let ReadAssemblyInfo (a: Mono.Cecil.AssemblyDefinition) =
        let key = M.AssemblyInfo.EmbeddedResourceName
        a.MainModule.Resources
        |> Seq.tryPick (function
            | :? Mono.Cecil.EmbeddedResource as r when r.Name = key ->
                use s = r.GetResourceStream()
                try
                    Some (M.AssemblyInfo.FromStream s)
                with e ->
                    failwithf "Failed to read assembly metadata for: %s" a.FullName
            | _ -> None)

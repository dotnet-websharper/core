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

/// Exposes the compiler front-end for programmatic use.
namespace IntelliFactory.WebSharper.Compiler

[<Sealed>]
type BundleCommand() =
    member val AssemblyPaths : seq<string> = Seq.empty with get, set
    member val FileName = "Bundle" with get, set
    member val OutputDirectory = "." with get, set

    member this.Execute() =
        let bundle =
            Bundle.Create().WithDefaultReferences()
            |> (fun b ->
                (b, this.AssemblyPaths)
                ||> Seq.fold (fun b p -> b.WithAssembly(p)))
            |> (fun b -> b.WithTransitiveReferences())
        let write (c: Content) (ext: string) =
            c.WriteFile(Path.Combine(this.OutputDirectory, this.FileName + ext))
        write bundle.CSS ".css"
        write bundle.HtmlHeaders ".head.html"
        write bundle.JavaScriptHeaders ".head.js"
        write bundle.JavaScript ".js"
        write bundle.MinifiedJavaScript ".min.js"
        write bundle.TypeScript ".d.ts"

